#[macro_use]
extern crate bitflags;

mod registers;
mod flags;
mod ops;
mod parse;
mod macros;
mod instructions;
mod opcodes;
mod host;

use core::num::Wrapping;
use core::mem::swap;

use serde::{Serialize, Deserialize};
#[allow(unused_imports)]
use log::{error, warn, info, debug, trace, log_enabled};
use arrayvec::ArrayVec;

use flags::*;
use registers::*;
use parse::*;
pub use host::*;
use host::cycles::*;
pub use parse::{Reg8, Reg16, Prefix};

pub const NMI_RESTART: u16 = 0x66;

pub type CpuDebugCode = ArrayVec::<[u8;4]>;
pub type CpuDebugFn = fn(Prefix, CpuDebugCode, &str, core::fmt::Arguments);

pub mod opconsts {
    pub const JP_OPCODE     : u8 = 0xC3;
    pub const RETI_OPCODE   : u8 = 0x4D;
    pub const RST_0H_OPCODE : u8 = 0xC7;
    pub const RST_8H_OPCODE : u8 = 0xCF;
    pub const RST_10H_OPCODE: u8 = 0xD7;
    pub const RST_18H_OPCODE: u8 = 0xDF;
    pub const RST_20H_OPCODE: u8 = 0xE7;
    pub const RST_28H_OPCODE: u8 = 0xEF;
    pub const RST_30H_OPCODE: u8 = 0xF7;
    pub const RST_38H_OPCODE: u8 = 0xFF;
}

enum LoopExitReason {
    LimitReached,
    WriteIo,
    Halt,
    EnableInt,
    Irq
}
// https://faqwiki.zxnet.co.uk/wiki/ULAplus
// https://spectrumforeveryone.com/technical/zx-spectrum-ula-types/
// https://github.com/fabriziotappero/ip-cores/blob/video_controller_ula_chip_for_zx_spectrum/fpga_version/rtl/ula.v
// http://www.piclist.com/techref/mem/dram/slide4.html
// https://scratchpad.fandom.com/wiki/Contended_memory
// https://en.wikipedia.org/wiki/Zilog_Z80
// http://www.worldofspectrum.org/faq/reference/128kreference.htm
// http://www.worldofspectrum.org/faq/reference/48kreference.htm
// https://maker.pro/pic/projects/z80-computer-project-part-1-the-cpu
// https://spectrumforeveryone.com/technical/memory-contention-floating-bus/
// https://spectrumforeveryone.com/technical/spectrum-compatibility-issues/

#[derive(Clone,Serialize,Deserialize,Default,PartialEq,Eq)]
pub struct Cpu {
    af: RegisterPair,
    af_alt: RegisterPair,
    regs: GeneralRegisters,
    regs_alt: GeneralRegisters,
    index: IndexRegisters,
    pc: RegisterPair,
    sp: RegisterPair,
    memptr: RegisterPair,
    last_ei: bool,
    ir: RegisterPair,
    im: InterruptMode,
    iff1: bool,
    iff2: bool,
    halt: bool,
    prefix: Prefix,
    r: Wrapping<u8>
}

impl core::fmt::Debug for Cpu {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "Cpu {{ pc: {:?}, sp: {:?}, \
            af: {:?}, bc: {:?}, de: {:?}, hl: {:?}, \
            af': {:?}, bc': {:?}, de': {:?}, hl': {:?}, \
            ix: {:?}, iy: {:?}, ir: {:?}, r: {:02X}, im: {}, \
            mp: {:?}, iff1: {}, iff2: {}, halt: {}, ei:{}, prefix: {:02X} }} \
            f: {:?}",
             self.pc, self.sp, 
             self.af, self.regs.bc, self.regs.de, self.regs.hl,
             self.af_alt, self.regs_alt.bc, self.regs_alt.de, self.regs_alt.hl,
             self.index.ix, self.index.iy, self.ir, self.r.0 & 0x7F, self.im as u8,
             self.memptr, self.iff1 as u8, self.iff2 as u8, self.halt as u8,
             self.last_ei, self.prefix as u8, self.get_flags())
    }
}

/// Returns `true` if the interrupt should be accepted based on the prefix and t-state counter of the last ei instruction.
/// Both NMI and IRQ should be accepted only if this condition is `true`.
#[inline(always)]
fn is_int_allowed(prefix: Prefix, last_ei: bool) -> bool {
    match prefix {
        Prefix::None => !last_ei,
        _ => false
    }
}

impl Cpu {
    pub fn reset(&mut self) {
        self.af.set16(0xFFFF);
        self.af_alt.set16(0xFFFF);
        self.regs.bc.set16(0);
        self.regs.de.set16(0);
        self.regs.hl.set16(0);
        self.regs_alt.bc.set16(0);
        self.regs_alt.de.set16(0);
        self.regs_alt.hl.set16(0);
        self.index.ix.set16(0);
        self.index.iy.set16(0);
        self.pc.set16(0);
        self.sp.set16(0xFFFF);
        self.memptr.set16(0);
        self.last_ei = false;
        self.ir.set16(0);
        self.im = InterruptMode::Mode0;
        self.iff1 = false;
        self.iff2 = false;
        self.halt = false;
        self.prefix = Prefix::None;
        self.r = Wrapping(0);
    }

    #[inline]
    pub fn get_pc(&self) -> u16 {
        self.pc.get16()
    }

    #[inline]
    pub fn set_pc(&mut self, pc: u16) {
        self.pc.set16(pc)
    }

    #[inline]
    pub fn get_sp(&self) -> u16 {
        self.sp.get16()
    }

    #[inline]
    pub fn set_sp(&mut self, sp: u16) {
        self.sp.set16(sp)
    }

    #[inline]
    pub fn get_af(&self) -> u16 {
        self.af.get16()
    }

    #[inline]
    pub fn set_af(&mut self, sp: u16) {
        self.af.set16(sp)
    }

    #[inline]
    pub fn get_im(&self) -> InterruptMode {
        self.im
    }

    #[inline]
    pub fn set_im(&mut self, im: InterruptMode) {
        self.im = im;
    }

    #[inline]
    pub fn get_iffs(&self) -> (bool, bool) {
        (self.iff1, self.iff2)
    }

    #[inline]
    pub fn set_iffs(&mut self, iff1: bool, iff2: bool) {
        self.iff1 = iff1;
        self.iff2 = iff2;
    }

    #[inline]
    pub fn get_prefix(&self) -> Prefix {
        self.prefix
    }

    #[inline]
    pub fn is_after_prefix(&self) -> bool {
        self.prefix != Prefix::None
    }

    #[inline]
    pub fn is_after_ei(&self) -> bool {
        self.last_ei
    }

    #[inline]
    pub fn get_memptr(&self) -> u16 {
        self.memptr.get16()
    }

    #[inline]
    pub fn set_memptr(&mut self, memptr: u16) {
        self.memptr.set16(memptr)
    }

    #[inline(always)]
    pub fn inc_r(&mut self) {
        self.r += Wrapping(1);
    }

    #[inline]
    pub fn set_r(&mut self, r: u8) {
        self.r = Wrapping(r);
        self.ir.set8lo(r);
    }

    #[inline]
    pub fn get_r(&self) -> u8 {
        let r = self.ir.get8lo();
        r & 0x80 | self.r.0 & 0x007F
    }

    pub fn normalize_r(&mut self) {
        self.set_r(self.get_r());
    }

    #[inline]
    pub fn get_ir(&mut self) -> u16 {
        let ir = self.ir.get16();
        ir & 0xFF80 | self.r.0 as u16 & 0x007F
    }

    #[inline]
    pub fn set_i(&mut self, i: u8) {
        self.ir.set8hi(i);
    }

    #[inline]
    pub fn get_i(&mut self) -> u8 {
        self.ir.get8hi()
    }

    #[inline(always)]
    pub fn is_irq_possible(&self) -> bool {
        self.iff1 && is_int_allowed(self.prefix, self.last_ei)
    }

    /// If the interrupt could not be accepted at this moment returns None.
    /// Err indicates OUT instruction aborted execution, probably due to the need of Contention to be changed.
    /// This is only possible in interrupt mode 0 and if the OUT instruction 0xD3 was placed on the bus.
    pub fn irq<M, T, F>(&mut self, control: &mut M, tsc: T, debug: Option<F>) -> Option<Result<T, T>>
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>, T: TstateCounter,
          F: FnOnce(Prefix, CpuDebugCode, &str, core::fmt::Arguments)
    {
        if self.is_irq_possible() {
            Some(self.irq_no_check::<M,T,F>(control, tsc, debug))
        }
        else {
            None
        }
    }

    /// Force IRQ
    /// Err indicates OUT instruction aborted execution, probably due to the need of Contention to be changed.
    fn irq_no_check<M, T, F>(&mut self, control: &mut M, mut tsc: T, debug: Option<F>) -> Result<T, T>
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>, T: TstateCounter,
          F: FnOnce(Prefix, CpuDebugCode, &str, core::fmt::Arguments)
    {
        self.di();
        self.inc_r();
        let mut pc = self.pc.get16();
        if self.halt {
            self.halt = false;
            pc = pc.wrapping_add(1);
            self.pc.set16(pc);
        }
        tsc.add_irq(pc);
        match self.im {
            InterruptMode::Mode0 => {
                let code = control.irq_data(pc, tsc.as_timestamp());
                let res = self.execute_instruction::<M,T,F>(control, tsc, debug, code);
                if self.halt {
                    // HALT decreases PC we don't want that here
                    self.pc.set16(self.pc.get16().wrapping_add(1));
                }
                res
            }
            InterruptMode::Mode1 => {
                self.execute_instruction::<M,T,F>(control, tsc, debug, opconsts::RST_38H_OPCODE)
            }
            InterruptMode::Mode2 => {
                let ir = self.get_ir();
                tsc.add_no_mreq(ir, 1); // ir:1
                push16!(pc; self, control, tsc); // sp-1:3, sp-2:3
                let vaddr = ir & 0xFF00 | control.irq_data(pc, tsc.as_timestamp()) as u16;
                self.pc.set16(vaddr);
                self.execute_instruction::<M,T,F>(control, tsc, debug, opconsts::JP_OPCODE) // pc+1:3,pc+2:3
            }
        }
    }

    /// NMI
    /// If the interrupt could not be accepted at this moment returns None.
    pub fn nmi<M, T>(&mut self, control: &mut M, tsc: T) -> Option<T>
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>, T: TstateCounter
    {
        if is_int_allowed(self.prefix, self.last_ei) {
            Some(self.nmi_no_check::<M,T>(control, tsc))
        }
        else {
            None
        }
    }

    /// Force NMI
    fn nmi_no_check<M, T>(&mut self, control: &mut M, mut tsc: T) -> T
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>, T: TstateCounter
    {
        self.last_ei = false;
        self.iff1 = false;
        self.inc_r();
        let mut pc = self.pc.get16();
        if self.halt {
            self.halt = false;
            pc = pc.wrapping_add(1);
        }
        tsc.add_mreq(pc, M1_CYCLE); // pc:4
        tsc.add_no_mreq(self.get_ir(), 1); // ir:1
        push16!(pc; self, control, tsc); // sp-1:3, sp-2:3
        self.pc.set16(NMI_RESTART);
        tsc
    }

    #[inline]
    fn restore_iff(&mut self) {
        self.iff1 = self.iff2;
    }

    #[inline]
    pub fn di(&mut self) {
        self.iff1 = false;
        self.iff2 = false;
    }

    #[inline]
    pub fn ei(&mut self) {
        self.iff1 = true;
        self.iff2 = true;
        self.last_ei = true;
    }

    pub fn halt(&mut self) {
        self.halt = true;
    }

    pub fn is_halt(&self) -> bool {
        self.halt
    }

    fn im(&mut self, mode: InterruptMode) {
        self.im = mode;
    }

    #[inline(always)]
    pub fn get_flags(&self) -> CpuFlags {
        CpuFlags::from_bits_truncate(self.af.get8lo())
    }

    #[inline]
    pub fn set_flags(&mut self, flags: CpuFlags) {
        self.af.set8lo(flags.bits());
    }

    #[inline]
    pub fn ex_af_af(&mut self, flags: CpuFlags) -> CpuFlags {
        let a = self.af.get8hi();
        self.af = self.af_alt;
        self.af_alt.set(a, flags.bits());
        self.get_flags()
    }

    #[inline]
    pub fn exx(&mut self) {
        swap(&mut self.regs, &mut self.regs_alt);
    }

    #[inline]
    pub fn ex_de_hl(&mut self) {
        swap(&mut self.regs.de, &mut self.regs.hl);
    }

    #[inline]
    pub fn load8(&mut self, dst: Reg8, src: Reg8, prefix: Prefix) {
        self.set8(dst, self.get8(src, prefix), prefix)
    }

    #[inline]
    pub fn get8(&self, src: Reg8, prefix: Prefix) -> u8 {
        match src {
            Reg8::B => self.regs.bc.get8hi(),
            Reg8::C => self.regs.bc.get8lo(),
            Reg8::D => self.regs.de.get8hi(),
            Reg8::E => self.regs.de.get8lo(),
            Reg8::H => match prefix {
                Prefix::None => self.regs.hl.get8hi(),
                Prefix::Xdd => self.index.ix.get8hi(),
                Prefix::Yfd => self.index.iy.get8hi(),
            },
            Reg8::L => match prefix {
                Prefix::None => self.regs.hl.get8lo(),
                Prefix::Xdd => self.index.ix.get8lo(),
                Prefix::Yfd => self.index.iy.get8lo(),
            }
            Reg8::A => self.af.get8hi(),
        }
    }

    #[inline]
    unsafe fn reg8_ptr(&mut self, tgt: Reg8, prefix: Prefix) -> *mut u8 {
        match tgt {
            Reg8::B => self.regs.bc.ptr8hi(),
            Reg8::C => self.regs.bc.ptr8lo(),
            Reg8::D => self.regs.de.ptr8hi(),
            Reg8::E => self.regs.de.ptr8lo(),
            Reg8::H => match prefix {
                Prefix::None => self.regs.hl.ptr8hi(),
                Prefix::Xdd => self.index.ix.ptr8hi(),
                Prefix::Yfd => self.index.iy.ptr8hi(),
            }
            Reg8::L => match prefix {
                Prefix::None => self.regs.hl.ptr8lo(),
                Prefix::Xdd => self.index.ix.ptr8lo(),
                Prefix::Yfd => self.index.iy.ptr8lo(),
            }
            Reg8::A => self.af.ptr8hi()
        }
    }

    #[inline]
    pub fn set8(&mut self, dst: Reg8, val: u8, prefix: Prefix) {
        unsafe {
            *self.reg8_ptr(dst, prefix) = val;
        }
    }

    #[inline]
    pub fn op8<F>(&mut self, tgt: Reg8, prefix: Prefix, op: F)
    where F: FnOnce(u8) -> u8
    {
        unsafe {
            let ptr = self.reg8_ptr(tgt, prefix);
            *ptr = op(*ptr);
        }
    }

    #[inline]
    fn reg16_ref(&self, src: Reg16) -> &RegisterPair {
        match src {
            Reg16::BC => &self.regs.bc,
            Reg16::DE => &self.regs.de,
            Reg16::HL => &self.regs.hl,
            Reg16::SP => &self.sp
        }        
    }


    #[inline]
    pub fn get16(&self, src: Reg16) -> u16 {
        self.reg16_ref(src).get16()
    }

    #[inline]
    pub fn get2(&self, src: Reg16) -> (u8, u8) {
        self.reg16_ref(src).get()
    }

    #[inline]
    pub fn get_prefix16(&self, src: Reg16, prefix: Prefix) -> u16 {
        match src {
            Reg16::BC => self.regs.bc.get16(),
            Reg16::DE => self.regs.de.get16(),
            Reg16::HL => self.get_index16(prefix),
            Reg16::SP => self.sp.get16()
        }        
    }

    #[inline]
    fn index16_ref(&self, prefix: Prefix) -> &RegisterPair {
        match prefix {
            Prefix::Xdd => &self.index.ix,
            Prefix::Yfd => &self.index.iy,
            Prefix::None => &self.regs.hl,
        }        
    }

    #[inline]
    pub fn get_index16(&self, prefix: Prefix) -> u16 {
        self.index16_ref(prefix).get16()
    }

    #[inline]
    pub fn get_index2(&self, prefix: Prefix) -> (u8, u8) {
        self.index16_ref(prefix).get()
    }

    #[inline]
    fn reg16_mut(&mut self, src: Reg16) -> &mut RegisterPair {
        match src {
            Reg16::BC => &mut self.regs.bc,
            Reg16::DE => &mut self.regs.de,
            Reg16::HL => &mut self.regs.hl,
            Reg16::SP => &mut self.sp
        }        
    }

    #[inline]
    pub fn set16(&mut self, src: Reg16, val: u16) {
        self.reg16_mut(src).set16(val)
    }

    #[inline]
    fn index16_mut(&mut self, prefix: Prefix) -> &mut RegisterPair {
        match prefix {
            Prefix::Xdd => &mut self.index.ix,
            Prefix::Yfd => &mut self.index.iy,
            Prefix::None => &mut self.regs.hl,
        }        
    }

    #[inline]
    pub fn set_index16(&mut self, prefix: Prefix, val: u16) {
        self.index16_mut(prefix).set16(val)
    }

    fn block_transfer<M, T>(&mut self, control: &mut M, tsc: &mut T, flags: &mut CpuFlags, delta: BlockDelta, pc: Option<Wrapping<u16>>) -> Option<Wrapping<u16>>
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>, T: TstateCounter
    { // hl:3, de:3, de:1 x 2 [de:1 x 5]
        let hl = self.regs.hl.get16();
        let de = self.regs.de.get16();
        let val = control.read_mem(hl, tsc.add_mreq(hl, MEMRW_CYCLE));
        control.write_mem(de, val, tsc.add_mreq(de, MEMRW_CYCLE));
        tsc.add_no_mreq(de, 2);
        self.regs.hl.set16(hl.wrapping_add(delta as u16));
        self.regs.de.set16(de.wrapping_add(delta as u16));
        let is_over = self.regs.bc.dec16_is_zero();
        ops::ldx(self.af.get8hi(), val, is_over, flags);
        if let Some(pc) = pc {
            if !is_over {
                tsc.add_no_mreq(de, 5);
                // MEMPTR = PC + 1
                self.memptr.set16((pc - Wrapping(1)).0);
                return Some(pc - Wrapping(2))
            }
        }
        None
    }

    fn block_search<M, T>(&mut self, control: &mut M, tsc: &mut T, flags: &mut CpuFlags, delta: BlockDelta, pc: Option<Wrapping<u16>>) -> Option<Wrapping<u16>>
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>, T: TstateCounter
    { // hl:3, hl:1 x 5, [hl:1 x 5]
        let hl = self.regs.hl.get16();
        let val = control.read_mem(hl, tsc.add_mreq(hl, MEMRW_CYCLE));
        tsc.add_no_mreq(hl, 5);
        self.regs.hl.set16(hl.wrapping_add(delta as u16));
        let is_over = ops::cpx( self.af.get8hi(),
                                val,
                                self.regs.bc.dec16_is_zero(),
                                flags);
        if let Some(pc) = pc {
            if !is_over {
                tsc.add_no_mreq(hl, 5);
                // MEMPTR = PC + 1
                self.memptr.set16((pc - Wrapping(1)).0);
                return Some(pc - Wrapping(2))
            }
        }
        // MEMPTR = MEMPTR +/- 1
        self.memptr.add16(delta as u16);
        None
    }

    fn block_in<M, T>(&mut self, control: &mut M, tsc: &mut T, flags: &mut CpuFlags, delta: BlockDelta, pc: Option<Wrapping<u16>>) -> Option<Wrapping<u16>>
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>, T: TstateCounter
    { // ir:1, IO, hl:3, [hl:1 x 5]
        tsc.add_no_mreq(self.get_ir(), 1);
        let bc = self.regs.bc.get16();
        let val = control.read_io(bc, tsc.add_io(bc));
        let hl = self.regs.hl.get16();
        control.write_mem(hl, val, tsc.add_mreq(hl, MEMRW_CYCLE));
        // MEMPTR = BC_before_decrementing_B +/- 1
        self.memptr.set16(bc.wrapping_add(delta as u16));
        let b = ((bc >> 8) as u8).wrapping_sub(1);
        ops::iox(val, b, (bc as u8).wrapping_add(delta as u8), flags);
        self.regs.bc.set8hi(b);
        self.regs.hl.set16(hl.wrapping_add(delta as u16));
        if let Some(pc) = pc {
            if b != 0 {
                tsc.add_no_mreq(hl, 5);
                return Some(pc - Wrapping(2))
            }
        }
        None
    }

    fn block_out<M, T>(&mut self, control: &mut M, tsc: &mut T, flags: &mut CpuFlags, delta: BlockDelta, pc: Option<Wrapping<u16>>) -> (bool, Option<Wrapping<u16>>)
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>, T: TstateCounter
    { // ir:1, hl:3, IO, [bc:1 x 5]
        tsc.add_no_mreq(self.get_ir(), 1);
        let hl = self.regs.hl.get16();
        let val = control.read_mem(hl, tsc.add_mreq(hl, MEMRW_CYCLE));
        let (b, c) = self.regs.bc.get();
        let hl1 = hl.wrapping_add(delta as u16);
        let b = b.wrapping_sub(1);
        ops::iox(val, b, hl1 as u8, flags);
        let bc = (b as u16) << 8 | c as u16;
        // MEMPTR = BC_after_decrementing_B +/- 1
        self.memptr.set16(bc.wrapping_add(delta as u16));
        let should_break = control.write_io(bc, val, tsc.add_io(bc));
        self.regs.bc.set8hi(b);
        self.regs.hl.set16(hl1);
        if let Some(pc) = pc {
            if b != 0 {
                tsc.add_no_mreq(bc, 5);
                return (should_break, Some(pc - Wrapping(2)))
            }
        }
        (should_break, None)
    }

    /// Err indicates Io has signalled on OUT instruction, e.g. due to the need of Contention to be changed.
    pub fn execute_instruction<M, T, F>(&mut self, control: &mut M, mut tsc: T, debug: Option<F>, code: u8) -> Result<T, T>
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>, T: TstateCounter,
          F: FnOnce(Prefix, CpuDebugCode, &str, core::fmt::Arguments)
    {
        self.last_ei = false;
        self.halt = false;
        let mut prefix = self.prefix;
        let mut pc = Wrapping(self.pc.get16());
        let mut flags = self.get_flags();

        let reason: LoopExitReason = 'main: loop {
            execute_instruction! {[code] debug; prefix, flags, pc, self, control, tsc; break 'main }
            break 'main LoopExitReason::LimitReached;
        };
        self.set_flags(flags);
        self.pc.set16(pc.0);
        self.prefix = prefix;
        match reason {
            LoopExitReason::WriteIo => Err(tsc),
            _ => Ok(tsc),
        }
    }

    /// Err indicates OUT instruction aborted execution, probably due to the need of Contention to be changed.
    pub fn execute_next<M, T, F>(&mut self, control: &mut M, mut tsc: T, debug: Option<F>) -> Result<T, T>
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>, T: TstateCounter,
          F: FnOnce(Prefix, CpuDebugCode, &str, core::fmt::Arguments)
    {
        if control.is_irq(tsc.as_timestamp()) && self.is_irq_possible() {
            return self.irq_no_check::<M,T,F>(control, tsc, debug);
        }
        if self.halt {
            tsc.add_mreq(self.pc.get16(), M1_CYCLE);
            self.inc_r();
            Ok(tsc)
        }
        else {
            let mut pc = Wrapping(self.pc.get16());
            let code: u8 = fetch_next_opcode!(self, control, pc, tsc);
            self.pc.set16(pc.0);
            self.execute_instruction::<M,T,F>(control, tsc, debug, code)
        }
    }

    /// Executes instructions until vertical counter reaches vc_limit or other conditions.
    /// Ok is returned only when limit is reached.
    /// Returns Err before limit has been reached when interrupt has been initiated or HALT instruction was executed.
    /// If limit has been reached despite of one of the above conditions Ok is being returned.
    /// When Io signalled to break on one of the OUT family instructions returns Err regardless if limit has been reached or not.
    pub fn execute_with_limit<M, T>(&mut self, control: &mut M, mut tsc: T, vc_limit: T::Limit) -> Result<T, T>
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>, T: TstateCounter
    {
        const DEBUG: Option<CpuDebugFn> = None;

        if self.halt {
            debug_assert_eq!(self.prefix, Prefix::None);
            debug_assert_eq!(self.last_ei, false);
            let pc = self.pc.get16();
            loop {
                if tsc.is_at_limit(vc_limit) {
                    return Ok(tsc);
                }
                if self.iff1 && control.is_irq(tsc.as_timestamp()) {
                    break
                } // else if !iff1 TODO: halt forever optimal
                tsc.add_mreq(pc, M1_CYCLE);
                self.inc_r();
            }
        }
        else if tsc.is_at_limit(vc_limit) {
            return Ok(tsc);
        }

        if self.last_ei {
            self.last_ei = false;
        }
        else if is_int_allowed(self.prefix, false) && self.iff1 && control.is_irq(tsc.as_timestamp()) {
            match self.irq_no_check::<M,T,_>(control, tsc, DEBUG) {
                Ok(t) => {
                    tsc = t;
                }
                Err(t) => return Err(t)
            }
            if tsc.is_at_limit(vc_limit) {
                return Ok(tsc);
            }
        }

        let mut prefix = self.prefix;
        let mut pc = Wrapping(self.pc.get16());
        let mut flags = self.get_flags();

        loop {
            let reason: LoopExitReason = 'main: loop {
                // can break 'main with Halt, WriteIo or EnableInt
                execute_next_instruction! { DEBUG; prefix, flags, pc, self, control, tsc; break 'main }

                if tsc.is_at_limit(vc_limit)  {
                    break 'main LoopExitReason::LimitReached
                }

                if is_int_allowed(prefix, false) && self.iff1 && control.is_irq(tsc.as_timestamp()) {
                    break 'main LoopExitReason::Irq
                }
            };

            match reason {
                LoopExitReason::EnableInt => {
                    if !tsc.is_at_limit(vc_limit)  {
                        self.last_ei = false;
                        continue // this way skipping irq check immediately after EI
                    }
                }
                _ => {}
            }

            self.set_flags(flags);
            self.pc.set16(pc.0);
            self.prefix = prefix;

            match reason {
                LoopExitReason::EnableInt|
                LoopExitReason::LimitReached => return Ok(tsc),
                LoopExitReason::WriteIo => return Err(tsc), // I/O
                LoopExitReason::Halt => {
                    if tsc.is_at_limit(vc_limit)  {
                        return Ok(tsc); // limit reached
                    }
                    else {
                        return Err(tsc); // halt
                    }
                },
                LoopExitReason::Irq => match self.irq_no_check::<M,T,_>(control, tsc, DEBUG) {
                    Ok(t) => {
                        if t.is_at_limit(vc_limit) {
                            return Ok(t);
                        }
                        tsc = t;
                        prefix = self.prefix;
                        pc = Wrapping(self.pc.get16());
                        flags = self.get_flags();
                        continue;
                    }
                    Err(t) => return Err(t) // I/O
                }
            }
        }
    }
}
