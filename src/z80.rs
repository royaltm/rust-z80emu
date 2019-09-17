//! A home of the Cpu implementations.
mod macros;
mod ops;
mod instructions;
mod opcodes;
mod internal;
mod debug;

use core::convert::TryFrom;
use core::num::Wrapping;
use core::mem::swap;

#[cfg(feature = "serde")]
use serde::{Serialize, Deserialize};

use crate::cpu::*;
use crate::host::*;
use crate::opconsts;
use internal::cycles::*;

enum LoopExitReason<O, R> {
    LimitReached,
    WriteIo(O),
    Reti(R),
    Halt,
    EnableInt,
    Irq
}
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Default, PartialEq, Eq)] // TODO: implement PartialEq and Eq with regard of R
pub struct Z80 {
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

/// Returns `true` if the interrupt should be accepted based on `prefix` and `last EI` state.
/// Both NMI and maskable interrupts should be accepted only if this condition is `true`.
#[inline(always)]
fn is_int_allowed(prefix: Prefix, last_ei: bool) -> bool {
    match prefix {
        Prefix::None => !last_ei,
        _ => false
    }
}

impl Z80 {
    /// Creates a new instance of Z80 with the state just after `RESET`.
    pub fn new() -> Self {
        let mut cpu = Self::default();
        cpu.reset();
        cpu
    }

    #[inline]
    pub fn get_memptr(&self) -> u16 {
        self.memptr.get16()
    }

    #[inline]
    pub fn set_memptr(&mut self, memptr: u16) {
        self.memptr.set16(memptr)
    }

    pub fn normalize_r(&mut self) {
        self.set_r(self.get_r());
    }
}

impl Cpu for Z80 {
    fn reset(&mut self) {
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
    fn get_pc(&self) -> u16 {
        self.pc.get16()
    }

    #[inline]
    fn set_pc(&mut self, pc: u16) {
        self.pc.set16(pc)
    }

    #[inline]
    fn get_sp(&self) -> u16 {
        self.sp.get16()
    }

    #[inline]
    fn set_sp(&mut self, sp: u16) {
        self.sp.set16(sp)
    }

    #[inline]
    fn get_af(&self) -> u16 {
        self.af.get16()
    }

    #[inline]
    fn set_af(&mut self, af: u16) {
        self.af.set16(af)
    }

    #[inline(always)]
    fn get_flags(&self) -> CpuFlags {
        CpuFlags::from_bits_truncate(self.af.get8lo())
    }

    #[inline]
    fn set_flags(&mut self, flags: CpuFlags) {
        self.af.set8lo(flags.bits());
    }

    #[inline(always)]
    fn inc_r(&mut self) {
        self.r += Wrapping(1);
    }

    #[inline]
    fn add_r(&mut self, delta: i32) {
        self.r += Wrapping(delta as u8);
    }

    #[inline]
    fn get_r(&self) -> u8 {
        let r = self.ir.get8lo();
        r & 0x80 | self.r.0 & 0x007F
    }

    #[inline]
    fn set_r(&mut self, r: u8) {
        self.r = Wrapping(r);
        self.ir.set8lo(r);
    }

    #[inline]
    fn get_i(&mut self) -> u8 {
        self.ir.get8hi()
    }

    #[inline]
    fn set_i(&mut self, i: u8) {
        self.ir.set8hi(i);
    }

    #[inline]
    fn get_iffs(&self) -> (bool, bool) {
        (self.iff1, self.iff2)
    }

    #[inline]
    fn set_iffs(&mut self, iff1: bool, iff2: bool) {
        self.iff1 = iff1;
        self.iff2 = iff2;
    }

    #[inline]
    fn halt(&mut self) {
        self.halt = true;
    }

    #[inline]
    fn is_halt(&self) -> bool {
        self.halt
    }

    #[inline]
    fn get_im(&self) -> InterruptMode {
        self.im
    }

    #[inline]
    fn set_im(&mut self, im: InterruptMode) {
        self.im = im;
    }

    #[inline]
    fn ex_af_af(&mut self) {
        swap(&mut self.af, &mut self.af_alt);
    }

    #[inline]
    fn exx(&mut self) {
        swap(&mut self.regs, &mut self.regs_alt);
    }

    #[inline]
    fn get_reg(&self, reg: Reg8, prefix: Prefix) -> u8 {
        match reg {
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
    fn set_reg(&mut self, dst: Reg8, prefix: Prefix, val: u8) {
        unsafe {
            *self.reg8_ptr(dst, prefix) = val;
        }        
    }

    #[inline]
    fn get_reg2(&self, src: Reg16) -> (u8, u8) {
        self.reg16_ref(src).get()
    }

    #[inline]
    fn get_reg16(&self, src: Reg16) -> u16 {
        self.reg16_ref(src).get16()
    }

    #[inline]
    fn set_reg16(&mut self, src: Reg16, val: u16) {
        self.reg16_mut(src).set16(val)
    }

    #[inline]
    fn get_index2(&self, prefix: Prefix) -> (u8, u8) {
        self.index16_ref(prefix).get()
    }

    #[inline]
    fn get_index16(&self, prefix: Prefix) -> u16 {
        self.index16_ref(prefix).get16()
    }

    #[inline]
    fn set_index16(&mut self, prefix: Prefix, val: u16) {
        self.index16_mut(prefix).set16(val)
    }

    #[inline(always)]
    fn is_irq_allowed(&self) -> bool {
        self.iff1 && is_int_allowed(self.prefix, self.last_ei)
    }

    #[inline(always)]
    fn is_nmi_allowed(&self) -> bool {
        is_int_allowed(self.prefix, self.last_ei)
    }

    #[inline]
    fn restore_iff1(&mut self) {
        self.iff1 = self.iff2;
    }

    #[inline]
    fn disable_interrupts(&mut self) {
        self.iff1 = false;
        self.iff2 = false;
    }

    #[inline]
    fn enable_interrupts(&mut self) {
        self.iff1 = true;
        self.iff2 = true;
        self.last_ei = true;
    }

    #[inline]
    fn is_after_ei(&self) -> bool {
        self.last_ei
    }

    #[inline]
    fn is_after_prefix(&self) -> bool {
        match self.prefix {
            Prefix::None => false,
            _ => true
        }
    }

    #[inline]
    fn get_prefix(&self) -> Prefix {
        self.prefix
    }

    fn irq<M, T, F>(&mut self, control: &mut M, tsc: &mut T, debug: Option<F>) -> Option<Result<M::WrIoBreak, M::RetiBreak>>
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>,
          T: Clock,
          F: FnOnce(CpuDebug)
    {
        if self.is_irq_allowed() {
            Some(self.irq_no_check::<M,T,F>(control, tsc, debug))
        }
        else {
            None
        }
    }

    fn nmi<M, T>(&mut self, control: &mut M, tsc: &mut T) -> bool
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>,
          T: Clock
    {
        if is_int_allowed(self.prefix, self.last_ei) {
            self.nmi_no_check::<M,T>(control, tsc);
            true
        }
        else {
            false
        }
    }

    fn execute_instruction<M, T, F>(&mut self, control: &mut M, tsc: &mut T, debug: Option<F>, code: u8) -> Result<M::WrIoBreak, M::RetiBreak>
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>,
          T: Clock,
          F: FnOnce(CpuDebug)
    {
        self.last_ei = false;
        self.halt = false;
        let mut prefix = self.prefix;
        let mut pc = Wrapping(self.pc.get16());
        let mut flags = self.get_flags();

        let reason: LoopExitReason<_,_> = 'main: loop {
            execute_instruction! {[code] debug; prefix, flags, pc, self, control, tsc; break 'main }
            break 'main LoopExitReason::LimitReached;
        };
        self.set_flags(flags);
        self.pc.set16(pc.0);
        self.prefix = prefix;
        match reason {
            LoopExitReason::Halt => Err(BreakCause::Halt),
            LoopExitReason::WriteIo(cause) => Err(BreakCause::WriteIo(cause)),
            LoopExitReason::Reti(cause) => Err(BreakCause::Reti(cause)),
            _ => Ok(()),
        }
    }

    fn execute_next<M, T, F>(&mut self, control: &mut M, tsc: &mut T, debug: Option<F>) -> Result<M::WrIoBreak, M::RetiBreak>
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>,
          T: Clock,
          F: FnOnce(CpuDebug)
    {
        if self.is_irq_allowed() && control.is_irq(tsc.as_timestamp()) {
            return self.irq_no_check::<M,T,F>(control, tsc, debug);
        }
        if self.halt {
            tsc.add_m1(self.pc.get16());
            self.inc_r();
            Ok(())
        }
        else {
            let mut pc = Wrapping(self.pc.get16());
            let code: u8 = fetch_next_opcode!(self, control, pc, tsc);
            self.pc.set16(pc.0);
            self.execute_instruction::<M,T,F>(control, tsc, debug, code)
        }
    }

    fn execute_with_limit<M, T>(&mut self, control: &mut M, tsc: &mut T, vc_limit: T::Limit) -> Result<M::WrIoBreak, M::RetiBreak>
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>,
          T: Clock
    {
        const DEBUG: Option<CpuDebugFn> = None;

        if self.halt {
            debug_assert_eq!(self.prefix, Prefix::None);
            debug_assert_eq!(self.last_ei, false);
            let pc = self.pc.get16();
            loop {
                if tsc.is_past_limit(vc_limit) {
                    return Ok(());
                }
                if self.iff1 && control.is_irq(tsc.as_timestamp()) {
                    break
                } // else if !iff1 TODO: halt forever optimal
                tsc.add_m1(pc);
                self.inc_r();
            }
        }
        else if tsc.is_past_limit(vc_limit) {
            return Ok(());
        }

        if self.last_ei {
            self.last_ei = false;
        }
        else if is_int_allowed(self.prefix, false) && self.iff1 && control.is_irq(tsc.as_timestamp()) {
            if let Err(cause) = self.irq_no_check::<M,T,_>(control, tsc, DEBUG) {
                return Err(cause);
            }
            if tsc.is_past_limit(vc_limit) {
                return Ok(());
            }
        }

        let mut prefix = self.prefix;
        let mut pc = Wrapping(self.pc.get16());
        let mut flags = self.get_flags();

        loop {
            let reason: LoopExitReason<_,_> = 'main: loop {
                // can break 'main with Halt, WriteIo, Reti or EnableInt
                execute_next_instruction! { DEBUG; prefix, flags, pc, self, control, tsc; break 'main }

                if tsc.is_past_limit(vc_limit)  {
                    break 'main LoopExitReason::LimitReached
                }

                if is_int_allowed(prefix, false) && self.iff1 && control.is_irq(tsc.as_timestamp()) {
                    break 'main LoopExitReason::Irq
                }
            };

            match reason {
                LoopExitReason::EnableInt => {
                    if !tsc.is_past_limit(vc_limit)  {
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
                LoopExitReason::LimitReached => return Ok(()),
                LoopExitReason::Halt => return Err(BreakCause::Halt),
                LoopExitReason::WriteIo(cause) => return Err(BreakCause::WriteIo(cause)),
                LoopExitReason::Reti(cause) => return Err(BreakCause::Reti(cause)),
                LoopExitReason::Irq => match self.irq_no_check::<M,T,_>(control, tsc, DEBUG) {
                    Ok(()) => {
                        if tsc.is_past_limit(vc_limit) {
                            return Ok(());
                        }
                        prefix = self.prefix;
                        pc = Wrapping(self.pc.get16());
                        flags = self.get_flags();
                        continue;
                    }
                    Err(cause) => return Err(cause)
                }
            }
        }
    }
}
