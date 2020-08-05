//! A home of the Cpu implementations.
#![allow(clippy::let_and_return)]
mod macros;
mod ops;
mod instructions;
mod opcodes;
mod internal;
mod flavours;
mod debug;
#[cfg(feature = "serde")]
mod serde;
pub mod any;

#[cfg(test)]
mod tests;

use core::num::Wrapping;
use core::mem::swap;

use crate::cpu::*;
use crate::host::*;
use crate::opconsts;
use internal::*;
use internal::cycles::*;

pub use flavours::*;
/// Emulates a Zilog's `NMOS Z80 CPU` via the [Cpu] trait.
pub type Z80NMOS = Z80<NMOS>;
/// Emulates a Zilog's `CMOS Z80 CPU` via the [Cpu] trait.
pub type Z80CMOS = Z80<CMOS>;
/// Emulates one of the clones (presumably the `KP1858BM1` or `T34BM1`) of the `Z80 CPU` via the [Cpu] trait.
pub type Z80BM1 = Z80<BM1>;

enum LoopExitReason<O, R> {
    LimitReached,
    WriteIo(O),
    Reti(R),
    Halt,
    EnableInt,
    Irq
}

/// Emulates a Zilog's `Z80 CPU` in various ["flavours"][crate::z80] via the [Cpu] trait.
#[derive(Clone, Default, PartialEq, Eq)]
pub struct Z80<Q: Flavour> {
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
    prefix: Option<Prefix>,
    r: Wrapping<u8>,
    /// Exposes an instance of the [Flavour] implementation.
    pub flavour: Q
}

/// Returns `true` if the interrupt should be accepted based on `prefix` and `last EI` state.
/// Both NMI and maskable interrupts should be accepted only if this condition is `true`.
#[inline(always)]
fn is_int_allowed(prefix: Option<Prefix>, last_ei: bool) -> bool {
    prefix.is_none() && !last_ei
}

impl<Q: Flavour> Z80<Q> {
    /// Creates a new instance of Z80 with the state just after `RESET`.
    pub fn new() -> Self {
        let mut cpu = Self::default();
        cpu.reset();
        cpu
    }

    /// Retrieves the internal state of the MEMPTR register.
    #[inline]
    pub fn get_memptr(&self) -> u16 {
        self.memptr.get16()
    }

    /// Changes the internal state of the MEMPTR register.
    #[inline]
    pub fn set_memptr(&mut self, memptr: u16) {
        self.memptr.set16(memptr)
    }

    /// The content of the `R` register is lazy evaluated when its value is being set or retrieved.
    /// This method normalizes the internal state of the `R` register, so e.g. two instances of
    /// [Z80] can be compared if they represent the same `CPU` state.
    pub fn normalize_r(&mut self) {
        self.set_r(self.get_r());
    }

    /// Converts between instances of [Z80] with different flavours.
    ///
    /// **NOTE**: Some [Flavour] related information may be lost during conversion.
    pub fn into_flavour<F: Flavour>(self) -> Z80<F>
        where F: From<Q>
    {
        Z80 {
            af: self.af,
            af_alt: self.af_alt,
            regs: self.regs,
            regs_alt: self.regs_alt,
            index: self.index,
            pc: self.pc,
            sp: self.sp,
            memptr: self.memptr,
            last_ei: self.last_ei,
            ir: self.ir,
            im: self.im,
            iff1: self.iff1,
            iff2: self.iff2,
            halt: self.halt,
            prefix: self.prefix,
            r: self.r,
            flavour: self.flavour.into()
        }
    }

    /// Converts between instances of [Z80] with different flavours.
    ///
    /// **NOTE**: Some [Flavour] related information may be lost during conversion.
    #[inline]
    pub fn from_flavour<F: Flavour>(cpu: Z80<F>) -> Self
        where Q: From<F>
    {
        cpu.into_flavour::<Q>()
    }
}

impl<Q: Flavour> Cpu for Z80<Q> {
    fn reset(&mut self) {
        self.af.set16(u16::max_value());
        self.af_alt.set16(u16::max_value());
        self.regs.bc.set16(0);
        self.regs.de.set16(0);
        self.regs.hl.set16(0);
        self.regs_alt.bc.set16(0);
        self.regs_alt.de.set16(0);
        self.regs_alt.hl.set16(0);
        self.index.ix.set16(0);
        self.index.iy.set16(0);
        self.pc.set16(0);
        self.sp.set16(u16::max_value());
        self.memptr.set16(0);
        self.last_ei = false;
        self.ir.set16(0);
        self.im = InterruptMode::Mode0;
        self.iff1 = false;
        self.iff2 = false;
        self.halt = false;
        self.prefix = None;
        self.r = Wrapping(0);
        self.flavour.reset();
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
    fn get_acc(&self) -> u8 {
        self.af.get8hi()
    }

    #[inline]
    fn set_acc(&mut self, val: u8) {
        self.af.set8hi(val)
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
        r & 0x80 | self.r.0 & 0x7F
    }

    #[inline]
    fn set_r(&mut self, r: u8) {
        self.r = Wrapping(r);
        self.ir.set8lo(r);
    }

    #[inline]
    fn get_i(&self) -> u8 {
        self.ir.get8hi()
    }

    #[inline]
    fn set_i(&mut self, i: u8) {
        self.ir.set8hi(i);
    }

    #[inline]
    fn get_ir(&self) -> u16 {
        let ir = self.ir.get16();
        ir & 0xFF80 | self.r.0 as u16 & 0x007F
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
    fn get_reg(&self, reg: Reg8, prefix: Option<Prefix>) -> u8 {
        match reg {
            Reg8::B => self.regs.bc.get8hi(),
            Reg8::C => self.regs.bc.get8lo(),
            Reg8::D => self.regs.de.get8hi(),
            Reg8::E => self.regs.de.get8lo(),
            Reg8::H => match prefix {
                None => self.regs.hl.get8hi(),
                Some(Prefix::Xdd) => self.index.ix.get8hi(),
                Some(Prefix::Yfd) => self.index.iy.get8hi(),
            },
            Reg8::L => match prefix {
                None => self.regs.hl.get8lo(),
                Some(Prefix::Xdd) => self.index.ix.get8lo(),
                Some(Prefix::Yfd) => self.index.iy.get8lo(),
            }
            Reg8::A => self.af.get8hi(),
        }
    }

    #[inline]
    fn set_reg(&mut self, dst: Reg8, prefix: Option<Prefix>, val: u8) {
        unsafe {
            *self.reg8_ptr(dst, prefix) = val;
        }        
    }

    #[inline]
    fn get_reg2(&self, src: StkReg16) -> (u8, u8) {
        self.stkreg16_ref(src).get()
    }

    #[inline]
    fn get_alt_reg2(&self, src: StkReg16) -> (u8, u8) {
        self.stkreg16_alt_ref(src).get()
    }

    #[inline]
    fn get_reg16(&self, src: StkReg16) -> u16 {
        self.stkreg16_ref(src).get16()
    }

    #[inline]
    fn get_alt_reg16(&self, src: StkReg16) -> u16 {
        self.stkreg16_alt_ref(src).get16()
    }

    #[inline]
    fn set_reg16(&mut self, src: StkReg16, val: u16) {
        self.stkreg16_mut(src).set16(val)
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
        self.prefix.is_some()
    }

    #[inline]
    fn get_prefix(&self) -> Option<Prefix> {
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

    #[allow(clippy::cognitive_complexity,clippy::never_loop)]
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
            let code: u8 = fetch_next_opcode_ext!(self, control, pc, tsc);
            self.pc.set16(pc.0);
            self.execute_instruction::<M,T,F>(control, tsc, debug, code)
        }
    }

    #[allow(clippy::cognitive_complexity)]
    fn execute_with_limit<M, T>(&mut self, control: &mut M, tsc: &mut T, vc_limit: T::Limit) -> Result<M::WrIoBreak, M::RetiBreak>
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>,
          T: Clock
    {
        const DEBUG: Option<CpuDebugFn> = None;

        if self.halt {
            debug_assert_eq!(self.prefix, None);
            debug_assert_eq!(self.last_ei, false);
            let pc = self.pc.get16();
            loop {
                if tsc.is_past_limit(vc_limit) {
                    return Ok(());
                }
                if self.iff1 && control.is_irq(tsc.as_timestamp()) {
                    break
                }
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

            if let LoopExitReason::EnableInt = reason {
                if !tsc.is_past_limit(vc_limit)  {
                    self.last_ei = false;
                    continue // this way skipping irq check immediately after EI
                }
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
