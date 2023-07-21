/*
    z80emu: ZiLOG Z80 microprocessor emulation library.
    Copyright (C) 2019-2023  Rafal Michalski

    For the full copyright notice, see the lib.rs file.
*/
//! Features an [enum][Z80Any] of [Z80] with any [Flavour].
use core::fmt;

use crate::cpu::{
    Cpu,
    CpuDebug,
    CpuFlags,
    InterruptMode,
    Prefix,
    Reg8,
    StkReg16
};
use crate::host::{Result, Clock, Memory, Io};
use super::{Z80, flavours::*};

macro_rules! cpu_dispatch_any {
    ($cpuany:ident($cpu:ident) => $expr:expr) => {
        cpu_dispatch_any!(($cpuany)($cpu) => $expr)
    };
    ($cpuany:ident(mut $cpu:ident) => $expr:expr) => {
        cpu_dispatch_any!(($cpuany)(mut $cpu) => $expr)
    };
    (($cpuany:expr)($($cpu:tt)*) => $expr:expr) => {
        match $cpuany {
            $crate::z80::any::Z80Any::NMOS($($cpu)*) => $expr,
            $crate::z80::any::Z80Any::CMOS($($cpu)*) => $expr,
            $crate::z80::any::Z80Any::BM1($($cpu)*) => $expr,
        }
    };
}

/// [Z80] with any [Flavour].
///
/// This enum can be used in place of [Z80] when changing of [Flavour] is required in run time.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Z80Any {
    NMOS(Z80<NMOS>),
    CMOS(Z80<CMOS>),
    BM1(Z80<BM1>),
}

impl<Q: Flavour> From<Z80<Q>> for Z80Any {
    fn from(cpu: Z80<Q>) -> Z80Any {
        Q::cpu_into_any(cpu)
    }
}

impl<Q> From<Z80Any> for Z80<Q>
    where Q: Flavour + From<NMOS> + From<CMOS> + From<BM1>
{
    fn from(cpu_any: Z80Any) -> Z80<Q> {
        cpu_dispatch_any!(cpu_any(cpu) => cpu.into_flavour())
    }
}

impl Z80Any {
    /// Returns the tag of the current Z80 variant as string.
    pub fn tag(&self) -> &'static str {
        match self {
            Z80Any::NMOS(..) => NMOS::tag(),
            Z80Any::CMOS(..) => CMOS::tag(),
            Z80Any::BM1(..) => BM1::tag(),
        }
    }

    /// Creates a new instance of [Z80Any] with the given `tag` on success with the state just after `RESET`.
    ///
    /// Returns `None` if the provided `tag` is unknown.
    pub fn with_tag(tag: &str) -> Option<Self> {
        if tag.eq_ignore_ascii_case("NMOS") {
            Some(Self::new_nmos())
        }
        else if tag.eq_ignore_ascii_case("CMOS") {
            Some(Self::new_cmos())
        }
        else if tag.eq_ignore_ascii_case("BM1") {
            Some(Self::new_bm1())
        }
        else {
            None
        }
    }

    /// Creates a new instance of [Z80Any::NMOS] variant with the state just after `RESET`.
    pub fn new_nmos() -> Z80Any {
        Z80Any::NMOS(Z80::new())
    }

    /// Creates a new instance of [Z80Any::CMOS] variant with the state just after `RESET`.
    pub fn new_cmos() -> Z80Any {
        Z80Any::CMOS(Z80::new())
    }

    /// Creates a new instance of [Z80Any::BM1] variant with the state just after `RESET`.
    pub fn new_bm1() -> Z80Any {
        Z80Any::BM1(Z80::new())
    }

    /// Returns `true` if the variant of `self` is [Z80Any::NMOS].
    pub fn is_nmos(&self) -> bool {
        if let Z80Any::NMOS(..) = self {
            return true
        }
        false
    }

    /// Returns `true` if the variant of `self` is [Z80Any::CMOS].
    pub fn is_cmos(&self) -> bool {
        if let Z80Any::CMOS(..) = self {
            return true
        }
        false
    }

    /// Returns `true` if the variant of `self` is [Z80Any::BM1].
    pub fn is_bm1(&self) -> bool {
        if let Z80Any::BM1(..) = self {
            return true
        }
        false
    }

    /// Converts an instance of any variant of [Z80Any] to a [Z80Any::NMOS] variant.
    pub fn into_nmos(self) -> Z80Any {
        cpu_dispatch_any!(self(cpu) => Z80Any::NMOS(cpu.into_flavour()))
    }

    /// Converts an instance of any variant of [Z80Any] to a [Z80Any::CMOS] variant.
    pub fn into_cmos(self) -> Z80Any {
        cpu_dispatch_any!(self(cpu) => Z80Any::CMOS(cpu.into_flavour()))
    }

    /// Converts an instance of any variant of [Z80Any] to a [Z80Any::BM1] variant.
    pub fn into_bm1(self) -> Z80Any {
        cpu_dispatch_any!(self(cpu) => Z80Any::BM1(cpu.into_flavour()))
    }

    /// Returns the contained [`Z80<NMOS>`][Z80] value, consuming the self value.
    ///
    /// # Panics
    /// Panics if the self value is not a [Z80Any::NMOS] variant.
    pub fn unwrap_nmos(self) -> Z80<NMOS> {
        if let Z80Any::NMOS(cpu) = self {
            cpu
        }
        else {
            panic!("called `Z80Any::unwrap_nmos()` on a non `Z80Any::NMOS` variant")
        }
    }

    /// Returns the contained [`Z80<CMOS>`][Z80] value, consuming the self value.
    ///
    /// # Panics
    /// Panics if the self value is not a [Z80Any::CMOS] variant.
    pub fn unwrap_cmos(self) -> Z80<CMOS> {
        if let Z80Any::CMOS(cpu) = self {
            cpu
        }
        else {
            panic!("called `Z80Any::unwrap_cmos()` on a non `Z80Any::CMOS` variant")
        }
    }

    /// Returns the contained [`Z80<BM1>`][Z80] value, consuming the self value.
    ///
    /// # Panics
    /// Panics if the self value is not a [Z80Any::BM1] variant.
    pub fn unwrap_bm1(self) -> Z80<BM1> {
        if let Z80Any::BM1(cpu) = self {
            cpu
        }
        else {
            panic!("called `Z80Any::unwrap_bm1()` on a non `Z80Any::BM1` variant")
        }
    }

    /// Retrieves the internal state of the MEMPTR register.
    pub fn get_memptr(&self) -> u16 {
        cpu_dispatch_any!(self(cpu) => cpu.get_memptr())
    }

    /// Changes the internal state of the MEMPTR register.
    pub fn set_memptr(&mut self, memptr: u16) {
        cpu_dispatch_any!(self(cpu) => cpu.set_memptr(memptr))
    }

    /// The content of the `R` register is lazy evaluated when its value is being set or retrieved.
    /// This method normalizes the internal state of the `R` register, so e.g. two instances of
    /// [Z80] can be compared if they represent the same `CPU` state.
    pub fn normalize_r(&mut self) {
        cpu_dispatch_any!(self(cpu) => cpu.normalize_r())
    }
}

impl Default for Z80Any {
    fn default() -> Z80Any {
        Z80Any::NMOS(Default::default())
    }
}

/// Displays the tag of the current Z80 variant.
impl fmt::Display for Z80Any {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.tag())
    }
}

impl Cpu for Z80Any {
    fn reset(&mut self) {
        cpu_dispatch_any!(self(cpu) => cpu.reset())
    }

    fn get_pc(&self) -> u16 {
        cpu_dispatch_any!(self(cpu) => cpu.get_pc())
    }

    fn set_pc(&mut self, pc: u16) {
        cpu_dispatch_any!(self(cpu) => cpu.set_pc(pc))
    }

    fn get_sp(&self) -> u16 {
        cpu_dispatch_any!(self(cpu) => cpu.get_sp())
    }

    fn set_sp(&mut self, sp: u16) {
        cpu_dispatch_any!(self(cpu) => cpu.set_sp(sp))
    }

    fn get_acc(&self) -> u8 {
        cpu_dispatch_any!(self(cpu) => cpu.get_acc())
    }

    fn set_acc(&mut self, val: u8) {
        cpu_dispatch_any!(self(cpu) => cpu.set_acc(val))
    }

    fn get_flags(&self) -> CpuFlags {
        cpu_dispatch_any!(self(cpu) => cpu.get_flags())
    }

    fn set_flags(&mut self, flags: CpuFlags) {
        cpu_dispatch_any!(self(cpu) => cpu.set_flags(flags))
    }

    fn inc_r(&mut self) {
        cpu_dispatch_any!(self(cpu) => cpu.inc_r())
    }

    fn add_r(&mut self, delta: i32) {
        cpu_dispatch_any!(self(cpu) => cpu.add_r(delta))
    }

    fn get_r(&self) -> u8 {
        cpu_dispatch_any!(self(cpu) => cpu.get_r())
    }

    fn set_r(&mut self, r: u8) {
        cpu_dispatch_any!(self(cpu) => cpu.set_r(r))
    }

    fn get_i(&self) -> u8 {
        cpu_dispatch_any!(self(cpu) => cpu.get_i())
    }

    fn set_i(&mut self, i: u8) {
        cpu_dispatch_any!(self(cpu) => cpu.set_i(i))
    }

    fn get_ir(&self) -> u16 {
        cpu_dispatch_any!(self(cpu) => cpu.get_ir())
    }

    fn get_iffs(&self) -> (bool, bool) {
        cpu_dispatch_any!(self(cpu) => cpu.get_iffs())
    }

    fn set_iffs(&mut self, iff1: bool, iff2: bool) {
        cpu_dispatch_any!(self(cpu) => cpu.set_iffs(iff1, iff2))
    }

    fn halt(&mut self) {
        cpu_dispatch_any!(self(cpu) => cpu.halt())
    }

    fn is_halt(&self) -> bool {
        cpu_dispatch_any!(self(cpu) => cpu.is_halt())
    }

    fn get_im(&self) -> InterruptMode {
        cpu_dispatch_any!(self(cpu) => cpu.get_im())
    }

    fn set_im(&mut self, im: InterruptMode) {
        cpu_dispatch_any!(self(cpu) => cpu.set_im(im))
    }

    fn ex_af_af(&mut self) {
        cpu_dispatch_any!(self(cpu) => cpu.ex_af_af())
    }

    fn exx(&mut self) {
        cpu_dispatch_any!(self(cpu) => cpu.exx())
    }

    fn get_reg(&self, reg: Reg8, prefix: Option<Prefix>) -> u8 {
        cpu_dispatch_any!(self(cpu) => cpu.get_reg(reg, prefix))
    }

    fn set_reg(&mut self, dst: Reg8, prefix: Option<Prefix>, val: u8) {
        cpu_dispatch_any!(self(cpu) => cpu.set_reg(dst, prefix, val))
    }

    fn get_reg2(&self, src: StkReg16) -> (u8, u8) {
        cpu_dispatch_any!(self(cpu) => cpu.get_reg2(src))
    }

    fn get_alt_reg2(&self, src: StkReg16) -> (u8, u8) {
        cpu_dispatch_any!(self(cpu) => cpu.get_alt_reg2(src))
    }

    fn get_reg16(&self, src: StkReg16) -> u16 {
        cpu_dispatch_any!(self(cpu) => cpu.get_reg16(src))
    }

    fn get_alt_reg16(&self, src: StkReg16) -> u16 {
        cpu_dispatch_any!(self(cpu) => cpu.get_alt_reg16(src))
    }

    fn set_reg2(&mut self, src: StkReg16, hi: u8, lo: u8) {
        cpu_dispatch_any!(self(cpu) => cpu.set_reg2(src, hi, lo))
    }

    fn set_reg16(&mut self, src: StkReg16, val: u16) {
        cpu_dispatch_any!(self(cpu) => cpu.set_reg16(src, val))
    }

    fn get_index2(&self, prefix: Prefix) -> (u8, u8) {
        cpu_dispatch_any!(self(cpu) => cpu.get_index2(prefix))
    }

    fn get_index16(&self, prefix: Prefix) -> u16 {
        cpu_dispatch_any!(self(cpu) => cpu.get_index16(prefix))
    }

    fn set_index2(&mut self, prefix: Prefix, hi: u8, lo: u8) {
        cpu_dispatch_any!(self(cpu) => cpu.set_index2(prefix, hi, lo))
    }

    fn set_index16(&mut self, prefix: Prefix, val: u16) {
        cpu_dispatch_any!(self(cpu) => cpu.set_index16(prefix, val))
    }

    fn is_irq_allowed(&self) -> bool {
        cpu_dispatch_any!(self(cpu) => cpu.is_irq_allowed())
    }

    fn is_nmi_allowed(&self) -> bool {
        cpu_dispatch_any!(self(cpu) => cpu.is_nmi_allowed())
    }

    fn restore_iff1(&mut self) {
        cpu_dispatch_any!(self(cpu) => cpu.restore_iff1())
    }

    fn disable_interrupts(&mut self) {
        cpu_dispatch_any!(self(cpu) => cpu.disable_interrupts())
    }

    fn enable_interrupts(&mut self) {
        cpu_dispatch_any!(self(cpu) => cpu.enable_interrupts())
    }

    fn is_after_ei(&self) -> bool {
        cpu_dispatch_any!(self(cpu) => cpu.is_after_ei())
    }

    fn is_after_prefix(&self) -> bool {
        cpu_dispatch_any!(self(cpu) => cpu.is_after_prefix())
    }

    fn get_prefix(&self) -> Option<Prefix> {
        cpu_dispatch_any!(self(cpu) => cpu.get_prefix())
    }

    fn irq<M, T, F>(&mut self, control: &mut M, tsc: &mut T, debug: Option<F>) -> Option<Result<M::WrIoBreak, M::RetiBreak>>
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>,
          T: Clock,
          F: FnOnce(CpuDebug)
    {
        cpu_dispatch_any!(self(cpu) => cpu.irq(control, tsc, debug))
    }

    fn nmi<M, T>(&mut self, control: &mut M, tsc: &mut T) -> bool
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>,
          T: Clock
    {
        cpu_dispatch_any!(self(cpu) => cpu.nmi(control, tsc))
    }

    fn execute_instruction<M, T, F>(&mut self, control: &mut M, tsc: &mut T, debug: Option<F>, code: u8) -> Result<M::WrIoBreak, M::RetiBreak>
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>,
          T: Clock,
          F: FnOnce(CpuDebug)
    {
        cpu_dispatch_any!(self(cpu) => cpu.execute_instruction(control, tsc, debug, code))
    }

    fn execute_next<M, T, F>(&mut self, control: &mut M, tsc: &mut T, debug: Option<F>) -> Result<M::WrIoBreak, M::RetiBreak>
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>,
          T: Clock,
          F: FnOnce(CpuDebug)
    {
        cpu_dispatch_any!(self(cpu) => cpu.execute_next(control, tsc, debug))
    }

    fn execute_with_limit<M, T>(&mut self, control: &mut M, tsc: &mut T, vc_limit: T::Limit) -> Result<M::WrIoBreak, M::RetiBreak>
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>,
          T: Clock
    {
        cpu_dispatch_any!(self(cpu) => cpu.execute_with_limit(control, tsc, vc_limit))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn z80any() {
        let cpu_any = Z80Any::with_tag(NMOS::tag()).unwrap();
        assert_eq!(cpu_any.tag(), "NMOS");
        assert_eq!(cpu_any, Z80Any::new_nmos());
        assert_eq!(cpu_any.is_nmos(), true);
        assert_eq!(cpu_any.is_cmos(), false);
        assert_eq!(cpu_any.is_bm1(), false);
        let cpu = cpu_any.clone().unwrap_nmos();
        assert_eq!(cpu_any, Z80Any::NMOS(cpu));

        let cpu_any = cpu_any.into_cmos();
        assert_eq!(cpu_any.tag(), "CMOS");
        assert_eq!(cpu_any, Z80Any::with_tag(CMOS::tag()).unwrap());
        assert_eq!(cpu_any, Z80Any::new_cmos());
        assert_eq!(cpu_any.is_nmos(), false);
        assert_eq!(cpu_any.is_cmos(), true);
        assert_eq!(cpu_any.is_bm1(), false);
        let cpu = cpu_any.clone().unwrap_cmos();
        assert_eq!(cpu_any, Z80Any::CMOS(cpu));

        let cpu_any = cpu_any.into_bm1();
        assert_eq!(cpu_any.tag(), "BM1");
        assert_eq!(cpu_any, Z80Any::with_tag(BM1::tag()).unwrap());
        assert_eq!(cpu_any, Z80Any::new_bm1());
        assert_eq!(cpu_any.is_nmos(), false);
        assert_eq!(cpu_any.is_cmos(), false);
        assert_eq!(cpu_any.is_bm1(), true);
        let cpu = cpu_any.clone().unwrap_bm1();
        assert_eq!(cpu_any, Z80Any::BM1(cpu));

        let cpu_any = cpu_any.into_nmos();
        assert_eq!(cpu_any.tag(), "NMOS");
        assert_eq!(cpu_any, Z80Any::new_nmos());
    }
}
