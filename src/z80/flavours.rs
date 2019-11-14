//! See: https://faqwiki.zxnet.co.uk/wiki/Z80#Differences_between_NMOS_and_CMOS_Z80s
#[cfg(feature = "serde")]
use serde::{Serialize, Deserialize};

use super::CpuFlags;

/// A trait for implementing exceptions to the undocumented Z80 behaviour.
///
/// It's been [reported] that depending on the CPU technology (NMOS, CMOS) and the manufacturer (Zilog, NEC, other clones)
/// there are certain differences of undocumented behaviour and mainly affects the way the Flags' undocumented
/// bits 3 and 5 are being modified.
///
/// [reported]: https://faqwiki.zxnet.co.uk/wiki/Z80#Differences_between_NMOS_and_CMOS_Z80s
pub trait Flavour: Clone + Copy + Default + PartialEq + Eq {
    /// The value being actually put on the data bus while executing the undocumented instruction `OUT (C),(HL)`.
    const CONSTANT_OUT_DATA: u8;
    /// Should be `true` if the IFF2 is being reset early when accepting an interrupt, while an instruction
    /// is being executed, so `LD A,I` or `LD A,R` may report modified IFF2 value.
    const ACCEPTING_INT_RESETS_IFF2_EARLY: bool;
    /// The way MEMPTR is being updated for: `LD (nnnn),A`, `LD (BC),A`, `LD (DE),A` and `OUT (nn),A`
    /// is being controlled by this function. The current Accumulator value is being passed as `msb` and
    /// the lower 8-bits of the current destination address is being provided as `lsb`.
    /// The function should return the (MSB, LSB) value to set the MEMPTR with.
    fn memptr_mix(msb: u8, lsb: u8) -> (u8, u8);
    /// This method is being called each time before an instructions is being executed or an interrupt is being
    /// accepted, including NMI. It might modify some state and is being used together with [Flavour::flags_modified]
    /// and [Flavour::get_q] to prepare a value applied to bits 3 and 5 of the Flags for the SCF/CCF instructions.
    fn begin_instruction(&mut self);
    /// This method is being called each time an instructions modifies the Flags register.
    fn flags_modified(&mut self);
    /// Bits 3 and 5 of the returned value will be copied to the Flags register.
    fn get_q(&self, acc:u8, flags: CpuFlags) -> u8;
    /// Should reset the state. Called by [crate::Cpu::reset]. The default implementation resets state to the default.
    #[inline(always)]
    fn reset(&mut self) {
        *self = Default::default();
    }
}

/// The struct implements a [Flavour] that emulates the Zilog Z80 NMOS version.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Copy, Default, PartialEq, Eq)]
pub struct NMOS {
    flags_modified: bool,
    last_flags_modified: bool
}

/// The struct implements a [Flavour] that emulates the Zilog Z80 CMOS version.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Copy, Default, PartialEq, Eq)]
pub struct CMOS;

/// The struct implements a [Flavour] that (supposedly) emulates the KP1858BM1 or T34BM1 clones of the Z80.
///
/// It differs from the NMOS implementation in the way [Flavour::memptr_mix] works.
/// In this implementation the returned MSB is always 0.
/// The [Flavour::ACCEPTING_INT_RESETS_IFF2_EARLY] value is `false`.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Copy, Default, PartialEq, Eq)]
pub struct BM1 {
    flags_modified: bool,
    last_flags_modified: bool
}

impl Flavour for NMOS {
    const CONSTANT_OUT_DATA: u8 = 0;
    const ACCEPTING_INT_RESETS_IFF2_EARLY: bool = true;
    #[inline(always)]
    fn memptr_mix(msb: u8, lsb: u8) -> (u8, u8) {
        (msb, lsb.wrapping_add(1))
    }
    #[inline(always)]
    fn begin_instruction(&mut self) {
        self.last_flags_modified = self.flags_modified;
        self.flags_modified = false;
    }
    #[inline(always)]
    fn flags_modified(&mut self) {
        self.flags_modified = true;
    }
    #[inline(always)]
    fn get_q(&self, acc: u8, flags: CpuFlags) -> u8 {
        if self.last_flags_modified {
            acc
        }
        else {
            acc | flags.bits()
        }
    }
}

impl Flavour for CMOS {
    const CONSTANT_OUT_DATA: u8 = u8::max_value();
    const ACCEPTING_INT_RESETS_IFF2_EARLY: bool = false;
    #[inline(always)]
    fn memptr_mix(msb: u8, lsb: u8) -> (u8, u8) {
        (msb, lsb.wrapping_add(1))
    }
    #[inline(always)]
    fn begin_instruction(&mut self) {}
    #[inline(always)]
    fn flags_modified(&mut self) {}
    #[inline(always)]
    fn get_q(&self, acc: u8, _flags: CpuFlags) -> u8 { acc }
}

impl Flavour for BM1 {
    const CONSTANT_OUT_DATA: u8 = 0;
    const ACCEPTING_INT_RESETS_IFF2_EARLY: bool = false;
    #[inline(always)]
    fn memptr_mix(_msb: u8, lsb: u8) -> (u8, u8) {
        (0, lsb.wrapping_add(1))
    }
    #[inline(always)]
    fn begin_instruction(&mut self) {
        self.last_flags_modified = self.flags_modified;
        self.flags_modified = false;
    }
    #[inline(always)]
    fn flags_modified(&mut self) {
        self.flags_modified = true;
    }
    #[inline(always)]
    fn get_q(&self, acc: u8, flags: CpuFlags) -> u8 {
        if self.last_flags_modified {
            acc
        }
        else {
            acc | flags.bits()
        }
    }
}
