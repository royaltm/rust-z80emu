/*
    z80emu: a minimalistic Z80 CPU emulation library.
    Copyright (C) 2019-2022  Rafal Michalski

    For the full copyright notice, see the lib.rs file.
*/
//! See: <https://faqwiki.zxnet.co.uk/wiki/Z80#Differences_between_NMOS_and_CMOS_Z80s>
#[cfg(feature = "serde")]
use serde::{Serialize, Deserialize};

use super::{Z80, any::Z80Any, CpuFlags};

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
    /// Returns the string identifier of this flavour.
    fn tag() -> &'static str;
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
    /// Converts a [Z80] struct of this flavour into an [Z80Any] enum.
    fn cpu_into_any(cpu: Z80<Self>) -> Z80Any;
    /// Returns the contained [`Z80<Self>`][Z80] value, consuming the `cpu` value.
    ///
    /// # Panics
    /// Panics if the `cpu_any` value is not a variant corresponding to this `Flavour`.
    fn unwrap_cpu_any(cpu_any: Z80Any) -> Z80<Self>;
    /// Should reset the state. Called by [crate::Cpu::reset]. The default implementation resets the state to default.
    // #[inline(always)]
    fn reset(&mut self) {
        *self = Default::default();
    }
}

/// The struct implements a [Flavour] that emulates the Zilog Z80 NMOS version.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(default, rename_all(serialize = "camelCase")))]
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct NMOS {
    #[cfg_attr(feature = "serde", serde(alias = "flagsModified"))]
    flags_modified: bool,
    #[cfg_attr(feature = "serde", serde(alias = "lastFlagsModified"))]
    last_flags_modified: bool
}

/// The struct implements a [Flavour] that emulates the Zilog Z80 CMOS version.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(into = "NMOS", from = "NMOS"))]
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct CMOS;

/// The struct implements a [Flavour] that (supposedly) emulates the KP1858BM1 or T34BM1 clones of the Z80.
///
/// It differs from the NMOS implementation in the way [Flavour::memptr_mix] works.
/// In this implementation the returned MSB is always 0.
/// The [Flavour::ACCEPTING_INT_RESETS_IFF2_EARLY] value is `false`.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(default, rename_all(serialize = "camelCase")))]
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct BM1 {
    #[cfg_attr(feature = "serde", serde(alias = "flagsModified"))]
    flags_modified: bool,
    #[cfg_attr(feature = "serde", serde(alias = "lastFlagsModified"))]
    last_flags_modified: bool
}

impl Flavour for NMOS {
    const CONSTANT_OUT_DATA: u8 = 0;
    const ACCEPTING_INT_RESETS_IFF2_EARLY: bool = true;

    fn tag() -> &'static str {
        "NMOS"
    }
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

    fn cpu_into_any(cpu: Z80<Self>) -> Z80Any {
        Z80Any::NMOS(cpu)
    }

    fn unwrap_cpu_any(cpu_any: Z80Any) -> Z80<Self> {
        cpu_any.unwrap_nmos()
    }
}

impl Flavour for CMOS {
    const CONSTANT_OUT_DATA: u8 = u8::max_value();
    const ACCEPTING_INT_RESETS_IFF2_EARLY: bool = false;

    fn tag() -> &'static str {
        "CMOS"
    }
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

    fn cpu_into_any(cpu: Z80<Self>) -> Z80Any {
        Z80Any::CMOS(cpu)
    }

    fn unwrap_cpu_any(cpu_any: Z80Any) -> Z80<Self> {
        cpu_any.unwrap_cmos()
    }
}

impl Flavour for BM1 {
    const CONSTANT_OUT_DATA: u8 = 0;
    const ACCEPTING_INT_RESETS_IFF2_EARLY: bool = false;

    fn tag() -> &'static str {
        "BM1"
    }
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

    fn cpu_into_any(cpu: Z80<Self>) -> Z80Any {
        Z80Any::BM1(cpu)
    }

    fn unwrap_cpu_any(cpu_any: Z80Any) -> Z80<Self> {
        cpu_any.unwrap_bm1()
    }
}

/// This conversion is lossy. When CMOS [Flavour] is converted back information is lost.
impl From<NMOS> for CMOS {
    fn from(_: NMOS) -> Self {
        CMOS
    }
}

impl From<CMOS> for NMOS {
    fn from(_: CMOS) -> Self {
        NMOS::default()
    }
}

/// This conversion is lossy. When CMOS [Flavour] is converted back information is lost.
impl From<BM1> for CMOS {
    fn from(_: BM1) -> Self {
        CMOS
    }
}

impl From<CMOS> for BM1 {
    fn from(_: CMOS) -> Self {
        BM1::default()
    }
}

impl From<NMOS> for BM1 {
    fn from(NMOS{flags_modified, last_flags_modified}: NMOS) -> Self {
        BM1 {flags_modified, last_flags_modified}
    }
}

impl From<BM1> for NMOS {
    fn from(BM1{flags_modified, last_flags_modified}: BM1) -> Self {
        NMOS {flags_modified, last_flags_modified}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::z80::Z80;

    #[test]
    fn flavour_works() {
        assert_eq!(NMOS::memptr_mix(1, 1), (1, 2));
        assert_eq!(NMOS::memptr_mix(1, 255), (1, 0));
        assert_eq!(CMOS::memptr_mix(1, 1), (1, 2));
        assert_eq!(CMOS::memptr_mix(1, 255), (1, 0));
        assert_eq!(BM1::memptr_mix(1, 1), (0, 2));
        assert_eq!(BM1::memptr_mix(1, 255), (0, 0));

        let flags = CpuFlags::all();

        let mut flav = NMOS::default();
        assert!(!flav.last_flags_modified);
        assert!(!flav.flags_modified);
        assert_eq!(flav.get_q(0, flags), 0xFF);
        flav.begin_instruction();
        assert!(!flav.last_flags_modified);
        assert!(!flav.flags_modified);
        assert_eq!(flav.get_q(0, flags), 0xFF);
        flav.flags_modified();
        assert!(!flav.last_flags_modified);
        assert!(flav.flags_modified);
        assert_eq!(flav.get_q(0, flags), 0xFF);
        flav.begin_instruction();
        assert!(flav.last_flags_modified);
        assert!(!flav.flags_modified);
        assert_eq!(flav.get_q(0, flags), 0);

        let mut flav = BM1::default();
        assert!(!flav.last_flags_modified);
        assert!(!flav.flags_modified);
        assert_eq!(flav.get_q(0, flags), 0xFF);
        flav.begin_instruction();
        assert!(!flav.last_flags_modified);
        assert!(!flav.flags_modified);
        assert_eq!(flav.get_q(0, flags), 0xFF);
        flav.flags_modified();
        assert!(!flav.last_flags_modified);
        assert!(flav.flags_modified);
        assert_eq!(flav.get_q(0, flags), 0xFF);
        flav.begin_instruction();
        assert!(flav.last_flags_modified);
        assert!(!flav.flags_modified);
        assert_eq!(flav.get_q(0, flags), 0);

        let mut flav = CMOS::default();
        assert_eq!(flav.get_q(0, flags), 0);
        flav.begin_instruction();
        assert_eq!(flav.get_q(1, flags), 1);
        flav.flags_modified();
        assert_eq!(flav.get_q(2, flags), 2);
        flav.begin_instruction();
        assert_eq!(flav.get_q(3, flags), 3);
    }

    #[test]
    fn flavour_conversion() {
        let cmos: Z80<CMOS> = Z80::<BM1>::default().into_flavour();
        let nmos = Z80::<NMOS>::from_flavour(cmos);
        let bm1 = nmos.into_flavour::<BM1>();
        assert_eq!(bm1, Z80::<BM1>::default());
        let cmos1: Z80<CMOS> = Z80::<CMOS>::default();
        let cmos2: Z80<CMOS> = Z80::<CMOS>::default();
        assert_eq!(Z80Any::from(cmos1), Z80Any::CMOS(cmos2));
        let nmos1: Z80<NMOS> = Z80::<NMOS>::default();
        let nmos2: Z80<NMOS> = Z80::<NMOS>::default();
        assert_eq!(Z80Any::from(nmos1), Z80Any::NMOS(nmos2));
        let bm1_1: Z80<BM1> = Z80::<BM1>::default();
        let bm1_2: Z80<BM1> = Z80::<BM1>::default();
        assert_eq!(Z80Any::from(bm1_1), Z80Any::BM1(bm1_2));
    }

    #[cfg(feature = "serde")]
    #[test]
    fn flavour_serde() {
        assert_eq!(NMOS::tag(), "NMOS");
        assert_eq!(CMOS::tag(), "CMOS");
        assert_eq!(BM1::tag(), "BM1");
        assert_eq!(serde_json::to_string(&NMOS::default()).unwrap(), r#"{"flagsModified":false,"lastFlagsModified":false}"#);
        assert_eq!(serde_json::to_string(&CMOS::default()).unwrap(), r#"{"flagsModified":false,"lastFlagsModified":false}"#);
        assert_eq!(serde_json::to_string(&BM1::default()).unwrap(), r#"{"flagsModified":false,"lastFlagsModified":false}"#);
        let flav: NMOS = serde_json::from_str(r#"{"flags_modified":false,"last_flags_modified":false}"#).unwrap();
        assert!(flav == NMOS::default());
        let flav: NMOS = serde_json::from_str(r#"{}"#).unwrap();
        assert!(flav == NMOS::default());
        let flav: NMOS = serde_json::from_str(r#"{"flagsModified":true,"lastFlagsModified":true}"#).unwrap();
        assert!(flav == NMOS { flags_modified: true, last_flags_modified: true});
        let flav: CMOS = serde_json::from_str(r#"{"flags_modified":false,"last_flags_modified":false}"#).unwrap();
        assert!(flav == CMOS::default());
        let flav: CMOS = serde_json::from_str(r#"{}"#).unwrap();
        assert!(flav == CMOS::default());
        let flav: CMOS = serde_json::from_str(r#"{"flagsModified":true,"lastFlagsModified":true}"#).unwrap();
        assert!(flav == CMOS);
        let flav: BM1 = serde_json::from_str(r#"{"flags_modified":false,"last_flags_modified":false}"#).unwrap();
        assert!(flav == BM1::default());
        let flav: BM1 = serde_json::from_str(r#"{}"#).unwrap();
        assert!(flav == BM1::default());
        let flav: BM1 = serde_json::from_str(r#"{"flagsModified":true,"lastFlagsModified":true}"#).unwrap();
        assert!(flav == BM1 { flags_modified: true, last_flags_modified: true});
    }
}
