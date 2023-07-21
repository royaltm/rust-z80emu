/*
    z80emu: a minimalistic Z80 CPU emulation library.
    Copyright (C) 2019-2023  Rafal Michalski

    For the full copyright notice, see the lib.rs file.
*/
//! Cpu flags register bits definitions and flag helper methods.
use bitflags::bitflags;

bitflags! {
    /// Z80 [Cpu](crate::Cpu) Flags.
    #[derive(Default, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone, Copy)]
    pub struct CpuFlags: u8 {
        /// Carry Flag.
        const C  = 0b0000_0001;
        /// Add/Subtract Flag.
        const N  = 0b0000_0010;
        /// Parity/Overflow Flag.
        const PV = 0b0000_0100;
        /// Undocumented bit 3 of the Flag.
        const X  = 0b0000_1000;
        /// Half Carry Flag.
        const H  = 0b0001_0000;
        /// Undocumented bit 5 of the Flag.
        const Y  = 0b0010_0000;
        /// Zero Flag.
        const Z  = 0b0100_0000;
        /// Sign Flag.
        const S  = 0b1000_0000;
    }
}

impl CpuFlags {
    /// An alias of [CpuFlags::PV].
    pub const P: Self  = CpuFlags::from_bits_retain(Self::PV.bits());
    /// An alias of [CpuFlags::PV].
    pub const V: Self  = CpuFlags::from_bits_retain(Self::PV.bits());
    /// A mask of both undocumented Flag's bits 3 and 5. [CpuFlags::X] | [CpuFlags::Y].
    pub const XY: Self = CpuFlags::from_bits_retain(Self::X.bits() | Self::Y.bits());
    /// A mask over bits 0 to 3 being used to detect half-byte carry.
    pub const HMASK: Self = CpuFlags::from_bits_retain(CpuFlags::H.bits() - 1);

    /// Resets all [CpuFlags] to `false`.
    pub fn reset(&mut self) {
        const RESET: CpuFlags = CpuFlags::empty();
        *self = RESET;
    }

    /// Returns a value of the Sign Flag.
    #[inline(always)]
    pub fn sf(self) -> bool {
        self.contains(CpuFlags::S)
    }

    /// Returns a value of the Zero Flag.
    #[inline(always)]
    pub fn zf(self) -> bool {
        self.contains(CpuFlags::Z)
    }

    /// Returns a value of the Half Carry Flag.
    #[inline(always)]
    pub fn hf(self) -> bool {
        self.contains(CpuFlags::H)
    }

    /// Returns a value of the Parity/Overflow Flag.
    #[inline(always)]
    pub fn pvf(self) -> bool {
        self.contains(CpuFlags::PV)
    }

    /// Returns a value of the Add/Subtract Flag.
    #[inline(always)]
    pub fn nf(self) -> bool {
        self.contains(CpuFlags::N)
    }

    /// Returns a value of the Carry Flag.
    #[inline(always)]
    pub fn cf(self) -> bool {
        self.contains(CpuFlags::C)
    }

    /// Returns a new instance of [CpuFlags] with Flags
    /// [C][CpuFlags::C] | [V][CpuFlags::V] | [Z][CpuFlags::Z]
    /// where each Flag is set depending on the value being given in the arguments.
    #[inline(always)]
    pub fn mask_cvz(cf: bool, vf: bool, zf: bool) -> Self {
        let mut bits = CpuFlags::empty();
        if cf {
            bits |= CpuFlags::C;
        }
        if vf {
            bits |= CpuFlags::V;
        }
        if zf {
            bits |= CpuFlags::Z;
        }
        bits
    }

    /// Returns a new instance of [CpuFlags] with the [S][CpuFlags::S] Flag being set
    /// depending on the top-most bit of the given 8-bit unsigned value being set.
    #[inline(always)]
    pub fn mask_sign(res: u8) -> Self {
        Self::from_bits_retain(res & CpuFlags::S.bits())
    }

    /// Returns a new instance of [CpuFlags] with the [Z][CpuFlags::Z] Flag being set
    /// depending on the given value being equal to 0 (zero).
    #[inline(always)]
    pub fn mask_zero(res: u8) -> Self {
        if res == 0 {
            CpuFlags::Z
        }
        else {
            CpuFlags::empty()
        }
    }

    /// Returns a new instance of [CpuFlags] with the [C][CpuFlags::C] Flag being set
    /// depending on the given value.
    #[inline(always)]
    pub fn mask_carry(cf: bool) -> Self {
        if cf {
            CpuFlags::C
        }
        else {
            CpuFlags::empty()
        }
    }

    /// Returns a new instance of [CpuFlags] with the [N][CpuFlags::N] Flag being set
    /// depending on the given value.
    #[inline(always)]
    pub fn mask_nf(nf: bool) -> Self {
        if nf {
            CpuFlags::N
        }
        else {
            CpuFlags::empty()
        }
    }

    /// Returns a new instance of [CpuFlags] with the [H][CpuFlags::H] Flag being set
    /// depending on the given value.
    #[inline(always)]
    pub fn mask_hf(hf: bool) -> Self {
        if hf {
            CpuFlags::H
        }
        else {
            CpuFlags::empty()
        }
    }

    /// Returns a new instance of [CpuFlags] with Flags [C][CpuFlags::C] | [H][CpuFlags::H] being
    /// set depending on the given value.
    #[inline(always)]
    pub fn mask_hcf(hcf: bool) -> Self {
        if hcf {
            CpuFlags::H|CpuFlags::C
        }
        else {
            CpuFlags::empty()
        }
    }

    /// Returns a new instance of [CpuFlags] with the [PV][CpuFlags::PV] Flag being set
    /// depending on the given value.
    #[inline(always)]
    pub fn mask_pvf(pvf: bool) -> Self {
        if pvf {
            CpuFlags::PV
        }
        else {
            CpuFlags::empty()
        }
    }

    /// Returns a new instance of [CpuFlags] with the [PV][CpuFlags::PV] Flag being set
    /// if the number of bits equal to 1 is even in the given value.
    #[inline(always)]
    pub fn parity(res: u8) -> Self {
        Self::mask_pvf(res.count_ones() & 1 == 0)
    }

    /// Returns a new instance of [CpuFlags] with Flags [X][CpuFlags::X] | [Y][CpuFlags::Y]
    /// being set depending on the bit value 3 for `X` and 5 for `Y` in the given argument.
    #[inline(always)]
    pub fn mask_xy(res: u8) -> Self {
        Self::from_bits_retain(res & CpuFlags::XY.bits())
    }

    /// Returns a new instance of [CpuFlags] with Flags [S][CpuFlags::S] | [X][CpuFlags::X] | [Y][CpuFlags::Y]
    /// being set depending on the bit value 7 for `S`, 3 for `X` and 5 for `Y` in the given argument.
    #[inline(always)]
    pub fn mask_sxy(res: u8) -> Self {
        Self::from_bits_retain(res & (CpuFlags::S.bits()|CpuFlags::XY.bits()))
    }

    #[inline(always)]
    pub(crate) fn mask_bitops(res: u8, hf: bool, cf: bool) -> Self {
        let mut bits = Self::mask_sxy(res);
        if res == 0 {
            bits |= CpuFlags::Z;
        }
        if hf {
            bits |= CpuFlags::H;
        }
        if cf {
            bits |= CpuFlags::C;
        }
        if res.count_ones() & 1 == 0 {
            bits |= CpuFlags::P;
        }
        bits
    }

    #[inline(always)]
    pub(crate) fn mask_nh_add(tgt: u8, add: u8) -> Self {
        if (tgt & CpuFlags::HMASK.bits()).wrapping_add(add & CpuFlags::HMASK.bits()) & CpuFlags::H.bits() != 0 {
            CpuFlags::H
        }
        else {
            CpuFlags::empty()
        }
    }

    #[inline(always)]
    pub(crate) fn mask_nh_add16(tgt: u16, add: u16) -> Self {
        const HMASK16: u16 = ((CpuFlags::H.bits() as u16) << 8) - 1;
        if (tgt & HMASK16).wrapping_add(add & HMASK16) & ((CpuFlags::H.bits() as u16) << 8) != 0 {
            CpuFlags::H
        }
        else {
            CpuFlags::empty()
        }
    }

    #[inline(always)]
    pub(crate) fn mask_nh_adc(tgt: u8, add: u8, cf: bool) -> Self {
        let tgt = (tgt & CpuFlags::HMASK.bits()).wrapping_add(add & CpuFlags::HMASK.bits());
        if (tgt & CpuFlags::H.bits() != 0) ||
           ((tgt & CpuFlags::HMASK.bits()).wrapping_add(u8::from(cf)) & CpuFlags::H.bits() != 0) {
            CpuFlags::H
        }
        else {
            CpuFlags::empty()
        }
    }

    #[inline(always)]
    pub(crate) fn mask_nh_adc16(tgt: u16, add: u16, cf: bool) -> Self {
        const HMASK16: u16 = ((CpuFlags::H.bits() as u16) << 8) - 1;
        const H16: u16 = (CpuFlags::H.bits() as u16) << 8;
        let tgt = (tgt & HMASK16).wrapping_add(add & HMASK16);
        if (tgt & H16 != 0) || ((tgt & HMASK16).wrapping_add(u16::from(cf)) & H16 != 0) {
            CpuFlags::H
        }
        else {
            CpuFlags::empty()
        }
    }

    #[inline(always)]
    pub(crate) fn mask_nh_sub(tgt: u8, sub: u8) -> Self {
        if (tgt & CpuFlags::HMASK.bits()).wrapping_sub(sub & CpuFlags::HMASK.bits()) & CpuFlags::H.bits() != 0 {
            CpuFlags::H|CpuFlags::N
        }
        else {
            CpuFlags::N
        }
    }

    #[inline(always)]
    pub(crate) fn mask_nh_sbc(tgt: u8, sub: u8, cf: bool) -> Self {
        let tgt = (tgt & CpuFlags::HMASK.bits()).wrapping_sub(sub & CpuFlags::HMASK.bits());
        if (tgt & CpuFlags::H.bits() != 0) ||
            ((tgt & CpuFlags::HMASK.bits()).wrapping_sub(u8::from(cf)) & CpuFlags::H.bits() != 0) {
            CpuFlags::H|CpuFlags::N
        }
        else {
            CpuFlags::N
        }
    }

    #[inline(always)]
    pub(crate) fn mask_nh_sbc16(tgt: u16, sub: u16, cf: bool) -> Self {
        const HMASK16: u16 = ((CpuFlags::H.bits() as u16) << 8) - 1;
        const H16: u16 = (CpuFlags::H.bits() as u16) << 8;
        let tgt = (tgt & HMASK16).wrapping_sub(sub & HMASK16);
        if (tgt & H16 != 0) || ((tgt & HMASK16).wrapping_sub(u16::from(cf)) & H16 != 0) {
            CpuFlags::H|CpuFlags::N
        }
        else {
            CpuFlags::N
        }
    }

    #[inline(always)]
    pub(crate) fn mask_block_op_xy(n: u8) -> Self {
        Self::from_bits_retain(n & CpuFlags::X.bits() | n << 4 & CpuFlags::Y.bits())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn flags_base() {
        let flags = CpuFlags::default();
        assert_eq!(flags.bits(), 0);
        assert_eq!(flags.bits(), 0);
        assert_eq!(flags.sf(), false);
        assert_eq!(CpuFlags::S.sf(), true);
        assert_eq!(flags.zf(), false);
        assert_eq!(CpuFlags::Z.zf(), true);
        assert_eq!(flags.hf(), false);
        assert_eq!(CpuFlags::H.hf(), true);
        assert_eq!(flags.pvf(), false);
        assert_eq!(CpuFlags::PV.pvf(), true);
        assert_eq!(CpuFlags::P.pvf(), true);
        assert_eq!(CpuFlags::V.pvf(), true);
        assert_eq!(flags.nf(), false);
        assert_eq!(CpuFlags::N.nf(), true);
        assert_eq!(flags.cf(), false);
        assert_eq!(CpuFlags::C.cf(), true);
    }

    #[test]
    fn flags_work() {
        assert_eq!(0u8, CpuFlags::empty().bits());
        assert_eq!(false, CpuFlags::empty().cf());
        let mut flags = CpuFlags::empty();
        assert_eq!(false, flags.cf());
        flags.set(CpuFlags::C, true);
        assert_eq!(true, flags.cf());
        flags.set(CpuFlags::C, false);
        assert_eq!(false, flags.cf());
        assert_eq!(0u8, flags.bits());
        assert_eq!(false, flags.hf());
        flags.set(CpuFlags::H, true);
        assert_eq!(true, flags.hf());
        flags.set(CpuFlags::H, false);
        assert_eq!(false, flags.hf());
        assert_eq!(0u8, flags.bits());
        assert_eq!(false, flags.nf());
        flags.set(CpuFlags::N, true);
        assert_eq!(true, flags.nf());
        flags.set(CpuFlags::N, false);
        assert_eq!(false, flags.nf());
        assert_eq!(0u8, flags.bits());
        flags = CpuFlags::all();
        assert_eq!(0xFFu8, flags.bits());
        flags.reset();
        assert_eq!(0u8, flags.bits());
        assert_eq!((CpuFlags::C|CpuFlags::V|CpuFlags::Z).bits(), CpuFlags::mask_cvz(true, true, true).bits());
        assert_eq!(0, CpuFlags::mask_xy(0).bits());
        assert_eq!(CpuFlags::XY.bits(), CpuFlags::mask_xy(0b11111111).bits());
        assert_eq!(CpuFlags::X.bits(), CpuFlags::mask_xy(0b00001000).bits());
        assert_eq!(CpuFlags::Y.bits(), CpuFlags::mask_xy(0b11110111).bits());
        assert_eq!((CpuFlags::V|CpuFlags::Z).bits(), CpuFlags::mask_cvz(false, true, true).bits());
        assert_eq!(CpuFlags::V.bits(), CpuFlags::mask_cvz(false, true, false).bits());
        assert_eq!(CpuFlags::Z.bits(), CpuFlags::mask_cvz(false, false, true).bits());
        assert_eq!(0, CpuFlags::mask_cvz(false, false, false).bits());
        assert_eq!((CpuFlags::C|CpuFlags::V|CpuFlags::Z).bits(), CpuFlags::mask_cvz(true, true, true).bits());
        assert_eq!((CpuFlags::S|CpuFlags::XY).bits(), CpuFlags::mask_sxy(0b11111111).bits());
        assert_eq!(CpuFlags::S.bits(), CpuFlags::mask_sxy(0b11010111).bits());
        assert_eq!(CpuFlags::XY.bits(), CpuFlags::mask_sxy(0b00101000).bits());
        assert_eq!(0, CpuFlags::mask_nh_add(0b00001110, 1).bits());
        assert_eq!(CpuFlags::H.bits(), CpuFlags::mask_nh_add(0b00001111, 1).bits());
        assert_eq!(CpuFlags::N.bits(), CpuFlags::mask_nh_sub(0b00011111, 1).bits());
        assert_eq!((CpuFlags::H|CpuFlags::N).bits(), CpuFlags::mask_nh_sub(0b00010000, 1).bits());
        assert_eq!((CpuFlags::H).bits(), CpuFlags::mask_nh_adc(0b00001110, 1, true).bits());
        assert_eq!(0, CpuFlags::mask_nh_adc(0b00001110, 1, false).bits());
        assert_eq!(CpuFlags::N.bits(), CpuFlags::mask_nh_sbc(0b00000001, 1, false).bits());
        assert_eq!((CpuFlags::H|CpuFlags::N).bits(), CpuFlags::mask_nh_sbc(0b00000000, 1, false).bits());
        assert_eq!((CpuFlags::H|CpuFlags::N).bits(), CpuFlags::mask_nh_sbc(0b00000001, 1, true).bits());
        assert_eq!((CpuFlags::H|CpuFlags::N).bits(), CpuFlags::mask_nh_sbc(0b00000000, 1, true).bits());
        assert_eq!((CpuFlags::N).bits(), CpuFlags::mask_nh_sbc(0b00001000, 0, true).bits());
        assert_eq!((CpuFlags::H|CpuFlags::N).bits(), CpuFlags::mask_nh_sbc(0b00010000, 0, true).bits());
        assert_eq!(0, CpuFlags::mask_nh_add16(0b00001110, 1).bits());
        assert_eq!(0, CpuFlags::mask_nh_add16(0b00001111, 1).bits());
        assert_eq!(0, CpuFlags::mask_nh_add16(0b00001111_11111110, 1).bits());
        assert_eq!(CpuFlags::H.bits(), CpuFlags::mask_nh_add16(0b00001111_11111111, 1).bits());
        assert_eq!(0, CpuFlags::mask_nh_adc16(0b00001110, 1, false).bits());
        assert_eq!(0, CpuFlags::mask_nh_adc16(0b00001110, 1, true).bits());
        assert_eq!(CpuFlags::H.bits(), CpuFlags::mask_nh_adc16(0b00001111_11111110, 1, true).bits());
        assert_eq!(CpuFlags::N.bits(), CpuFlags::mask_nh_sbc16(0b00000001, 1, false).bits());
        assert_eq!((CpuFlags::H|CpuFlags::N).bits(), CpuFlags::mask_nh_sbc16(0b00000000, 1, false).bits());
        assert_eq!((CpuFlags::H|CpuFlags::N).bits(), CpuFlags::mask_nh_sbc16(0b00000001, 1, true).bits());
        assert_eq!((CpuFlags::H|CpuFlags::N).bits(), CpuFlags::mask_nh_sbc16(0b00000000, 1, true).bits());
        assert_eq!(CpuFlags::N.bits(), CpuFlags::mask_nh_sbc16(0b00010000, 0, true).bits());
        assert_eq!((CpuFlags::H|CpuFlags::N).bits(), CpuFlags::mask_nh_sbc16(0b00010000_00000000, 0, true).bits());
        assert_eq!(CpuFlags::N.bits(), CpuFlags::mask_nh_sbc16(0b00001000_00000000, 0, true).bits());
        assert_eq!((CpuFlags::H|CpuFlags::N).bits(), CpuFlags::mask_nh_sbc16(0b00010000_00000000, 1, true).bits());
        assert_eq!(CpuFlags::N.bits(), CpuFlags::mask_nh_sbc16(0b00001000_00000000, 1, true).bits());
        assert_eq!((CpuFlags::H|CpuFlags::N).bits(), CpuFlags::mask_nh_sbc16(0b00010000_00000000, 1, false).bits());
        assert_eq!((CpuFlags::N).bits(), CpuFlags::mask_nh_sbc16(0b00001000_00000000, 1, false).bits());
        for i in 0..=127 {
            assert_eq!(0, CpuFlags::mask_sign(i).bits());
        }
        for i in 128..=255 {
            assert_eq!(CpuFlags::S.bits(), CpuFlags::mask_sign(i).bits());
        }
        for i in 1..=255 {
            assert_eq!(0, CpuFlags::mask_zero(i).bits());
        }
        assert_eq!(CpuFlags::Z.bits(), CpuFlags::mask_zero(0).bits());
        assert_eq!(0, CpuFlags::mask_carry(false).bits());
        assert_eq!(CpuFlags::C.bits(), CpuFlags::mask_carry(true).bits());
        assert_eq!(0, CpuFlags::mask_nf(false).bits());
        assert_eq!(CpuFlags::N.bits(), CpuFlags::mask_nf(true).bits());
        assert_eq!(0, CpuFlags::mask_hf(false).bits());
        assert_eq!(CpuFlags::H.bits(), CpuFlags::mask_hf(true).bits());
        assert_eq!(0, CpuFlags::mask_hcf(false).bits());
        assert_eq!(CpuFlags::H.bits()|CpuFlags::C.bits(), CpuFlags::mask_hcf(true).bits());
        assert_eq!(0, CpuFlags::mask_pvf(false).bits());
        assert_eq!(CpuFlags::PV.bits(), CpuFlags::mask_pvf(true).bits());
        assert_eq!(0, CpuFlags::parity(1).bits());
        assert_eq!(0, CpuFlags::parity(7).bits());
        assert_eq!(0, CpuFlags::parity(254).bits());
        assert_eq!(CpuFlags::PV.bits(), CpuFlags::parity(0).bits());
        assert_eq!(CpuFlags::PV.bits(), CpuFlags::parity(3).bits());
        assert_eq!(CpuFlags::PV.bits(), CpuFlags::parity(255).bits());
        assert_eq!((CpuFlags::Z|CpuFlags::P).bits(), CpuFlags::mask_bitops(0, false, false).bits());
        assert_eq!((CpuFlags::Z|CpuFlags::P|CpuFlags::H).bits(), CpuFlags::mask_bitops(0, true, false).bits());
        assert_eq!((CpuFlags::Z|CpuFlags::P|CpuFlags::H|CpuFlags::C).bits(), CpuFlags::mask_bitops(0, true, true).bits());
        assert_eq!((CpuFlags::Z|CpuFlags::P|CpuFlags::C).bits(), CpuFlags::mask_bitops(0, false, true).bits());
        assert_eq!(CpuFlags::S.bits(), CpuFlags::mask_bitops(0x80, false, false).bits());
        assert_eq!(CpuFlags::Y.bits(), CpuFlags::mask_bitops(0b00100000, false, false).bits());
        assert_eq!(CpuFlags::X.bits(), CpuFlags::mask_bitops(0b00001000, false, false).bits());
        assert_eq!((CpuFlags::S|CpuFlags::XY|CpuFlags::H|CpuFlags::C).bits(), CpuFlags::mask_bitops(0b10101000, true, true).bits());
        assert_eq!((CpuFlags::P|CpuFlags::S|CpuFlags::XY|CpuFlags::H|CpuFlags::C).bits(), CpuFlags::mask_bitops(0b10101001, true, true).bits());
        for i in 0..=255 {
            assert_eq!(CpuFlags::from_bits_retain(i), CpuFlags::from_bits_truncate(i))
        }
    }
}
