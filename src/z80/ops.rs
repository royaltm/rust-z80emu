/*
    z80emu: a minimalistic Z80 CPU emulation library.
    Copyright (C) 2019-2020  Rafal Michalski

    For the full copyright notice, see the lib.rs file.
*/
//! Arithmetic, logic, bit and block operations.
//!
//! All Flags involved instructions uses these methods to alter Flags state.
use crate::cpu::CpuFlags;

const CARRY16: u32 = u16::max_value() as u32 + 1;
const CARRY8: u16 = u8::max_value() as u16 + 1;
const LEFTMOSTBIT8: u8 = 1 << 7;
const HALF8_MASK_HI: u8 = 0xF0;
const HALF8_MASK_LO: u8 = 0x0F;

#[inline]
pub fn add16(val: u16, add: u16, flags: &mut CpuFlags) -> (u8, u8) {
    let (rval, cf) = val.overflowing_add(add);
    let [rlo, rhi] = rval.to_le_bytes();
    flags.remove(CpuFlags::XY|CpuFlags::H|CpuFlags::N|CpuFlags::C);
    flags.insert(CpuFlags::mask_nh_add16(val, add) |
                 CpuFlags::mask_xy(rhi) |
                 CpuFlags::mask_carry(cf));
    (rhi, rlo)
}

#[inline]
pub fn adc16(val: u16, add: u16, flags: &mut CpuFlags) -> (u8, u8) {
    let cf0 = flags.cf();
    let resu = (val as u32).wrapping_add(add as u32).wrapping_add(u32::from(cf0));
    let cf = resu & CARRY16 != 0;
    let (vali, vf) = (val as i16).overflowing_add(add as i16);
    let vf = vf ^ vali.checked_add(i16::from(cf0)).is_none();
    let rval = resu as u16;
    let zf = rval == 0;
    let [rlo, rhi] = rval.to_le_bytes();
    *flags = CpuFlags::mask_nh_adc16(val, add, cf0) |
             CpuFlags::mask_sxy(rhi) |
             CpuFlags::mask_cvz(cf, vf, zf);
    (rhi, rlo)
}

#[inline]
pub fn sbc16(val: u16, sub: u16, flags: &mut CpuFlags) -> (u8, u8) {
    let cf0 = flags.cf();
    let resu = (val as u32).wrapping_sub(sub as u32).wrapping_sub(u32::from(cf0));
    let cf = resu & CARRY16 != 0;
    let (vali, vf) = (val as i16).overflowing_sub(sub as i16);
    let vf = vf ^ vali.checked_sub(i16::from(cf0)).is_none();
    let rval = resu as u16;
    let zf = rval == 0;
    let [rlo, rhi] = rval.to_le_bytes();
    *flags = CpuFlags::mask_nh_sbc16(val, sub, cf0) |
             CpuFlags::mask_sxy(rhi) |
             CpuFlags::mask_cvz(cf, vf, zf);
    (rhi, rlo)
}

#[inline]
pub fn add(val: u8, add: u8, flags: &mut CpuFlags) -> u8 {
    let (rval, cf) = val.overflowing_add(add);
    let vf = (val as i8).checked_add(add as i8).is_none();
    let zf = rval == 0;
    *flags = CpuFlags::mask_nh_add(val, add) |
             CpuFlags::mask_sxy(rval) |
             CpuFlags::mask_cvz(cf, vf, zf);
    rval
}

#[inline]
pub fn sub(val: u8, sub: u8, flags: &mut CpuFlags) -> u8 {
    let (rval, cf) = val.overflowing_sub(sub);
    let vf = (val as i8).checked_sub(sub as i8).is_none();
    let zf = rval == 0;
    *flags = CpuFlags::mask_nh_sub(val, sub) |
             CpuFlags::mask_sxy(rval) |
             CpuFlags::mask_cvz(cf, vf, zf);
    rval
}

#[inline]
pub fn cp(val: u8, cmp: u8, flags: &mut CpuFlags) {
    let (rval, cf) = val.overflowing_sub(cmp);
    let vf = (val as i8).checked_sub(cmp as i8).is_none();
    let zf = rval == 0;
    *flags = CpuFlags::mask_nh_sub(val, cmp) |
             CpuFlags::mask_sign(rval) |
             CpuFlags::mask_xy(cmp) |
             CpuFlags::mask_cvz(cf, vf, zf);
}

#[inline]
pub fn adc(val: u8, add: u8, flags: &mut CpuFlags) -> u8 {
    let cf0 = flags.cf();
    let resu = (val as u16).wrapping_add(add as u16).wrapping_add(u16::from(cf0));
    let cf = resu & CARRY8 != 0;
    let (vali, vf) = (val as i8).overflowing_add(add as i8);
    let vf = vf ^ vali.checked_add(i8::from(cf0)).is_none();
    let rval = resu as u8;
    let zf = rval == 0;
    *flags = CpuFlags::mask_nh_adc(val, add, cf0) |
             CpuFlags::mask_sxy(rval) |
             CpuFlags::mask_cvz(cf, vf, zf);
    rval
}

#[inline]
pub fn sbc(val: u8, sub: u8, flags: &mut CpuFlags) -> u8 {
    let cf0 = flags.cf();
    let resu = (val as u16).wrapping_sub(sub as u16).wrapping_sub(u16::from(cf0));
    let cf = resu & CARRY8 != 0;
    let (vali, vf) = (val as i8).overflowing_sub(sub as i8);
    let vf = vf ^ vali.checked_sub(i8::from(cf0)).is_none();
    let rval = resu as u8;
    let zf = rval == 0;
    *flags = CpuFlags::mask_nh_sbc(val, sub, cf0) |
             CpuFlags::mask_sxy(rval) |
             CpuFlags::mask_cvz(cf, vf, zf);
    rval
}

#[inline]
pub fn and(val: u8, arg: u8, flags: &mut CpuFlags) -> u8 {
    let rval = val & arg;
    *flags = CpuFlags::mask_bitops(rval, true, false);
    rval
}

#[inline]
pub fn xor(val: u8, arg: u8, flags: &mut CpuFlags) -> u8 {
    let rval = val ^ arg;
    *flags = CpuFlags::mask_bitops(rval, false, false);
    rval
}

#[inline]
pub fn or(val: u8, arg: u8, flags: &mut CpuFlags) -> u8 {
    let rval = val | arg;
    *flags = CpuFlags::mask_bitops(rval, false, false);
    rval
}

#[inline]
pub fn inc(val: u8, flags: &mut CpuFlags) -> u8 {
    let rval = val.wrapping_add(1);
    let vf = (val as i8).checked_add(1).is_none();
    let zf = rval == 0;
    *flags = CpuFlags::mask_nh_add(val, 1) |
             CpuFlags::mask_sxy(rval) |
             CpuFlags::mask_cvz(flags.cf(), vf, zf);
    rval
}

#[inline]
pub fn dec(val: u8, flags: &mut CpuFlags) -> u8 {
    let rval = val.wrapping_sub(1);
    let vf = (val as i8).checked_sub(1).is_none();
    let zf = rval == 0;
    *flags = CpuFlags::mask_nh_sub(val, 1) |
             CpuFlags::mask_sxy(rval) |
             CpuFlags::mask_cvz(flags.cf(), vf, zf);
    rval
}

#[inline]
pub fn neg(acc: u8, flags: &mut CpuFlags) -> u8 {
    sub(0, acc, flags)
}

#[inline]
pub fn cpl(acc: u8, flags: &mut CpuFlags) -> u8 {
    let rval = !acc;
    flags.remove(CpuFlags::XY);
    flags.insert(CpuFlags::mask_xy(rval)|CpuFlags::H|CpuFlags::N);
    rval
}

#[inline]
pub fn ccf(q: u8, flags: &mut CpuFlags) {
    let cf = flags.cf();
    flags.remove(CpuFlags::XY|CpuFlags::H|CpuFlags::N|CpuFlags::C);
    flags.insert(CpuFlags::mask_xy(q)|CpuFlags::mask_hf(cf)|CpuFlags::mask_carry(!cf));
}

#[inline]
pub fn scf(q: u8, flags: &mut CpuFlags) {
    flags.remove(CpuFlags::XY|CpuFlags::H|CpuFlags::N);
    flags.insert(CpuFlags::mask_xy(q)|CpuFlags::C);
}

#[inline]
pub fn daa(acc: u8, flags: &mut CpuFlags) -> u8 {
    let cf0 = flags.cf();
    let hf0 = flags.hf();
    let nf0 = flags.nf();
    let high_nibble = acc & HALF8_MASK_HI;
    let low_nibble  = acc & HALF8_MASK_LO;
    let diff = match (cf0, high_nibble, hf0, low_nibble) {
        (false, 0x00..=0x90, false, 0x0..=0x9) => 0x00,
        (false, 0x00..=0x90, true , 0x0..=0x9) => 0x06,
        (false, 0x00..=0x80,   _  , 0xA..=0xF) => 0x06,
        (false, 0xA0..=0xF0, false, 0x0..=0x9) => 0x60,
        (true ,      _     , false, 0x0..=0x9) => 0x60,
        (true ,      _     , true , 0x0..=0x9) => 0x66,
        (true ,      _     ,   _  , 0xA..=0xF) => 0x66,
        (false, 0x90..=0xF0,   _  , 0xA..=0xF) => 0x66,
        (false, 0xA0..=0xF0, true , 0x0..=0x9) => 0x66,
        _ => debug_unreachable_unchecked!()
    };
    let cf = match (cf0, high_nibble, low_nibble) {
        (false, 0x00..=0x90, 0x0..=0x9) => false,
        (false, 0x00..=0x80, 0xA..=0xF) => false,
        (false, 0x90..=0xF0, 0xA..=0xF) => true,
        (false, 0xA0..=0xF0, 0x0..=0x9) => true,
        (true ,      _     ,     _    ) => true,
        _ => debug_unreachable_unchecked!()
    };
    let hf = match (nf0, hf0, low_nibble) {
        (false,   _  , 0x0..=0x9) => false,
        (false,   _  , 0xA..=0xF) => true,
        (true , false,     _    ) => false,
        (true , true , 0x6..=0xF) => false,
        (true , true , 0x0..=0x5) => true,
        _ => debug_unreachable_unchecked!()
    };
    let res = if nf0 {
        acc.wrapping_sub(diff)
    }
    else {
        acc.wrapping_add(diff)
    };
    *flags = CpuFlags::mask_bitops(res, hf, cf) | CpuFlags::mask_nf(nf0);
    res
}

#[inline]
pub fn rlca(acc: u8, flags: &mut CpuFlags) -> u8 {
    let acc = acc.rotate_left(1);
    let cflag = CpuFlags::from_bits_truncate(acc & 1);
    flags.remove(CpuFlags::XY|CpuFlags::H|CpuFlags::N|CpuFlags::C);
    flags.insert(CpuFlags::mask_xy(acc)|cflag);
    acc
}

#[inline]
pub fn rrca(acc: u8, flags: &mut CpuFlags) -> u8 {
    let cflag = CpuFlags::from_bits_truncate(acc & 1);
    let acc = acc.rotate_right(1);
    flags.remove(CpuFlags::XY|CpuFlags::H|CpuFlags::N|CpuFlags::C);
    flags.insert(CpuFlags::mask_xy(acc)|cflag);
    acc
}

#[inline]
pub fn rla(acc: u8, flags: &mut CpuFlags) -> u8 {
    let acc = acc.rotate_left(1);
    let cflag = CpuFlags::from_bits_truncate(acc & 1);
    let acc = (acc & !1) | (*flags & CpuFlags::C).bits();
    flags.remove(CpuFlags::XY|CpuFlags::H|CpuFlags::N|CpuFlags::C);
    flags.insert(CpuFlags::mask_xy(acc)|cflag);
    acc
}

#[inline]
pub fn rra(acc: u8, flags: &mut CpuFlags) -> u8 {
    let cflag = CpuFlags::from_bits_truncate(acc & 1);
    let acc = ((acc & !1) | (*flags & CpuFlags::C).bits()).rotate_right(1);
    flags.remove(CpuFlags::XY|CpuFlags::H|CpuFlags::N|CpuFlags::C);
    flags.insert(CpuFlags::mask_xy(acc)|cflag);
    acc
}

#[inline]
pub fn rlc(val: u8, flags: &mut CpuFlags) -> u8 {
    let val = val.rotate_left(1);
    let cf = val & 1 != 0;
    *flags = CpuFlags::mask_bitops(val, false, cf);
    val
}

#[inline]
pub fn rrc(val: u8, flags: &mut CpuFlags) -> u8 {
    let cf = val & 1 != 0;
    let val = val.rotate_right(1);
    *flags = CpuFlags::mask_bitops(val, false, cf);
    val
}

#[inline]
pub fn rl(val: u8, flags: &mut CpuFlags) -> u8 {
    let val = val.rotate_left(1);
    let cf = val & 1 != 0;
    let val = (val & !1) | (*flags & CpuFlags::C).bits();
    *flags = CpuFlags::mask_bitops(val, false, cf);
    val
}

#[inline]
pub fn rr(val: u8, flags: &mut CpuFlags) -> u8 {
    let cf = val & 1 != 0;
    let val = ((val & !1) | (*flags & CpuFlags::C).bits()).rotate_right(1);
    *flags = CpuFlags::mask_bitops(val, false, cf);
    val
}

#[inline]
pub fn sla(val: u8, flags: &mut CpuFlags) -> u8 {
    let cf = val & LEFTMOSTBIT8 != 0;
    let val = val << 1;
    *flags = CpuFlags::mask_bitops(val, false, cf);
    val
}

#[inline]
pub fn sll(val: u8, flags: &mut CpuFlags) -> u8 {
    let cf = val & LEFTMOSTBIT8 != 0;
    let val = (val << 1) | 1;
    *flags = CpuFlags::mask_bitops(val, false, cf);
    val
}

#[inline]
pub fn srl(val: u8, flags: &mut CpuFlags) -> u8 {
    let cf = val & 1 != 0;
    let val = val >> 1;
    *flags = CpuFlags::mask_bitops(val, false, cf);
    val
}

#[inline]
pub fn sra(val: u8, flags: &mut CpuFlags) -> u8 {
    let cf = val & 1 != 0;
    let val = ((val as i8) >> 1) as u8;
    *flags = CpuFlags::mask_bitops(val, false, cf);
    val
}

#[inline]
pub fn rld(acc: u8, oth: u8, flags: &mut CpuFlags) -> (u8, u8) {
    let oth = oth.rotate_left(4);
    let (acc, oth) = ((acc & HALF8_MASK_HI)|(oth & HALF8_MASK_LO), (oth & HALF8_MASK_HI)|(acc & HALF8_MASK_LO));
    *flags = CpuFlags::mask_bitops(acc, false, flags.cf());
    (acc, oth)
}

#[inline]
pub fn rrd(acc: u8, oth: u8, flags: &mut CpuFlags) -> (u8, u8) {
    let (acc, oth) = ((acc & HALF8_MASK_HI)|(oth & HALF8_MASK_LO), (oth & HALF8_MASK_HI)|(acc & HALF8_MASK_LO));
    let oth = oth.rotate_right(4);
    *flags = CpuFlags::mask_bitops(acc, false, flags.cf());
    (acc, oth)
}

/// Use on register
#[inline]
pub fn bit(n: u32, val: u8, flags: &mut CpuFlags) {
    debug_assert!(n <= 7);
    let res = val & (1 << n);
    *flags = CpuFlags::mask_sign(res)|CpuFlags::mask_xy(val)|CpuFlags::H|(*flags & CpuFlags::C)|(if res == 0 {
        CpuFlags::Z|CpuFlags::P
    } else {
        CpuFlags::empty()
    });
}

/// Use on memory (b35=MEMPTR.HI)
#[inline]
pub fn bit_mp(n: u32, val: u8, b35: u8, flags: &mut CpuFlags) {
    debug_assert!(n <= 7);
    let res = val & (1 << n);
    *flags = CpuFlags::mask_sign(res)|CpuFlags::mask_xy(b35)|CpuFlags::H|(*flags & CpuFlags::C)|(if res == 0 {
        CpuFlags::Z|CpuFlags::P
    } else {
        CpuFlags::empty()
    });
}

#[inline]
pub fn res(b: u32, v: u8) -> u8 { !(1 << b) & v }

#[inline]
pub fn set(b: u32, v: u8) -> u8 {  (1 << b) | v }

#[inline]
pub fn ld_a_ir(ir: u8, iff2: bool, flags: &mut CpuFlags) {
    *flags = CpuFlags::mask_sxy(ir)|CpuFlags::mask_zero(ir)|CpuFlags::mask_pvf(iff2)|(*flags & CpuFlags::C);
}

#[inline]
pub fn io(val: u8, flags: &mut CpuFlags) {
    *flags = CpuFlags::mask_bitops(val, false, flags.cf());
}

#[inline]
pub fn ldx(acc: u8, val: u8, bc_is_zero: bool, flags: &mut CpuFlags) {
    let n = val.wrapping_add(acc);
    flags.remove(CpuFlags::H|CpuFlags::XY|CpuFlags::PV|CpuFlags::N);
    flags.insert(CpuFlags::mask_block_op_xy(n) |
                 CpuFlags::mask_pvf(!bc_is_zero));
}

#[inline]
pub fn cpx(acc: u8, cmp: u8, bc_is_zero: bool, flags: &mut CpuFlags) -> bool { // TODO: write tests
    let res = acc.wrapping_sub(cmp);
    let bits = CpuFlags::mask_nh_sub(acc, cmp) |
               CpuFlags::mask_sign(res) |
               CpuFlags::mask_zero(res) |
               CpuFlags::mask_pvf(!bc_is_zero);
    // A - (HL) - HF.
    let n = res.wrapping_sub(bits.hf() as u8);
    *flags = bits |
             CpuFlags::mask_block_op_xy(n) |
             (*flags & CpuFlags::C);
    bc_is_zero || res == 0
}

#[inline]
pub fn iox(io: u8, b: u8, m: u8, flags: &mut CpuFlags) { // TODO: write tests
// SF, ZF, YF, XF flags Affected by decreasing register B, as in DEC B.
// NF flag A copy of bit 7 of the value read from or written to an I/O port.
// HF and CF Both set if ((HL) + L > 255)
// PF The parity of ((((HL) + L) & 7) xor B)
    let (k, hcf) = io.overflowing_add(m);
    let nf = io & (1<<7) != 0;
    *flags = CpuFlags::mask_sxy(b)  |
             CpuFlags::mask_zero(b) |
             CpuFlags::mask_nf(nf)  |
             CpuFlags::mask_hcf(hcf)|
             CpuFlags::parity(k & 7 ^ b);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn add16_works() {
        let mut flags = CpuFlags::empty();
        assert_eq!((0u8, 0u8), add16(0, 0, &mut flags));
        assert_eq!(0u8, flags.bits());
        flags.insert(CpuFlags::N);
        assert_eq!(CpuFlags::N, flags);
        assert_eq!((0u8, 1u8), add16(0, 1, &mut flags));
        assert_eq!(0u8, flags.bits());
        assert_eq!((0x10u8, 0u8), add16(0x0FFF, 1, &mut flags));
        assert_eq!(CpuFlags::H, flags);
        assert_eq!((0xFFu8, 0xFEu8), add16(0xFFFF, 0xFFFF, &mut flags));
        assert_eq!(CpuFlags::H|CpuFlags::C|CpuFlags::XY, flags);
        assert_eq!((0x00u8, 0x00u8), add16(0x0101, 0xFEFF, &mut flags));
        assert_eq!(CpuFlags::H|CpuFlags::C, flags);
        assert_eq!((0x00u8, 0x07u8), add16(0x1007, 0xF000, &mut flags));
        assert_eq!(CpuFlags::C, flags);
        flags.reset();
        assert_eq!(0u8, flags.bits());
        assert_eq!((0x00u8, 0x00u8), adc16(0x0000, 0x0000, &mut flags));
        assert_eq!(CpuFlags::Z, flags);
        flags.insert(CpuFlags::C);
        assert_eq!((0x00u8, 0x01u8), adc16(0x0000, 0x0000, &mut flags));
        assert_eq!(CpuFlags::empty(), flags);
        assert_eq!((0x00u8, 0xFFu8), adc16(0x0100, 0xFFFF, &mut flags));
        assert_eq!(CpuFlags::H|CpuFlags::C, flags);
        flags.remove(CpuFlags::C);
        assert_eq!((0x00u8, 0xFFu8), adc16(0xFFFF, 0x0100, &mut flags));
        assert_eq!(CpuFlags::H|CpuFlags::C, flags);
        assert_eq!((0x01u8, 0x00u8), adc16(0x0100, 0xFFFF, &mut flags));
        assert_eq!(CpuFlags::H|CpuFlags::C, flags);
        flags.remove(CpuFlags::C);
        assert_eq!((0x00u8, 0x00u8), adc16(0x8000, 0x8000, &mut flags));
        assert_eq!(CpuFlags::Z|CpuFlags::V|CpuFlags::C, flags);
        assert_eq!((0x00u8, 0x01u8), adc16(0x8000, 0x8000, &mut flags));
        assert_eq!(CpuFlags::V|CpuFlags::C, flags);
        assert_eq!((0x00u8, 0x00u8), adc16(0x7FFF, 0x8000, &mut flags));
        assert_eq!(CpuFlags::Z|CpuFlags::H|CpuFlags::C, flags);
        flags.remove(CpuFlags::C);
        assert_eq!((0xFFu8, 0xFFu8), adc16(0x7FFF, 0x8000, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::XY, flags);
        assert_eq!((0xFFu8, 0xFFu8), adc16(0x8000, 0x7FFF, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::XY, flags);
        flags.insert(CpuFlags::C);
        assert_eq!((0x00u8, 0x00u8), adc16(0x8000, 0x7FFF, &mut flags));
        assert_eq!(CpuFlags::Z|CpuFlags::H|CpuFlags::C, flags);
        assert_eq!((0x80u8, 0x00u8), adc16(0x0000, 0x7FFF, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::H|CpuFlags::V, flags);
        assert_eq!((0x7Fu8, 0xFFu8), adc16(0x0000, 0x7FFF, &mut flags));
        assert_eq!(CpuFlags::XY, flags);
        assert_eq!((0x7Fu8, 0xFFu8), adc16(0x7FFF, 0x0000, &mut flags));
        assert_eq!(CpuFlags::XY, flags);
        assert_eq!((0x80u8, 0x00u8), adc16(0x7FFF, 0x0001, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::H|CpuFlags::V, flags);
        flags.insert(CpuFlags::C);
        assert_eq!((0x80u8, 0x00u8), adc16(0x7FFF, 0x0000, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::H|CpuFlags::V, flags);
        flags.insert(CpuFlags::C);
        assert_eq!((0x80u8, 0x01u8), adc16(0x7FFF, 0x0001, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::H|CpuFlags::V, flags);
        assert_eq!((0x7Fu8, 0xFFu8), adc16(0x8000, 0xFFFF, &mut flags));
        assert_eq!(CpuFlags::XY|CpuFlags::V|CpuFlags::C, flags);
        assert_eq!((0x80u8, 0x00u8), adc16(0x8000, 0xFFFF, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::H|CpuFlags::C, flags);
        assert_eq!((0x80u8, 0x00u8), adc16(0xFFFF, 0x8000, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::H|CpuFlags::C, flags);
        flags.remove(CpuFlags::C);
        assert_eq!((0x7Fu8, 0xFFu8), adc16(0xFFFF, 0x8000, &mut flags));
        assert_eq!(CpuFlags::XY|CpuFlags::V|CpuFlags::C, flags);
    }

    #[test]
    fn sbc16_works() {
        let mut flags = CpuFlags::empty();
        assert_eq!((0u8, 0u8), sbc16(0, 0, &mut flags));
        assert_eq!(CpuFlags::Z|CpuFlags::N, flags);
        flags.insert(CpuFlags::C);
        assert_eq!((0xFFu8, 0xFFu8), sbc16(0, 0, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::XY|CpuFlags::H|CpuFlags::N|CpuFlags::C, flags);
        assert_eq!((0x7Fu8, 0xFFu8), sbc16(0xFFFF, 0x7FFF, &mut flags));
        assert_eq!(CpuFlags::XY|CpuFlags::H|CpuFlags::V|CpuFlags::N, flags);
        assert_eq!((0x80u8, 0x00u8), sbc16(0xFFFF, 0x7FFF, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::N, flags);
        assert_eq!((0x80u8, 0x00u8), sbc16(0x7FFF, 0xFFFF, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::V|CpuFlags::N|CpuFlags::C, flags);
        assert_eq!((0x7Fu8, 0xFFu8), sbc16(0x7FFF, 0xFFFF, &mut flags));
        assert_eq!(CpuFlags::XY|CpuFlags::H|CpuFlags::N|CpuFlags::C, flags);
        assert_eq!((0xFFu8, 0xFFu8), sbc16(0x8000, 0x8000, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::XY|CpuFlags::H|CpuFlags::N|CpuFlags::C, flags);
        flags.remove(CpuFlags::C);
        assert_eq!((0x00u8, 0x00u8), sbc16(0x8000, 0x8000, &mut flags));
        assert_eq!(CpuFlags::Z|CpuFlags::N, flags);
        assert_eq!((0x80u8, 0x01u8), sbc16(0x8000, 0xFFFF, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::H|CpuFlags::N|CpuFlags::C, flags);
        assert_eq!((0x80u8, 0x00u8), sbc16(0x8000, 0xFFFF, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::H|CpuFlags::N|CpuFlags::C, flags);
        assert_eq!((0x7Fu8, 0xFFu8), sbc16(0x8000, 0, &mut flags));
        assert_eq!(CpuFlags::XY|CpuFlags::H|CpuFlags::V|CpuFlags::N, flags);
        assert_eq!((0x80u8, 0x00u8), sbc16(0x8000, 0, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::N, flags);
        assert_eq!((0x7Fu8, 0xFFu8), sbc16(0x8000, 1, &mut flags));
        assert_eq!(CpuFlags::XY|CpuFlags::H|CpuFlags::V|CpuFlags::N, flags);
        flags.insert(CpuFlags::C);
        assert_eq!((0x7Fu8, 0xFEu8), sbc16(0x8000, 1, &mut flags));
        assert_eq!(CpuFlags::XY|CpuFlags::H|CpuFlags::V|CpuFlags::N, flags);
        assert_eq!((0x00u8, 0x00u8), sbc16(1, 1, &mut flags));
        assert_eq!(CpuFlags::Z|CpuFlags::N, flags);
        flags.insert(CpuFlags::C);
        assert_eq!((0xFFu8, 0xFFu8), sbc16(1, 1, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::XY|CpuFlags::H|CpuFlags::N|CpuFlags::C, flags);
    }

    #[test]
    fn add_works() {
        let mut flags = CpuFlags::empty();
        assert_eq!(0u8, add(0, 0, &mut flags));
        assert_eq!(CpuFlags::Z, flags);
        flags.insert(CpuFlags::N);
        assert_eq!(CpuFlags::Z|CpuFlags::N, flags);
        assert_eq!(1u8, add(0, 1, &mut flags));
        assert_eq!(0u8, flags.bits());
        assert_eq!(0x10u8, add(0x0F, 1, &mut flags));
        assert_eq!(CpuFlags::H, flags);
        assert_eq!(0xFEu8, add(0xFF, 0xFF, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::H|CpuFlags::C|CpuFlags::XY, flags);
        assert_eq!(0x00u8, add(0x11, 0xEF, &mut flags));
        assert_eq!(CpuFlags::Z|CpuFlags::H|CpuFlags::C, flags);
        assert_eq!(0x07u8, add(0x17, 0xF0, &mut flags));
        assert_eq!(CpuFlags::C, flags);
        flags.reset();
        assert_eq!(0u8, flags.bits());
        assert_eq!(0u8, adc(0x00, 0x00, &mut flags));
        assert_eq!(CpuFlags::Z, flags);
        flags.insert(CpuFlags::C);
        assert_eq!(0x01u8, adc(0x00, 0x00, &mut flags));
        assert_eq!(CpuFlags::empty(), flags);
        assert_eq!(0x0Fu8, adc(0x10, 0xFF, &mut flags));
        assert_eq!(CpuFlags::X|CpuFlags::C, flags);
        flags.remove(CpuFlags::C);
        assert_eq!(0x0Fu8, adc(0xFF, 0x10, &mut flags));
        assert_eq!(CpuFlags::X|CpuFlags::C, flags);
        assert_eq!(0x10u8, adc(0x10, 0xFF, &mut flags));
        assert_eq!(CpuFlags::H|CpuFlags::C, flags);
        flags.remove(CpuFlags::C);
        assert_eq!(0u8, adc(0x80, 0x80, &mut flags));
        assert_eq!(CpuFlags::Z|CpuFlags::V|CpuFlags::C, flags);
        assert_eq!(1u8, adc(0x80, 0x80, &mut flags));
        assert_eq!(CpuFlags::V|CpuFlags::C, flags);
        assert_eq!(0u8, adc(0x7F, 0x80, &mut flags));
        assert_eq!(CpuFlags::Z|CpuFlags::H|CpuFlags::C, flags);
        flags.remove(CpuFlags::C);
        assert_eq!(0xFFu8, adc(0x7F, 0x80, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::XY, flags);
        assert_eq!(0xFFu8, adc(0x80, 0x7F, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::XY, flags);
        flags.insert(CpuFlags::C);
        assert_eq!(0u8, adc(0x80, 0x7F, &mut flags));
        assert_eq!(CpuFlags::Z|CpuFlags::H|CpuFlags::C, flags);
        assert_eq!(0x80u8, adc(0x00, 0x7F, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::H|CpuFlags::V, flags);
        assert_eq!(0x7Fu8, adc(0x00, 0x7F, &mut flags));
        assert_eq!(CpuFlags::XY, flags);
        assert_eq!(0x7Fu8, adc(0x7F, 0x00, &mut flags));
        assert_eq!(CpuFlags::XY, flags);
        assert_eq!(0x80u8, adc(0x7F, 0x01, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::H|CpuFlags::V, flags);
        flags.insert(CpuFlags::C);
        assert_eq!(0x80u8, adc(0x7F, 0x00, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::H|CpuFlags::V, flags);
        flags.insert(CpuFlags::C);
        assert_eq!(0x81u8, adc(0x7F, 0x01, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::H|CpuFlags::V, flags);
        assert_eq!(0x7Fu8, adc(0x80, 0xFF, &mut flags));
        assert_eq!(CpuFlags::XY|CpuFlags::V|CpuFlags::C, flags);
        assert_eq!(0x80u8, adc(0x80, 0xFF, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::H|CpuFlags::C, flags);
        assert_eq!(0x80u8, adc(0xFF, 0x80, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::H|CpuFlags::C, flags);
        flags.remove(CpuFlags::C);
        assert_eq!(0x7Fu8, adc(0xFF, 0x80, &mut flags));
        assert_eq!(CpuFlags::XY|CpuFlags::V|CpuFlags::C, flags);
    }

    #[test]
    fn sub_works() {
        let mut flags = CpuFlags::empty();
        assert_eq!(0u8, sub(0, 0, &mut flags));
        assert_eq!(CpuFlags::Z|CpuFlags::N, flags);
        assert_eq!(0u8, neg(0, &mut flags));
        assert_eq!(CpuFlags::Z|CpuFlags::N, flags);
        for i in 1..=255 {
            flags.remove(CpuFlags::N);
            assert_eq!(CpuFlags::Z, flags);
            assert_eq!(0u8, sub(i, i, &mut flags));
            assert_eq!(CpuFlags::Z|CpuFlags::N, flags);
        }
        assert_eq!(1u8, sub(2, 1, &mut flags));
        assert_eq!(CpuFlags::N, flags);
        assert_eq!(0x0Fu8, sub(0x10, 1, &mut flags));
        assert_eq!(CpuFlags::H|CpuFlags::X|CpuFlags::N, flags);
        assert_eq!(0xFFu8, sub(0xFE, 0xFF, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::H|CpuFlags::XY|CpuFlags::N|CpuFlags::C, flags);
        assert_eq!(0x01u8, sub(0xFF, 0xFE, &mut flags));
        assert_eq!(CpuFlags::N, flags);
        assert_eq!(0x11u8, sub(0x00, 0xEF, &mut flags));
        assert_eq!(CpuFlags::H|CpuFlags::N|CpuFlags::C, flags);
        assert_eq!(0x11u8, neg(0xEF, &mut flags));
        assert_eq!(CpuFlags::H|CpuFlags::N|CpuFlags::C, flags);
        assert_eq!(0xEFu8, neg(0x11, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::XY|CpuFlags::H|CpuFlags::N|CpuFlags::C, flags);
        assert_eq!(0x17u8, sub(0x07, 0xF0, &mut flags));
        assert_eq!(CpuFlags::C|CpuFlags::N, flags);
        assert_eq!(0x7Fu8, sub(0x80, 1, &mut flags));
        assert_eq!(CpuFlags::V|CpuFlags::XY|CpuFlags::H|CpuFlags::N, flags);
        assert_eq!(0x81u8, sub(1, 0x80, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::V|CpuFlags::N|CpuFlags::C, flags);
        assert_eq!(0x80u8, sub(0x7F, 0xFF, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::V|CpuFlags::N|CpuFlags::C, flags);
        assert_eq!(0x80u8, sub(0xFF, 0x7F, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::N, flags);
        flags.reset();
        assert_eq!(0u8, sbc(0, 0, &mut flags));
        assert_eq!(CpuFlags::Z|CpuFlags::N, flags);
        flags.insert(CpuFlags::C);
        assert_eq!(0xFFu8, sbc(0, 0, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::XY|CpuFlags::H|CpuFlags::N|CpuFlags::C, flags);
        assert_eq!(0x7Fu8, sbc(0xFF, 0x7F, &mut flags));
        assert_eq!(CpuFlags::XY|CpuFlags::H|CpuFlags::V|CpuFlags::N, flags);
        assert_eq!(0x80u8, sbc(0xFF, 0x7F, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::N, flags);
        assert_eq!(0x80u8, sbc(0x7F, 0xFF, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::V|CpuFlags::N|CpuFlags::C, flags);
        assert_eq!(0x7Fu8, sbc(0x7F, 0xFF, &mut flags));
        assert_eq!(CpuFlags::XY|CpuFlags::H|CpuFlags::N|CpuFlags::C, flags);
        assert_eq!(0xFFu8, sbc(0x80, 0x80, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::XY|CpuFlags::H|CpuFlags::N|CpuFlags::C, flags);
        flags.remove(CpuFlags::C);
        assert_eq!(0x00u8, sbc(0x80, 0x80, &mut flags));
        assert_eq!(CpuFlags::Z|CpuFlags::N, flags);
        assert_eq!(0x81u8, sbc(0x80, 0xFF, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::H|CpuFlags::N|CpuFlags::C, flags);
        assert_eq!(0x80u8, sbc(0x80, 0xFF, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::H|CpuFlags::N|CpuFlags::C, flags);
        assert_eq!(0x7Fu8, sbc(0x80, 0, &mut flags));
        assert_eq!(CpuFlags::XY|CpuFlags::H|CpuFlags::V|CpuFlags::N, flags);
        assert_eq!(0x80u8, sbc(0x80, 0, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::N, flags);
        assert_eq!(0x7Fu8, sbc(0x80, 1, &mut flags));
        assert_eq!(CpuFlags::XY|CpuFlags::H|CpuFlags::V|CpuFlags::N, flags);
        flags.insert(CpuFlags::C);
        assert_eq!(0x7Eu8, sbc(0x80, 1, &mut flags));
        assert_eq!(CpuFlags::XY|CpuFlags::H|CpuFlags::V|CpuFlags::N, flags);
        assert_eq!(0u8, sbc(1, 1, &mut flags));
        assert_eq!(CpuFlags::Z|CpuFlags::N, flags);
        flags.insert(CpuFlags::C);
        assert_eq!(0xFFu8, sbc(1, 1, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::XY|CpuFlags::H|CpuFlags::N|CpuFlags::C, flags);
    }

    #[test]
    fn cp_works() {
        let mut flags = CpuFlags::empty();
        cp(0, 0, &mut flags);
        assert_eq!(CpuFlags::Z|CpuFlags::N, flags);
        flags.remove(CpuFlags::N);
        assert_eq!(CpuFlags::Z, flags);
        for i in 1..=255 {
            cp(i, i, &mut flags);
            assert_eq!(CpuFlags::Z|CpuFlags::N|CpuFlags::mask_xy(i), flags);
        }
        cp(2, 1, &mut flags);
        assert_eq!(CpuFlags::N, flags);
        cp(0x10, 1, &mut flags);
        assert_eq!(CpuFlags::H|CpuFlags::N, flags);
        cp(0xFE, 0xFF, &mut flags);
        assert_eq!(CpuFlags::S|CpuFlags::H|CpuFlags::C|CpuFlags::XY|CpuFlags::N, flags);
        cp(0xFF, 0xFE, &mut flags);
        assert_eq!(CpuFlags::XY|CpuFlags::N, flags);
        cp(0x00, 0xEF, &mut flags);
        assert_eq!(CpuFlags::H|CpuFlags::C|CpuFlags::XY|CpuFlags::N, flags);
        cp(0x07, 0xF0, &mut flags);
        assert_eq!(CpuFlags::C|CpuFlags::N|CpuFlags::Y, flags);
        cp(0x80, 1, &mut flags);
        assert_eq!(CpuFlags::V|CpuFlags::H|CpuFlags::N, flags);
        cp(1, 0x80, &mut flags);
        assert_eq!(CpuFlags::S|CpuFlags::V|CpuFlags::C|CpuFlags::N, flags);
        cp(0x7F, 0xFF, &mut flags);
        assert_eq!(CpuFlags::S|CpuFlags::V|CpuFlags::C|CpuFlags::XY|CpuFlags::N, flags);
        cp(0xFF, 0x7F, &mut flags);
        assert_eq!(CpuFlags::S|CpuFlags::XY|CpuFlags::N, flags);
    }

    #[test]
    fn inc_works() {
        let mut flags = CpuFlags::empty();
        assert_eq!(1u8, inc(0, &mut flags));
        assert_eq!(CpuFlags::empty(), flags);
        flags.insert(CpuFlags::N);
        assert_eq!(0u8, inc(0xFF, &mut flags));
        assert_eq!(CpuFlags::Z|CpuFlags::H, flags);
        flags.insert(CpuFlags::C);
        assert_eq!(0u8, inc(0xFF, &mut flags));
        assert_eq!(CpuFlags::Z|CpuFlags::H|CpuFlags::C, flags);
        flags.remove(CpuFlags::C);
        assert_eq!(0x10u8, inc(0x0F, &mut flags));
        assert_eq!(CpuFlags::H, flags);
        assert_eq!(0x11u8, inc(0x10, &mut flags));
        assert_eq!(CpuFlags::empty(), flags);
        assert_eq!(0x80u8, inc(0x7F, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::H|CpuFlags::V, flags);
        assert_eq!(0x81u8, inc(0x80, &mut flags));
        assert_eq!(CpuFlags::S, flags);
        assert_eq!(0x90u8, inc(0x8F, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::H, flags);
        assert_eq!(0x89u8, inc(0x88, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::X, flags);
        assert_eq!(0x79u8, inc(0x78, &mut flags));
        assert_eq!(CpuFlags::XY, flags);
    }

    #[test]
    fn dec_works() {
        let mut flags = CpuFlags::empty();
        assert_eq!(0x01u8, dec(2, &mut flags));
        assert_eq!(CpuFlags::N, flags);
        flags.insert(CpuFlags::C);
        assert_eq!(0u8, dec(1, &mut flags));
        assert_eq!(CpuFlags::Z|CpuFlags::N|CpuFlags::C, flags);
        flags.remove(CpuFlags::C);
        assert_eq!(0u8, dec(1, &mut flags));
        assert_eq!(CpuFlags::Z|CpuFlags::N, flags);
        assert_eq!(0xFFu8, dec(0, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::H|CpuFlags::XY|CpuFlags::N, flags);
        assert_eq!(0x7Fu8, dec(0x80, &mut flags));
        assert_eq!(CpuFlags::H|CpuFlags::XY|CpuFlags::V|CpuFlags::N, flags);
    }

    #[test]
    fn cpl_works() {
        let mut flags = CpuFlags::S|CpuFlags::Z|CpuFlags::C|CpuFlags::PV;
        assert_eq!(0xAAu8, cpl(0x55, &mut flags));
        assert_eq!(CpuFlags::all(), flags);
        assert_eq!(0x55u8, cpl(0xAA, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::Z|CpuFlags::C|CpuFlags::PV|CpuFlags::H|CpuFlags::N, flags);
        flags.reset();
        assert_eq!(0xFFu8, cpl(0x00, &mut flags));
        assert_eq!(CpuFlags::XY|CpuFlags::H|CpuFlags::N, flags);
        assert_eq!(0x00u8, cpl(0xFF, &mut flags));
        assert_eq!(CpuFlags::H|CpuFlags::N, flags);
    }

    #[test]
    fn xcf_works() {
        let mut flags = CpuFlags::all();
        ccf(0, &mut flags);
        assert_eq!(CpuFlags::S|CpuFlags::Z|CpuFlags::PV|CpuFlags::H, flags);
        ccf(0, &mut flags);
        assert_eq!(CpuFlags::S|CpuFlags::Z|CpuFlags::PV|CpuFlags::C, flags);
        ccf(0xFF, &mut flags);
        assert_eq!(CpuFlags::S|CpuFlags::Z|CpuFlags::PV|CpuFlags::H|CpuFlags::XY, flags);
        ccf(0xFF, &mut flags);
        assert_eq!(CpuFlags::S|CpuFlags::Z|CpuFlags::PV|CpuFlags::C|CpuFlags::XY, flags);
        ccf(0xF0, &mut flags);
        assert_eq!(CpuFlags::S|CpuFlags::Z|CpuFlags::PV|CpuFlags::H|CpuFlags::Y, flags);
        scf(0, &mut flags);
        assert_eq!(CpuFlags::S|CpuFlags::Z|CpuFlags::PV|CpuFlags::C, flags);
        scf(0xFF, &mut flags);
        assert_eq!(CpuFlags::S|CpuFlags::Z|CpuFlags::PV|CpuFlags::C|CpuFlags::XY, flags);
        scf(0x0F, &mut flags);
        assert_eq!(CpuFlags::S|CpuFlags::Z|CpuFlags::PV|CpuFlags::C|CpuFlags::X, flags);
    }

    #[test]
    fn bitops_work() {
        let mut flags = CpuFlags::C;
        assert_eq!(0u8, and(0, 0, &mut flags));
        assert_eq!(CpuFlags::Z|CpuFlags::P|CpuFlags::H, flags);
        flags.insert(CpuFlags::C);
        assert_eq!(0u8, and(0, 1, &mut flags));
        assert_eq!(CpuFlags::Z|CpuFlags::P|CpuFlags::H, flags);
        assert_eq!(1u8, and(1, 1, &mut flags));
        assert_eq!(CpuFlags::H, flags);
        assert_eq!(3u8, and(3, 255, &mut flags));
        assert_eq!(CpuFlags::H|CpuFlags::P, flags);
        assert_eq!(255u8, and(255, 255, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::XY|CpuFlags::H|CpuFlags::P, flags);
        flags.insert(CpuFlags::C);
        assert_eq!(0u8, or(0, 0, &mut flags));
        assert_eq!(CpuFlags::Z|CpuFlags::P, flags);
        assert_eq!(1u8, or(0, 1, &mut flags));
        assert_eq!(CpuFlags::empty(), flags);
        assert_eq!(1u8, or(1, 1, &mut flags));
        assert_eq!(CpuFlags::empty(), flags);
        assert_eq!(255u8, or(3, 255, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::XY|CpuFlags::P, flags);
        assert_eq!(255u8, or(255, 255, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::XY|CpuFlags::P, flags);
        flags.insert(CpuFlags::C);
        assert_eq!(0u8, xor(0, 0, &mut flags));
        assert_eq!(CpuFlags::Z|CpuFlags::P, flags);
        assert_eq!(1u8, xor(0, 1, &mut flags));
        assert_eq!(CpuFlags::empty(), flags);
        assert_eq!(0u8, xor(1, 1, &mut flags));
        assert_eq!(CpuFlags::Z|CpuFlags::P, flags);
        assert_eq!(252u8, xor(3, 255, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::XY|CpuFlags::P, flags);
        assert_eq!(0u8, xor(255, 255, &mut flags));
        assert_eq!(CpuFlags::Z|CpuFlags::P, flags);
    }

    #[test]
    fn daa_works() {
        let mut flags = CpuFlags::empty();
        assert_eq!(0u8, daa(add(0, 0, &mut flags), &mut flags));
        assert_eq!(CpuFlags::Z|CpuFlags::P, flags);
        assert_eq!(2u8, daa(add(1, 1, &mut flags), &mut flags));
        assert_eq!(CpuFlags::empty(), flags);
        assert_eq!(0x10u8, daa(add(9, 1, &mut flags), &mut flags));
        assert_eq!(CpuFlags::H, flags);
        assert_eq!(0x10u8, daa(add(1, 9, &mut flags), &mut flags));
        assert_eq!(CpuFlags::H, flags);
        assert_eq!(0x18u8, daa(add(9, 9, &mut flags), &mut flags));
        assert_eq!(CpuFlags::X|CpuFlags::P, flags);
        assert_eq!(0x64u8, daa(add(0xFF, 0xFF, &mut flags), &mut flags));
        assert_eq!(CpuFlags::Y|CpuFlags::H|CpuFlags::C, flags);
        assert_eq!(0x65u8, daa(add(0, 0xFF, &mut flags), &mut flags));
        assert_eq!(CpuFlags::Y|CpuFlags::H|CpuFlags::P|CpuFlags::C, flags);
        assert_eq!(0x65u8, daa(add(0xFF, 0, &mut flags), &mut flags));
        assert_eq!(CpuFlags::Y|CpuFlags::H|CpuFlags::P|CpuFlags::C, flags);
        assert_eq!(0xFEu8, daa(add(0xFF, 0x99, &mut flags), &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::XY|CpuFlags::C, flags);
        assert_eq!(0xFEu8, daa(add(0x99, 0xFF, &mut flags), &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::XY|CpuFlags::C, flags);
        assert_eq!(0x98u8, daa(add(0x99, 0x99, &mut flags), &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::X|CpuFlags::C, flags);
        assert_eq!(0x10u8, daa(add(0x99, 0x11, &mut flags), &mut flags));
        assert_eq!(CpuFlags::H|CpuFlags::C, flags);
        assert_eq!(0x10u8, daa(add(0x11, 0x99, &mut flags), &mut flags));
        assert_eq!(CpuFlags::H|CpuFlags::C, flags);
        assert_eq!(0u8, daa(add(0x99, 1, &mut flags), &mut flags));
        assert_eq!(CpuFlags::Z|CpuFlags::H|CpuFlags::P|CpuFlags::C, flags);
        assert_eq!(0u8, daa(add(1, 0x99, &mut flags), &mut flags));
        assert_eq!(CpuFlags::Z|CpuFlags::H|CpuFlags::P|CpuFlags::C, flags);
        assert_eq!(0x09u8, daa(add(0x90, 0x19, &mut flags), &mut flags));
        assert_eq!(CpuFlags::X|CpuFlags::P|CpuFlags::C, flags);
        assert_eq!(0x09u8, daa(add(0x19, 0x90, &mut flags), &mut flags));
        assert_eq!(CpuFlags::X|CpuFlags::P|CpuFlags::C, flags);
        for i in 0..=255 {
            assert_eq!(0u8, daa(sub(i, i, &mut flags), &mut flags));
            assert_eq!(CpuFlags::Z|CpuFlags::P|CpuFlags::N, flags);
        }
        assert_eq!(1u8, daa(sub(1, 0, &mut flags), &mut flags));
        assert_eq!(CpuFlags::N, flags);
        assert_eq!(0x99u8, daa(sub(0, 1, &mut flags), &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::X|CpuFlags::P|CpuFlags::N|CpuFlags::C, flags);
        for i in [0, 2, 3].iter().cloned() {
            assert_eq!(0x9Bu8+i, daa(sub(i, 0xFF, &mut flags), &mut flags));
            assert_eq!(CpuFlags::S|CpuFlags::H|CpuFlags::X|CpuFlags::N|CpuFlags::C, flags);
        }
        for i in [1, 4].iter().cloned() {
            assert_eq!(0x9Bu8+i, daa(sub(i, 0xFF, &mut flags), &mut flags));
            assert_eq!(CpuFlags::S|CpuFlags::H|CpuFlags::X|CpuFlags::P|CpuFlags::N|CpuFlags::C, flags);
        }
        for i in [5,8,0xA,0xB].iter().cloned() {
            assert_eq!(0x9Bu8+i, daa(sub(i, 0xFF, &mut flags), &mut flags));
            assert_eq!(CpuFlags::S|CpuFlags::Y|CpuFlags::P|CpuFlags::N|CpuFlags::C, flags);
        }
        for i in [6,7,9,0xC].iter().cloned() {
            assert_eq!(0x9Bu8+i, daa(sub(i, 0xFF, &mut flags), &mut flags));
            assert_eq!(CpuFlags::S|CpuFlags::Y|CpuFlags::N|CpuFlags::C, flags);
        }
        assert_eq!(0xB0u8, daa(sub(0x0F, 0xFF, &mut flags), &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::Y|CpuFlags::N|CpuFlags::C, flags);
        assert_eq!(0xA8u8, daa(sub(0x0D, 0xFF, &mut flags), &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::XY|CpuFlags::N|CpuFlags::C, flags);
        assert_eq!(0xA9u8, daa(sub(0x0E, 0xFF, &mut flags), &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::XY|CpuFlags::P|CpuFlags::N|CpuFlags::C, flags);
        assert_eq!(0x99u8, daa(sub(0xFF, 0, &mut flags), &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::X|CpuFlags::P|CpuFlags::N|CpuFlags::C, flags);
        assert_eq!(0x09u8, daa(sub(0x10, 0x01, &mut flags), &mut flags));
        assert_eq!(CpuFlags::X|CpuFlags::P|CpuFlags::N, flags);
        assert_eq!(0x91u8, daa(sub(0x01, 0x10, &mut flags), &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::N|CpuFlags::C, flags);
        assert_eq!(0x12u8, daa(sub(0x11, 0x99, &mut flags), &mut flags));
        assert_eq!(CpuFlags::P|CpuFlags::N|CpuFlags::C, flags);
        assert_eq!(0x88u8, daa(sub(0x99, 0x11, &mut flags), &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::X|CpuFlags::P|CpuFlags::N, flags);
        assert_eq!(0x80u8, daa(sub(0x90, 0x10, &mut flags), &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::N, flags);
        assert_eq!(0x20u8, daa(sub(0x10, 0x90, &mut flags), &mut flags));
        assert_eq!(CpuFlags::Y|CpuFlags::N|CpuFlags::C, flags);
        assert_eq!(0x92u8, daa(sub(0x01, 0x09, &mut flags), &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::N|CpuFlags::C, flags);
        assert_eq!(0x08u8, daa(sub(0x09, 0x01, &mut flags), &mut flags));
        assert_eq!(CpuFlags::X|CpuFlags::N, flags);
        assert_eq!(0x99u8, daa(sub(0x98, 0x99, &mut flags), &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::X|CpuFlags::P|CpuFlags::N|CpuFlags::C, flags);
        assert_eq!(0x01u8, daa(sub(0x99, 0x98, &mut flags), &mut flags));
        assert_eq!(CpuFlags::N, flags);
        assert_eq!(0x81u8, daa(sub(0x90, 0x09, &mut flags), &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::P|CpuFlags::N, flags);
        assert_eq!(0x19u8, daa(sub(0x09, 0x90, &mut flags), &mut flags));
        assert_eq!(CpuFlags::X|CpuFlags::N|CpuFlags::C, flags);
        assert_eq!(0x40u8, daa(sub(0xAA, 0x0A, &mut flags), &mut flags));
        assert_eq!(CpuFlags::N|CpuFlags::C, flags);
        assert_eq!(0u8, daa(sub(0x0A, 0xAA, &mut flags), &mut flags));
        assert_eq!(CpuFlags::Z|CpuFlags::P|CpuFlags::N|CpuFlags::C, flags);

    }

    #[test]
    fn rotshift_works() {
        let mut flags = CpuFlags::all();
        assert_eq!(0u8, rlca(0, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::Z|CpuFlags::P, flags);
        flags = CpuFlags::all();
        assert_eq!(0u8, rrca(0, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::Z|CpuFlags::P, flags);
        flags = CpuFlags::all();
        assert_eq!(1u8, rlca(0x80, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::Z|CpuFlags::P|CpuFlags::C, flags);
        flags = CpuFlags::all();
        assert_eq!(0x80u8, rrca(1, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::Z|CpuFlags::P|CpuFlags::C, flags);
        flags = CpuFlags::all();
        assert_eq!(0xFEu8, rlca(0x7F, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::Z|CpuFlags::P|CpuFlags::XY, flags);
        flags = CpuFlags::all();
        assert_eq!(0x7Fu8, rrca(0xFE, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::Z|CpuFlags::P|CpuFlags::XY, flags);
        flags = CpuFlags::all();
        assert_eq!(0x01u8, rla(0, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::Z|CpuFlags::P, flags);
        flags = CpuFlags::all();
        assert_eq!(0x80u8, rra(0, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::Z|CpuFlags::P, flags);
        flags = CpuFlags::empty();
        assert_eq!(0u8, rla(0, &mut flags));
        assert_eq!(CpuFlags::empty(), flags);
        flags = CpuFlags::empty();
        assert_eq!(0u8, rra(0, &mut flags));
        assert_eq!(CpuFlags::empty(), flags);
        flags = CpuFlags::all();
        assert_eq!(0xFFu8, rla(0x7F, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::Z|CpuFlags::P|CpuFlags::XY, flags);
        flags = CpuFlags::all();
        assert_eq!(0xFFu8, rra(0xFE, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::Z|CpuFlags::P|CpuFlags::XY, flags);
        flags = CpuFlags::empty();
        assert_eq!(0u8, rla(0x80, &mut flags));
        assert_eq!(CpuFlags::C, flags);
        assert_eq!(0x80u8, rra(0, &mut flags));
        assert_eq!(CpuFlags::empty(), flags);
        assert_eq!(0u8, rra(1, &mut flags));
        assert_eq!(CpuFlags::C, flags);
        assert_eq!(0x01u8, rla(0, &mut flags));
        assert_eq!(CpuFlags::empty(), flags);

        flags = CpuFlags::all();
        assert_eq!(0u8, rlc(0, &mut flags));
        assert_eq!(CpuFlags::Z|CpuFlags::P, flags);
        flags = CpuFlags::empty();
        assert_eq!(0u8, rlc(0, &mut flags));
        assert_eq!(CpuFlags::Z|CpuFlags::P, flags);
        flags = CpuFlags::all();
        assert_eq!(0u8, rrc(0, &mut flags));
        assert_eq!(CpuFlags::Z|CpuFlags::P, flags);
        flags = CpuFlags::empty();
        assert_eq!(0u8, rrc(0, &mut flags));
        assert_eq!(CpuFlags::Z|CpuFlags::P, flags);
        flags = CpuFlags::all();
        assert_eq!(1u8, rlc(0x80, &mut flags));
        assert_eq!(CpuFlags::C, flags);
        flags = CpuFlags::all();
        assert_eq!(0x80u8, rrc(1, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::C, flags);
        flags = CpuFlags::all();
        assert_eq!(0xFEu8, rlc(0x7F, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::XY, flags);
        flags = CpuFlags::all();
        assert_eq!(0x7Fu8, rrc(0xFE, &mut flags));
        assert_eq!(CpuFlags::XY, flags);
        flags = CpuFlags::all();
        assert_eq!(0x01u8, rl(0, &mut flags));
        assert_eq!(CpuFlags::empty(), flags);
        flags = CpuFlags::all();
        assert_eq!(0x80u8, rr(0, &mut flags));
        assert_eq!(CpuFlags::S, flags);
        flags = CpuFlags::empty();
        assert_eq!(0u8, rl(0, &mut flags));
        assert_eq!(CpuFlags::Z|CpuFlags::P, flags);
        flags = CpuFlags::empty();
        assert_eq!(0u8, rr(0, &mut flags));
        assert_eq!(CpuFlags::Z|CpuFlags::P, flags);
        flags = CpuFlags::all();
        assert_eq!(0xFFu8, rl(0x7F, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::P|CpuFlags::XY, flags);
        flags = CpuFlags::all();
        assert_eq!(0xFFu8, rr(0xFE, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::P|CpuFlags::XY, flags);
        flags = CpuFlags::empty();
        assert_eq!(0u8, rl(0x80, &mut flags));
        assert_eq!(CpuFlags::Z|CpuFlags::P|CpuFlags::C, flags);
        assert_eq!(0x80u8, rr(0, &mut flags));
        assert_eq!(CpuFlags::S, flags);
        assert_eq!(0u8, rr(1, &mut flags));
        assert_eq!(CpuFlags::Z|CpuFlags::P|CpuFlags::C, flags);
        assert_eq!(1u8, rl(0, &mut flags));
        assert_eq!(CpuFlags::empty(), flags);

        flags = CpuFlags::all();
        assert_eq!(0u8, sla(0, &mut flags));
        assert_eq!(CpuFlags::Z|CpuFlags::P, flags);
        flags = CpuFlags::all();
        assert_eq!(0x01u8, sll(0, &mut flags));
        assert_eq!(CpuFlags::empty(), flags);
        flags = CpuFlags::all();
        assert_eq!(0u8, sla(0x80, &mut flags));
        assert_eq!(CpuFlags::Z|CpuFlags::P|CpuFlags::C, flags);
        flags = CpuFlags::all();
        assert_eq!(0x01u8, sll(0x80, &mut flags));
        assert_eq!(CpuFlags::C, flags);
        flags = CpuFlags::all();
        assert_eq!(0x01u8, sll(0x80, &mut flags));
        assert_eq!(CpuFlags::C, flags);
        flags = CpuFlags::all();
        assert_eq!(0x1Eu8, sla(0x0F, &mut flags));
        assert_eq!(CpuFlags::P|CpuFlags::X, flags);
        flags = CpuFlags::all();
        assert_eq!(0x1Fu8, sll(0x0F, &mut flags));
        assert_eq!(CpuFlags::X, flags);
        flags = CpuFlags::all();
        assert_eq!(0x30u8, sla(0x18, &mut flags));
        assert_eq!(CpuFlags::P|CpuFlags::Y, flags);
        flags = CpuFlags::all();
        assert_eq!(0x31u8, sll(0x18, &mut flags));
        assert_eq!(CpuFlags::Y, flags);
        flags = CpuFlags::all();
        assert_eq!(0xFEu8, sla(0xFF, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::XY|CpuFlags::C, flags);
        flags = CpuFlags::all();
        assert_eq!(0xFFu8, sll(0xFF, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::XY|CpuFlags::P|CpuFlags::C, flags);

        flags = CpuFlags::all();
        assert_eq!(0u8, sra(0, &mut flags));
        assert_eq!(CpuFlags::Z|CpuFlags::P, flags);
        flags = CpuFlags::all();
        assert_eq!(0u8, srl(0, &mut flags));
        assert_eq!(CpuFlags::Z|CpuFlags::P, flags);
        flags = CpuFlags::all();
        assert_eq!(0xC0u8, sra(0x81, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::P|CpuFlags::C, flags);
        assert_eq!(0xC0u8, sra(0x80, &mut flags));
        assert_eq!(CpuFlags::S|CpuFlags::P, flags);
        flags = CpuFlags::all();
        assert_eq!(0x40u8, srl(0x81, &mut flags));
        assert_eq!(CpuFlags::C, flags);
        assert_eq!(0x40u8, srl(0x80, &mut flags));
        assert_eq!(CpuFlags::empty(), flags);
        flags = CpuFlags::all();
        assert_eq!(0x0Cu8, sra(0x18, &mut flags));
        assert_eq!(CpuFlags::P|CpuFlags::X, flags);
        flags = CpuFlags::all();
        assert_eq!(0x0Cu8, srl(0x18, &mut flags));
        assert_eq!(CpuFlags::P|CpuFlags::X, flags);

        flags = CpuFlags::all();
        assert_eq!((0u8, 0u8), rld(0, 0, &mut flags));
        assert_eq!(CpuFlags::Z|CpuFlags::P|CpuFlags::C, flags);
        flags = CpuFlags::empty();
        assert_eq!((0u8, 0u8), rld(0, 0, &mut flags));
        assert_eq!(CpuFlags::Z|CpuFlags::P, flags);
        flags = CpuFlags::all();
        assert_eq!((0x5Fu8, 0xF0u8), rld(0x50, 0xFF, &mut flags));
        assert_eq!(CpuFlags::X|CpuFlags::P|CpuFlags::C, flags);
        flags = CpuFlags::empty();
        assert_eq!((0x5Fu8, 0xF0u8), rld(0x50, 0xFF, &mut flags));
        assert_eq!(CpuFlags::X|CpuFlags::P, flags);
        flags = CpuFlags::all();
        assert_eq!((0u8, 0u8), rrd(0, 0, &mut flags));
        assert_eq!(CpuFlags::Z|CpuFlags::P|CpuFlags::C, flags);
        flags = CpuFlags::empty();
        assert_eq!((0u8, 0u8), rrd(0, 0, &mut flags));
        assert_eq!(CpuFlags::Z|CpuFlags::P, flags);
        flags = CpuFlags::all();
        assert_eq!((0x5Fu8, 0x0Fu8), rrd(0x50, 0xFF, &mut flags));
        assert_eq!(CpuFlags::X|CpuFlags::P|CpuFlags::C, flags);
        flags = CpuFlags::empty();
        assert_eq!((0x5Fu8, 0x0Fu8), rrd(0x50, 0xFF, &mut flags));
        assert_eq!(CpuFlags::X|CpuFlags::P, flags);
    }

    #[test]
    fn bit_works() {
        let mut flags;
        for i in 0..=255 {
            for b in 0..=7 {
                let mask = 1 << b;
                let ibit = i|mask;
                let fs = CpuFlags::S & CpuFlags::from_bits_truncate(mask);
                let fxy = CpuFlags::XY & CpuFlags::from_bits_truncate(ibit);
                flags = CpuFlags::all();
                bit(b, ibit, &mut flags);
                assert_eq!(CpuFlags::H|CpuFlags::C|fxy|fs, flags);
                flags = CpuFlags::empty();
                bit(b, ibit, &mut flags);
                assert_eq!(CpuFlags::H|fxy|fxy|fs, flags);
                let ineg = i&!mask;
                let fxy = CpuFlags::XY & CpuFlags::from_bits_truncate(ineg);
                flags = CpuFlags::all();
                bit(b, ineg, &mut flags);
                assert_eq!(CpuFlags::Z|CpuFlags::P|CpuFlags::H|CpuFlags::C|fxy, flags);
                flags = CpuFlags::empty();
                bit(b, ineg, &mut flags);
                assert_eq!(CpuFlags::Z|CpuFlags::P|CpuFlags::H|fxy, flags);
            }
        }
        for i in 0..=255 {
            for mp in 0..=255 {
                for b in 0..=7 {
                    let mask = 1 << b;
                    let ibit = i|mask;
                    let fs = CpuFlags::S & CpuFlags::from_bits_truncate(mask);
                    let fxy = CpuFlags::XY & CpuFlags::from_bits_truncate(mp);
                    flags = CpuFlags::all();
                    bit_mp(b, ibit, mp, &mut flags);
                    assert_eq!(CpuFlags::H|CpuFlags::C|fxy|fs, flags);
                    flags = CpuFlags::empty();
                    bit_mp(b, ibit, mp, &mut flags);
                    assert_eq!(CpuFlags::H|fxy|fxy|fs, flags);
                    let ineg = i&!mask;
                    let fxy = CpuFlags::XY & CpuFlags::from_bits_truncate(mp);
                    flags = CpuFlags::all();
                    bit_mp(b, ineg, mp, &mut flags);
                    assert_eq!(CpuFlags::Z|CpuFlags::P|CpuFlags::H|CpuFlags::C|fxy, flags);
                    flags = CpuFlags::empty();
                    bit_mp(b, ineg, mp, &mut flags);
                    assert_eq!(CpuFlags::Z|CpuFlags::P|CpuFlags::H|fxy, flags);
                }
            }
        }
    }

    #[test]
    fn set_res_works() {
        assert_eq!(set(0, 0), 1);
        assert_eq!(set(7, 1), 0b10000001);
        assert_eq!(res(0, 0b10000001), 0b10000000);
    }

    #[test]
    fn ld_a_ir_works() {
        let mut flags;
        flags = CpuFlags::all();
        ld_a_ir(0, true, &mut flags);
        assert_eq!(CpuFlags::Z|CpuFlags::P|CpuFlags::C, flags);
        ld_a_ir(0, false, &mut flags);
        assert_eq!(CpuFlags::Z|CpuFlags::C, flags);
        flags = CpuFlags::empty();
        ld_a_ir(0, true, &mut flags);
        assert_eq!(CpuFlags::Z|CpuFlags::P, flags);
        ld_a_ir(0, false, &mut flags);
        assert_eq!(CpuFlags::Z, flags);
        flags = CpuFlags::all();
        ld_a_ir(0xFF, true, &mut flags);
        assert_eq!(CpuFlags::S|CpuFlags::XY|CpuFlags::P|CpuFlags::C, flags);
        ld_a_ir(0xFF, false, &mut flags);
        assert_eq!(CpuFlags::S|CpuFlags::XY|CpuFlags::C, flags);
        flags = CpuFlags::empty();
        ld_a_ir(0xFF, true, &mut flags);
        assert_eq!(CpuFlags::S|CpuFlags::XY|CpuFlags::P, flags);
        ld_a_ir(0xFF, false, &mut flags);
        assert_eq!(CpuFlags::S|CpuFlags::XY, flags);
        flags = CpuFlags::all();
        ld_a_ir(0x0F, true, &mut flags);
        assert_eq!(CpuFlags::X|CpuFlags::P|CpuFlags::C, flags);
        ld_a_ir(0x0F, false, &mut flags);
        assert_eq!(CpuFlags::X|CpuFlags::C, flags);
        flags = CpuFlags::empty();
        ld_a_ir(0x0F, true, &mut flags);
        assert_eq!(CpuFlags::X|CpuFlags::P, flags);
        ld_a_ir(0x0F, false, &mut flags);
        assert_eq!(CpuFlags::X, flags);
        flags = CpuFlags::all();
        ld_a_ir(0xF0, true, &mut flags);
        assert_eq!(CpuFlags::S|CpuFlags::Y|CpuFlags::P|CpuFlags::C, flags);
        ld_a_ir(0xF0, false, &mut flags);
        assert_eq!(CpuFlags::S|CpuFlags::Y|CpuFlags::C, flags);
        flags = CpuFlags::empty();
        ld_a_ir(0xF0, true, &mut flags);
        assert_eq!(CpuFlags::S|CpuFlags::Y|CpuFlags::P, flags);
        ld_a_ir(0xF0, false, &mut flags);
        assert_eq!(CpuFlags::S|CpuFlags::Y, flags);
    }

    #[test]
    fn ldx_works() {
        let mut flags;
        flags = CpuFlags::all();
        ldx(0, 0, true, &mut flags);
        assert_eq!(CpuFlags::S|CpuFlags::Z|CpuFlags::C, flags);
        ldx(0, 0, false, &mut flags);
        assert_eq!(CpuFlags::S|CpuFlags::Z|CpuFlags::C|CpuFlags::PV, flags);
        flags = CpuFlags::empty();
        ldx(0, 0, true, &mut flags);
        assert_eq!(CpuFlags::empty(), flags);
        ldx(0, 0, false, &mut flags);
        assert_eq!(CpuFlags::PV, flags);
        ldx(0b00000010, 0, false, &mut flags);
        assert_eq!(CpuFlags::Y|CpuFlags::PV, flags);
        ldx(0b00001000, 0, false, &mut flags);
        assert_eq!(CpuFlags::X|CpuFlags::PV, flags);
        ldx(0, 0b00000010, false, &mut flags);
        assert_eq!(CpuFlags::Y|CpuFlags::PV, flags);
        ldx(0, 0b00001000, false, &mut flags);
        assert_eq!(CpuFlags::X|CpuFlags::PV, flags);
        ldx(0b00000010, 0b00001000, true, &mut flags);
        assert_eq!(CpuFlags::XY, flags);
        ldx(0b00001000, 0b00000010, true, &mut flags);
        assert_eq!(CpuFlags::XY, flags);
        ldx(0b00001000, 0b00000010, false, &mut flags);
        assert_eq!(CpuFlags::XY|CpuFlags::PV, flags);
        ldx(0b0000010, 0b00000010, false, &mut flags);
        assert_eq!(CpuFlags::PV, flags);
        ldx(0b00001010, 0b00000010, false, &mut flags);
        assert_eq!(CpuFlags::X|CpuFlags::PV, flags);
        ldx(0b00001010, 0b00001000, false, &mut flags);
        assert_eq!(CpuFlags::Y|CpuFlags::PV, flags);
        ldx(0b00001010, 0b00000010, true, &mut flags);
        assert_eq!(CpuFlags::X, flags);
        ldx(0b00001010, 0b00001000, true, &mut flags);
        assert_eq!(CpuFlags::Y, flags);
    }
}
