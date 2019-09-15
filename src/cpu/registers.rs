//! This module contains cpu registers related building blocks.
#![allow(dead_code)]
use serde::{Serialize, Deserialize};

/// The interrupt mode enum.
#[repr(u8)]
#[derive(Debug,Copy,Clone,Serialize,Deserialize,PartialEq,Eq)]
pub enum InterruptMode {
    Mode0 = 0,
    Mode1 = 1,
    Mode2 = 2,
}

impl Default for InterruptMode {
    fn default() -> Self {
        InterruptMode::Mode0
    }
}

impl core::fmt::Debug for RegisterPair {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{:04X}", self.get16())
    }
}

impl core::convert::TryFrom<u8> for InterruptMode {
    type Error = ();

    #[inline(always)]
    fn try_from(im: u8) -> Result<Self, Self::Error> {
        match im {
            0 => Ok(InterruptMode::Mode0),
            1 => Ok(InterruptMode::Mode1),
            2 => Ok(InterruptMode::Mode2),
            _ => Err(())
        }
    }
}

/// A block of BC, DE and HL registers.
#[derive(Clone,Copy,Default,PartialEq,Eq,Serialize,Deserialize,Debug)]
pub(crate) struct GeneralRegisters {
    pub(crate) bc: RegisterPair,
    pub(crate) de: RegisterPair,
    pub(crate) hl: RegisterPair
}

/// A block of IX and IY registers.
#[derive(Clone,Copy,Default,PartialEq,Eq,Serialize,Deserialize,Debug)]
pub(crate) struct IndexRegisters {
    pub(crate) ix: RegisterPair,
    pub(crate) iy: RegisterPair
}

/// A struct that represent a register pair, that can be treated as a single 16-bit
/// register or a separate 8-bit (hi/lo) registers.
#[derive(Clone,Copy,Serialize,Deserialize,PartialEq,Eq,Default)]
pub struct RegisterPair([u8;2]);

impl RegisterPair {
    #[inline]
    pub unsafe fn ptr8hi(&mut self) -> *mut u8 {
        self.0.as_mut_ptr().offset(1)
    }

    #[inline]
    pub unsafe fn ptr8lo(&mut self) -> *mut u8 {
        self.0.as_mut_ptr().offset(0)
    }

    #[inline]
    pub fn get16(&self) -> u16 {
        u16::from_le_bytes(self.0)
    }

    #[inline]
    pub fn set16(&mut self, val: u16) {
        self.0 = val.to_le_bytes();
    }

    #[inline]
    pub fn get8hi(&self) -> u8 {
        let [_, hi] = self.0;
        hi
    }

    #[inline]
    pub fn get8lo(&self) -> u8 {
        let [lo, _] = self.0;
        lo
    }

    #[inline]
    pub fn set8hi(&mut self, val: u8) {
        unsafe { *self.ptr8hi() = val; }
    }

    #[inline]
    pub fn set8lo(&mut self, val: u8) {
        unsafe { *self.ptr8lo() = val; }
    }

    #[inline]
    pub fn get(&self) -> (u8, u8) {
        let [lo, hi] = self.0;
        (hi, lo)
    }

    #[inline]
    pub fn set(&mut self, hi: u8, lo: u8) {
        self.0 = [lo, hi];
    }

    #[inline]
    pub fn inc16(&mut self) {
        self.set16(self.get16().wrapping_add(1));
    }

    #[inline]
    pub fn add16(&mut self, val: u16) {
        self.set16(self.get16().wrapping_add(val));
    }

    #[inline]
    pub fn dec16(&mut self) {
        self.set16(self.get16().wrapping_sub(1));
    }

    /// Subtracts 1 from the 16-bit register and returns true if the result is 0.
    #[inline]
    pub fn dec16_is_zero(&mut self) -> bool {
        let val = self.get16().wrapping_sub(1);
        self.set16(val);
        val == 0
    }

    /// Applies op to the 16-bit register value and modifies it in place.
    #[inline]
    pub fn op16<F: FnOnce(u16) -> (u8, u8)>(&mut self, op: F) {
        let (vhi, vlo) = op(self.get16());
        self.set(vhi, vlo);
    }

    /// Applies op to the 8-bit high half value and modifies it in place.
    #[inline]
    pub fn op8hi<F: FnOnce(u8) -> u8>(&mut self, op: F) {
        unsafe {
            let ptr = self.ptr8hi();
            *ptr = op(*ptr);
        }
    }

    /// Applies op to the 8-bit low half value and modifies it in place.
    #[inline]
    pub fn op8lo<F: FnOnce(u8) -> u8>(&mut self, op: F) {
        unsafe {
            let ptr = self.ptr8lo();
            *ptr = op(*ptr);
        }
    }

}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn registers_work() {
        let mut regs = RegisterPair::default();
        assert_eq!(regs.get16(), 0u16);
        assert_eq!(regs.get(), (0u8, 0u8));
        assert_eq!(regs.get8hi(), 0u8);
        assert_eq!(regs.get8lo(), 0u8);
        regs.set16(0xA542);
        assert_eq!(regs.0, [0x42, 0xA5]);
        regs.set(0xFF, 0x33);
        assert_eq!(regs.0, [0x33, 0xFF]);
        regs.set8hi(1);
        assert_eq!(regs.0, [0x33, 1]);
        regs.set8lo(255);
        assert_eq!(regs.0, [255, 1]);
        regs.inc16();
        assert_eq!(regs.0, [0, 2]);
        regs.dec16();
        assert_eq!(regs.0, [255, 1]);
        regs.set16(2);
        assert_eq!(regs.dec16_is_zero(), false);
        assert_eq!(regs.0, [1, 0]);
        assert_eq!(regs.dec16_is_zero(), true);
        assert_eq!(regs.0, [0, 0]);
        assert_eq!(regs.dec16_is_zero(), false);
        assert_eq!(regs.0, [255, 255]);
        regs.add16(2);
        assert_eq!(regs.0, [1, 0]);
        regs.add16(0xffff);
        assert_eq!(regs.0, [0, 0]);
    }
}
