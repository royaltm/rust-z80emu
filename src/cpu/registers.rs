//! This module contains cpu registers related building blocks.
#![allow(dead_code)]
#[cfg(feature = "serde")] use serde::{Serialize, Deserialize, Serializer, de::{
                                            self, Deserializer, Visitor, SeqAccess}};
#[cfg(feature = "serde")] use std::fmt;

/// The interrupt mode enum.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Copy,Clone,PartialEq,Eq,Hash,Debug)]
#[repr(u8)]
pub enum InterruptMode {
    Mode0 = 0,
    Mode1 = 1,
    Mode2 = 2,
}

/// A struct that represents a register pair, that can be treated as a single 16-bit
/// register or a separate 8-bit (MSB/LSB) registers.
#[derive(Clone,Copy,PartialEq,Eq,Default,Hash,Debug)]
pub struct RegisterPair([u8;2]);

/// A block of BC, DE and HL registers.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone,Copy,Default,PartialEq,Eq,Debug)]
pub(crate) struct GeneralRegisters {
    pub(crate) bc: RegisterPair,
    pub(crate) de: RegisterPair,
    pub(crate) hl: RegisterPair
}

/// A block of IX and IY registers.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone,Copy,Default,PartialEq,Eq,Debug)]
pub(crate) struct IndexRegisters {
    pub(crate) ix: RegisterPair,
    pub(crate) iy: RegisterPair
}

impl Default for InterruptMode {
    fn default() -> Self {
        InterruptMode::Mode0
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

impl RegisterPair {
    #[inline]
    pub fn ptr8hi(&mut self) -> *mut u8 {
        unsafe { self.0.as_mut_ptr().offset(1) }
    }

    #[inline]
    pub fn ptr8lo(&mut self) -> *mut u8 {
        unsafe { self.0.as_mut_ptr().offset(0) }
    }

    #[inline]
    pub fn get16(self) -> u16 {
        u16::from_le_bytes(self.0)
    }

    #[inline]
    pub fn set16(&mut self, val: u16) {
        self.0 = val.to_le_bytes();
    }

    #[inline]
    pub fn get8hi(self) -> u8 {
        let [_, hi] = self.0;
        hi
    }

    #[inline]
    pub fn get8lo(self) -> u8 {
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
    pub fn get(self) -> (u8, u8) {
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

impl From<u16> for RegisterPair {
    fn from(uint: u16) -> Self {
        RegisterPair(uint.to_le_bytes())
    }
}

impl From<i16> for RegisterPair {
    fn from(int: i16) -> Self {
        RegisterPair(int.to_le_bytes())
    }
}

impl From<[u8;2]> for RegisterPair {
    fn from(pair: [u8;2]) -> Self {
        RegisterPair(pair)
    }
}

#[cfg(feature = "serde")]
impl Serialize for RegisterPair {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        serializer.serialize_u16(self.get16())
    }
}

#[cfg(feature = "serde")]
struct RegisterPairVisitor;

#[cfg(feature = "serde")]
impl<'de> Visitor<'de> for RegisterPairVisitor {
    type Value = RegisterPair;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("a signed or unsigned 16-bit integer, a tuple of 8-bit integers or a hex string")
    }

    fn visit_i16<E: de::Error>(self, value: i16) -> Result<Self::Value, E> {
        Ok(RegisterPair::from(value))
    }

    fn visit_u16<E: de::Error>(self, value: u16) -> Result<Self::Value, E> {
        Ok(RegisterPair::from(value))
    }

    fn visit_i64<E: de::Error>(self, value: i64) -> Result<Self::Value, E> {
        if value >= i64::from(std::i16::MIN) && value <= i64::from(std::i16::MAX) {
            Ok(RegisterPair::from(value as i16))
        } else {
            Err(E::custom(format!("RegisterPair out of range: {}", value)))
        }
    }

    fn visit_u64<E: de::Error>(self, value: u64) -> Result<Self::Value, E> {
        if value <= u64::from(std::u16::MAX) {
            Ok(RegisterPair::from(value as u16))
        } else {
            Err(E::custom(format!("RegisterPair out of range: {}", value)))
        }
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
        where A: SeqAccess<'de>
    {
        if let Some(lo) = seq.next_element::<u8>()? {
            if let Some(hi) = seq.next_element::<u8>()? {
                if seq.next_element::<u8>()?.is_none() {
                    return Ok(RegisterPair::from([lo, hi]))
                }
            }
        }
        Err(de::Error::custom("RegisterPair expects a tuple of 8-bit integers"))
    }

    fn visit_str<E: de::Error>(self, s: &str) -> Result<Self::Value, E> {
        let body = if s.starts_with("$") {
            &s[1..]
        }
        else if s.starts_with("0x") {
            &s[2..]
        }
        else {
            s
        };
        let uint = u16::from_str_radix(body, 16).map_err(|_|
                        de::Error::custom("RegisterPair expects a hexadecimal string"))?;
        Ok(RegisterPair::from(uint))
    }
}

#[cfg(feature = "serde")]
impl<'de> Deserialize<'de> for RegisterPair {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        if deserializer.is_human_readable() {
            deserializer.deserialize_any(RegisterPairVisitor)
        }
        else {
            deserializer.deserialize_u16(RegisterPairVisitor)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[cfg(feature = "serde")]
    #[test]
    fn registers_serde() {
        let regs: RegisterPair = serde_json::from_str("[0,0]").unwrap();
        assert_eq!(regs, RegisterPair::default());
        let regs: RegisterPair = serde_json::from_str("0").unwrap();
        assert_eq!(regs, RegisterPair::default());
        let mut regs = GeneralRegisters::default();
        regs.hl.set16(42);
        regs.bc.set16(776);
        regs.de.set16(u16::max_value());
        let serialized = serde_json::to_string(&regs).unwrap();
        assert_eq!(serialized, r#"{"bc":776,"de":65535,"hl":42}"#);
        let regs_de: GeneralRegisters = serde_json::from_str(&serialized).unwrap();
        assert_eq!(regs, regs_de);
        let regs_de: GeneralRegisters = serde_json::from_str(r#"{"bc":[8,3],"de":-1,"hl":[42,0]}"#).unwrap();
        assert_eq!(regs, regs_de);
        let regs_de: GeneralRegisters = serde_json::from_str(r#"{"bc":"308","de":"$ffff","hl":"0x2A"}"#).unwrap();
        assert_eq!(regs, regs_de);

        let encoded: Vec<u8> = bincode::serialize(&regs).unwrap();
        let regs_de: GeneralRegisters = bincode::deserialize(&encoded).unwrap();
        assert_eq!(regs, regs_de);
    }

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
