/*
    z80emu: ZiLOG Z80 microprocessor emulation library.
    Copyright (C) 2019-2024  Rafal Michalski

    For the full copyright notice, see the lib.rs file.
*/
//! This module contains various op-code bits parsing methods and their enum representations.
#![allow(clippy::inconsistent_digit_grouping)]
#![allow(clippy::unusual_byte_groupings)]
#![allow(clippy::upper_case_acronyms)]
use core::fmt;
#[cfg(feature = "serde")] use serde::{Serialize, Deserialize};
use super::flags::CpuFlags;

/// An enum that indicates how the next executed op-code will be modified.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum Prefix {
    /// Modifies instructions to use `(IX+d)` indexed addressing or `IX` instead of `HL`.
    Xdd = 0xDD,
    /// Modifies instructions to use `(IY+d)` indexed addressing or `IY` instead of `HL`.
    Yfd = 0xFD
}

impl Prefix {
    /// Return a bytecode of the prefix.
    #[inline(always)]
    pub const fn to_code(self) -> u8 {
        self as u8
    }
}

impl From<Prefix> for u8 {
    /// Return a bytecode from this type.
    #[inline(always)]
    fn from(prefix: Prefix) -> u8 {
        prefix.to_code()
    }
}

impl core::convert::TryFrom<u8> for Prefix {
    type Error = ();
    /// Attempt to convert to this type from an opcode.
    fn try_from(code: u8) -> Result<Self, Self::Error> {
        match code {
            0xDD => Ok(Prefix::Xdd),
            0xFD => Ok(Prefix::Yfd),
            _ => Err(())
        }
    }
}

/// Displays prefix as a corresponding register pair.
impl fmt::Display for Prefix {
    #[inline(never)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
            Prefix::Xdd  => "IX",
            Prefix::Yfd  => "IY",
        })
    }
}

macro_rules! reg_enum_mask_try_from {
    ($(#[$doc:meta])? $vis:vis $name:ident & ($mask:expr) {$($n:ident = $e:expr;)*} else { $err:expr }) => {
        $(#[$doc])?
        #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
        #[repr(u8)]
        $vis enum $name {
            $($n = $e,)*
        }

        impl $name {
            /// Return an opcode part.
            #[inline(always)]
            pub const fn to_code(self) -> u8 {
                self as u8
            }
        }

        impl From<$name> for u8 {
            /// Return an opcode part from this type.
            #[inline(always)]
            fn from(reg: $name) -> Self {
                reg.to_code()
            }
        }

        impl core::convert::TryFrom<u8> for $name {
            type Error = ();

            /// Attempt to convert to this type from an opcode.
            #[inline(always)]
            fn try_from(value: u8) -> Result<Self, Self::Error> {
                match value & ($mask) {
                    $($e => Ok($name::$n),)*
                    $err => Err(()),
                    _ => unsafe { core::hint::unreachable_unchecked() }
                }
            }
        }

        impl From<$name> for &str {
            /// Return a string representation of this type.
            #[inline(never)]
            fn from(value: $name) -> Self {
                match value {
                    $($name::$n => stringify!($n),)*
                }
            }
        }

        impl core::fmt::Display for $name {
            #[inline(never)]
            fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                write!(f, "{}", match self {
                    $($name::$n => stringify!($n),)*
                })
            }
        }
    };
}

/// # Safety
///
/// 1. The item values & $mask in parantheses must match all item values
/// 2. All items must exhaust all possible bitwise combinations.
/// Otherwise UB.
macro_rules! reg_enum_mask_from {
    ($(#[$doc:meta])? $vis:vis $name:ident & ($mask:expr) {$($n:ident = $e:expr;)*}) => {
        $(#[$doc])?
        #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
        #[repr(u8)]
        $vis enum $name {
            $($n = $e,)*
        }

        impl $name {
            /// Parse an opcode and return the matching type.
            #[inline(always)]
            pub const fn from_code(code: u8) -> Self {
                match code & ($mask) {
                    $($e => $name::$n,)*
                    _ => unsafe { core::hint::unreachable_unchecked() }
                }
            }
            /// Return an opcode part.
            #[inline(always)]
            pub const fn to_code(self) -> u8 {
                self as u8
            }
        }

        impl From<$name> for u8 {
            /// Return an opcode part from this type.
            #[inline(always)]
            fn from(reg: $name) -> Self {
                reg.to_code()
            }
        }

        impl From<u8> for $name {
            /// Convert to this type from an opcode.
            #[inline(always)]
            fn from(value: u8) -> Self {
                $name::from_code(value)
            }
        }

        impl From<$name> for &str {
            /// Return a string representation of this type.
            #[inline(never)]
            fn from(value: $name) -> Self {
                match value {
                    $($name::$n => stringify!($n),)*
                }
            }
        }

        impl core::fmt::Display for $name {
            #[inline(never)]
            fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                write!(f, "{}", match self {
                    $($name::$n => stringify!($n),)*
                })
            }
        }
    };
}

reg_enum_mask_try_from!{
/// An enum of 8-bit registers.
pub Reg8 & (0b111) {
        B = 0b000;
        C = 0b001;
        D = 0b010;
        E = 0b011;
        H = 0b100;
        L = 0b101;
        // (HL)|n = 0b110;
        A = 0b111;
    }
    else {
        0b110
    }
}

reg_enum_mask_from!{
/// An enum of 16-bit registers for the stack-related operations.
pub StkReg16
          & (0b00_11_0000) {
        BC = 0b00_00_0000;
        DE = 0b00_01_0000;
        HL = 0b00_10_0000;
        AF = 0b00_11_0000;
    }
}

reg_enum_mask_from!{
/// An enum of 16-bit registers.
pub Reg16 & (0b00_11_0000) {
        BC = 0b00_00_0000;
        DE = 0b00_01_0000;
        HL = 0b00_10_0000;
        SP = 0b00_11_0000;
    }
}

reg_enum_mask_from!{
pub(crate) Ops8
           & (0b00_111_000) {
        ADD = 0b00_000_000;
        ADC = 0b00_001_000;
        SUB = 0b00_010_000;
        SBC = 0b00_011_000;
        AND = 0b00_100_000;
        XOR = 0b00_101_000;
        OR  = 0b00_110_000;
        CP  = 0b00_111_000;
    }
}

reg_enum_mask_from!{
pub(crate) Rot
           & (0b00_111_000) {
        RLC = 0b00_000_000;
        RRC = 0b00_001_000;
        RL  = 0b00_010_000;
        RR  = 0b00_011_000;
        SLA = 0b00_100_000;
        SRA = 0b00_101_000;
        SLL = 0b00_110_000;
        SRL = 0b00_111_000;
    }
}

reg_enum_mask_from!{
/// An enum of condition code suffixes.
pub Condition
           & (0b00_111_000) {
        NZ  = 0b00_000_000;
        Z   = 0b00_001_000;
        NC  = 0b00_010_000;
        C   = 0b00_011_000;
        PO  = 0b00_100_000;
        PE  = 0b00_101_000;
        P   = 0b00_110_000;
        M   = 0b00_111_000;
    }
}

impl Condition {
    /// Parse JR cc OPCODE into one of the conditional variants.
    #[inline(always)]
    pub const fn from_jr_subset(code: u8) -> Self {
        Condition::from_code(code & 0b00_011_000)
    }
    #[inline(always)]
    /// Return whether flags satisfy a condition.
    pub const fn is_satisfied(self, flags: CpuFlags) -> bool {
        match self {
            Condition::NZ =>  !flags.contains(CpuFlags::Z),
            Condition::Z  =>  flags.contains(CpuFlags::Z),
            Condition::NC =>  !flags.contains(CpuFlags::C),
            Condition::C  =>  flags.contains(CpuFlags::C),
            Condition::PO =>  !flags.contains(CpuFlags::PV),
            Condition::PE =>  flags.contains(CpuFlags::PV),
            Condition::P  =>  !flags.contains(CpuFlags::S),
            Condition::M  =>  flags.contains(CpuFlags::S),
        }
    }
}

impl core::str::FromStr for Condition {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "NZ" => Ok(Condition::NZ),
            "Z"  => Ok(Condition::Z),
            "NC" => Ok(Condition::NC),
            "C"  => Ok(Condition::C),
            "PO" => Ok(Condition::PO),
            "PE" => Ok(Condition::PE),
            "P"  => Ok(Condition::P),
            "M"  => Ok(Condition::M),
            _ => Err(())
        }
    }
}

impl Reg8 {
    /// Attepmpts to convert bits 3..=5 and 0..=2 of code into a tuple of Reg8 enums.
    #[inline(always)]
    pub(crate) fn tuple_from_b5_3_and_b2_0(code: u8) -> (Result<Reg8,()>, Result<Reg8,()>) {
        (Reg8::try_from(code >> 3), Reg8::try_from(code))
    }

    /// Attepmpts to convert bits 3..=5 of code into a Reg8 enum.
    #[inline(always)]
    pub(crate) fn from_b5_3(code: u8) -> Result<Reg8,()> {
        Reg8::try_from(code >> 3)
    }

    /// Attepmpts to convert bits 0..=2 of code into a Reg8 enum.
    #[inline(always)]
    pub(crate) fn from_b2_0(code: u8) -> Result<Reg8,()> {
        Reg8::try_from(code)
    }

    /// Formats `Reg8` as a string with the given formatter with `prefix` modification.
    /// E.g. for [Reg8::H] writes "IXH" if prefix is [Prefix::Xdd].
    #[inline(never)]
    pub fn format_with_prefix(self, f: &mut fmt::Formatter<'_>, prefix: Option<Prefix>) -> fmt::Result {
        match (self, prefix) {
            (Reg8::H, Some(Prefix::Xdd)) => f.write_str("IXH"),
            (Reg8::H, Some(Prefix::Yfd)) => f.write_str("IYH"),
            (Reg8::L, Some(Prefix::Xdd)) => f.write_str("IXL"),
            (Reg8::L, Some(Prefix::Yfd)) => f.write_str("IYL"),
            _ => fmt::Display::fmt(&self, f)
        }
    }
}

impl Reg16 {
    /// Formats `Reg16` as a string with the given formatter with `prefix` modification.
    /// E.g. for [Reg16::HL] writes "IX" if prefix is [Prefix::Xdd].
    #[inline(never)]
    pub fn format_with_prefix(self, f: &mut fmt::Formatter<'_>, prefix: Option<Prefix>) -> fmt::Result {
        match (self, prefix) {
            (Reg16::HL, Some(Prefix::Xdd)) => f.write_str("IX"),
            (Reg16::HL, Some(Prefix::Yfd)) => f.write_str("IY"),
            _ => fmt::Display::fmt(&self, f),
        }
    }
}

/// An enum for dispatching 0xCB prefixed instructions.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub(crate) enum BitOps {
    /// Bitwise shift and rotate group.
    Rot(Rot,Result<Reg8,()>),
    /// BIT b, r|(HL)
    Bit(u32,Result<Reg8,()>),
    /// RES b, r|(HL)
    Res(u32,Result<Reg8,()>),
    /// SET b, r|(HL)
    Set(u32,Result<Reg8,()>),
}

#[inline(always)]
fn parse_code_bitnum(code: u8) -> u32 {
    ((code >> 3) & 7) as u32
}

/// For parsing op-codes after 0xCB prefix.
impl From<u8> for BitOps {
    #[inline(always)]
    fn from(code: u8) -> Self {
        let arg = Reg8::from_b2_0(code);
        match code & 0b11_000_000 {
            0b00_000_000 => BitOps::Rot(Rot::from(code), arg),
            0b01_000_000 => BitOps::Bit(parse_code_bitnum(code), arg),
            0b10_000_000 => BitOps::Res(parse_code_bitnum(code), arg),
            0b11_000_000 => BitOps::Set(parse_code_bitnum(code), arg),
            _ => unsafe { core::hint::unreachable_unchecked() }
        }
    }
}

/// Parses RST instruction code as an absolute target address.
#[inline(always)]
pub(crate) fn parse_restart_address(code: u8) -> u16 {
    (code & 0b00_111_000) as u16
}
