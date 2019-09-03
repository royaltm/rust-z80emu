//! This module contains various bit op-code parsing methods, their enum representations and
//! their core::fmt::Display implementations for a debugger.
use core::convert::TryFrom;
use core::fmt;
use serde::{Serialize, Deserialize};
use super::flags::CpuFlags;
use super::ops;

/// A prefix enum that modifies behaviour of the next op-code.
/// It's also instrumental in preventing interrupts prematurely.
#[derive(Debug,Copy,Clone,Serialize,Deserialize,PartialEq,Eq)]
#[repr(u8)]
pub enum Prefix {
    None = 0x00,
    Xdd  = 0xDD,
    Yfd  = 0xFD
}

impl Default for Prefix {
    fn default() -> Self {
        Prefix::None
    }
}

/// Displays prefix as a corresponding address register pair. Used by the debugger.
impl fmt::Display for Prefix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
            Prefix::None => "HL",
            Prefix::Xdd  => "IX",
            Prefix::Yfd  => "IY",
        })
    }
}

macro_rules! reg_enum_mask_try_from {
    ($name:ident & ($mask:expr) {$($n:ident = $e:expr;)*}) => {
        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        #[repr(u8)]
        pub enum $name {
            $($n = $e,)*
        }

        impl core::convert::TryFrom<u8> for $name {
            type Error = ();

            #[inline(always)]
            fn try_from(value: u8) -> Result<Self, Self::Error> {
                match value & ($mask) {
                    $($e => Ok($name::$n),)*
                    _ => Err(())
                }
            }
        }

        impl core::fmt::Display for $name {
            fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                write!(f, "{}", match self {
                    $($name::$n => stringify!($n),)*
                })
            }
        }
    };
}

// macro_rules! item_mask_validate {
//     ($val:expr & $mask:expr) => { const_assert_eq!($val & $mask, $val); },
//     ($mask:expr; ($val:expr),+) => 
// }

/// # Safety
///
/// 1. The item values & $mask in parantheses must match all item values
/// 2. All items must exhaust all possible bitwise combinations.
/// Otherwise UB.
macro_rules! reg_enum_mask_from {
    ($name:ident & ($mask:expr) {$($n:ident = $e:expr;)*}) => {
        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        #[repr(u8)]
        pub enum $name {
            $($n = $e,)*
        }

        impl From<u8> for $name {
            #[inline(always)]
            fn from(value: u8) -> Self {
                match value & ($mask) {
                    $($e => $name::$n,)*
                    _ => unsafe { core::hint::unreachable_unchecked() }
                }
            }
        }

        impl From<$name> for &str {
            #[inline(always)]
            fn from(value: $name) -> Self {
                match value {
                    $($name::$n => stringify!($n),)*
                }
            }
        }

        impl core::fmt::Display for $name {
            fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                write!(f, "{}", match self {
                    $($name::$n => stringify!($n),)*
                })
            }
        }
    };
}

reg_enum_mask_try_from!{
    Reg8 & (0b111) {
        B = 0b000;
        C = 0b001;
        D = 0b010;
        E = 0b011;
        H = 0b100;
        L = 0b101;
        // (HL)|n = 0b110;
        A = 0b111;
    }
}

reg_enum_mask_from!{
    StkReg16  & (0b00_11_0000) {
            BC = 0b00_00_0000;
            DE = 0b00_01_0000;
            HL = 0b00_10_0000;
            AF = 0b00_11_0000;
    }
}

reg_enum_mask_from!{
    Reg16 & (0b00_11_0000) {
        BC = 0b00_00_0000;
        DE = 0b00_01_0000;
        HL = 0b00_10_0000;
        SP = 0b00_11_0000;
    }
}

reg_enum_mask_from!{
    Ops8   & (0b00_111_000) {
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
    Rot    & (0b00_111_000) {
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

reg_enum_mask_from!{ Condition
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
    /// Parses JR cc OPCODE into one of the conditional variant.
    #[inline]
    pub fn from_jr_subset(code: u8) -> Self {
        Condition::from(code & 0b00_011_000)
    }
    #[inline]
    pub fn is_satisfied(self, flags: CpuFlags) -> bool {
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

/// Converts Rot enum into one of the appriopriate ops function.
impl From<Rot> for fn(u8, &mut CpuFlags) -> u8 {
    #[inline]
    fn from(rot: Rot) -> Self {
        match rot {
            Rot::RLC  => ops::rlc,
            Rot::RRC  => ops::rrc,
            Rot::RL   => ops::rl,
            Rot::RR   => ops::rr,
            Rot::SLA  => ops::sla,
            Rot::SRA  => ops::sra,
            Rot::SLL  => ops::sll,
            Rot::SRL  => ops::srl
        }
    }
}

impl Reg8 {
    /// Attepmpts to convert bits 3..=5 and 0..=2 of code into a tuple of Reg8 enums.
    #[inline(always)]
    pub fn tuple_from_b5_3_and_b2_0(code: u8) -> (Result<Reg8,()>, Result<Reg8,()>) {
        (Reg8::try_from(code >> 3), Reg8::try_from(code))
    }

    /// Attepmpts to convert bits 3..=5 of code into a Reg8 enum.
    #[inline(always)]
    pub fn from_b5_3(code: u8) -> Result<Reg8,()> {
        Reg8::try_from(code >> 3)
    }

    /// Attepmpts to convert bits 0..=2 of code into a Reg8 enum.
    #[inline(always)]
    pub fn from_b2_0(code: u8) -> Result<Reg8,()> {
        Reg8::try_from(code)
    }

    /// Returns a different fmt::Display implementation based on prefix.
    /// E.g. for Reg8::H returns &"IXh" if prefix is Prefix::Xdd.
    /// Otherwise returns the default implementation.
    pub fn as_display(&self, prefix: Prefix) -> &dyn fmt::Display {
        match self {
            Reg8::H => match prefix {
                Prefix::None => self,
                Prefix::Xdd => &"IXh",
                Prefix::Yfd => &"IYh",
            }
            Reg8::L => match prefix {
                Prefix::None => self,
                Prefix::Xdd => &"IXl",
                Prefix::Yfd => &"IYl",
            }
            _ => self
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Reg8ParseResultDisplayWrap(pub Result<Reg8,()>);

/// Displays the result of code conversion into Reg8 as a register symbol
/// on Ok(Reg8) or "(HL)" on Err(()).
impl fmt::Display for Reg8ParseResultDisplayWrap {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self.0 {
            Ok(reg) => reg.fmt(f),
            Err(_) => write!(f, "(HL)"),
        }
    }
}

impl Reg16 {
    /// Returns a different fmt::Display implementation based on prefix.
    /// E.g. for Reg16::HL returns &"IX" if prefix is Prefix::Xdd.
    /// Otherwise returns the default implementation.
    pub fn as_display(&self, prefix: Prefix) -> &dyn fmt::Display {
        match self {
            Reg16::HL => match prefix {
                Prefix::None => self,
                Prefix::Xdd => &"IX",
                Prefix::Yfd => &"IY",
            }
            _ => self
        }
    }
}

/// An enum for dispatching 0xCB prefixed instructions.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum BitOps {
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

#[derive(Clone, Copy, Debug)]
pub struct BitNumDisplayWrap(pub Option<u32>);

/// Displays the bit number and a comma afterwards on Some(u32) otherwise nothing.
impl fmt::Display for BitNumDisplayWrap {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self.0 {
            Some(bit) => write!(f, "{}, ", bit),
            None => Ok(())
        }
    }
}

/// Parses RST instruction code as an absolute target address.
#[inline(always)]
pub fn parse_restart_address(code: u8) -> u16 {
    (code & 0b00_111_000) as u16
}

/// Determines the direction for the block instruction group.
#[derive(Clone, Copy, Debug)]
#[repr(i8)]
pub enum BlockDelta {
    Increase = 1,
    Decrease = -1
}
