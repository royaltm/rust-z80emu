/*
    z80emu: a minimalistic Z80 CPU emulation library.
    Copyright (C) 2019-2022  Rafal Michalski

    For the full copyright notice, see the lib.rs file.
*/
use core::fmt::{self, Write};
use arrayvec::ArrayString;
use super::{Reg8, Reg16, StkReg16, Prefix, Condition, InterruptMode};

/// The type that stores a copy of the instruction's full byte code.
pub type CpuDebugCode = arrayvec::ArrayVec::<u8,4>;

/// This type can be passed to [Cpu][crate::Cpu] methods that require a `debug` argument.
///
/// E.g.:
/// ```ignore
/// cpu.execute_instruction::<_,_,CpuDebugFn>(control, clock, None, code)
/// ```
pub type CpuDebugFn = fn(CpuDebug);

/// This struct is being passed to the user debugger function when the command is being executed.
///
/// The [Display][core::fmt::Display], [LowerHex][core::fmt::LowerHex] and [UpperHex][core::fmt::UpperHex]
/// traits are implemented for this type and all its components so it's just easy to use:
/// ```ignore
/// format_args!("{}", deb)
/// ```
/// to get the commands as text:
/// ```text
///   143 SUB  B              [144]
///   144 ADC  HL, HL         [237, 106]
///   146 JR   NC, 151        [48, 3]
/// ```
/// The hex modifier `"{:x}"`:
/// ```text
/// 008fh SUB  B              [90]
/// 0090h ADC  HL, HL         [ed, 6a]
/// 0092h JR   NC, 0097h      [30, 03]
/// ```
/// and it's alternative `"{:#x}"`:
/// ```text
/// 0x008f SUB  B               [90]
/// 0x0090 ADC  HL, HL          [ed, 6a]
/// 0x0092 JR   NC, 0x0097      [30, 03]
/// ```
/// can be used to format numbers. One can use upper case hex also: `"{:X}"` and `"{:#X}"`.
/// If the different formatting or spacing is required the following can be used:
/// ```ignore
/// format_args!("{:04x} : {:6} {:#20x} {:02X?}", deb.pc, deb.mnemonic, deb.args, deb.code.as_slice())
/// ```
///
/// ```text
/// 008f : SUB    B                    [90]
/// 0090 : ADC    HL, HL               [ED, 6A]
/// 0092 : JR     NC, 0x0097           [30, 03]
/// ```
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct CpuDebug {
    /// A copy of bytes that assemble the command that has been executed.
    pub code: CpuDebugCode,
    /// An assmebler mnemonic symbol of the executed command.
    pub mnemonic: &'static str,
    /// A program counter addressing the executed command in memory.
    pub pc: u16,
    /// An optional prefix of the executed command.
    pub prefix: Option<Prefix>,
    /// Arguments of the command.
    pub args: CpuDebugArgs
}

/// An address command argument.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum CpuDebugAddr {
    /// An immediate address.
    ImmAddr(u16),
    /// An indirect addressing via a 16-bit register.
    RegAddr(Reg16),
    /// An indirect addressing via an indexing register indicated by the [Prefix] and with an optional 8-bit signed index offset.
    /// `Option<i8>` is `None` only in arguments to `JP (IX)`, `JP (IY)`.
    IndexAddr(Prefix, Option<i8>)
}

/// An I/O port address.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum CpuDebugPort {
    /// An immediate port address.
    ImmPort(u8),
    /// An indirect port address via `BC` register.
    RegPort
}

/// A command argument.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum CpuDebugArg {
    /// An immediate 8-bit integer.
    Imm8(u8),
    /// A bit number
    Bit(u32),
    /// A mode
    IntMode(InterruptMode),
    /// An 8-bit register. Prefix changes the meaning of [H][Reg8::H] and [L][Reg8::L] registers into
    /// `IXh` and `IXl` or `IYh` and `IYl` accordingly.
    Reg8(Option<Prefix>, Reg8),
    /// An immediate 16-bit integer.
    Imm16(u16),
    /// A 16-bit register. Prefix changes the meaning of [HL][Reg16::HL] register into `IX` or `IY`.
    Reg16(Option<Prefix>, Reg16),
    /// A 16-bit register used with the machine stack commands `POP` and `PUSH`.
    Stk16(StkReg16),
    /// An indirect value via memory address.
    Addr(CpuDebugAddr),
    /// An I/O port address. 
    Port(CpuDebugPort),
    /// A branching condition.
    Cond(Condition),
    /// Interrupt page register.
    I,
    /// Memory refresh register.
    R
}

/// An enum holding the command arguments.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum CpuDebugArgs {
    /// The command had no arguments.
    None,
    /// The command had a single argument.
    Single(CpuDebugArg),
    /// The command had two arguments.
    Double(CpuDebugArg, CpuDebugArg),
    /// Some undocumented variants of SET and RES require 3 arguments.
    /// The first argument is a bit number and the last is a register the result is being stored in.
    BitOpExt(u32, CpuDebugArg, Reg8),
}

impl fmt::Display for CpuDebugAddr {
    #[inline(never)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CpuDebugAddr::ImmAddr(nn)  => write!(f, "({})", nn),
            CpuDebugAddr::RegAddr(r16) => write!(f, "({})", r16),
            CpuDebugAddr::IndexAddr(px, Some(d))  => write!(f, "({}{:+})", px, d),
            CpuDebugAddr::IndexAddr(px, None)  => write!(f, "({})", px),
        }
    }
}

impl fmt::LowerHex for CpuDebugAddr {
    #[inline(never)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CpuDebugAddr::ImmAddr(nn) => if f.alternate() {
                write!(f, "({:#06x})", nn)
            }
            else {
                write!(f, "({:04x}h)", nn)
            }
            CpuDebugAddr::IndexAddr(px, Some(d@0..=127)) => if f.alternate() {
                write!(f, "({}+{:#04x})", px, d)
            }
            else {
                write!(f, "({}+{:02x}h)", px, d)
            }
            CpuDebugAddr::IndexAddr(px, Some(d@-128..=-1)) => if f.alternate() {
                write!(f, "({}-{:#04x})", px, 0u8.wrapping_sub(*d as u8))
            }
            else {
                write!(f, "({}-{:02x}h)", px, 0u8.wrapping_sub(*d as u8))
            }
            _ => fmt::Display::fmt(self, f)
        }
    }
}

impl fmt::UpperHex for CpuDebugAddr {
    #[inline(never)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CpuDebugAddr::ImmAddr(nn) => if f.alternate() {
                write!(f, "({:#06X})", nn)
            }
            else {
                write!(f, "({:04X}h)", nn)
            }
            CpuDebugAddr::IndexAddr(px, Some(d@0..=127)) => if f.alternate() {
                write!(f, "({}+{:#04X})", px, d)
            }
            else {
                write!(f, "({}+{:02X}h)", px, d)
            }
            CpuDebugAddr::IndexAddr(px, Some(d@-128..=-1)) => if f.alternate() {
                write!(f, "({}-{:#04X})", px, 0u8.wrapping_sub(*d as u8))
            }
            else {
                write!(f, "({}-{:02X}h)", px, 0u8.wrapping_sub(*d as u8))
            }
            _ => fmt::Display::fmt(self, f)
        }
    }
}

impl fmt::Display for CpuDebugPort {
    #[inline(never)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CpuDebugPort::ImmPort(n)  => write!(f, "({})", n),
            CpuDebugPort::RegPort => f.write_str("(C)")
        }
    }
}

impl fmt::LowerHex for CpuDebugPort {
    #[inline(never)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CpuDebugPort::ImmPort(n)  => if f.alternate() {
                write!(f, "({:#04x})", n)
            }
            else {
                write!(f, "({:02x}h)", n)
            }
            _ => fmt::Display::fmt(self, f)
        }
    }
}

impl fmt::UpperHex for CpuDebugPort {
    #[inline(never)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CpuDebugPort::ImmPort(n)  => if f.alternate() {
                write!(f, "({:#04X})", n)
            }
            else {
                write!(f, "({:02X}h)", n)
            }
            _ => fmt::Display::fmt(self, f)
        }
    }
}

impl fmt::Display for CpuDebugArg {
    #[inline(never)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CpuDebugArg::Imm8(n) => write!(f, "{}", n),
            CpuDebugArg::Bit(n)  => write!(f, "{}", n),
            CpuDebugArg::IntMode(m)  => write!(f, "{}", *m as u8),
            CpuDebugArg::Reg8(px, r8)  => r8.format_with_prefix(f, *px),
            CpuDebugArg::Imm16(nn) => write!(f, "{}", nn),
            CpuDebugArg::Reg16(px, r16) => r16.format_with_prefix(f, *px),
            CpuDebugArg::Stk16(r16) => write!(f, "{}", r16),
            CpuDebugArg::Addr(addr) => addr.fmt(f),
            CpuDebugArg::Cond(cond) => cond.fmt(f),
            CpuDebugArg::Port(port) => port.fmt(f),
            CpuDebugArg::I => f.write_str("I"),
            CpuDebugArg::R => f.write_str("R")
        }
    }
}

impl fmt::LowerHex for CpuDebugArg {
    #[inline(never)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CpuDebugArg::Imm8(n) => if f.alternate() {
                write!(f, "{:#04x}", n)
            }
            else {
                write!(f, "{:02x}h", n)
            }
            CpuDebugArg::Imm16(nn) => if f.alternate() {
                write!(f, "{:#06x}", nn)
            }
            else {
                write!(f, "{:04x}h", nn)
            }
            CpuDebugArg::Addr(addr) => addr.fmt(f),
            CpuDebugArg::Port(port) => port.fmt(f),
            _ => fmt::Display::fmt(self, f)
        }
    }
}

impl fmt::UpperHex for CpuDebugArg {
    #[inline(never)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CpuDebugArg::Imm8(n) => if f.alternate() {
                write!(f, "{:#04X}", n)
            }
            else {
                write!(f, "{:02X}h", n)
            }
            CpuDebugArg::Imm16(nn) => if f.alternate() {
                write!(f, "{:#06X}", nn)
            }
            else {
                write!(f, "{:04X}h", nn)
            }
            CpuDebugArg::Addr(addr) => addr.fmt(f),
            CpuDebugArg::Port(port) => port.fmt(f),
            _ => fmt::Display::fmt(self, f)
        }
    }
}

macro_rules! pad {
    ([$size:expr] $f:ident, $templ:literal, $($args:expr),*) => {
        {
            let mut temp = ArrayString::<$size>::new();
            write!(temp, $templ, $($args),*)?;
            $f.pad(temp.as_str())
        }
    };
    ([$size:expr] $f:ident, (#$alt:literal, $templ:literal), $($args:expr),*) => {
        {
            let mut temp = ArrayString::<$size>::new();
            if $f.alternate() {
                write!(temp, $alt, $($args),*)?;
            }
            else {
                write!(temp, $templ, $($args),*)?;
            }
            $f.pad(temp.as_str())
        }
    };
}

impl fmt::Display for CpuDebugArgs {
    #[inline(never)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.width().is_some() {
            pad!([14] f, "{}", self)
        } else {
            match self {
                CpuDebugArgs::None => Ok(()),
                CpuDebugArgs::Single(arg) => arg.fmt(f),
                CpuDebugArgs::Double(CpuDebugArg::Stk16(StkReg16::AF), CpuDebugArg::Stk16(StkReg16::AF)) => {
                    f.write_str("AF, AF'")
                }
                CpuDebugArgs::Double(arg1, arg2) => write!(f, "{}, {}", arg1, arg2),
                CpuDebugArgs::BitOpExt(arg1, arg2, arg3) => write!(f, "{}, {}, {}", arg1, arg2, arg3),
            }
        }
    }
}

impl fmt::LowerHex for CpuDebugArgs {
    #[inline(never)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.width().is_some() {
            pad!([15] f, (#"{:#x}", "{:x}"), self)
        } else {
            match self {
                CpuDebugArgs::None => Ok(()),
                CpuDebugArgs::Single(arg) => arg.fmt(f),
                CpuDebugArgs::Double(CpuDebugArg::Stk16(StkReg16::AF), CpuDebugArg::Stk16(StkReg16::AF)) => {
                    f.write_str("AF, AF'")
                }
                CpuDebugArgs::Double(arg1, arg2) => if f.alternate() {
                    write!(f, "{:#x}, {:#x}", arg1, arg2)
                }
                else {
                    write!(f, "{:x}, {:x}", arg1, arg2)
                }
                CpuDebugArgs::BitOpExt(arg1, arg2, arg3) => if f.alternate() {
                    write!(f, "{}, {:#x}, {}", arg1, arg2, arg3)
                }
                else {
                    write!(f, "{}, {:x}, {}", arg1, arg2, arg3)
                }
            }
        }
    }
}

impl fmt::UpperHex for CpuDebugArgs {
    #[inline(never)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.width().is_some() {
            pad!([15] f, (#"{:#X}", "{:X}"), self)
        } else {
            match self {
                CpuDebugArgs::None => Ok(()),
                CpuDebugArgs::Single(arg) => arg.fmt(f),
                CpuDebugArgs::Double(CpuDebugArg::Stk16(StkReg16::AF), CpuDebugArg::Stk16(StkReg16::AF)) => {
                    f.write_str("AF, AF'")
                }
                CpuDebugArgs::Double(arg1, arg2) => if f.alternate() {
                    write!(f, "{:#X}, {:#X}", arg1, arg2)
                }
                else {
                    write!(f, "{:X}, {:X}", arg1, arg2)
                }
                CpuDebugArgs::BitOpExt(arg1, arg2, arg3) => if f.alternate() {
                    write!(f, "{}, {:#X}, {}", arg1, arg2, arg3)
                }
                else {
                    write!(f, "{}, {:X}, {}", arg1, arg2, arg3)
                }
            }
        }
    }
}

impl fmt::Display for CpuDebug {
    #[inline(never)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:>5} {:4} {:14} {:?}", self.pc, self.mnemonic, self.args, self.code.as_slice())
    }
}

impl fmt::LowerHex for CpuDebug {
    #[inline(never)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            write!(f, "{:#06x} {:4} {:#15x} {:02x?}", self.pc, self.mnemonic, self.args, self.code.as_slice())
        }
        else {
            write!(f, "{:04x}h {:4} {:14x} {:02x?}", self.pc, self.mnemonic, self.args, self.code.as_slice())
        }
    }
}

impl fmt::UpperHex for CpuDebug {
    #[inline(never)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            write!(f, "{:#06X} {:4} {:#15X} {:02X?}", self.pc, self.mnemonic, self.args, self.code.as_slice())
        }
        else {
            write!(f, "{:04X}h {:4} {:14X} {:02X?}", self.pc, self.mnemonic, self.args, self.code.as_slice())
        }
    }
}
