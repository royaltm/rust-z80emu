use core::fmt::{self, Write};
use arrayvec::ArrayString;
use crate::parse::{Reg8, Reg16, StkReg16, Prefix, Condition};

pub type CpuDebugCode = arrayvec::ArrayVec::<[u8;4]>;
pub type CpuDebugFn = fn(CpuDebug);

#[derive(Clone, Debug, PartialEq)]
pub struct CpuDebug {
    pub code: CpuDebugCode,
    pub mnemonic: &'static str,
    pub pc: u16,
    pub prefix: Prefix,
    pub args: CpuDebugArgs
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum CpuDebugAddr {
    ImmAddr(u16),
    RegAddr(Reg16),
    IndexAddr(Prefix,Option<i8>)
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum CpuDebugPort {
    ImmPort(u8),
    RegPort
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum CpuDebugArg {
    Imm8(u8),
    Reg8(Prefix,Reg8),
    Imm16(u16),
    Reg16(Prefix,Reg16),
    Stk16(StkReg16),
    Addr(CpuDebugAddr),
    Port(CpuDebugPort),
    Cond(Condition),
    I,
    R
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum CpuDebugArgs {
    None,
    Single(CpuDebugArg),
    Double(CpuDebugArg, CpuDebugArg),
    BitOpExt(u32, CpuDebugArg, Reg8),
}

impl fmt::Display for CpuDebugAddr {
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
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CpuDebugAddr::ImmAddr(nn) => if f.alternate() {
                write!(f, "({:#06x})", nn)
            }
            else {
                write!(f, "({:04x}h)", nn)
            }
            CpuDebugAddr::IndexAddr(px, Some(d)) => if f.alternate() {
                write!(f, "({}{:+#04x})", px, d)
            }
            else {
                write!(f, "({}{:+02x}h)", px, d)
            }
            _ => fmt::Display::fmt(self, f)
        }
    }
}

impl fmt::UpperHex for CpuDebugAddr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CpuDebugAddr::ImmAddr(nn) => if f.alternate() {
                write!(f, "({:#06X})", nn)
            }
            else {
                write!(f, "({:04X}h)", nn)
            }
            CpuDebugAddr::IndexAddr(px, Some(d)) => if f.alternate() {
                write!(f, "({}{:+#04X})", px, d)
            }
            else {
                write!(f, "({}{:+02X}h)", px, d)
            }
            _ => fmt::Display::fmt(self, f)
        }
    }
}

impl fmt::Display for CpuDebugPort {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CpuDebugPort::ImmPort(n)  => write!(f, "({})", n),
            CpuDebugPort::RegPort => f.write_str("(C)")
        }
    }
}

impl fmt::LowerHex for CpuDebugPort {
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
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CpuDebugArg::Imm8(n) => write!(f, "{}", n),
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
            let mut temp = ArrayString::<[_;$size]>::new();
            write!(temp, $templ, $($args),*)?;
            $f.pad(temp.as_str())
        }
    };
    ([$size:expr] $f:ident, (#$alt:literal, $templ:literal), $($args:expr),*) => {
        {
            let mut temp = ArrayString::<[_;$size]>::new();
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
                    write!(f, "{:#x}, {:#x}, {}", arg1, arg2, arg3)
                }
                else {
                    write!(f, "{:x}, {:x}, {}", arg1, arg2, arg3)
                }
            }
        }
    }
}

impl fmt::UpperHex for CpuDebugArgs {
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
                    write!(f, "{:#X}, {:#X}, {}", arg1, arg2, arg3)
                }
                else {
                    write!(f, "{:X}, {:X}, {}", arg1, arg2, arg3)
                }
            }
        }
    }
}

impl fmt::Display for CpuDebug {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:>5} {:4} {:14} {:?}", self.pc, self.mnemonic, self.args, self.code.as_slice())
    }
}

impl fmt::LowerHex for CpuDebug {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            write!(f, "{:#06x} {:5} {:#16x} {:02x?}", self.pc, self.mnemonic, self.args, self.code.as_slice())
        }
        else {
            write!(f, "{:04x}h {:5} {:14x} {:02x?}", self.pc, self.mnemonic, self.args, self.code.as_slice())
        }
    }
}

impl fmt::UpperHex for CpuDebug {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            write!(f, "{:#06X} {:5} {:#16X} {:02X?}", self.pc, self.mnemonic, self.args, self.code.as_slice())
        }
        else {
            write!(f, "{:04X}h {:5} {:14X} {:02X?}", self.pc, self.mnemonic, self.args, self.code.as_slice())
        }
    }
}
