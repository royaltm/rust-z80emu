/*
    z80emu: ZiLOG Z80 microprocessor emulation library.
    Copyright (C) 2019-2024  Rafal Michalski

    z80emu is free software: you can redistribute it and/or modify it under
    the terms of the GNU Lesser General Public License (LGPL) as published
    by the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    z80emu is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

    Author contact information: see Cargo.toml file, section [package.authors].
*/
/*! # Z80 emu
 
`z80emu` crate provides building blocks for emulators based on Zilog's Z80 CPU family.

To build the crate with `no_std` support make sure to set `default-features` to `false` and select
the required features only.


```text
  _______
=|       |=
=|       |=
=|       |= ---------------- =[   Clock   ]
=|       |=                         |
=|       |=                         |
=|       |=                         |
=|       |=                         |
=|       |=                         |
=|  Cpu  |=                    _____|_____
=|       |=                   |           |
=|  Z80  |= \                 |           |
=|       |= <--------------> =| Memory+Io |=:::::
=|       |= /                 |           |
=|       |=                   |___________|
=|       |=
=|       |=
=|       |=
=|       |=
=|       |=
=|_______|=
```

`z80emu` was developed as an attempt to create a minimalistic emulation library. It provides the necessary tools
for the retro emulators to be built upon, avoiding any assumptions about side effects of those emulators.

The idea is to leverage the Rust's trait based OO model for this purpose.

There are four important traits in this library - the essential components of an emulated computer:

* [Cpu] - an interface to the finite state machine that can alter its state by executing the machine code
          instructions as one of the Z80 family processors.
* [Clock] - an interface to the CPU cycle (T-state) counter, which can be used to synchronize the emulation with the
            emulator's side effects.
* [Memory] - an interface to the host's memory that the [Cpu] is using to read from and write to it.
* [Io] - an interface to the host's I/O devices that the [Cpu] is using to access them.

`z80emu` crate provides the [Cpu] trait [implementations](z80) and an example [implementation](host::TsCounter)
for the [Clock] trait.
The rest of the traits need to be implemented by the emulator's developer.
Please see the documentation of this [module][host] for more information on how to implement them.

The [Z80] struct implements the [Cpu] trait with a selectable [Flavour][z80::Flavour] as its generic parameter.

Currently, there are 3 ["flavour"][z80] implementations for which the following CPU types are available:

* [Z80NMOS] - A Zilog's NMOS Z80.
* [Z80CMOS] - A CMOS version of Z80.
* [z80::Z80BM1] - A clone of Z80.

The difference between each of them is very [subtle][z80::Flavour] and only affects undocumented behavior.
Alternatively, a [Z80Any] enum can be used if changing of the [z80::Flavour] in run time is required.

## Debugger

The [Cpu] interface provides ways to debug the executed Z80 machine code. [Cpu::execute_next],
[Cpu::execute_instruction] and [Cpu::irq] methods accept the optional callback argument: `debug`.
The callback, if provided, is being fed with the extended information about the instruction being executed,
and can be used to display a human-readable text of the disassembled instructions or gather statistics.

In `z80emu` the command execution code and the debugger are both implemented in a single unit.
This way there is only a single machine code [dispatcher]. This minimizes the probability of a debugger
suffering from "schizophrenic effects" showing results not compatible with the execution unit.
The Rust and LLVM compilator can optimize out the debugger parts when they are not
[needed](https://github.com/royaltm/rust-z80emu/blob/master/examples/shuffle.rs).

The debugger provides information in a form of a [CpuDebug] struct which implements [Display][core::fmt::Display],
[LowerHex][core::fmt::LowerHex], and [UpperHex][core::fmt::UpperHex] traits. The `debug` closure can just print
the information out or provide a complete customized debugging [solution][disasm].

## How To

Start by inspecting the [tests](https://github.com/royaltm/rust-z80emu/tree/master/tests) directory and
the [shuffle](https://github.com/royaltm/rust-z80emu/tree/master/examples/shuffle.rs) example.
All of the test cases run minimalistic Z80 virtual computers and can be useful in learning the essentials.

For a bigger picture see the crate's [repository example](https://github.com/royaltm/rust-z80emu/tree/master/examples/ral1243)
implementation of the imaginary Z80 based computer, to see how a system bus could be implemented with
custom PIO and CTC peripheral chips.

For the most optimized emulator code execution, when the debugger is not needed, the emulators should
use the [Cpu::execute_with_limit] method to execute code in time frames. The code is executed in a loop
where mostly used Z80 registers can be kept in host CPU registers or its data cache. The optimizer 
removes all debugging related code for this method, even though it uses exactly the same instruction
execution source underneath as [Cpu::execute_next].

## Example

```
use z80emu::*;
use opconsts::HALT_OPCODE;
// Let's use the simple T-state counter.
type TsClock = host::TsCounter<i32>;

// Let's add some memory.
#[derive(Clone, Debug, Default)]
struct Bus {
    rom: [u8;11]
}

impl Io for Bus {
    type Timestamp = i32;
    type WrIoBreak = ();
    type RetiBreak = ();
}

impl Memory for Bus {
    type Timestamp = i32;
    fn read_debug(&self, addr: u16) -> u8 {
        self.rom[addr as usize]
    }
}

const FIB_N: u8 = 24; // 1..=24

let mut tsc = TsClock::default();
let mut fibbo = Bus { rom: [
    0x21, 0x00, 0x00, // 0x0000 LD   HL, 0x0000
    0x11, 0x01, 0x00, // 0x0003 LD   DE, 0x0001
    0xEB,             // 0x0006 EX   DE, HL
    0x19,             // 0x0007 ADD  HL, DE
    0x10, 0xFC,       // 0x0008 DJNZ 0x0006
    HALT_OPCODE       // 0x000A HALT
] };
let mut cpu = Z80NMOS::default();
cpu.reset(); // PC = 0
cpu.set_reg(Reg8::B, None, FIB_N); // Cpu register B = FIB_N
// Let's calculate a Fibbonacci number
loop {
    match cpu.execute_next(&mut fibbo, &mut tsc,
            Some(|deb| println!("{:#X}", deb) )) {
        Err(BreakCause::Halt) => { break }
        _ => {}
    }
}
// the content of the HL registers
let result = cpu.get_reg16(StkReg16::HL);
assert_eq!(result, 46368); // Fib(24)
// the number of T-states passed
assert_eq!(tsc.as_timestamp(), 10+10+(FIB_N as i32)*(4+11+13)-5+4);
```

[dispatcher]: https://github.com/royaltm/rust-z80emu/blob/master/src/z80/opcodes.rs
*/
#![cfg_attr(not(feature = "std"), no_std)]

mod cpu;
pub mod disasm;
pub mod host;
pub mod macros;
pub mod z80;

pub use cpu::*;
pub use host::{Clock, Io, Memory, BreakCause};
pub use z80::{Z80, Z80NMOS, Z80CMOS};
pub use z80::any::Z80Any;

/// An address of the NMI routine.
pub const NMI_RESTART: u16 = 0x66;

/// Selected Z80 opcodes.
///
/// For example a convenient argument to the [Cpu::execute_instruction] function
/// or a return value from [Io::irq_data].
///
/// Match instruction conditions with:
///
/// * `JR cc` opcode - [`Condition::from_jr_subset`],
/// * other `cc` opcodes - [`Condition::from_code`].
pub mod opconsts {
    #[allow(unused_imports)]
    use crate::Prefix;
    /// Extended opcode prefix.
    pub const ED_PREFIX     : u8 = 0xED;
    /// [Prefix::Xdd] prefix.
    pub const DD_PREFIX     : u8 = 0xDD;
    /// [Prefix::Yfd] prefix.
    pub const FD_PREFIX     : u8 = 0xFD;
    /// No operation.
    pub const NOP_OPCODE    : u8 = 0x00;
    /// Halt execution.
    pub const HALT_OPCODE   : u8 = 0x76;
    /// Disable interrupts.
    pub const DI_OPCODE     : u8 = 0xF3;
    /// Enable interrupts.
    pub const EI_OPCODE     : u8 = 0xFB;
    /// Return from subroutine.
    pub const RET_OPCODE    : u8 = 0xC9;
    /// Base of the conditional `RET cc` opcode.
    ///
    /// Match instructions with: `(code & RET_CC_OPMASK) == RET_CC_OPBASE`.
    ///
    /// Build instructions with: `RET_CC_OPBASE|Condition::CC.to_code()`.
    pub const RET_CC_OPBASE : u8 = 0b11_000_000;
    /// Opcode mask of the conditional `RET cc`.
    pub const RET_CC_OPMASK : u8 = 0b11_000_111;
    /// The officially documented `RETI` opcode.
    pub const RETI_OPCODE_T2: (u8, u8) = (ED_PREFIX, 0x4D);
    /// The officially documented `RETN` opcode.
    pub const RETN_OPCODE_T2: (u8, u8) = (ED_PREFIX, 0x45);
    /// All `RETN/RETI` instructions 2nd opcode base after [`ED_PREFIX`].
    ///
    /// Match instructions with: `code[0] == ED_PREFIX && (code[1] & RETN_OP2_MASK) == RETN_OP2_BASE`.
    pub const RETN_OP2_BASE : u8 = 0b01_000_101;
    /// All `RETN/RETI` instructions 2nd opcode mask after [`ED_PREFIX`].
    pub const RETN_OP2_MASK : u8 = 0b11_000_111;
    /// Call a subroutine.
    pub const CALL_OPCODE   : u8 = 0xCD;
    /// Base of the conditional `CALL cc` opcode.
    ///
    /// Match instructions with: `(code & CALL_CC_OPMASK) == CALL_CC_OPBASE`.
    ///
    /// Build instructions with: `CALL_CC_OPBASE|Condition::CC.to_code()`.
    pub const CALL_CC_OPBASE: u8 = 0b11_000_100;
    /// Opcode mask of the conditional `CALL cc`.
    pub const CALL_CC_OPMASK: u8 = 0b11_000_111;
    /// Branch to an absolute address.
    pub const JP_OPCODE     : u8 = 0xC3;
    /// Base of the conditional `JP cc` opcode.
    ///
    /// Match instructions with: `(code & JP_CC_OPMASK) == JP_CC_OPBASE`.
    ///
    /// Build instructions with: `JP_CC_OPBASE|Condition::CC.to_code()`.
    pub const JP_CC_OPBASE  : u8 = 0b11_000_010;
    /// Opcode mask of the conditional `JP cc`.
    pub const JP_CC_OPMASK  : u8 = 0b11_000_111;
    /// Branch to a relative address.
    pub const JR_OPCODE     : u8 = 0x18;
    /// Base of the conditional `JR cc` opcode.
    ///
    /// Match instructions with: `(code & JR_CC_OPMASK) == JR_CC_OPBASE`.
    ///
    /// Build instructions with: `JR_CC_OPBASE|Condition::CC.to_code()`.
    ///
    /// **Note:** only `NZ`, `Z`, `NC`, `C` are valid `JR cc` conditions.
    pub const JR_CC_OPBASE  : u8 = 0b00_100_000;
    /// Opcode mask of the conditional `JR cc`.
    pub const JR_CC_OPMASK  : u8 = 0b11_100_111;
    /// Call a system subroutine at `0x00`.
    pub const RST_00H_OPCODE: u8 = 0xC7;
    /// Call a system subroutine at `0x08`.
    pub const RST_08H_OPCODE: u8 = 0xCF;
    /// Call a system subroutine at `0x10`.
    pub const RST_10H_OPCODE: u8 = 0xD7;
    /// Call a system subroutine at `0x18`.
    pub const RST_18H_OPCODE: u8 = 0xDF;
    /// Call a system subroutine at `0x20`.
    pub const RST_20H_OPCODE: u8 = 0xE7;
    /// Call a system subroutine at `0x28`.
    pub const RST_28H_OPCODE: u8 = 0xEF;
    /// Call a system subroutine at `0x30`.
    pub const RST_30H_OPCODE: u8 = 0xF7;
    /// Call a system subroutine at `0x38`.
    pub const RST_38H_OPCODE: u8 = 0xFF;
    /// Base of the `RST p` opcode.
    ///
    /// Match instructions with: `(code & RST_OPMASK) == RST_OPBASE`.
    ///
    /// Build instructions with: `RST_OPBASE|addr` where `addr` is one of:
    /// `0x00u8, 0x08, 0x10, 0x18, 0x20, 0x28, 0x30, 0x38`.
    pub const RST_OPBASE:  u8 = 0b11_000_111;
    /// Opcode mask of the `RST p` instruction.
    pub const RST_OPMASK:  u8 = 0b11_000_111;
    /// Decrement `B` and branch to a relative address unless `B=0`.
    pub const DJNZ_OPCODE:    u8 = 0x10;
}

#[cfg(test)]
mod tests {
    use super::*;
    use disasm::disasm_memory;
    use z80::NMOS;
    use opconsts::*;

    fn test_opcode(len: usize, opcode: u8, matching: &str) -> CpuDebug {
        let deb = debug_opcode(None, opcode, len);
        let res = format!("{} {}", deb.mnemonic, deb.args);
        assert_eq!(res.as_str(), matching);
        deb
    }

    fn test_opcode2(len: usize, opcode: (u8, u8), matching: &str) -> CpuDebug {
        let deb = debug_opcode(Some(opcode.0), opcode.1, len);
        let res = format!("{} {}", deb.mnemonic, deb.args);
        assert_eq!(res.as_str(), matching);
        deb
    }

    fn debug_opcode(prefix: Option<u8>, opcode: u8, len: usize) -> CpuDebug {
        let mut dbg = None;
        let mut memory = [0u8;4];
        if let Some(pfx) = prefix {
            memory[0] = pfx;
            memory[1] = opcode;
        }
        else {
            memory[0] = opcode;
        }
        disasm_memory::<Z80<NMOS>,_,()>(0, &memory[0..len], |deb| {
            if dbg.is_some() {
                return Err(());
            }
            dbg = Some(deb);
            Ok(())
        }).expect("only a single instruction");
        dbg.expect("some opcode")
    }

    #[test]
    fn opconst_opcodes() {
        test_opcode(1, NOP_OPCODE,     "NOP ");
        test_opcode(1, HALT_OPCODE,    "HALT ");
        test_opcode(1, DI_OPCODE,      "DI ");
        test_opcode(1, EI_OPCODE,      "EI ");
        test_opcode(1, RET_OPCODE,     "RET ");
        test_opcode2(2, RETI_OPCODE_T2,"RETI ");
        test_opcode2(2, RETN_OPCODE_T2,"RETN ");
        test_opcode(3, CALL_OPCODE,    "CALL 0");
        test_opcode(3, JP_OPCODE,      "JP 0");
        test_opcode(2, JR_OPCODE,      "JR 2");
        test_opcode(1, RST_00H_OPCODE, "RST 0");
        test_opcode(1, RST_08H_OPCODE, "RST 8");
        test_opcode(1, RST_10H_OPCODE, "RST 16");
        test_opcode(1, RST_18H_OPCODE, "RST 24");
        test_opcode(1, RST_20H_OPCODE, "RST 32");
        test_opcode(1, RST_28H_OPCODE, "RST 40");
        test_opcode(1, RST_30H_OPCODE, "RST 48");
        test_opcode(1, RST_38H_OPCODE, "RST 56");
        test_opcode(2, DJNZ_OPCODE,    "DJNZ 2");
    }

    fn test_base_match(len: usize, opbase: u8, opmask: u8, code: u8, matching: &str) {
        let opcode = opbase|code;
        test_opcode(len, opcode, matching);
        assert_eq!((opcode & opmask), opbase);
    }
    fn test_base_match_ed(len: usize, opbase: u8, opmask: u8, cond: Condition, matching: &str) {
        let opcode = opbase|cond.to_code();
        test_opcode2(len, (ED_PREFIX, opcode), matching);
        assert_eq!((opcode & opmask), opbase);
    }

    #[test]
    fn opconst_base() {
        test_base_match(3, JP_CC_OPBASE, JP_CC_OPMASK, Condition::NZ.to_code(), "JP NZ, 0");
        test_base_match(3, JP_CC_OPBASE, JP_CC_OPMASK, Condition::Z.to_code(),  "JP Z, 0");
        test_base_match(3, JP_CC_OPBASE, JP_CC_OPMASK, Condition::NC.to_code(), "JP NC, 0");
        test_base_match(3, JP_CC_OPBASE, JP_CC_OPMASK, Condition::C.to_code(),  "JP C, 0");
        test_base_match(3, JP_CC_OPBASE, JP_CC_OPMASK, Condition::PO.to_code(), "JP PO, 0");
        test_base_match(3, JP_CC_OPBASE, JP_CC_OPMASK, Condition::PE.to_code(), "JP PE, 0");
        test_base_match(3, JP_CC_OPBASE, JP_CC_OPMASK, Condition::P.to_code(),  "JP P, 0");
        test_base_match(3, JP_CC_OPBASE, JP_CC_OPMASK, Condition::M.to_code(),  "JP M, 0");
        test_base_match(3, CALL_CC_OPBASE, CALL_CC_OPMASK, Condition::NZ.to_code(), "CALL NZ, 0");
        test_base_match(3, CALL_CC_OPBASE, CALL_CC_OPMASK, Condition::Z.to_code(),  "CALL Z, 0");
        test_base_match(3, CALL_CC_OPBASE, CALL_CC_OPMASK, Condition::NC.to_code(), "CALL NC, 0");
        test_base_match(3, CALL_CC_OPBASE, CALL_CC_OPMASK, Condition::C.to_code(),  "CALL C, 0");
        test_base_match(3, CALL_CC_OPBASE, CALL_CC_OPMASK, Condition::PO.to_code(), "CALL PO, 0");
        test_base_match(3, CALL_CC_OPBASE, CALL_CC_OPMASK, Condition::PE.to_code(), "CALL PE, 0");
        test_base_match(3, CALL_CC_OPBASE, CALL_CC_OPMASK, Condition::P.to_code(),  "CALL P, 0");
        test_base_match(3, CALL_CC_OPBASE, CALL_CC_OPMASK, Condition::M.to_code(),  "CALL M, 0");
        test_base_match(1, RET_CC_OPBASE, RET_CC_OPMASK, Condition::NZ.to_code(), "RET NZ");
        test_base_match(1, RET_CC_OPBASE, RET_CC_OPMASK, Condition::Z.to_code(),  "RET Z");
        test_base_match(1, RET_CC_OPBASE, RET_CC_OPMASK, Condition::NC.to_code(), "RET NC");
        test_base_match(1, RET_CC_OPBASE, RET_CC_OPMASK, Condition::C.to_code(),  "RET C");
        test_base_match(1, RET_CC_OPBASE, RET_CC_OPMASK, Condition::PO.to_code(), "RET PO");
        test_base_match(1, RET_CC_OPBASE, RET_CC_OPMASK, Condition::PE.to_code(), "RET PE");
        test_base_match(1, RET_CC_OPBASE, RET_CC_OPMASK, Condition::P.to_code(),  "RET P");
        test_base_match(1, RET_CC_OPBASE, RET_CC_OPMASK, Condition::M.to_code(),  "RET M");
        test_base_match(2, JR_CC_OPBASE, JR_CC_OPMASK, Condition::NZ.to_code(), "JR NZ, 2");
        test_base_match(2, JR_CC_OPBASE, JR_CC_OPMASK, Condition::Z.to_code(),  "JR Z, 2");
        test_base_match(2, JR_CC_OPBASE, JR_CC_OPMASK, Condition::NC.to_code(), "JR NC, 2");
        test_base_match(2, JR_CC_OPBASE, JR_CC_OPMASK, Condition::C.to_code(),  "JR C, 2");
        test_base_match(2, JR_CC_OPBASE, JR_CC_OPMASK, Condition::PO.to_code(), "JR NZ, 2");
        test_base_match(2, JR_CC_OPBASE, JR_CC_OPMASK, Condition::PE.to_code(), "JR Z, 2");
        test_base_match(2, JR_CC_OPBASE, JR_CC_OPMASK, Condition::P.to_code(),  "JR NC, 2");
        test_base_match(2, JR_CC_OPBASE, JR_CC_OPMASK, Condition::M.to_code(),  "JR C, 2");
        test_base_match(1, RST_OPBASE, RST_OPMASK, 0x00, "RST 0");
        test_base_match(1, RST_OPBASE, RST_OPMASK, 0x08, "RST 8");
        test_base_match(1, RST_OPBASE, RST_OPMASK, 0x10, "RST 16");
        test_base_match(1, RST_OPBASE, RST_OPMASK, 0x18, "RST 24");
        test_base_match(1, RST_OPBASE, RST_OPMASK, 0x20, "RST 32");
        test_base_match(1, RST_OPBASE, RST_OPMASK, 0x28, "RST 40");
        test_base_match(1, RST_OPBASE, RST_OPMASK, 0x30, "RST 48");
        test_base_match(1, RST_OPBASE, RST_OPMASK, 0x38, "RST 56");
        test_base_match_ed(2, RETN_OP2_BASE, RETN_OP2_MASK, Condition::NZ, "RETN ");
        test_base_match_ed(2, RETN_OP2_BASE, RETN_OP2_MASK, Condition::Z,  "RETI ");
        test_base_match_ed(2, RETN_OP2_BASE, RETN_OP2_MASK, Condition::NC, "RETN ");
        test_base_match_ed(2, RETN_OP2_BASE, RETN_OP2_MASK, Condition::C,  "RETN ");
        test_base_match_ed(2, RETN_OP2_BASE, RETN_OP2_MASK, Condition::PO, "RETN ");
        test_base_match_ed(2, RETN_OP2_BASE, RETN_OP2_MASK, Condition::PE, "RETN ");
        test_base_match_ed(2, RETN_OP2_BASE, RETN_OP2_MASK, Condition::P,  "RETN ");
        test_base_match_ed(2, RETN_OP2_BASE, RETN_OP2_MASK, Condition::M,  "RETN ");
    }
}
