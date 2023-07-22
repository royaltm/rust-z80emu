/*
    z80emu: ZiLOG Z80 microprocessor emulation library.
    Copyright (C) 2019-2023  Rafal Michalski

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

pub mod host;
mod cpu;
pub mod z80;
pub mod disasm;

pub use cpu::*;
pub use host::{Clock, Io, Memory, BreakCause};
pub use z80::{Z80, Z80NMOS, Z80CMOS};
pub use z80::any::Z80Any;

/// An address of the NMI routine.
pub const NMI_RESTART: u16 = 0x66;

/// Selected Z80 opcodes.
///
/// May be used as a convenient argument to the [Cpu::execute_instruction] function or as a return value from [Io::irq_data].
pub mod opconsts {
    pub const NOP_OPCODE    : u8 = 0x00;
    pub const HALT_OPCODE   : u8 = 0x76;
    pub const RET_OPCODE    : u8 = 0xC9;
    pub const RETI_OPCODE_T2: (u8, u8) = (0xED, 0x4D);
    pub const DI_OPCODE     : u8 = 0xF3;
    pub const EI_OPCODE     : u8 = 0xFB;
    pub const JP_OPCODE     : u8 = 0xC3;
    pub const RST_00H_OPCODE: u8 = 0xC7;
    pub const RST_08H_OPCODE: u8 = 0xCF;
    pub const RST_10H_OPCODE: u8 = 0xD7;
    pub const RST_18H_OPCODE: u8 = 0xDF;
    pub const RST_20H_OPCODE: u8 = 0xE7;
    pub const RST_28H_OPCODE: u8 = 0xEF;
    pub const RST_30H_OPCODE: u8 = 0xF7;
    pub const RST_38H_OPCODE: u8 = 0xFF;
}
