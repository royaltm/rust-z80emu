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

For an emulator to be complete, 4 traits need to be implemented.

The traits:

* [Cpu] - An interface to the finite state machine that is able to "execute" the machine code instructions as one of
         the Z80 family processors.
         Interacting with the memory or I/O devices as well as synchronizing the execution via T-states counting is
         realized via the following traits.
* [Clock] - A Cpu T-state cycle counter which can be used to synchronize the Cpu emulation with the emulator's side effects.
* [Memory] - The Cpu interacts with the memory via this trait.
* [Io] - The Cpu uses this for I/O and maskable interrupts.

`z80emu` crate provides [Cpu] trait [implementations](z80) and an example [implementation](host::TsCounter)
for the [Clock] trait.

The rest of the traits need to be implemented by the emulator's developer.

Please see each trait's documentation on how to implement them.

In this crate one implementation of the Cpu trait is provided with some selectable ["flavours"][z80]:

* [Z80NMOS] - A Zilog's NMOS Z80.
* [Z80CMOS] - A CMOS version of Z80.
* [z80::Z80BM] - A clone of Z80.

The difference is very subtle and only affects the undocumented behaviour.

## Debugger

The Cpu trait provides an ability to debug the executed machine code.
Some of the Cpu functions accept the optional callback argument: `debug`.
This callback is being fed with the extended information about the command being executed and can be used
to e.g. display human readable text of the disassembled instructions or gather statistics.

In `z80emu` the command execution code and the debugger code is implemented together in a single unit.
This way there is only a single machine code [dispatcher]. This minimizes the probability of a debugger suffering
from "schizophrenic effects" showing results not compatible with the execution unit.
Thanks to Rust and LLVM, the compilator is able to optimize out the debugger parts when they are not
[needed](https://github.com/royaltm/rust-z80emu/blob/master/benches/shuffle.rs).

The debugger provides information as a [CpuDebug] struct. It implements [Display][core::fmt::Display],
[LowerHex][core::fmt::LowerHex] and [UpperHex][core::fmt::UpperHex] traits so it's easy to print it OOB
as well as provide complete customized debugging solution.

## How To

Start by inspecting the [tests](https://github.com/royaltm/rust-z80emu/tree/master/tests) and [benches](https://github.com/royaltm/rust-z80emu/tree/master/benches) directory.
All of the test cases run a minimalistic Z80 virtual computers and can be usefull in learning about the essentials.

For a bigger picture see the crate's [repository example](https://github.com/royaltm/rust-z80emu/tree/master/examples/ral1243)
implementation of the imaginary Z80 based computer, to see how a system bus could be implemented with custom PIO and CTC
peripheral chips. This is of course not the only way one can implement that, but perhaps it can give some ideas.

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
            Some(|deb| { println!("{:#X}", deb); })) {
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

#[macro_use]
extern crate bitflags;

pub mod host;
mod cpu;
pub mod z80;
pub mod disasm;

pub use cpu::*;
pub use host::{Clock, Io, Memory, BreakCause};
pub use z80::{Z80, Z80NMOS, Z80CMOS};

/// An address of the NMI routine.
pub const NMI_RESTART: u16 = 0x66;

/// Some of the selected Z80 op-codes for providing to the [Cpu::execute_instruction] or returning from [Io::irq_data].
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
