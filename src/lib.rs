/*! # Z80 emu
 
 This Rust language crate provides building blocks for emulators based on Zilog's Z80 CPU family.
 The crate supports `no_std`.
 
 
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
 
 The presented solution tries to be minimalistic in it's approach to emulation and makes no assumptions
 about the side efffects (except the debugger) of the emulators being built upon it.
 
 The complete emulation requires 4 traits to be implemented.
 Some of them ([Cpu], [Clock]) has some implementations provided.
 The others need to be implemented by the user.
 
 The traits:
 
 * [Cpu] - An interface to the finite state machine that is able to "execute" the machine code instructions as one of
           the Z80 family processors.
           Interacting with the memory or I/O devices as well as T-states counting is realized via the following traits.
 * [Clock] - A Cpu cycle T-states counter which can be used to synchronize the Cpu emulation with the emulator's side effects.
 * [Memory] - The Cpu interacts with the memory via this trait.
 * [Io] - The Cpu uses this for it's IN/OUT communication and maskable interrupts.
 
 Please see each trait's documentation on how to implement them.
 
 Currently there is one implementation of the Cpu trait with some selectable "flavours".
 
 * Z80 - A Zilog's NMOS Z80.
 * Z80CMOS - A CMOS version of Z80.
 * Z80BM - A clone of Z80.
 
 The difference is very subtle and only affects the undocumented behaviour.
 
 ## Debugger
 
 The Cpu trait provides the possibility to debug the executed machine code.
 Some of the Cpu functions accept the optional `debug` argument which is a callback.
 This callback is being fed with the extended information about the command being executed and can be used
 to e.g. display human readable text of the disassembled instructions.
 
 In this solution the command execution code and the debugger code is provided together as a single unit.
 This way there is only a single machine code dispatcher. This minimizes the probability of a debugger suffering from "schizophrenic effects" showing results not compatible with the execution unit.
 Thanks to the way the Rust and LLVM is designed, the compilator is able to optimize out the debugger parts
 when they are not needed.
 
 The debugger provides information as a [CpuDebug] struct. It implements [Display][core::fmt::Display],
 [LowerHex][core::fmt::LowerHex] and [UpperHex][core::fmt::UpperHex] traits so it's easy to print it OOB
 as well as provide complete customized debugging solution.
*/
#![cfg_attr(not(feature = "std"), no_std)]

#[macro_use]
extern crate bitflags;

pub mod host;
mod cpu;
pub mod z80;

pub use cpu::*;
pub use host::{Clock, Io, Memory, BreakCause};
pub use z80::Z80;

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
    pub const RST_00H_OPCODE : u8 = 0xC7;
    pub const RST_08H_OPCODE : u8 = 0xCF;
    pub const RST_10H_OPCODE: u8 = 0xD7;
    pub const RST_18H_OPCODE: u8 = 0xDF;
    pub const RST_20H_OPCODE: u8 = 0xE7;
    pub const RST_28H_OPCODE: u8 = 0xEF;
    pub const RST_30H_OPCODE: u8 = 0xF7;
    pub const RST_38H_OPCODE: u8 = 0xFF;
}

// https://faqwiki.zxnet.co.uk/wiki/ULAplus
// https://spectrumforeveryone.com/technical/zx-spectrum-ula-types/
// https://github.com/fabriziotappero/ip-cores/blob/video_controller_ula_chip_for_zx_spectrum/fpga_version/rtl/ula.v
// http://www.piclist.com/techref/mem/dram/slide4.html
// https://scratchpad.fandom.com/wiki/Contended_memory
// https://en.wikipedia.org/wiki/Zilog_Z80
// http://www.worldofspectrum.org/faq/reference/128kreference.htm
// http://www.worldofspectrum.org/faq/reference/48kreference.htm
// https://maker.pro/pic/projects/z80-computer-project-part-1-the-cpu
// https://spectrumforeveryone.com/technical/memory-contention-floating-bus/
// https://spectrumforeveryone.com/technical/spectrum-compatibility-issues/

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
