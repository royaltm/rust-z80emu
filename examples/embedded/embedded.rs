/*
    embedded: Example program for the z80emu library.
    Copyright (C) 2024  Rafal Michalski

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

    Author contact information: see Cargo.toml file, section [package.authors].
*/
#![no_std]
#![no_main]
use panic_halt as _;
use cortex_m_rt::entry;

use z80emu::*;
use opconsts::HALT_OPCODE;

#[entry]
fn main() -> ! {
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
              Some(|deb| { core::hint::black_box(deb); })) {
          Err(BreakCause::Halt) => { break }
          _ => {}
      }
  }
  // the content of the HL registers
  let result = cpu.get_reg16(StkReg16::HL);
  assert_eq!(result, 46368); // Fib(24)
  // the number of T-states passed
  assert_eq!(tsc.as_timestamp(), 10+10+(FIB_N as i32)*(4+11+13)-5+4);

  loop {}
}
