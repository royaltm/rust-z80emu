/*
    shuffle: Emulator components for z80emu shuffling tests and benchmarks.
    Copyright (C) 2019-2020  Rafal Michalski

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
use z80emu::{Io, Memory};

#[derive(Clone, Debug, Default)]
pub struct TestShuffle {
    pub mem: Vec<u8>
}

impl Io for TestShuffle {
    type Timestamp = i32;
    type WrIoBreak = ();
    type RetiBreak = ();
}

impl Memory for TestShuffle {
    type Timestamp = i32;
    fn read_mem16(&self, addr: u16, _ts: Self::Timestamp) -> u16 {
        let mut bytes = [0;2];
        bytes.copy_from_slice(&self.mem[addr as usize..=addr as usize + 1]);
        u16::from_le_bytes(bytes)
    }
    /// This is used by the Cpu for writing to the memory.
    fn write_mem(&mut self, addr: u16, value: u8, _ts: Self::Timestamp) {
        self.mem[addr as usize] = value;
    }
    /// Used by the Cpu debugger to get conditional command argument (DJNZ/JR when not jumping). No timestamp.
    fn read_debug(&self, addr: u16) -> u8 {
        self.mem[addr as usize]
    }
}
