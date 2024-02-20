/*
    shuffle: Emulator components for z80emu shuffling tests and benchmarks.
    Copyright (C) 2019-2022  Rafal Michalski

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
use core::num::NonZeroU16;
use std::ops::Index;
use std::collections::HashMap;
use std::{io::Read, fs::File};
use serde_json::Value;
use z80emu::{Io, Memory};

macro_rules! dir {
    ($file:expr) => {
        concat!(env!("CARGO_MANIFEST_DIR"), "/tests/shuffle/", $file)
    };
}

#[allow(unused_imports)]
pub(crate) use dir;

#[derive(Clone, Debug, Default)]
pub struct TestShuffle {
    pub port: u16,
    pub mem: Vec<u8>,
    pub out: Option<String>,
    meta: HashMap<String,[u16;2]>,
    codetop: u16
}

impl TestShuffle {
    const OUT_PORT: u16 = 0x42;

    pub fn build() -> TestShuffle {
        let mut kernel = TestShuffle { port: Self::OUT_PORT, ..Default::default() };
        let mut bin = File::open(dir!(r"shuffle.bin")).expect("missing shuffle.bin file");
        assert!(bin.read_to_end(&mut kernel.mem).expect("couldn't read shuffle.bin") > 0);
        let meta = File::open(dir!(r"shuffle.meta")).expect("missing shuffle.meta file");
        match serde_json::from_reader(meta) {
            Ok(Value::Object(meta)) => {
                for (name, value) in &meta {
                    if !name.starts_with("+") {
                        let start: u16 = value.as_u64().unwrap_or_else(|| panic!("{} in shuffle.meta is not an unsigned integer", name))
                                .try_into().unwrap_or_else(|_| panic!("{} in shuffle.meta is not a 16-bit unsigned integer", name));
                        let size = meta.get(&format!("+{}", name)).unwrap_or_else(|| panic!("+{} in shuffle.meta is missing", name))
                                .as_u64().unwrap_or_else(|| panic!("+{} in shuffle.meta is not an unsigned integer", name))
                                .try_into().unwrap_or_else(|_| panic!("+{} in shuffle.meta is not a 16-bit unsigned integer", name));
                        let end = start.checked_add(size).unwrap_or_else(|| panic!("{} + size is overflowing a 16-bit unsigned integer", name));
                        kernel.meta.insert(name.to_string(), [start,end]);
                    }
                }
            },
            Ok(other) => panic!("unexpected content of shufle.meta: {:?}", other),
            Err(e) => panic!("error reading shuffle.meta file: {:?}", e)
        }
        if let Some([codetop,_]) = kernel.get("codetop") {
            kernel.codetop = *codetop;
        }
        kernel
    }

    pub fn get<'a>(&'a self, name: &str) -> Option<&'a [u16;2]> {
        self.meta.get(name)
    }
}

impl<'a> Index<&'a str> for TestShuffle {
    type Output = [u16;2];

    fn index(&self, name: &'a str) -> &Self::Output {
        self.get(name).unwrap_or_else(|| panic!("{} not found in meta", name))
    }
}

impl Io for TestShuffle {
    type Timestamp = i32;
    type WrIoBreak = u8;
    type RetiBreak = ();

    fn write_io(&mut self, port: u16, data: u8, _ts: Self::Timestamp) -> (Option<Self::WrIoBreak>, Option<NonZeroU16>)
    {
        if port & 0x00FF == self.port {
            if let Some(out) = self.out.as_mut() {
                out.push(data.into());
            }
            else {
                return (Some(data), None)
            }
        }
        (None, None)
    }
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
        if addr < self.codetop {
            panic!("Violated read-only instruction memory at 0x{:04x}", addr);
        }
        self.mem[addr as usize] = value;
    }
    /// Used by the Cpu debugger to get conditional command argument (DJNZ/JR when not jumping). No timestamp.
    fn read_debug(&self, addr: u16) -> u8 {
        self.mem[addr as usize]
    }
}
