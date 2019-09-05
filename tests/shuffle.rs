//! A shuffle test.
// use core::num::Wrapping;
// use std::error::Error;
// use std::convert::TryInto;
use std::fs::{File};
use std::io::Read;
use serde_json::Value;

use z80emu::*;

const TEST_SEEDS: [(u16,u16);3] = [(0, 25076), (0xBACA, 58893), (0xFFFF, 11751)];

type TsClock = TsCounter<i32>;

#[derive(Clone,Debug,Default)]
struct TestShuffle {
    mem: Vec<u8>
}

impl Io for TestShuffle {
    type Timestamp = i32;
    fn read_io(&mut self, _port: u16, _ts: Self::Timestamp) -> u8 { u8::max_value() }
    fn write_io(&mut self, _port: u16, _data: u8, _ts: Self::Timestamp) -> bool { false }
    fn is_irq(&self, _ts: Self::Timestamp) -> bool { false }
}

impl Memory for TestShuffle {
    type Timestamp = i32;
    fn read_mem(&self, addr: u16, _ts: Self::Timestamp) -> u8 {
        self.read_debug(addr)
    }
    fn read_mem16(&self, addr: u16, _ts: Self::Timestamp) -> u16 {
        let mut bytes = [0;2];
        bytes.copy_from_slice(&self.mem[addr as usize..=addr as usize + 1]);
        u16::from_le_bytes(bytes)
    }
    fn read_opcode(&mut self, pc: u16, _ir: u16, _ts: Self::Timestamp) -> u8 {
        self.read_debug(pc)
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

macro_rules! dir {
    ($file:expr) => {
        concat!(env!("CARGO_MANIFEST_DIR"), "/tests/shuffle/", $file)
    };
}

#[test]
fn test_shuffle() {
    let mut cpu = Cpu::default();
    let mut shuffle = TestShuffle::default();
    let mut bin = File::open(dir!(r"shuffle.bin")).expect("missing shuffle.bin file");
    assert!(bin.read_to_end(&mut shuffle.mem).expect("couldn't read shuffle.bin") > 0);
    let meta = File::open(dir!(r"shuffle.meta")).expect("missing shuffle.meta file");
    let (seed_offs, target) = match serde_json::from_reader(meta) {
        Ok(Value::Object(meta)) => (
            meta["seed"].as_u64().expect("no seed in meta") as usize,
            meta["target"].as_u64().expect("no target in meta") as usize,
        ),
        Ok(other) => panic!("unexpected content of meta: {:?}", other),
        Err(e) => panic!("error reading shuffle.meta file: {:?}", e)
    };

    for (seed_in, seed_out) in TEST_SEEDS.iter() {
        cpu.reset();
        assert!(!cpu.is_halt());
        shuffle.mem[seed_offs..=seed_offs+1].copy_from_slice(&seed_in.to_le_bytes());
        let mut shuffle1 = shuffle.clone();
        let mut tsc1 = TsClock::default();
        let debug = |info| {
            eprintln!("{:x}", info);
        };
        while !cpu.is_halt() {
            tsc1 = cpu.execute_next(&mut shuffle1, tsc1, Some(debug)).unwrap();
            if tsc1.is_at_limit(500_000) {
                panic!("the shuffle takes too long");
            }
        }
        assert_eq!(&seed_out.to_le_bytes(), &shuffle1.mem[seed_offs..=seed_offs+1]);
        let mut bin = File::open(format!(dir!("shuffled_{:04X}.bin"), seed_in)).expect("missing shuffle result file");
        let mut templ = Vec::with_capacity(0x100);
        assert_eq!(0x100, bin.read_to_end(&mut templ).unwrap());
        assert_eq!(templ[..], shuffle1.mem[target..target + 0x100]);
        eprintln!("{:?}", tsc1);

        cpu.reset();
        assert!(!cpu.is_halt());
        let mut shuffle2 = shuffle.clone();
        let mut tsc2 = TsClock::default();
        tsc2 = match cpu.execute_with_limit(&mut shuffle2, tsc2, 500_000) {
            Ok(t) => panic!("the shuffle took too long: {:?}", t),
            Err(t) => t
        };
        assert!(cpu.is_halt());
        assert_eq!(&seed_out.to_le_bytes(), &shuffle2.mem[seed_offs..=seed_offs+1]);
        assert_eq!(templ[..], shuffle2.mem[target..target + 0x100]);
        assert_eq!(tsc1, tsc2);
    }
}
