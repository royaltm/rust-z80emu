// cargo +nightly bench --bench shuffle -- --nocapture
#![feature(test)]
extern crate test;
use core::num::NonZeroI64;
use test::{black_box, Bencher, stats::Summary};
use serde_json::Value;


use rand::prelude::*;
use z80emu::*;

const KERNEL: &[u8] = include_bytes!("../tests/shuffle/shuffle.bin");
const META: &str = include_str!("../tests/shuffle/shuffle.meta");

type TsClock = TsCounter<i32>;

#[derive(Clone,Debug,Default)]
struct TestShuffle {
    mem: Vec<u8>
}

impl Io for TestShuffle {
    type Timestamp = i32;
    #[inline(always)]
    fn read_io(&mut self, _port: u16, _ts: Self::Timestamp) -> u8 { u8::max_value() }
    #[inline(always)]
    fn write_io(&mut self, _port: u16, _data: u8, _ts: Self::Timestamp) -> bool { false }
    #[inline(always)]
    fn is_irq(&self, _ts: Self::Timestamp) -> bool { false }
}

impl Memory for TestShuffle {
    type Timestamp = i32;
    #[inline(always)]
    fn read_mem(&self, addr: u16, _ts: Self::Timestamp) -> u8 {
        self.read_debug(addr)
    }
    #[inline(always)]
    fn read_mem16(&self, addr: u16, _ts: Self::Timestamp) -> u16 {
        let mut bytes = [0;2];
        bytes.copy_from_slice(&self.mem[addr as usize..=addr as usize + 1]);
        u16::from_le_bytes(bytes)
    }
    #[inline(always)]
    fn read_opcode(&mut self, pc: u16, _ir: u16, _ts: Self::Timestamp) -> u8 {
        self.read_debug(pc)
    }
    /// This is used by the Cpu for writing to the memory.
    #[inline(always)]
    fn write_mem(&mut self, addr: u16, value: u8, _ts: Self::Timestamp) {
        self.mem[addr as usize] = value;
    }
    /// Used by the Cpu debugger to get conditional command argument (DJNZ/JR when not jumping). No timestamp.
    #[inline(always)]
    fn read_debug(&self, addr: u16) -> u8 {
        self.mem[addr as usize]
    }
}


#[bench]
fn bench_shuffle(ben: &mut Bencher) {
    let mut cpu = Cpu::default();
    let mut shuffle = TestShuffle::default();
    shuffle.mem.extend_from_slice(KERNEL);

    let seed_offs = match serde_json::from_str(META) {
        Ok(Value::Object(meta)) => {
            meta["seed"].as_u64().expect("no seed in meta") as usize
        }
        Ok(other) => panic!("unexpected content of meta: {:?}", other),
        Err(e) => panic!("error reading shuffle.meta file: {:?}", e)
    };

    let seed_zero: u16 = random();

    eprintln!("seed: {}", seed_zero);
    let mut tstates_total = NonZeroI64::new(0);
    let Summary { median, .. } = ben.bench(|ben| {
        ben.iter(|| {
            let mut sum = 0i64;
            for i in 0..1000 {
                let seed: u16 = seed_zero.wrapping_add(i);
                shuffle.mem[seed_offs..=seed_offs+1].copy_from_slice(&seed.to_le_bytes());
                cpu.reset();
                assert!(!cpu.is_halt());
                let mut tsc = TsClock::default();
                tsc = match cpu.execute_with_limit(&mut shuffle, tsc, 500_000) {
                    Ok(t) => panic!("the shuffle took too long: {:?}", t),
                    Err(t) => t
                };
                assert!(cpu.is_halt());
                sum += i64::from((*tsc).0);
                black_box(tsc);
                black_box(&cpu);
            }
            match tstates_total {
                Some(tst) => { assert_eq!(sum, tst.get()); },
                None => { tstates_total = NonZeroI64::new(sum); }
            }
            
        });
    }).unwrap();
    let time = median / 1.0e9;
    eprintln!("Median time: {} s CPU MHz: {}", time, tstates_total.unwrap().get() as f64/time/1.0e6);
}
