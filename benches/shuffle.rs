// cargo +nightly bench --bench shuffle -- --nocapture
#![feature(test)]
extern crate test;
use core::num::NonZeroI64;
use test::{black_box, Bencher, stats::Summary};
use serde_json::Value;
use rand::prelude::*;

#[path = "../tests/shuffle/mod.rs"]
mod shuffle;

use z80emu::*;
use shuffle::*;

const KERNEL: &[u8] = include_bytes!("../tests/shuffle/shuffle.bin");
const META: &str = include_str!("../tests/shuffle/shuffle.meta");

type TsClock = host::TsCounter<i32>;

#[bench]
fn bench_shuffle(ben: &mut Bencher) {
    let mut cpu = Z80::default();
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
                match cpu.execute_with_limit(&mut shuffle, &mut tsc, 500_000) {
                    Ok(()) => panic!("the shuffle took too long: {:?}", tsc),
                    Err(BreakCause::Halt) => {}
                    Err(cause) => panic!("an unexpected break cause: {:?}", cause),
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
