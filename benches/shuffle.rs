// To see the estimated CPU MHZ run with:
//
// cargo +nightly bench --bench shuffle -- --nocapture
//
// on my windows box the results are:
// bench_shuffle_cmos_debug: ~ 530 CPU MHz
// bench_shuffle_cmos_steps: ~ 800 CPU MHz
// bench_shuffle_cmos_limit: ~1100 CPU MHz
// bench_shuffle_nmos_debug: ~ 520 CPU MHz
// bench_shuffle_nmos_steps: ~ 760 CPU MHz
// bench_shuffle_nmos_limit: ~1070 CPU MHz
#![feature(test)]
extern crate test;
use core::num::NonZeroI64;
use test::{black_box, Bencher, stats::Summary};
use serde_json::Value;

#[path = "../tests/shuffle/mod.rs"]
mod shuffle;

use z80emu::{*, z80::Z80BM};
use shuffle::*;

trait CpuExec {
    fn cpu_exec<C: Cpu>(cpu: &mut C, shuffle: &mut TestShuffle, tsc: &mut TsClock, limit: i32) -> host::Result<(), ()>;
}

const NO_DEBUG: Option<CpuDebugFn> = None;
const SEED_ZERO: u16 = u16::max_value();
const KERNEL: &[u8] = include_bytes!("../tests/shuffle/shuffle.bin");
const META: &str = include_str!("../tests/shuffle/shuffle.meta");

type TsClock = host::TsCounter<i32>;

#[bench]
fn bench_shuffle_nmos_debug(ben: &mut Bencher) {
    bench_shuffle_cpu::<Z80NMOS, CpuExecDebug>(ben);
}

#[bench]
fn bench_shuffle_nmos_steps(ben: &mut Bencher) {
    bench_shuffle_cpu::<Z80NMOS, CpuExecSteps>(ben);
}

#[bench]
fn bench_shuffle_nmos_limit(ben: &mut Bencher) {
    bench_shuffle_cpu::<Z80NMOS, CpuExecWithLimit>(ben);
}

#[bench]
fn bench_shuffle_cmos_debug(ben: &mut Bencher) {
    bench_shuffle_cpu::<Z80CMOS, CpuExecDebug>(ben);
}

#[bench]
fn bench_shuffle_cmos_steps(ben: &mut Bencher) {
    bench_shuffle_cpu::<Z80CMOS, CpuExecSteps>(ben);
}

#[bench]
fn bench_shuffle_cmos_limit(ben: &mut Bencher) {
    bench_shuffle_cpu::<Z80CMOS, CpuExecWithLimit>(ben);
}

#[bench]
fn bench_shuffle_bm_debug(ben: &mut Bencher) {
    bench_shuffle_cpu::<Z80BM, CpuExecDebug>(ben);
}

#[bench]
fn bench_shuffle_bm_steps(ben: &mut Bencher) {
    bench_shuffle_cpu::<Z80BM, CpuExecSteps>(ben);
}

#[bench]
fn bench_shuffle_bm_limit(ben: &mut Bencher) {
    bench_shuffle_cpu::<Z80BM, CpuExecWithLimit>(ben);
}

fn bench_shuffle_cpu<C: Cpu, E: CpuExec>(ben: &mut Bencher) {
    let mut cpu = C::default();
    let mut shuffle = TestShuffle::default();
    shuffle.mem.extend_from_slice(KERNEL);

    let seed_offs = match serde_json::from_str(META) {
        Ok(Value::Object(meta)) => {
            meta["seed"].as_u64().expect("no seed in meta") as usize
        }
        Ok(other) => panic!("unexpected content of meta: {:?}", other),
        Err(e) => panic!("error reading shuffle.meta file: {:?}", e)
    };

    let seed_zero: u16 = SEED_ZERO;

    eprintln!("seed: {}", seed_zero);
    let mut tstates_total = NonZeroI64::new(0);
    let Summary { median, .. } = ben.bench(|ben| {
        ben.iter(|| {
            let mut sum = 0i64;
            for i in 0..300 {
                let seed: u16 = seed_zero.wrapping_add(i * 217);
                shuffle.mem[seed_offs..=seed_offs+1].copy_from_slice(&seed.to_le_bytes());
                cpu.reset();
                assert!(!cpu.is_halt());
                let mut tsc = TsClock::default();
                match E::cpu_exec(&mut cpu, &mut shuffle, &mut tsc, 500_000) {
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

struct CpuExecWithLimit;
impl CpuExec for CpuExecWithLimit {
    #[inline(always)]
    fn cpu_exec<C: Cpu>(cpu: &mut C, shuffle: &mut TestShuffle, tsc: &mut TsClock, limit: i32) -> host::Result<(), ()> {
        cpu.execute_with_limit(shuffle, tsc, limit)
    }
}

struct CpuExecSteps;
impl CpuExec for CpuExecSteps {
    #[inline(always)]
    fn cpu_exec<C: Cpu>(cpu: &mut C, shuffle: &mut TestShuffle, tsc: &mut TsClock, limit: i32) -> host::Result<(), ()> {
        loop {
            if tsc.is_past_limit(limit) {
                return Ok(());
            }
            if let Err(err) = cpu.execute_next(shuffle, tsc, NO_DEBUG) {
                return Err(err);
            }
        }
    }
}

struct CpuExecDebug;
impl CpuExec for CpuExecDebug {
    #[inline(always)]
    fn cpu_exec<C: Cpu>(cpu: &mut C, shuffle: &mut TestShuffle, tsc: &mut TsClock, limit: i32) -> host::Result<(), ()> {
        loop {
            if tsc.is_past_limit(limit) {
                return Ok(());
            }
            if let Err(err) = cpu.execute_next(shuffle, tsc, Some(|deb| {
                black_box(deb);
            })) {
                return Err(err);
            }
        }
    }
}
