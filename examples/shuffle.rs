/*
    shuffle: Benchmark program for the z80emu library.
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
use std::convert::TryInto;
use crate::z80::Flavour;
use std::env;
use std::time::{Duration, Instant};

#[path = "../tests/shuffle/mod.rs"]
mod shuffle;

use z80emu::{*, z80::Z80BM1};
use shuffle::*;

trait CpuExec {
    fn cpu_exec<C: Cpu>(cpu: &mut C, shuffle: &mut TestShuffle, tsc: &mut TsClock, limit: i32) -> host::Result<u8, ()>;
}

const SEED_ZERO: u16 = u16::max_value();

type TsClock = host::TsCounter<i32>;

const STEP_INTERVAL: Duration = Duration::from_secs(3);

const ZERO: Duration = Duration::from_nanos(0);

fn shuffle_run<E: CpuExec, C: Cpu>(mut cpu: C, mut total_duration: Duration) {
    let seed_zero: u16 = SEED_ZERO;
    let mut shuffle = TestShuffle::build();
    shuffle.port = 0x45;
    let seed_offs = shuffle["seed"][0] as usize;
    let routine = shuffle["routine"];
    let run_forever = shuffle["run_forever"][0];
    println!("seed: {}", seed_zero);
    let mut ticks = 0i64;
    let mut dur = ZERO;
    shuffle.mem[seed_offs..=seed_offs+1].copy_from_slice(&seed_zero.to_le_bytes());
    shuffle.mem[routine[0] as usize..routine[1] as usize].copy_from_slice(&run_forever.to_le_bytes());
    cpu.reset();
    assert!(!cpu.is_halt());
    loop {
        let mut tsc = TsClock::default();
        let start = Instant::now();
        match E::cpu_exec(&mut cpu, &mut shuffle, &mut tsc, 6_500_000) {
            Ok(()) => panic!("the shuffle took too long: {:?}", tsc),
            Err(BreakCause::WriteIo(0)) => {
                // eprintln!("tsc={:?} seed={}", tsc,
                //     u16::from_le_bytes(shuffle.mem[seed_offs..=seed_offs+1].try_into().unwrap()));
            },
            Err(BreakCause::WriteIo(ch)) => panic!("an unexpected IO write: 0x{ch:2x}", ch=ch),
            Err(BreakCause::Halt) => panic!("an unexpected HALT"),
            Err(cause) => panic!("an unexpected break cause: {:?}", cause),
        };
        dur += start.elapsed();
        ticks += i64::from((*tsc).0);
        assert!(!cpu.is_halt());
        if dur > STEP_INTERVAL {
            println!("CPU MHz: {:10.04} time: {:.02}s ticks: {:11} seed: {}",
                ticks as f64 / dur.as_secs_f64() / 1.0e6, dur.as_secs_f64(), ticks,
                u16::from_le_bytes(shuffle.mem[seed_offs..=seed_offs+1].try_into().unwrap()));
            if let Some(tdur) = total_duration.checked_sub(dur) {
                total_duration = tdur;
            }
            else {
                return;
            }
            ticks = 0;
            dur = ZERO;
        }
    }
}

struct CpuExecWithLimit;
impl CpuExec for CpuExecWithLimit {
    #[inline(never)]
    fn cpu_exec<C: Cpu>(cpu: &mut C, shuffle: &mut TestShuffle, tsc: &mut TsClock, limit: i32) -> host::Result<u8, ()> {
        cpu.execute_with_limit(shuffle, tsc, limit)
    }
}

// Ensure only a single Cpu::execute_instruction instance exists per each Cpu type.
#[inline(always)]
fn execute_next<C: Cpu>(
    cpu: &mut C,
    shuffle: &mut TestShuffle,
    tsc: &mut TsClock,
    debug: Option<&mut dyn FnMut(CpuDebug)>
) -> host::Result<u8, ()>
{
    cpu.execute_next(shuffle, tsc, debug)
}

struct CpuExecSteps;
impl CpuExec for CpuExecSteps {
    #[inline(never)]
    fn cpu_exec<C: Cpu>(cpu: &mut C, shuffle: &mut TestShuffle, tsc: &mut TsClock, limit: i32) -> host::Result<u8, ()> {
        const NO_DEBUG: Option<&mut dyn FnMut(CpuDebug)> = None;
        // const NO_DEBUG: Option<CpuDebugFn> = None;
        loop {
            if tsc.is_past_limit(limit) {
                return Ok(());
            }
            if let Err(err) = execute_next(cpu, shuffle, tsc, NO_DEBUG) {
                return Err(err);
            }
        }
    }
}

fn black_box(opt: &mut Option<CpuDebug>) { core::hint::black_box(opt); }

struct CpuExecDebug;
impl CpuExec for CpuExecDebug {
    #[inline(never)]
    fn cpu_exec<C: Cpu>(cpu: &mut C, shuffle: &mut TestShuffle, tsc: &mut TsClock, limit: i32) -> host::Result<u8, ()> {
        let mut debopt: Option<CpuDebug> = None;
        loop {
            if tsc.is_past_limit(limit) {
                return Ok(());
            }
            if let Err(err) = execute_next(cpu, shuffle, tsc, Some(&mut |deb| {
                debopt = Some(deb);
                black_box(&mut debopt);
            })) {
                return Err(err);
            }
            // if let Some(ref deb) = debopt {
            //     println!("{}", deb);
            // }
        }
    }
}

enum CpuSelect {
    Nmos(Z80NMOS),
    Cmos(Z80CMOS),
    Bm1(Z80BM1),
    Any(Z80Any),
}

#[derive(Clone, Copy, Debug)]
enum ExecSelect {
    Debug, Steps, Limit
}

impl ExecSelect {
    fn shuffle_run<C: Cpu>(self, cpu: C, total_duration: Duration) {
        match self {
            ExecSelect::Debug => shuffle_run::<CpuExecDebug, _>(cpu, total_duration),
            ExecSelect::Steps => shuffle_run::<CpuExecSteps, _>(cpu, total_duration),
            ExecSelect::Limit => shuffle_run::<CpuExecWithLimit, _>(cpu, total_duration),
        }
    }
}

fn shuffle_run_execs<Q: Flavour>(cpu: Z80<Q>, execs: &[ExecSelect], total_duration: Duration) {
    for exe in execs.iter() {
        println!("Z80<{}> {:?}", Q::tag(), exe);
        exe.shuffle_run(cpu.clone(), total_duration);
    }
}

fn shuffle_run_execs_any(cpu: Z80Any, execs: &[ExecSelect], total_duration: Duration) {
    for exe in execs.iter() {
        println!("Z80Any({}) {:?}", cpu.tag(), exe);
        exe.shuffle_run(cpu.clone(), total_duration);
    }
}

fn main() {
    let mut iter = env::args().skip(1);
    let mut cpu_select: Vec<CpuSelect> = vec![];
    let mut cpu_exec: Vec<ExecSelect> = vec![];
    let mut total_duration = Duration::new(u64::MAX, 1_000_000_000 - 1);
    while let Some(arg) = iter.next() {
        match arg.as_str() {
            "debug"   => cpu_exec.push(ExecSelect::Debug),
            "steps"   => cpu_exec.push(ExecSelect::Steps),
            "limit"   => cpu_exec.push(ExecSelect::Limit),
            "nmos"    => cpu_select.push(CpuSelect::Nmos(Z80NMOS::new())),
            "cmos"    => cpu_select.push(CpuSelect::Cmos(Z80CMOS::new())),
            "bm1"     => cpu_select.push(CpuSelect::Bm1(Z80BM1::new())),
            "anynmos" => cpu_select.push(CpuSelect::Any(Z80Any::new_nmos())),
            "anycmos" => cpu_select.push(CpuSelect::Any(Z80Any::new_cmos())),
            "anybm1"  => cpu_select.push(CpuSelect::Any(Z80Any::new_bm1())),
            n => {
                if let Ok(secs) = n.parse::<f64>() {
                    total_duration = Duration::from_secs_f64(secs);
                }
                else {
                    eprintln!("Unknown argument: {}", arg);
                    return;
                }
            }
        }
    }

    if cpu_select.is_empty() {
        cpu_select.push(CpuSelect::Nmos(Z80NMOS::new()));
    }
    if cpu_exec.is_empty() {
        cpu_exec.push(ExecSelect::Limit);
    }

    for cpu in cpu_select.into_iter() {
        match cpu {
            CpuSelect::Nmos(cpu) => shuffle_run_execs(cpu, &cpu_exec, total_duration),
            CpuSelect::Cmos(cpu) => shuffle_run_execs(cpu, &cpu_exec, total_duration),
            CpuSelect::Bm1(cpu)  => shuffle_run_execs(cpu, &cpu_exec, total_duration),
            CpuSelect::Any(cpu)  => shuffle_run_execs_any(cpu, &cpu_exec, total_duration),
        }
    }
}
