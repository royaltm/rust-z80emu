/*
    shuffle_test: Test program for the z80emu library.
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
//! A shuffle test.
use std::borrow::Cow;
use std::fs::{File};
use std::io::Read;
use shuffle::dir;

mod shuffle;

use z80emu::{*, z80::{Flavour, Z80BM1}};
use shuffle::*;

const TEST_SEEDS: [(u16,u16);6] = [(0, 25076), (0x0368, 31469), (0x8000, 24574),
                                   (0xBACA, 58893), (0xD9AB, 12786), (0xFFFF, 11751)];

type TsClock = host::TsCounter<i32>;

trait CpuTag {
    fn tag(&self) -> Cow<'static, str>;
}

impl<Q: Flavour> CpuTag for Z80<Q> {
    fn tag(&self) -> Cow<'static, str> {
        Q::tag().into()
    }
}

impl CpuTag for Z80Any {
    fn tag(&self) -> Cow<'static, str> {
        format!("Z80Any::{}", Z80Any::tag(self)).into()
    }
}

#[test]
fn test_shuffle() {
    test_shuffle_cpu(Z80NMOS::new());
    test_shuffle_cpu(Z80CMOS::new());
    test_shuffle_cpu(Z80BM1::new());
    test_shuffle_cpu(Z80Any::new_nmos());
    test_shuffle_cpu(Z80Any::new_cmos());
    test_shuffle_cpu(Z80Any::new_bm1());
}

fn test_shuffle_cpu<C: Cpu + CpuTag>(mut cpu: C) {
    eprintln!("CPU: {}", cpu.tag());
    let mut kernel = TestShuffle::build();
    let (stackbot, stackend, seed_offs, mult_offs, target, target_end, routine,
            kernel_offs,
            shuffle,
            ssort_asc,
            ssort_desc,
            isort_asc,
            isort_desc,
            qsort_asc,
            qsort_desc,
            mul_seed
        ) = (
            kernel["stackbot"][0],
            kernel["stackend"][0],
            kernel["seed"][0].into(),
            kernel["multiplicator"][0].into(),
            kernel["target"][0].into(),
            kernel["target_end"][0].into(),
            kernel["routine"][0].into(),
            kernel["kernel"],
            kernel["shuffle"],
            kernel["sort.selection_asc"],
            kernel["sort.selection_desc"],
            kernel["sort.insertion_asc"],
            kernel["sort.insertion_desc"],
            kernel["sort.quick_asc"],
            kernel["sort.quick_desc"],
            kernel["mul_seed"],
        );
    eprintln!("Stack 0x{:04X} <-> 0x{:04X} ", stackbot, stackend);

    const SHUFFLE_MAX: i32 = 302346;
    const MUL_SEED_MAX: i32 = 310036;
    let sort_routines: [([u16;2],bool,&str,i32);6] = [
        (ssort_asc, true, "SELECTION", 1574647),
        (ssort_desc, false, "SELECTION", 1574897),
        (isort_asc, true, "INSERTION", 1093467),
        (isort_desc, false, "INSERTION", 1128051),
        (qsort_asc, true, "QUICK", 500000),
        (qsort_desc, false, "QUICK", 500000)];

    for (seed_in, seed_out) in TEST_SEEDS.iter() {
        eprintln!("\r\n Shuffle 0x{:04X} -> 0x{:04X}  {} -> {}:", seed_in, seed_out, seed_in, seed_out);
        kernel.mem[seed_offs..=seed_offs+1].copy_from_slice(&seed_in.to_le_bytes());
        kernel.mem[mult_offs..=mult_offs+1].copy_from_slice(&seed_in.to_le_bytes());
        let mut bin = File::open(format!(dir!("shuffled_{:04X}.bin"), seed_in))
                           .expect("missing shuffle result file");
        let mut templ = Vec::with_capacity(0x100);
        assert_eq!(0x100, bin.read_to_end(&mut templ).unwrap());

        cpu.reset();
        assert!(!cpu.is_halt());
        let mut kernel1 = kernel.clone();
        let tsc1 = run_debug(&mut cpu, &mut kernel1, TsClock::default(),
                                 SHUFFLE_MAX, shuffle[0]..shuffle[1], kernel_offs[0]..kernel_offs[1],
                                 stackbot..=stackend);
        assert_eq!(&seed_out.to_le_bytes(), &kernel1.mem[seed_offs..=seed_offs+1]);
        assert_eq!(&seed_in.to_le_bytes(), &kernel1.mem[mult_offs..=mult_offs+1]);
        assert_eq!(templ[..], kernel1.mem[target..target_end]);

        eprintln!(" ...{:?}", tsc1);

        cpu.reset();
        assert!(!cpu.is_halt());
        let mut kernel2 = kernel.clone();
        let tsc2 = run_with_limits(&mut cpu, &mut kernel2, TsClock::default(), SHUFFLE_MAX);
        assert_eq!(&seed_out.to_le_bytes(), &kernel2.mem[seed_offs..=seed_offs+1]);
        assert_eq!(&seed_in.to_le_bytes(), &kernel2.mem[mult_offs..=mult_offs+1]);
        assert_eq!(templ[..], kernel2.mem[target..target_end]);

        assert_eq!(kernel1.mem[..], kernel2.mem[..]);
        assert_eq!(tsc1, tsc2);

        for (sort, is_asc, name, max_runtick) in &sort_routines
        {
            eprintln!(" {} SORT {}:", name, if *is_asc { "ASCENDING" }else{ "DESCENDING" });

            let mut kernel1s = kernel1.clone();
            cpu.reset();
            assert!(!cpu.is_halt());
            kernel1s.mem[routine..=routine+1].copy_from_slice(&sort[0].to_le_bytes());
            let tsc1s = run_debug(&mut cpu, &mut kernel1s, tsc1,
                                  *max_runtick, sort[0]..sort[1], kernel_offs[0]..kernel_offs[1],
                                  stackbot..=stackend);
            assert_mem_sorted(&kernel1s.mem[target..target_end], *is_asc);

            eprintln!(" {:?}...{:?} {}", tsc1, tsc1s, tsc1s.0 - tsc1.0);

            let mut kernel2s = kernel2.clone();
            cpu.reset();
            assert!(!cpu.is_halt());
            kernel2s.mem[routine..=routine+1].copy_from_slice(&sort[0].to_le_bytes());
            let tsc2s = run_with_limits(&mut cpu, &mut kernel2s, tsc2, *max_runtick);
            assert_mem_sorted(&kernel2s.mem[target..target_end], *is_asc);

            assert_eq!(kernel1s.mem[..], kernel2s.mem[..]);
            assert_eq!(tsc1s, tsc2s);
        }

        let expect = *seed_out as u32 * *seed_in as u32;
        eprintln!(" Mul SEED  {}*{}={}", seed_in, seed_out, expect);

        cpu.reset();
        assert!(!cpu.is_halt());
        kernel1.out = Some(String::new());
        kernel1.mem[routine..=routine+1].copy_from_slice(&mul_seed[0].to_le_bytes());
        let tsc1m = run_debug(&mut cpu, &mut kernel1, tsc1,
                              MUL_SEED_MAX, mul_seed[0]..mul_seed[1], kernel_offs[0]..kernel_offs[1],
                              stackbot..=stackend);
        eprintln!(" {:?}...{:?} {}", tsc1, tsc1m, tsc1m.0 - tsc1.0);
        let res = u32::from_le_bytes(kernel1.mem[seed_offs..=seed_offs+3].try_into().unwrap());
        assert_eq!(res, expect);
        let out = kernel1.out.take().unwrap();
        assert_eq!(out, format!("{}", res));

        cpu.reset();
        assert!(!cpu.is_halt());
        kernel2.out = Some(String::new());
        kernel2.mem[routine..=routine+1].copy_from_slice(&mul_seed[0].to_le_bytes());
        let tsc2m = run_with_limits(&mut cpu, &mut kernel2, tsc2, MUL_SEED_MAX);
        let res = u32::from_le_bytes(kernel2.mem[seed_offs..=seed_offs+3].try_into().unwrap());
        assert_eq!(res, expect);
        let out = kernel2.out.take().unwrap();
        assert_eq!(out, format!("{}", res));
        assert_eq!(tsc1m, tsc2m);
    }
}

fn assert_mem_sorted(mem: &[u8], is_asc: bool) {
    let countit: Box<dyn Iterator<Item=u8>> = if is_asc { Box::new(0u8..=255) } else { Box::new((0u8..=255).rev()) };
    for (val, exp) in mem.iter().zip(countit) {
        assert_eq!(*val, exp);
    }
}

fn run_debug<C: Cpu>(
        cpu: &mut C, kernel: &mut TestShuffle, mut tsc: TsClock,
        max_runtick: i32, prange: std::ops::Range<u16>, krange: std::ops::Range<u16>, 
        stack: std::ops::RangeInclusive<u16>) -> TsClock
{
    let mut spbot = u16::max_value();
    let mut ksubroutine = false;
    let mut pccheck = false;
    let mut spcheck = false;
    let mut debug = |deb:CpuDebug| {
        if pccheck {
            if prange.contains(&deb.pc) {
                ksubroutine = false;
            }
            else if ksubroutine {
                if !krange.contains(&deb.pc) {
                    println!("{:04x} : {:6} {:#20x} {:02X?}", deb.pc, deb.mnemonic, deb.args, deb.code.as_slice());
                    panic!("PC: Access violation!");
                }
            }
            else {
                println!("{:04x} : {:6} {:#20x} {:02X?}", deb.pc, deb.mnemonic, deb.args, deb.code.as_slice());
                if deb.pc == 0x20 {
                    ksubroutine = true;
                }
                else {
                    assert_eq!(deb.mnemonic, "HALT");
                }
            }
        }
        else if prange.contains(&deb.pc) {
            pccheck = true;
        }
        else {
            println!("{:04x} : {:6} {:#20x} {:02X?}", deb.pc, deb.mnemonic, deb.args, deb.code.as_slice());
            if !krange.contains(&deb.pc) {
                panic!("PC: Access violation!");
            }
        }
    };
    // const NO_DEBUG: Option<CpuDebugFn> = None;
    while !cpu.is_halt() {
        match cpu.execute_next(kernel, &mut tsc, Some(&mut debug)) {
            Ok(..) if tsc.is_past_limit(max_runtick) => {
                panic!("takes too long");
            }
            Ok(..) => {
                let sp = cpu.get_sp();
                if sp < spbot {
                    spbot = sp;
                }
                if spcheck {
                    assert!(stack.contains(&sp));
                }
                else if stack.contains(&sp) {
                    spcheck = true;
                }
                else {
                    assert_eq!(sp, 0xFFFF);
                }
            }
            Err(BreakCause::Halt) => { break }
            Err(cause) => panic!("an unexpected break cause: {:?}", cause)
        }
    }
    eprintln!(" SP bottom: 0x{:04X} -{}", spbot, *stack.end() - spbot);
    assert!(pccheck);
    assert!(spcheck);
    assert!(cpu.is_halt());
    assert_eq!(cpu.get_sp(), *stack.end());
    tsc
}

fn run_with_limits<C: Cpu>(cpu: &mut C, kernel: &mut TestShuffle, mut tsc: TsClock, max_runtick: i32) -> TsClock {
    let mut out = kernel.out.take();
    loop {
        match cpu.execute_with_limit(kernel, &mut tsc, max_runtick) {
            Ok(()) => panic!("took too long: {:?}", tsc),
            Err(BreakCause::WriteIo(ch)) => {
                if let Some(mut out) = out.take() {
                    out.push(ch.into());
                    kernel.out = Some(out);
                    continue;
                }
                else {
                    panic!("unexpected WriteIo");
                }
            }
            Err(BreakCause::Halt) => {}
            Err(cause) => panic!("an unexpected break cause: {:?}", cause),
        };
        break;
    }
    assert!(cpu.is_halt());
    tsc
}
