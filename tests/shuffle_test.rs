//! A shuffle test.
use std::fs::{File};
use std::io::Read;
use serde_json::Value;

mod shuffle;

use z80emu::{*, z80::Z80BM1};
use shuffle::*;

const TEST_SEEDS: [(u16,u16);6] = [(0, 25076), (0x0368, 31469), (0x8000, 24574),
                                   (0xBACA, 58893), (0xD9AB, 12786), (0xFFFF, 11751)];

type TsClock = host::TsCounter<i32>;

macro_rules! dir {
    ($file:expr) => {
        concat!(env!("CARGO_MANIFEST_DIR"), "/tests/shuffle/", $file)
    };
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

fn test_shuffle_cpu<C: Cpu>(mut cpu: C) {
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
        let debug = |deb:CpuDebug| {
            println!("{:04x} : {:6} {:#20x} {:02X?}", deb.pc, deb.mnemonic, deb.args, deb.code.as_slice());
        };
        while !cpu.is_halt() {
            match cpu.execute_next(&mut shuffle1, &mut tsc1, Some(debug)) {
                Ok(()) => if tsc1.is_past_limit(500_000) {
                    panic!("the shuffle takes too long");
                }
                Err(BreakCause::Halt) => { break }
                Err(cause) => panic!("an unexpected break cause: {:?}", cause)
            }
        }
        assert_eq!(&seed_out.to_le_bytes(), &shuffle1.mem[seed_offs..=seed_offs+1]);
        let mut bin = File::open(format!(dir!("shuffled_{:04X}.bin"), seed_in))
                           .expect("missing shuffle result file");
        let mut templ = Vec::with_capacity(0x100);
        assert_eq!(0x100, bin.read_to_end(&mut templ).unwrap());
        assert_eq!(templ[..], shuffle1.mem[target..target + 0x100]);
        eprintln!("{:?}", tsc1);

        cpu.reset();
        assert!(!cpu.is_halt());
        let mut shuffle2 = shuffle.clone();
        let mut tsc2 = TsClock::default();
        match cpu.execute_with_limit(&mut shuffle2, &mut tsc2, 500_000) {
            Ok(()) => panic!("the shuffle took too long: {:?}", tsc2),
            Err(BreakCause::Halt) => {}
            Err(cause) => panic!("an unexpected break cause: {:?}", cause),
        };
        assert!(cpu.is_halt());
        assert_eq!(&seed_out.to_le_bytes(), &shuffle2.mem[seed_offs..=seed_offs+1]);
        assert_eq!(templ[..], shuffle2.mem[target..target + 0x100]);
        assert_eq!(tsc1, tsc2);
    }
}
