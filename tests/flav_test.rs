use core::num::NonZeroU16;
use z80emu::{*, z80::*, opconsts::HALT_OPCODE};

type TsClock = host::TsCounter<i32>;

#[derive(Clone, Debug)]
pub struct TestCtrl {
    mem: Box<[u8]>,
    out_port: u16,
    out_data: u8,
}
impl Io for TestCtrl {
    type Timestamp = i32;
    type WrIoBreak = ();
    type RetiBreak = ();

    fn is_irq(&mut self, ts: i32) -> bool {
        ts > 8
    }

    fn write_io(&mut self, port: u16, data: u8, _ts: i32) -> (Option<()>, Option<NonZeroU16>) {
        self.out_port = port;
        self.out_data = data;
        (None, None)
    }
}

impl Memory for TestCtrl {
    type Timestamp = i32;
    fn read_debug(&self, addr: u16) -> u8 {
        *self.mem.get(addr as usize).unwrap_or(&!0)
    }
}

const TEST_CODE: &[u8] = &[
            0x01, 0x34, 0x12, // 0000: 01 34 12    LD   BC, 1234H       :test1
            0xED, 0x71,       // 0003: ED 71       OUT  (C)
            0x76,             // 0005: 76          HALT
            // OUT: 0 or 255 (CMOS)
            0x3E, 0xFF,       // 0006: 3E FF       LD   A, ffH          :test2
            0x32, 0xFF, 0x55, // 0008: 32 FF 55    LD   (55FFH), A
            0xCB, 0x6E,       // 000B: CB 6E       BIT  5, (HL)
            0x76,             // 000D: 76          HALT
            // memptr=0xFF00 or 0x0000 (BM1)
            // flags: Z|Y|H|X|P or Z|H|P (BM1)
            0x3A, 0x00, 0xFF, // 000E: 3A 00 FF    LD   A, (ff00H)      :test3
            0xCB, 0x5E,       // 0011: CB 5E       BIT  3, (HL)
            0x3E, 0x00,       // 0013: 3E 00       LD   A, 00H
            0x37,             // 0015: 37          SCF
            0x76,             // 0016: 76          HALT
            // memptr=0xFF01
            // BIT 3, (HL) flags: Z|Y|H|X|P
            // flags unmodified, A=0
            // Z|Y|X|P|C (F|=A)  or Z|P|C (CMOS) (F=A)
            0x3A, 0x00, 0xFF, // 0017: 3A 00 FF    LD   A, (ff00H)      :test4
            0xCB, 0x6E,       // 001A: CB 6E       BIT  5, (HL)
            0xAF,             // 001C: AF          XOR  A
            0x3F,             // 001D: 3F          CCF
            0x76,             // 001E: 76          HALT
            // memptr=0xFF01
            // BIT 5, (HL) flags: Z|Y|H|P
            // flags modified, A=0
            // Z|P|C
            0xFB,             // 001F: FB          EI                   :test5 (0) +4
            0xAF,             // 0020: AF          XOR  A               (4) +4
            0xED, 0x57,       // 0021: ED 57       LD   A, I            (8) +9 +13
            0x76,             // 0023: 76          HALT
                              // 0024: 00 00 00 00 00 00 00 00
                              // 002C: 00 00 00 00 00 00 00 00
                              // 0034: 00 00 00 00 00 00
                              // 0038: 76          HALT                 (30) + 4
            // memptr=0x0038
            // P=IFF2: Z (NMOS) Z|P (CMOS,BM1)
];

const TEST1_PC: u16 = 0x0000;
const TEST2_PC: u16 = 0x0006;
const TEST3_PC: u16 = 0x000E;
const TEST4_PC: u16 = 0x0017;
const TEST5_PC: u16 = 0x001F;

impl TestCtrl {
    fn new() -> TestCtrl {
        let mut mem = vec![0u8; 0x39].into_boxed_slice();
        mem[0..TEST_CODE.len()].copy_from_slice(TEST_CODE);
        mem[0x0038] = HALT_OPCODE;
        let out_data = 0b10101010;
        let out_port = 0;
        TestCtrl { mem, out_port, out_data }
    }
}

#[test]
fn flavour_test1() {
    flavour_test1_cpu(Z80NMOS::default(), 0);
    flavour_test1_cpu(Z80CMOS::default(), 0xff);
    flavour_test1_cpu(Z80BM1::default(), 0);
}

fn flavour_test1_cpu<C: Cpu>(mut cpu: C, expected_data: u8) {
    let ctrl = execute_test(&mut cpu, TEST1_PC);
    assert_eq!(ctrl.out_port, 0x1234);
    assert_eq!(ctrl.out_data, expected_data);
}

#[test]
fn flavour_test2() {
    flavour_test2345_cpu(Z80NMOS::default(), TEST2_PC, 0xFF00, CpuFlags::Z|CpuFlags::XY|CpuFlags::H|CpuFlags::P);
    flavour_test2345_cpu(Z80CMOS::default(), TEST2_PC, 0xFF00, CpuFlags::Z|CpuFlags::XY|CpuFlags::H|CpuFlags::P);
    flavour_test2345_cpu(Z80BM1::default(), TEST2_PC, 0x0000, CpuFlags::Z|CpuFlags::H|CpuFlags::P);
}

#[test]
fn flavour_test3() {
    flavour_test2345_cpu(Z80NMOS::default(), TEST3_PC, 0xFF01, CpuFlags::Z|CpuFlags::XY|CpuFlags::P|CpuFlags::C);
    flavour_test2345_cpu(Z80CMOS::default(), TEST3_PC, 0xFF01, CpuFlags::Z|CpuFlags::P|CpuFlags::C);
    flavour_test2345_cpu(Z80BM1::default(), TEST3_PC, 0xFF01, CpuFlags::Z|CpuFlags::XY|CpuFlags::P|CpuFlags::C);
}

#[test]
fn flavour_test4() {
    flavour_test2345_cpu(Z80NMOS::default(), TEST4_PC, 0xFF01, CpuFlags::Z|CpuFlags::P|CpuFlags::C);
    flavour_test2345_cpu(Z80CMOS::default(), TEST4_PC, 0xFF01, CpuFlags::Z|CpuFlags::P|CpuFlags::C);
    flavour_test2345_cpu(Z80BM1::default(), TEST4_PC, 0xFF01, CpuFlags::Z|CpuFlags::P|CpuFlags::C);
}

#[test]
fn flavour_test5() {
    flavour_test2345_cpu(Z80NMOS::default(), TEST5_PC, 0x0038, CpuFlags::Z);
    flavour_test2345_cpu(Z80CMOS::default(), TEST5_PC, 0x0038, CpuFlags::Z|CpuFlags::P);
    flavour_test2345_cpu(Z80BM1::default(), TEST5_PC, 0x0038, CpuFlags::Z|CpuFlags::P);
}

fn flavour_test2345_cpu<F: Flavour>(mut cpu: Z80<F>, start: u16, expect_memptr: u16, expect_flags: CpuFlags) {
    execute_test(&mut cpu, start);
    assert_eq!(cpu.get_memptr(), expect_memptr);
    assert_eq!(cpu.get_flags(), expect_flags);
}

fn debug(deb:CpuDebug) {
    println!("{:04x} : {:6} {:#20x} {:02X?}", deb.pc, deb.mnemonic, deb.args, deb.code.as_slice());
}

fn execute_test<C: Cpu>(cpu: &mut C, addr: u16) -> TestCtrl {
    cpu.reset();
    assert_eq!(cpu.get_acc(), !0);
    assert_eq!(cpu.get_flags(), CpuFlags::all());
    cpu.set_acc(0);
    cpu.set_flags(CpuFlags::empty());
    cpu.set_pc(addr);
    let mut ctrl = TestCtrl::new();
    let mut tsc = TsClock::default();
    while !cpu.is_halt() {
        match cpu.execute_next(&mut ctrl, &mut tsc, Some(debug)) {
            Ok(()) => if tsc.is_past_limit(100) {
                panic!("the code takes too long");
            }
            Err(BreakCause::Halt) => { break }
            Err(cause) => panic!("an unexpected break cause: {:?}", cause)
        }
        println!("FLAGS: {:?}", cpu.get_flags());
    }
    println!("FLAGS: {:?}", cpu.get_flags());
    println!("{:?}", tsc);
    ctrl
}
