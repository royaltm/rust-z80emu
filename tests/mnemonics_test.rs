//! Tests Cpu debugger.
use std::convert::TryFrom;
use std::str::FromStr;

use arrayvec::ArrayVec;
use rand::prelude::*;
mod and_then;
use and_then::AndThen;

use z80emu::*;
use opconsts::HALT_OPCODE;

const MNEMONICS: &str        = include_str!("mnemonics.txt");
const MNEMONICS_PREFIX: &str = include_str!("mnemonics_prefix.txt");

type Ts = u16;

struct Mem<'a>(&'a [u8]);

impl<'a> Io for Mem<'a> {
    type Timestamp = Ts;
    type WrIoBreak = ();
    type RetiBreak = ();
}

impl<'a> Memory for Mem<'a> {
    type Timestamp = Ts;
    fn read_debug(&self, address: u16) -> u8 {
        *self.0.get(address as usize).unwrap_or(&u8::max_value())
    }
}

enum ParseResult {
    Prefix(Prefix),
    Debug(CpuDebug)
}

fn parse_arg(prefix: Prefix, pc: u16, mnemonic: &'static str, arg: &'static str, code: &mut CpuDebugCode) -> CpuDebugArg {
    match (mnemonic, arg) {
        (_, "nn")   => {
            let n = random(); code.push(n);
            CpuDebugArg::Imm8(n)
        }
        (_, "nnnn") => {
            let nn: [u8;2] = random(); code.extend(nn.iter().copied());
            CpuDebugArg::Imm16(u16::from_le_bytes(nn))
        }
        (_, "(nnnn)") => {
            let nn: [u8;2] = random(); code.extend(nn.iter().copied());
            CpuDebugArg::Addr(CpuDebugAddr::ImmAddr(u16::from_le_bytes(nn)))
        }
        (_, "(nn)") => {
            let n = random(); code.push(n);
            CpuDebugArg::Port(CpuDebugPort::ImmPort(n))
        }
        (_, "ee") => {
            let e = random(); code.push(e);
            CpuDebugArg::Imm16(pc.wrapping_add(2).wrapping_add(e as i8 as i16 as u16))
        }
        (_, "(C)") => CpuDebugArg::Port(CpuDebugPort::RegPort),
        (_, "NZ") => CpuDebugArg::Cond(Condition::NZ),
        (_, "Z" ) => CpuDebugArg::Cond(Condition::Z),
        (_, "NC") => CpuDebugArg::Cond(Condition::NC),
        ("CALL", "C")|("JP", "C")|("JR", "C")|("RET", "C") => CpuDebugArg::Cond(Condition::C),
        (_, "PO") => CpuDebugArg::Cond(Condition::PO),
        (_, "PE") => CpuDebugArg::Cond(Condition::PE),
        (_, "P" ) => CpuDebugArg::Cond(Condition::P),
        (_, "M" ) => CpuDebugArg::Cond(Condition::M),
        (_, "(BC)") => CpuDebugArg::Addr(CpuDebugAddr::RegAddr(Reg16::BC)),
        (_, "(DE)") => CpuDebugArg::Addr(CpuDebugAddr::RegAddr(Reg16::DE)),
        (_, "(HL)") => CpuDebugArg::Addr(CpuDebugAddr::RegAddr(Reg16::HL)),
        (_, "(SP)") => CpuDebugArg::Addr(CpuDebugAddr::RegAddr(Reg16::SP)),
        (_, "(I?)") => CpuDebugArg::Addr(CpuDebugAddr::IndexAddr(prefix, None)),
        (_, "(I?+dd)") => {
            let d = random();
            code.insert(2, d);
            CpuDebugArg::Addr(CpuDebugAddr::IndexAddr(prefix, Some(d as i8)))
        }
        ("RST", nn) => CpuDebugArg::Imm8(u8::from_str_radix(nn, 16).expect("a restart address")),
        ("PUSH", "BC")|("POP", "BC") => CpuDebugArg::Stk16(StkReg16::BC),
        ("PUSH", "DE")|("POP", "DE") => CpuDebugArg::Stk16(StkReg16::DE),
        ("PUSH", "HL")|("POP", "HL") => CpuDebugArg::Stk16(StkReg16::HL),
        (_, "AF")|(_, "AF'") => CpuDebugArg::Stk16(StkReg16::AF),
        (_, "BC") => CpuDebugArg::Reg16(prefix, Reg16::BC),
        (_, "DE") => CpuDebugArg::Reg16(prefix, Reg16::DE),
        (_, "HL") => CpuDebugArg::Reg16(Prefix::None, Reg16::HL),
        (_, "I?") => CpuDebugArg::Reg16(prefix, Reg16::HL),
        ("LD", "SP") => CpuDebugArg::Reg16(Prefix::None, Reg16::SP),
        (_, "SP") => CpuDebugArg::Reg16(prefix, Reg16::SP),
        (_, "I?H") => CpuDebugArg::Reg8(prefix, Reg8::H),
        (_, "I?L") => CpuDebugArg::Reg8(prefix, Reg8::L),
        (_, "A") => CpuDebugArg::Reg8(Prefix::None, Reg8::A),
        (_, "B") => CpuDebugArg::Reg8(Prefix::None, Reg8::B),
        (_, "C") => CpuDebugArg::Reg8(Prefix::None, Reg8::C),
        (_, "D") => CpuDebugArg::Reg8(Prefix::None, Reg8::D),
        (_, "E") => CpuDebugArg::Reg8(Prefix::None, Reg8::E),
        (_, "H") => CpuDebugArg::Reg8(Prefix::None, Reg8::H),
        (_, "L") => CpuDebugArg::Reg8(Prefix::None, Reg8::L),
        (_, "I") => CpuDebugArg::I,
        (_, "R") => CpuDebugArg::R,
        ("IM", mode) => CpuDebugArg::IntMode(
            InterruptMode::try_from(u8::from_str(mode).expect("a mode number")
        ).expect("an interrupt mode")),
        ("BIT", bit)|("SET", bit)|("RES", bit) if bit.len() == 1 => match bit.chars().next() {
            Some('0'..='7') => CpuDebugArg::Bit(u32::from_str(bit).unwrap()),
            _ => panic!("expected a bit number: {:?}", arg)
        }
        _ => panic!("error parsing argument: {} {:?}", mnemonic, arg)
    }
}

fn parse_mnemonic(prefix: Prefix, pc: u16, code: &CpuDebugCode, mnemonic_with_args: &'static str) -> ParseResult {
    let (mnemonic, textargs) =
    match mnemonic_with_args.split_ascii_whitespace().collect::<ArrayVec<[_;2]>>()[..] {
        ["prefix", "IX"] => return ParseResult::Prefix(Prefix::Xdd),
        ["prefix", "IY"] => return ParseResult::Prefix(Prefix::Yfd),
        [mnemonic]       => (mnemonic, None),
        [mnemonic, args] => (mnemonic, Some(args)),
                       _ => panic!("error parsing mnemonic: {:?}", mnemonic_with_args)
    };

    let mut code = code.clone();
    if prefix != Prefix::None {
        code.insert(0, prefix as u8);
    }

    let args = match textargs {
        Some(args) => {
            match args.split(",").map(|arg| parse_arg(prefix, pc, mnemonic, arg, &mut code))
                                 .collect::<ArrayVec<[_;3]>>()[..] {
                [CpuDebugArg::Bit(bit), arg, CpuDebugArg::Reg8(Prefix::None, reg)] => {
                    CpuDebugArgs::BitOpExt(bit, arg, reg)
                }
                [CpuDebugArg::Reg8(_, dst), CpuDebugArg::Reg8(_, src)] if mnemonic == "LD" => {
                    CpuDebugArgs::Double(CpuDebugArg::Reg8(prefix, dst), CpuDebugArg::Reg8(prefix, src))
                }
                [arg1, arg2] => CpuDebugArgs::Double(arg1, arg2),
                [arg]        => CpuDebugArgs::Single(arg),
                ref args     => panic!("nonsensical arguments: {:?}", args)
            }
        }
        None => CpuDebugArgs::None
    };

    ParseResult::Debug(CpuDebug {
        code,
        mnemonic,
        pc,
        prefix,
        args
    })
}

fn parse_code(textcode: &str) -> CpuDebugCode {
    textcode.splitn(2, ",")
            .map(|c| u8::from_str_radix(c, 16).expect("a byte code") )
            .collect()
}

fn test_debug<C: Cpu>(counter: &mut usize, prefix: Prefix, code: &[u8], debug_expect: &CpuDebug, cpu: &mut C) {
    *counter += 1;
    let mut tsc = host::TsCounter::default();
    let mut mem = Mem(code);
    cpu.reset();

    println!("{:X}", debug_expect);

    if prefix != Prefix::None {
        match cpu.execute_next(&mut mem, &mut tsc, Some(|_| {
            panic!("prefix must not call debug closure");
        })) {
            Ok(()) => assert_eq!(cpu.get_prefix(), prefix),
            Err(cause) => panic!("should not break: {}", cause)
        }
    }
    let mut debug_called = false;
    let pc = cpu.get_pc();
    match cpu.execute_next(&mut mem, &mut tsc, Some(|ref debug_test| {
        assert_eq!(debug_test, debug_expect);
        debug_called = true;
    })) {
        Ok(()) => assert_ne!(code[pc as usize], HALT_OPCODE),
        Err(BreakCause::Halt) => assert_eq!(code[pc as usize], HALT_OPCODE),
        Err(cause) => panic!("should not break: {}", cause)
    }
    assert!(debug_called, "debug closure not called");
}

#[derive(Debug)]
struct CodeIterator([u8;2]);

impl Iterator for CodeIterator {
    type Item = String;
    fn next(&mut self) -> Option<Self::Item> {
        const DONE: [u8; 2] = [u8::max_value(); 2];
        let s: String;
        match self.0 {
            DONE => return None,
            [c0@0xCB, c1]|
            [c0@0xED, c1] => {
                s = format!("{:02x},{:02x} ", c0, c1);
                let c1 = c1.wrapping_add(1);
                self.0[1] = c1;
                if c1 == 0 { self.0[0] += 1; }
            }
            [code, 0] => {
                s = format!("{:02x} ", code);
                let code = code.wrapping_add(1);
                if code == 0 {
                    self.0 = DONE;
                }
                else {
                    self.0[0] = code;
                }
            }
            _ => panic!("wrong iterator value: {:?}", self)
        }
        Some(s)
    }
}

#[test]
fn test_mnemonics() {
    let mut cpu = Z80::new();
    let mut lines = MNEMONICS.lines().peekable();
    let mut prefixed_lines = MNEMONICS_PREFIX.lines().peekable();
    let code_iter = CodeIterator([0, 0]);
    let mut counter = 0;

    for textcode in code_iter {
        let code = parse_code(textcode.trim_end_matches(' '));
        let mnem = match lines.peek() {
            Some(s) if s.starts_with(&textcode) => {
                lines.next().and_then(|s| s.get(textcode.len()..))
            }
            _ => (code[0] == 0xED).and_then(|| Some("NOP*"))
        }.unwrap_or_else(||
            panic!("mnemonic not found for: {}", textcode));

        match parse_mnemonic(Prefix::None, 0, &code, mnem) {
            ParseResult::Prefix(pf) => {
                counter += 1;
                assert_eq!(code[0], pf as u8);
            }
            ParseResult::Debug(debug) => {
                test_debug(&mut counter, Prefix::None, &debug.code, &debug, &mut cpu);

                let pmnem = match prefixed_lines.peek() {
                    Some(s) if s.starts_with(&textcode) => prefixed_lines.next(),
                    _ => None
                }.and_then(|s| s.get(textcode.len()..));

                for &prefix in &[Prefix::Xdd, Prefix::Yfd] {
                    match pmnem {
                        Some(mnem) => match parse_mnemonic(prefix, 0, &code, mnem) {
                            ParseResult::Debug(debug) => {
                                test_debug(&mut counter, prefix, &debug.code, &debug, &mut cpu);
                            }
                            ParseResult::Prefix(_) => panic!("prefix after prefix"),
                        }
                        None => match parse_mnemonic(Prefix::None, 1, &code, mnem) {
                            ParseResult::Debug(debug) => {
                                let mut code = ArrayVec::<[u8;5]>::new();
                                code.push(prefix as u8);
                                code.extend(debug.code.iter().cloned());
                                test_debug(&mut counter, prefix, &code, &debug, &mut cpu);
                            }
                            ParseResult::Prefix(_) => unreachable!()
                        }
                    };
                }
            }
        }
    }
    assert_eq!(lines.next(), None, "not all mnemonics matched or were out of order");
    assert_eq!(prefixed_lines.next(), None, "not all prefix mnemonics matched or were out of order");
    assert_eq!(counter, 2 + (256 - 4)*3 + 256*3 + 256*3);
}
