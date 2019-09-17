//! Tests Cpu debugger mnemonics.
use std::convert::TryFrom;
use arrayvec::ArrayVec;
use std::str::FromStr;
use rand::prelude::*;

use z80emu::*;
use opconsts::HALT_OPCODE;

const MNEMONICS: &str = include_str!("mnemonics.txt");
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
        (_, "offset") => {
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
            match code[0] {
                0xCB => code.insert(1, d),
                   _ => code.push(d)
            }
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
        ("BIT", bit)|("SET", bit)|("RES", bit) => match bit.chars().next() {
            Some('0'..='7') => CpuDebugArg::Bit(u32::from_str(bit).unwrap()),
            _ => panic!("expected a bit number: {:?}", arg)
        }
        _ => panic!("error parsing argument: {} {:?}", mnemonic, arg)
    }
}

fn parse(prefix: Prefix, pc: u16, line: &'static str) -> (&'static str, ParseResult) {
    let (textcode, mnemonic, textargs) = match line.split_whitespace().collect::<Vec<_>>()[..] {
        ["dd", "prefix", "IX"] => return ("dd", ParseResult::Prefix(Prefix::Xdd)),
        ["fd", "prefix", "IY"] => return ("fd", ParseResult::Prefix(Prefix::Yfd)),
        [code, mnemonic] => (code, mnemonic, None),
        [code, mnemonic, args] => (code, mnemonic, Some(args)),
        _ => panic!("error parsing line: {:?}", line)
    };

    let mut code = textcode.split(",").map(|c| u8::from_str_radix(c, 16).expect("a byte code") ).collect::<CpuDebugCode>();
    assert!(code.len() <= 2);
    let args = match textargs {
        Some(args) => {
            match args.split(",").map(|arg| parse_arg(prefix, pc, mnemonic, arg, &mut code)).collect::<ArrayVec<[_;3]>>()[..] {
                [CpuDebugArg::Bit(bit), arg, CpuDebugArg::Reg8(Prefix::None, reg)] => {
                    CpuDebugArgs::BitOpExt(bit, arg, reg)
                }
                [CpuDebugArg::Reg8(_, dst), CpuDebugArg::Reg8(_, src)] if mnemonic == "LD" => {
                    CpuDebugArgs::Double(CpuDebugArg::Reg8(prefix, dst), CpuDebugArg::Reg8(prefix, src))
                }
                [arg1, arg2] => CpuDebugArgs::Double(arg1, arg2),
                [arg] => CpuDebugArgs::Single(arg),
                ref args => panic!("nonsensical arguments: {:?}", args)
            }
        }
        None => CpuDebugArgs::None
    };

    if prefix != Prefix::None {
        code.insert(0, prefix as u8);
    }

    (textcode, ParseResult::Debug(CpuDebug {
        code,
        mnemonic,
        pc,
        prefix,
        args
    }))
}

fn test_debug<C: Cpu>(counter: &mut usize, prefix: Prefix, code: &[u8], debug: &CpuDebug, cpu: &mut C) {
    *counter += 1;
    let mut tsc = TsCounter::default();
    let mut mem = Mem(code);
    cpu.reset();
    println!("{:X}", debug);
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
    match cpu.execute_next(&mut mem, &mut tsc, Some(|debug_test| {
        assert_eq!(&debug_test, debug);
        debug_called = true;
    })) {
        Ok(()) => assert_ne!(code[pc as usize], HALT_OPCODE),
        Err(BreakCause::Halt) => assert_eq!(code[pc as usize], HALT_OPCODE),
        Err(cause) => panic!("should not break: {}", cause)
    }
    assert!(debug_called, "debug closure not called");
}

#[test]
fn test_mnemonics() {
    let mut cpu = Z80::new();
    let mut prefixed_lines = MNEMONICS_PREFIX.lines().peekable();
    let mut codes: [u8;2] = [0, 0];
    let mut counter = 0;
    for line in MNEMONICS.lines() {
        let (textcode, result) = parse(Prefix::None, 0, line);
        let code = codes[0];
        loop {
            match codes {
                [0xCB, code] => {
                    assert_eq!(textcode, format!("cb,{:02x}", code));
                    codes[1] = code.wrapping_add(1);
                    if codes[1] == 0 {
                        codes[0] += 1;
                    }
                    break;
                }
                [0xED, code] => {
                    let ed_instr_exist = textcode == format!("ed,{:02x}", code);
                    if !ed_instr_exist {
                        let mut cc = CpuDebugCode::new();
                        cc.push(0xED);
                        cc.push(code);
                        let mut debug = CpuDebug {
                            code: cc,
                            mnemonic: "NOP*",
                            pc: 0,
                            prefix: Prefix::None,
                            args: CpuDebugArgs::None
                        };
                        test_debug(&mut counter, Prefix::None, &debug.code, &debug, &mut cpu);
                        debug.pc = 1;
                        for prefix in [Prefix::Xdd, Prefix::Yfd].iter().copied() {
                            let mut code = debug.code.clone();
                            code.insert(0, prefix as u8);
                            test_debug(&mut counter, prefix, &code, &debug, &mut cpu);
                        }
                    }
                    codes[1] = code.wrapping_add(1);
                    if codes[1] == 0 {
                        codes[0] += 1;
                    }
                    if ed_instr_exist {
                        break;
                    }
                }
                [code, _] => {
                    assert_eq!(textcode, format!("{:02x}", code));
                    codes[0] = code.wrapping_add(1);
                    break;
                }
            }
        }

        match result {
            ParseResult::Prefix(pf) => {
                assert_eq!(code, pf as u8);
            }
            ParseResult::Debug(debug) => {
                test_debug(&mut counter, Prefix::None, &debug.code, &debug, &mut cpu);
                let pline = match prefixed_lines.peek() {
                    Some(line) if line.starts_with(textcode) => Some(prefixed_lines.next().unwrap()),
                    _ => None
                };
                for prefix in [Prefix::Xdd, Prefix::Yfd].iter().copied() {
                    match pline {
                        Some(line) => match parse(prefix, 0, line).1 {
                            ParseResult::Prefix(_) => panic!("prefix after prefix"),
                            ParseResult::Debug(debug) => {
                                test_debug(&mut counter, prefix, &debug.code, &debug, &mut cpu);
                            }
                        }
                        None => match parse(Prefix::None, 1, line).1 {
                            ParseResult::Debug(debug) => {
                                let mut code = ArrayVec::<[u8;5]>::new();
                                code.push(prefix as u8);
                                code.extend(debug.code.iter().copied());
                                test_debug(&mut counter, prefix, &code, &debug, &mut cpu);
                            }
                            ParseResult::Prefix(_) => unreachable!()
                        }
                    };
                }
            }
        }
    }
    assert_eq!(prefixed_lines.next(), None);
    assert_eq!(counter, (256 - 4)*3 + 256*3 + 256*3);
}
