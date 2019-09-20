//! Tests cycles of Cpu instructions.
use core::cell::RefCell;
use core::num::{NonZeroU8, NonZeroU16};
use core::str::FromStr;
use std::collections::hash_map::HashMap;
use std::rc::Rc;

use arrayvec::ArrayVec;
use rand::prelude::*;
mod and_then;
use and_then::AndThen;

use z80emu::{host::{TsCounter, cycles::*}, *};
//#################################################################################//
//################################## Z80 API ######################################//
//#################################################################################//

type Ts = u16;

#[derive(Clone,Copy,Debug)]
enum CycleExpected {
    MOne(u16),
    ReadOpcode(u16,Ts),
    Internal(NonZeroU8,u16),
    Mreq(u16),
    MemRead(u16,Ts),
    MemWrite(u16,Ts),
    Io(u16),
    PortRead(u16,Ts),
    PortWrite(u16,Ts),
}

trait TestReadMemory {
    fn test_read_memory(&self, address: u16) -> u8;
}

impl TestReadMemory for [u8] {
    fn test_read_memory(&self, address: u16) -> u8 {
        *self.get(address as usize).unwrap_or(&(address as u8))
    }
}

type ExpectedCyclesT = Rc<RefCell<Vec<CycleExpected>>>;

#[derive(Clone,Debug)]
struct TClock {
    inner: TsCounter<Ts>,
    cycles: ExpectedCyclesT
}

struct Mem<'a>{
    mem: &'a [u8],
    cycles: ExpectedCyclesT
}

impl<'a> Mem<'a> {
    pub fn new(mem: &'a[u8], cycles: Vec<CycleExpected>) -> Self {
        let cycles = Rc::new(RefCell::new(cycles));
        Mem { mem, cycles }
    }
}

impl Clock for TClock {
    type Limit = Ts;
    type Timestamp = Ts;

    fn is_past_limit(&self, limit: Self::Limit) -> bool {
        self.inner.is_past_limit(limit)
    }

    fn add_irq(&mut self, _pc: u16) -> Self::Timestamp {
        panic!("no irq expected");
    }

    fn add_no_mreq(&mut self, address: u16, add_ts: NonZeroU8) {
        match self.cycles.borrow_mut().pop().expect("unexpected internal cycle") {
            CycleExpected::Internal(exp_add_ts, exp_addr) => {
                assert_eq!(exp_add_ts, add_ts, "internal cycles");
                assert_eq!(exp_addr, address, "internal cycles bus");
            },
            cycle => panic!("unexpected internal cycle, expected {:?}", cycle)
        }
        self.inner.add_no_mreq(address, add_ts);
    }

    fn add_m1(&mut self, address: u16) -> Self::Timestamp {
        match self.cycles.borrow_mut().pop().expect("unexpected M1 cycle") {
            CycleExpected::MOne(exp_addr) => assert_eq!(exp_addr, address, "M1 cycle bus"),
            cycle => panic!("unexpected M1 cycle, expected {:?}", cycle)
        }
        self.inner.add_m1(address)
    }

    fn add_mreq(&mut self, address: u16) -> Self::Timestamp {
        match self.cycles.borrow_mut().pop().expect("unexpected MREQ cycle") {
            CycleExpected::Mreq(exp_addr) => assert_eq!(exp_addr, address, "MREQ cycle bus"),
            cycle => panic!("unexpected MREQ cycle, expected {:?}", cycle)
        }
        self.inner.add_mreq(address)
    }

    fn add_io(&mut self, port: u16) -> Self::Timestamp {
        match self.cycles.borrow_mut().pop().expect("unexpected I/O cycle") {
            CycleExpected::Io(exp_port) => assert_eq!(exp_port, port, "I/O cycle bus"),
            cycle => panic!("unexpected I/O cycle, expected {:?}", cycle)
        }
        self.inner.add_io(port)
    }

    fn add_wait_states(&mut self, _bus: u16, _wait_states: NonZeroU16) {
        panic!("no wait states expected");
    }

    fn as_timestamp(&self) -> Self::Timestamp {
        self.inner.as_timestamp()
    }
}

impl<'a> Io for Mem<'a> {
    type Timestamp = Ts;
    type WrIoBreak = ();
    type RetiBreak = ();

    fn read_io(&mut self, port: u16, ts: Self::Timestamp) -> (u8, Option<NonZeroU16>) {
        match self.cycles.borrow_mut().pop().expect("unexpected I/O read cycle") {
            CycleExpected::PortRead(exp_port, exp_ts) => {
                assert_eq!(exp_port, port, "I/O read port");
                assert_eq!(exp_ts, ts, "I/O read timestamp");
            }
            cycle => panic!("unexpected I/O read cycle, expected {:?}", cycle)
        }
        (u8::max_value(), None)
    }

    fn write_io(&mut self, port: u16, _data: u8, ts: Self::Timestamp) -> (Option<Self::WrIoBreak>, Option<NonZeroU16>) {
        match self.cycles.borrow_mut().pop().expect("unexpected I/O write cycle") {
            CycleExpected::PortWrite(exp_port, exp_ts) => {
                assert_eq!(exp_port, port, "I/O write port");
                assert_eq!(exp_ts, ts, "I/O write timestamp");
            }
            cycle => panic!("unexpected I/O write cycle, expected {:?}", cycle)
        }
        (None, None)
    }
}

impl<'a> Memory for Mem<'a> {
    type Timestamp = Ts;

    fn read_mem(&self, address: u16, ts: Self::Timestamp) -> u8 {
        match self.cycles.borrow_mut().pop().expect("unexpected memory read cycle") {
            CycleExpected::MemRead(exp_addr, exp_ts) => {
                assert_eq!(exp_addr, address, "memory read address");
                assert_eq!(exp_ts, ts, "memory read timestamp");
            }
            cycle => panic!("unexpected memory read cycle, expected {:?}", cycle)
        }
        self.read_debug(address)
    }

    fn read_mem16(&self, address: u16, ts: Self::Timestamp) -> u16 {
        match self.cycles.borrow_mut().pop().expect("unexpected memory read cycle") {
            CycleExpected::MemRead(exp_addr, exp_ts) => {
                assert_eq!(exp_addr, address, "memory read address");
                assert_eq!(exp_ts, ts, "memory read timestamp");
            }
            cycle => panic!("unexpected memory read cycle, expected {:?}", cycle)
        }
        let exp_mreq_cycle = self.cycles.borrow_mut().pop().expect("unexpected MREQ cycle");
        match self.cycles.borrow_mut().pop().expect("unexpected memory read cycle") {
            CycleExpected::MemRead(exp_addr, exp_ts) => {
                assert_eq!(exp_addr, address.wrapping_add(1), "memory read address");
                assert_eq!(exp_ts, ts + Ts::from(MEMRW_CYCLE_TS), "memory read timestamp");
            }
            cycle => panic!("unexpected memory read cycle, expected {:?}", cycle)
        }
        self.cycles.borrow_mut().push(exp_mreq_cycle);
        u16::from_le_bytes([self.read_debug(address), self.read_debug(address.wrapping_add(1))])
    }

    fn read_opcode(&mut self, pc: u16, _ir: u16, ts: Self::Timestamp) -> u8 {
        match self.cycles.borrow_mut().pop().expect("unexpected op-code read cycle") {
            CycleExpected::ReadOpcode(exp_pc, exp_ts) => {
                assert_eq!(exp_pc, pc, "op-code read address");
                assert_eq!(exp_ts, ts, "op-code timestamp");
            }
            cycle => panic!("unexpected op-code read cycle, expected {:?}", cycle)
        }
        *self.mem.get(pc as usize).expect("op-code read from a random address")
    }

    fn write_mem(&mut self, address: u16, _value: u8, ts: Self::Timestamp) {
        match self.cycles.borrow_mut().pop().expect("unexpected memory write cycle") {
            CycleExpected::MemWrite(exp_addr, exp_ts) => {
                assert_eq!(exp_addr, address, "memory write address");
                assert_eq!(exp_ts, ts, "memory write timestamp");
            }
            cycle => panic!("unexpected memory write cycle, expected {:?}", cycle)
        }
    }

    fn read_debug(&self, address: u16) -> u8 {
        self.mem.test_read_memory(address)
    }
}

//#################################################################################//
//################################# P A R S E #####################################//
//#################################################################################//

#[derive(Clone, Debug)]
struct InstrCycles {
    cycles: Rc<[Cycle]>,
    immediate_values: usize,
    has_offset: bool,
    condition: Option<Condition>,
    prereq: Option<Prerequisite>,
    visited: bool
}

struct PrereqCycles(Option<Prerequisite>, Rc<[Cycle]>);

#[derive(Clone, Copy, Debug)]
enum Prerequisite {
    Flags,
    BEq1,
    BcEq1,
    BcEq1OrAEqMemHl,
}

#[derive(Clone, Copy, Debug)]
enum Cycle {
    MOne(u16),
    Internal(NonZeroU8, BusVal),
    Mreq(BusVal),
    MRead(BusVal),
    MreqRead(BusVal),
    MreqWrite(BusVal),
    IoRead(BusVal),
    IoWrite(BusVal),
    Split
}

#[derive(Clone, Copy, Debug)]
enum BusVal {
    Pc(i16),
    Ir(i16),
    Bc(i16),
    De(i16),
    Hl(i16),
    Sp(i16),
    Nn(i16),
    An(i16),
    Index
}

impl FromStr for BusVal {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "ii+d" {
            return Ok(BusVal::Index);
        }
        let (reg, offset) = match s.split_at(2) {
            (reg, "") => (reg, 0),
            (reg, off) => (reg,
                match off.chars().next() {
                    Some('+')|Some('-') => i16::from_str(off).map_err(|e|
                        format!("offset error: {}, while parsing: {:?}", e, s)),
                    _ => Err(format!("unrecognized offset while parsing: {:?}", s)),
                }?
            ),
        };
        Ok(match reg {
            "pc" => BusVal::Pc(offset),
            "ir" => BusVal::Ir(offset),
            "bc" => BusVal::Bc(offset),
            "de" => BusVal::De(offset),
            "hl" => BusVal::Hl(offset),
            "sp" => BusVal::Sp(offset),
            "nn" => BusVal::Nn(offset),
            "an" => BusVal::An(offset),
            _ => Err(format!("unrecognized bus value: {}, while parsing {:?}", reg, s))?
        })
    }
}

impl FromStr for Prerequisite {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        return Ok(match s {
            "<F!~cc>" => Prerequisite::Flags,
            "<B=1>" => Prerequisite::BEq1,
            "<BC=1>" => Prerequisite::BcEq1,
            "<BC=1||A=(HL)>" => Prerequisite::BcEq1OrAEqMemHl,
            _ => Err(format!("error parsing prerequisite: {:?}", s))?
        })
    }
}

impl FromStr for Cycle {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "|" {
            return Ok(Cycle::Split)
        }
        let ary = s.splitn(2, ":").collect::<ArrayVec<[_;2]>>().into_inner()
                .map_err(|_| format!("expected cycle type and bus value: {:?}", s))?;
        Ok(match ary {
            ["m1", bus] => Cycle::MOne(u16::from_str(bus).map_err(|e| e.to_string())?),
            ["mq", bus] => Cycle::Mreq(BusVal::from_str(bus)?),
            ["qr", bus] => Cycle::MRead(BusVal::from_str(bus)?),
            ["mr", bus] => Cycle::MreqRead(BusVal::from_str(bus)?),
            ["mw", bus] => Cycle::MreqWrite(BusVal::from_str(bus)?),
            ["pr", bus] => Cycle::IoRead(BusVal::from_str(bus)?),
            ["pw", bus] => Cycle::IoWrite(BusVal::from_str(bus)?),
            [ xx , bus] => match xx {
                "x1"|"x2"|"x4"|"x5"|"x7" => {
                    let ts = NonZeroU8::new(u8::from_str(&xx[1..]).unwrap()).unwrap();
                    Cycle::Internal(ts, BusVal::from_str(bus)?)
                }
                _ => Err(format!("error parsing cycle: {:?}", s))?
            }
        })
    }
}

impl FromStr for PrereqCycles {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut splitargs = s.split(",");
        let prereq = s.starts_with("<")
                    .and_then(|| splitargs.next())
                    .map(Prerequisite::from_str)
                    .transpose()?;
        splitargs.map(Cycle::from_str).collect::<Result<Vec<_>,_>>()
                 .map(Rc::from)
                 .map(|cycles| PrereqCycles(prereq, cycles))
    }
}

fn expand_arg(arg: &str) -> Option<&'static str> {
    match arg {
        "b"   => Some("0 1 2 3 4 5 6 7"),
        "r"   => Some("B C D E H L A"),
        "q"   => Some("B C D E I?H I?L A"),
        "q!"  => Some("B C D E A"),
        "m"   => Some("0 1 2"),
        "rr"  => Some("BC DE HL SP"),
        "qq"  => Some("BC DE I? SP"),
        "ss"  => Some("BC DE HL AF"),
        "cc"  => Some("NZ Z NC C PO PE P M"),
        "cc!" => Some("NZ Z NC C"),
        "pp"  => Some("00 08 10 18 20 28 30 38"),
        _ => None
    }
}

fn parse_args<'a, I, F>(mut output: String, sep: &str, mut iter: I, consume: &mut F)
where I: Iterator<Item=&'a str> + Clone, F: FnMut(String)
{
    match iter.next() {
        Some(arg) => {
            output.push_str(sep);
            if let Some(expanded) = expand_arg(arg) {
                for sym in expanded.split_ascii_whitespace() {
                    let mut output = output.clone();
                    output.push_str(sym);
                    parse_args(output, ",", iter.clone(), consume);
                }
            }
            else {
                output.push_str(arg);
                parse_args(output, ",", iter, consume);
            }
        }
        None => consume(output)
    }
}

fn parse_instruction_and_expand<F: FnMut(String)>(line: &str, mut consume: F) -> bool {
    let (mnemonic, textargs, asterisk) = match line.split_ascii_whitespace().collect::<ArrayVec<[_;3]>>()[..] {
        []                    => return false,
        [mnemonic]            => (mnemonic, "", ""),
        [mnemonic, args]      => (mnemonic, args, ""),
        [mnemonic, args, "*"] => (mnemonic, args, " *"),
        [       _,    _,   _] => panic!("error parsing instruction: {:?}", line),
                            _ => unreachable!()
    };
    parse_args(mnemonic.to_string(), " ", textargs.split_terminator(","), &mut |mut key: String| {
        key.push_str(asterisk);
        consume(key)
    });
    true
}

fn find_condition(instruction: &str) -> Option<Condition> {
    instruction.split_ascii_whitespace().nth(1)
    .and_then(|arg| arg.splitn(2,',').next())
    .and_then(|c| Condition::from_str(c).ok())
}

fn build_cycle_hash() -> Result<HashMap<String, InstrCycles>, String> {
    let mut hmap = HashMap::new();
    let mut lines = CYCLES.lines();
    let visited = false;
    while let Some(line) = lines.next() {
        if line.starts_with("#") {
            continue;
        }
        let PrereqCycles(prereq, cycles) = FromStr::from_str(line)?;
        loop {
            if let Some(line) = lines.next() {
                let immediate_values = line.matches("nn").count();
                let has_offset = line.contains("(I?+dd)");
                let has_condition = line.contains("cc");
                if parse_instruction_and_expand(line, |key| {
                    println!("  {:?}\t{:?}", key, line);
                    let condition = has_condition.and_then(|| find_condition(&key));
                    let cycles = Rc::clone(&cycles);
                    let value = InstrCycles { cycles, immediate_values, has_offset, condition, prereq, visited };
                    assert!(hmap.insert(key, value).is_none(), "key duplicate in {:?}", line)
                }) {
                    continue;
                }
            };
            break;
        }
    }
    Ok(hmap)
}

//#################################################################################//
//################################## T E S T ######################################//
//#################################################################################//

#[derive(Clone, Debug)]
struct TestEnv<C: Cpu> {
    expected: Vec<CycleExpected>,
    cpu: C,
    limit: Ts
}

fn randomize_cpu_state<C: Cpu>(cpu: &mut C) {
    for _ in 0..2 {
        cpu.set_af(random());
        cpu.set_reg16(Reg16::BC, random());
        cpu.set_reg16(Reg16::DE, random());
        cpu.set_reg16(Reg16::HL, random());
        cpu.set_reg16(Reg16::SP, random());
        cpu.ex_af_af();
        cpu.exx();
    }
    cpu.set_index16(Prefix::Xdd, random());
    cpu.set_index16(Prefix::Yfd, random());
    cpu.set_i(random());
    cpu.set_r(random());
}

impl BusVal {
    fn prepare_value<C: Cpu>(&self, cpu: &C, code: &CpuDebugCode) -> u16 {
        match *self {
            BusVal::Pc(off) => cpu.get_pc().wrapping_add(off as u16),
            BusVal::Ir(off) => {
                let mask = i8::max_value() as u16;
                let ir = cpu.get_ir();
                ir & !mask | ir.wrapping_add(off as u16) & mask
            }
            BusVal::Bc(off) => cpu.get_reg16(Reg16::BC).wrapping_add(off as u16),
            BusVal::De(off) => cpu.get_reg16(Reg16::DE).wrapping_add(off as u16),
            BusVal::Hl(off) => cpu.get_reg16(Reg16::HL).wrapping_add(off as u16),
            BusVal::Sp(off) => cpu.get_sp().wrapping_add(off as u16),
            BusVal::Nn(off) => {
                let nn = [code[code.len()-2], code[code.len()-1]];
                u16::from_le_bytes(nn).wrapping_add(off as u16)
            }
            BusVal::An(off) => {
                let n = code[code.len()-1];
                (cpu.get_af()&0xFF00|n as u16).wrapping_add(off as u16)
            }
            BusVal::Index => {
                let prefix = Prefix::from(code[0]);
                let d = code[2];
                cpu.get_index16(prefix).wrapping_add(d as i8 as i16 as u16)
            }
        }
    }
}

impl<C: Cpu> TestEnv<C> {
    fn prepare_cycle_pass(&mut self, code: &CpuDebugCode, cycle: &Cycle) -> bool {
        match *cycle {
            Cycle::MOne(bus) => {
                self.limit += Ts::from(M1_CYCLE_TS);
                self.expected.push(CycleExpected::MOne(bus));
                self.expected.push(CycleExpected::ReadOpcode(bus, self.limit));
            }
            Cycle::Internal(add_ts, bus) => {
                self.limit += Ts::from(add_ts.get());
                let value = bus.prepare_value(&self.cpu, code);
                self.expected.push(CycleExpected::Internal(add_ts, value));
            }
            Cycle::Mreq(bus) => {
                self.limit += Ts::from(MEMRW_CYCLE_TS);
                let value = bus.prepare_value(&self.cpu, code);
                self.expected.push(CycleExpected::Mreq(value));
            }
            Cycle::MRead(bus) => {
                let value = bus.prepare_value(&self.cpu, code);
                self.expected.push(CycleExpected::MemRead(value, self.limit));
            }
            Cycle::MreqRead(bus) => {
                self.limit += Ts::from(MEMRW_CYCLE_TS);
                let value = bus.prepare_value(&self.cpu, code);
                self.expected.push(CycleExpected::Mreq(value));
                self.expected.push(CycleExpected::MemRead(value, self.limit));
            }
            Cycle::MreqWrite(bus) => {
                self.limit += Ts::from(MEMRW_CYCLE_TS);
                let value = bus.prepare_value(&self.cpu, code);
                self.expected.push(CycleExpected::Mreq(value));
                self.expected.push(CycleExpected::MemWrite(value, self.limit));
            }
            Cycle::IoRead(bus) => {
                let value = bus.prepare_value(&self.cpu, code);
                self.expected.push(CycleExpected::Io(value));
                self.expected.push(CycleExpected::PortRead(value, self.limit + Ts::from(IO_IORQ_LOW_TS)));
                self.limit += Ts::from(IO_CYCLE_TS);
            }
            Cycle::IoWrite(bus) => {
                let value = bus.prepare_value(&self.cpu, code);
                self.expected.push(CycleExpected::Io(value));
                self.expected.push(CycleExpected::PortWrite(value, self.limit + Ts::from(IO_IORQ_LOW_TS)));
                self.limit += Ts::from(IO_CYCLE_TS);
            }
            Cycle::Split => return true
        }
        false
    }
}

fn prepare_test_environment<C: Cpu>(code: &mut CpuDebugCode, instruction: &str, ic: &InstrCycles)
   -> Result<(TestEnv<C>, Option<TestEnv<C>>), String>
{
    let limit = Ts::default();
    let expected = Vec::new();
    let mut cpu = C::default();
    cpu.reset();
    randomize_cpu_state(&mut cpu);

    if ic.has_offset {
        code.insert(2, random());
    }
    for _ in 0..ic.immediate_values {
        code.push(random());
    }

    let mut env = TestEnv { expected, cpu, limit };
    let mut tail: Option<TestEnv<C>> = ic.prereq.map(|prereq| -> Result<_,String> {
        let mut env_tail = env.clone();
        match prereq {
            Prerequisite::Flags => {
                let cond = ic.condition.ok_or_else(|| format!("no condition in: {:?}", instruction))?;
                let mut flags = env.cpu.get_flags();
                flags.toggle(CpuFlags::all());
                if cond.is_satisfied(flags) {
                    env.cpu.set_flags(flags);
                }
                else {
                    env_tail.cpu.set_flags(flags);
                }
            }
            Prerequisite::BEq1 => {
                let mut b = env.cpu.get_reg(Reg8::B, Prefix::None);
                if b == 1 {
                    while b == 1 { b = random(); }
                    env.cpu.set_reg(Reg8::B, Prefix::None, b);
                }
                else {
                    env_tail.cpu.set_reg(Reg8::B, Prefix::None, 1);
                }
            }
            Prerequisite::BcEq1 => {
                let mut bc = env.cpu.get_reg16(Reg16::BC);
                if bc == 1 {
                    while bc == 1 { bc = random(); }
                    env.cpu.set_reg16(Reg16::BC, bc);
                }
                else {
                    env_tail.cpu.set_reg16(Reg16::BC, 1);
                }
            }
            Prerequisite::BcEq1OrAEqMemHl => {
                let mut a = env.cpu.get_reg(Reg8::A, Prefix::None);
                let mut bc = env.cpu.get_reg16(Reg16::BC);
                let hl = env.cpu.get_reg16(Reg16::HL);
                let hl_mem = code.test_read_memory(hl);
                if bc == 1 || a == hl_mem {
                    while bc == 1 { bc = random(); }
                    if a == hl_mem { a = a.wrapping_add(1); }
                    env.cpu.set_reg16(Reg16::BC, bc);
                    env.cpu.set_reg(Reg8::A, Prefix::None, a);
                }
                else {
                    env_tail.cpu.set_reg16(Reg16::BC, 1);
                    env_tail.cpu.set_reg(Reg8::A, Prefix::None, hl_mem);
                }                        
            }
        }
        Ok(env_tail)
    }).transpose()?;

    for cycle in ic.cycles.iter() {
        env.prepare_cycle_pass(code, cycle);
    }
    env.expected.reverse();

    if let Some(env) = tail.as_mut() {
        for cycle in ic.cycles.iter() {
            if env.prepare_cycle_pass(code, cycle) {
                break;
            }
        }
        env.expected.reverse();
    }

    Ok((env, tail))
}

fn test_instruction<C: Cpu>(env: TestEnv<C>, code: &CpuDebugCode, instruction: &str) {
    let TestEnv {expected, mut cpu, limit} = env;
    let mut mem = Mem::new(code, expected);
    let mut tsc = TClock { inner: TsCounter::default(), cycles: Rc::clone(&mem.cycles)};
    loop {
        match cpu.execute_next(&mut mem, &mut tsc, Some(|debug| {
            println!("{:#x}", debug);
        })) {
            Ok(()) => {
                assert_ne!(instruction, "HALT");
                if cpu.is_after_prefix() {
                    continue;
                }
            },
            Err(BreakCause::Halt) => assert_eq!(instruction, "HALT"),
            Err(cause) => panic!("should not break: {}", cause)
        }
        break;
    }
    assert_eq!(tsc.as_timestamp(), limit, "incorrect number of t-states for: {:?}", instruction);
    assert!(mem.cycles.borrow_mut().is_empty(), "not all cycles occured");
}

const CYCLES: &str           = include_str!("mnemonics_cycles.txt");
const MNEMONICS: &str        = include_str!("mnemonics.txt");
const MNEMONICS_PREFIX: &str = include_str!("mnemonics_prefix.txt");

#[test]
fn test_cycles() -> Result<(), String> {
    let mut cycles_map = build_cycle_hash()?;
    for (mnemonics, prefix) in &[(MNEMONICS, None),
                                 (MNEMONICS_PREFIX, Some(Prefix::Xdd)),
                                 (MNEMONICS_PREFIX, Some(Prefix::Yfd))] {
        for line in mnemonics.lines() {
            let [code, instruction] = line.splitn(2, " ").collect::<ArrayVec<[_;2]>>().into_inner()
                    .map_err(|_| format!("expected instruction: {:?}", line))?;
            let mut code = code.splitn(2, ",").map(|c| u8::from_str_radix(c, 16))
                            .collect::<Result<CpuDebugCode,_>>()
                            .map_err(|e| format!("error parsing code: {}, while parsing: {:?}", e, line))?;
            if let &Some(pf) = prefix {
                code.insert(0, pf as u8);
            }
            else if Prefix::from(code[0]) != Prefix::None {
                println!("skipping {:02x?} {:?}", code, instruction);
                continue;
            }
            let cycles = cycles_map.get_mut(instruction).ok_or_else(||
                            format!("unknown instruction: {:?}", instruction))?;
            cycles.visited = true;
            println!("{}", line);
            let (env, tail) = prepare_test_environment::<Z80>(&mut code, instruction, cycles)?;

            test_instruction(env, &code, instruction);
            if let Some(env) = tail {
                test_instruction(env, &code, instruction);
            }
        }
    }
    assert!(cycles_map.values().all(|v| v.visited), "not all instruction cycles has been tested");
    Ok(())
}
