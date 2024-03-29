/*
    ral1243: Emulator program as an example implementation for the z80emu library.
    Copyright (C) 2019-2024  Rafal Michalski

    For the full copyright notice, see the lib.rs file.
*/
//! Debugger helpers.
/*
                                  SZYHXPNC A F  B C  D E  H L  A'F' B'C' D'E' H'L'  IX   IY   SP   PC   IR  M IF MPTR
0000 SET  3, (IX+00h), A 00000000 00000000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0 00 0000
0000>SET  3, (IX+00h), A 00 00 00 00

                         SZYHXPNC A F  B C  D E  H L  IXIY SPPC IRMP
0000 SET  3, (IX+00h), A 00000000 0000 0000 0000 0000 0000 0000 0000
0000 00 00 00 00         IF00 IM0 0000 0000 0000 0000 0000 0000 0000
0000>SET  3, (IX+00h), A 00 00 00 00

*/
use core::num::NonZeroU16;
use core::fmt;
use crate::{Ts, FrameRunner, bus::BusDevice};
use z80emu::{*, z80::Flavour, disasm::disasm_memory_once};

/// Implements [`fmt::Display`] for printing a debug header.
pub struct Header;

pub const HEADER: &str = "SZYHXPNC A F  B C  D E  H L  A'F' B'C' D'E' H'L'  IX   IY   SP   PC   IR  IFM MPTR";
pub const HEADER_ALT: &str = "SZYHXPNC A F  B C  D E  H L  IXIY SPPC IRMP";

impl fmt::Display for Header {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            write!(f, "{:25}{}", "", HEADER_ALT)
        }
        else {
            write!(f, "{:34}{}", "", HEADER)
        }
    }
}

/// Implements [`fmt::Display`] for printing a wrapped debug preview.
pub struct Preview<'a>(pub &'a CpuDebug);

impl<'a> Preview<'a> {
    pub const fn of(deb: &'a CpuDebug) -> Preview<'a> {
        Preview(deb)
    }
}

impl<'a> fmt::Display for Preview<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        format_preview(f, self.0)
    }
}

/// Implements [`fmt::Display`] for debug printing an instruction
/// and a `CPU` state.
pub struct Debugger<'a, 'b, F: Flavour> {
    pub deb: &'a CpuDebug,
    pub cpu: &'b Z80<F>
}

impl<'a, 'b, F: Flavour> Debugger<'a, 'b, F> {
    pub const fn of(deb: &'a CpuDebug, cpu: &'b Z80<F>) -> Debugger<'a, 'b, F> {
        Debugger { deb, cpu }
    }
}

impl<'a, 'b, F: Flavour> fmt::Display for Debugger<'a, 'b, F> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            format_debug_alt(f, self.deb, self.cpu)
        }
        else {
            format_debug(f, self.deb, self.cpu)
        }
    }
}

fn format_instr(f: &mut fmt::Formatter<'_>, deb: &CpuDebug) -> fmt::Result {
    write!(f, "{:04X} {:4} {:14X} ", deb.pc, deb.mnemonic, deb.args)
}

fn format_preview(f: &mut fmt::Formatter<'_>, deb: &CpuDebug) -> fmt::Result {
    write!(f, "{:04X}>{:4} {:14X} ", deb.pc, deb.mnemonic, deb.args)?;
    format_code(f, &deb.code)
}

fn format_code(f: &mut fmt::Formatter<'_>, code: &CpuDebugCode) -> fmt::Result {
    for b in code.iter() {
        write!(f, "{:02X} ", *b)?;
    }
    for _ in code.len()..4 {
        f.write_str("   ")?;
    }
    Ok(())
}

fn format_debug<F: Flavour>(f: &mut fmt::Formatter<'_>, deb: &CpuDebug, cpu: &Z80<F>) -> fmt::Result {
    format_instr(f, deb)?;
    for b in deb.code.iter() {
        write!(f, "{:02X}", *b)?;
    }
    for _ in deb.code.len()..4 {
        f.write_str("  ")?;
    }
    let (if1, if2) = cpu.get_iffs();
             // "SZYHXPNC A F  B C  D E  H L  A'F' B'C' D'E' H'L'  IX   IY   SP   PC   IR  IFM MPTR"
             // "00000000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 000 0000"
    write!(f, " {:08b} {:04X} {:04X} {:04X} {:04X} {:04X} {:04X} {:04X} {:04X} {:04X} {:04X} {:04X} {:04X} {:04X} {}{}{} {:04X}",
                cpu.get_flags().bits(),
                cpu.get_reg16(StkReg16::AF),
                cpu.get_reg16(StkReg16::BC),
                cpu.get_reg16(StkReg16::DE),
                cpu.get_reg16(StkReg16::HL),
                cpu.get_alt_reg16(StkReg16::AF),
                cpu.get_alt_reg16(StkReg16::BC),
                cpu.get_alt_reg16(StkReg16::DE),
                cpu.get_alt_reg16(StkReg16::HL),
                cpu.get_index16(Prefix::Xdd),
                cpu.get_index16(Prefix::Yfd),
                cpu.get_sp(),
                cpu.get_pc(),
                cpu.get_ir(),
                if1 as u8, if2 as u8,
                cpu.get_im().to_mode_number(),
                cpu.get_memptr())
}

fn format_debug_alt<F: Flavour>(f: &mut fmt::Formatter<'_>, deb: &CpuDebug, cpu: &Z80<F>) -> fmt::Result {
    format_instr(f, deb)?;
    let (if1, if2) = cpu.get_iffs();
                 // "SZYHXPNC A F  B C  D E  H L  IXIY SPPC IRMP"
                 // "00000000 0000 0000 0000 0000 0000 0000 0000"
                 // "IF00 IM0 0000 0000 0000 0000 0000 0000 0000"
    writeln!(f, "{:08b} {:04X} {:04X} {:04X} {:04X} {:04X} {:04X} {:04X}",
                cpu.get_flags().bits(),
                cpu.get_reg16(StkReg16::AF),
                cpu.get_reg16(StkReg16::BC),
                cpu.get_reg16(StkReg16::DE),
                cpu.get_reg16(StkReg16::HL),
                cpu.get_index16(Prefix::Xdd),
                cpu.get_sp(),
                cpu.get_ir())?;
    write!(f, "{:04X} ", deb.pc)?;
    format_code(f, &deb.code)?;
    write!(f, "        IF{}{} IM{} {:04X} {:04X} {:04X} {:04X} {:04X} {:04X} {:04X}",
                if1 as u8, if2 as u8,
                cpu.get_im().to_mode_number(),
                cpu.get_alt_reg16(StkReg16::AF),
                cpu.get_alt_reg16(StkReg16::BC),
                cpu.get_alt_reg16(StkReg16::DE),
                cpu.get_alt_reg16(StkReg16::HL),
                cpu.get_index16(Prefix::Yfd),
                cpu.get_pc(),
                cpu.get_memptr())
}

struct DBus<'a, M> {
    bus: &'a mut M,
    data: Option<u8>
}

impl<'a, M> DBus<'a, M> {
    fn new(bus: &'a mut M) -> Self {
        DBus { bus, data: None }
    }
}

impl<'a, M: Io<Timestamp = Ts>> Io for DBus<'a, M> {
    forward_host_io_types! { @ => M }
    forward_host_io_methods! { (write_io, read_io, is_irq, reti) => |m| m.bus }

    fn irq_data(&mut self, pc: u16, ts: Self::Timestamp) -> (u8, Option<NonZeroU16>) {
        let (data, delay) = self.bus.irq_data(pc, ts);
        self.data = Some(data);
        (data, delay)
    }
}

impl<'a, M: Memory> Memory for DBus<'a, M> {
    forward_host_memory_types!{ @ => M }
    forward_host_memory_methods!{ @ => |m| m.bus }
}

impl<const EXT_HZ: u32, const FRAME_HZ: u32> FrameRunner<EXT_HZ, FRAME_HZ> {
    /// Return a [`CpuDebug`] as a preview of the instruction to execute next.
    /// 
    /// The instruction shown will not necessarily be the one that will execute
    /// next in cases such as:
    /// 
    /// * An IRQ can be requested before CPU fetches the instructions.
    /// * The memory page `0x2000 - 0x3FFF` might contain the RAM page and will
    ///   change the contents to the ROM page when the CPU fetches an instruction.
    /// * A more than one of `0xFD/0xDD` prefixes in a row resides in memory at PC.
    pub fn debug_preview<C: Cpu, M>(&self, cpu: &C, bus: &M) -> CpuDebug
        where M: Memory<Timestamp=Ts>
    {
        let mut dbg = CpuDebug {
            pc: cpu.get_pc(),
            prefix: cpu.get_prefix(),
            ..Default::default()
        };
        if let Some(pfx) = dbg.prefix {
            /* prefix fetched previously */
            dbg.code.push(pfx.to_code());
            dbg.pc = dbg.pc.wrapping_sub(1);
        }
        let start: u16 = dbg.code.len().try_into().unwrap();
        for n in start..dbg.code.capacity().try_into().unwrap() {
            dbg.code.push(bus.read_debug(dbg.pc.wrapping_add(n)));
        }
        disasm_memory_once::<C>(
            dbg.pc,
            dbg.code.as_slice())
        .unwrap_or(dbg)
    }
    /// Run emulation step and return a pair of an optional [`CpuDebug`] of
    /// an executed instruction and a step duration in T-states.
    ///
    /// Returns `(None, Ts)` if [`Cpu`] was already halted.
    pub fn debug_step<C: Cpu, M>(&mut self, cpu: &mut C, bus: &mut M) -> (Option<CpuDebug>, Ts)
        where M: Memory<Timestamp=Ts> +
                 Io<Timestamp=Ts> +
                 BusDevice<Timestamp=Ts>
    {
        self.check_wrap_next_second(bus);
        let mut limit = self.limit + self.frame_tstates;
        let start_ts = self.clock.as_timestamp();
        let mut dbg: Option<CpuDebug> = None;
        let mut prefix = cpu.get_prefix();
        let pc = cpu.get_pc();
        let mut dbus = DBus::new(bus);
        loop {
            match cpu.execute_next(&mut dbus, &mut self.clock, Some(
                |deb| { dbg = Some(deb) }))
            {
                Ok(())|Err(BreakCause::Halt) => {}
                Err(cause) => panic!("no break request was expected: {}", cause)
            }

            if self.clock.is_past_limit(limit) {
                self.limit = limit;
                limit += self.frame_tstates;
                // Update bus devices once per frame
                dbus.bus.frame_end(self.clock.as_timestamp());
            }

            if dbg.is_some() || cpu.is_halt() {
                break
            }
            if !cpu.is_after_prefix() {
                panic!("invalid cpu state!");
            }
            if let Some(pfx) = prefix {
                dbg = Some(prefix_debug(pfx, pc));
                break;
            }
            /* just a single prefix, continue */
            prefix = cpu.get_prefix();
        }
        if let Some(data) = dbus.data {
            irq_debug_update(data, pc, cpu, &mut dbg);
        }
        (dbg, self.clock.as_timestamp().saturating_sub(start_ts))
    }
    /// Run emulation and stop on an IRQ request. Return a pair of an optional [`CpuDebug`]
    /// of the last instruction and a total duration in T-states.
    ///
    /// Returns `(None, Ts)` if CPU was already halted and no interrupt occured or
    /// a frame has passed.
    pub fn debug_runto_int<C: Cpu, M>(
            &mut self,
            cpu: &mut C,
            bus: &mut M,
        ) -> (Option<CpuDebug>, Ts)
        where M: Memory<Timestamp=Ts> +
              Io<Timestamp=Ts> +
              BusDevice<Timestamp=Ts>
    {
        self.check_wrap_next_second(bus);
        let limit = self.limit + self.frame_tstates;
        let start_ts = self.clock.as_timestamp();
        let mut dbg: Option<CpuDebug> = None;
        let mut pc = cpu.get_pc();
        let mut dbus = DBus::new(bus);
        loop {
            match cpu.execute_next(&mut dbus, &mut self.clock, Some(
                |deb| { dbg = Some(deb) }))
            {
                Ok(())|Err(BreakCause::Halt) => {}
                Err(cause) => panic!("no break request was expected: {}", cause)
            }
            if self.clock.is_past_limit(limit) {
                self.limit = limit;
                // Update bus devices once per frame
                dbus.bus.frame_end(self.clock.as_timestamp());
                if dbus.data.is_none() {
                    return (None, self.clock.as_timestamp().saturating_sub(start_ts));
                }
            }
            if let Some(data) = dbus.data {
                irq_debug_update(data, pc, cpu, &mut dbg);
                break
            }
            pc = cpu.get_pc();
        }
        (dbg, self.clock.as_timestamp().saturating_sub(start_ts))
    }
    /// Run emulation until a RET/RET cc/RETI/RETN is successfully executed.
    /// Return a pair of an optional [`CpuDebug`] of the last instruction and
    /// a total duration in T-states.
    ///
    /// Returns `(None, Ts)` if a frame has passed before any RET instruction.
    pub fn debug_runto_ret<C: Cpu, M>(
            &mut self,
            cpu: &mut C,
            bus: &mut M,
        ) -> (Option<CpuDebug>, Ts)
        where M: Memory<Timestamp=Ts> +
              Io<Timestamp=Ts> +
              BusDevice<Timestamp=Ts>
    {
        use opconsts::*;
        self.check_wrap_next_second(bus);
        let limit = self.limit + self.frame_tstates;
        let start_ts = self.clock.as_timestamp();
        let mut dbg: Option<CpuDebug> = None;
        let mut run = true;
        while run {
            let debug = Some(|deb: CpuDebug| {
                match deb.code[0] {
                    RET_OPCODE => {}
                    ED_PREFIX if (deb.code[1] & RETN_OP2_MASK) == RETN_OP2_BASE => {}
                    op if (op & RET_CC_OPMASK) == RET_CC_OPBASE => {}
                    _ => return
                }
                dbg = Some(deb)
            });
            match cpu.execute_next(bus, &mut self.clock, debug)
            {
                Ok(())|Err(BreakCause::Halt) => {}
                Err(cause) => panic!("no break request was expected: {}", cause)
            }
            if self.clock.is_past_limit(limit) {
                self.limit = limit;
                // Update bus devices once per frame
                bus.frame_end(self.clock.as_timestamp());
                run = false;
            }
            if let Some(deb) = dbg.as_ref() {
                if (deb.code[0] & RET_CC_OPMASK) == RET_CC_OPBASE &&
                    !Condition::from_code(deb.code[0])
                        .is_satisfied(cpu.get_flags())
                {
                    dbg = None;
                }
                else {
                    run = false;
                }
            }
        }
        (dbg, self.clock.as_timestamp().saturating_sub(start_ts))
    }
    /// Run emulation until PC equals to one of the brkpoints.
    /// Return a pair of an optional brkpoint index and a total duration in T-states.
    ///
    /// Returns `((None, Ts))` if no brkpoints were hit and a frame has passed.
    ///
    /// **NOTE**: `brkpts` MUST be sorted!
    pub fn run_until_brkpt<C: Cpu, M>(
            &mut self,
            cpu: &mut C,
            bus: &mut M,
            brkpts: &[u16]
        ) -> (Option<usize>, Ts)
        where M: Memory<Timestamp=Ts> +
              Io<Timestamp=Ts> +
              BusDevice<Timestamp=Ts>
    {
        const DEBUG: Option<CpuDebugFn> = None;
        self.check_wrap_next_second(bus);
        let limit = self.limit + self.frame_tstates;
        let start_ts = self.clock.as_timestamp();
        let mut pc = cpu.get_pc();
        loop {
            let next_bkpt = match brkpts.binary_search(&pc) {
                Ok(index) => return (Some(index),
                            self.clock.as_timestamp().saturating_sub(start_ts)),
                Err(index) => *brkpts.get(index).unwrap_or(&u16::MAX)
            };
            loop {
                match cpu.execute_next(bus, &mut self.clock, DEBUG) {
                    Ok(())|Err(BreakCause::Halt) => {}
                    Err(cause) => panic!("no break request was expected: {}", cause)
                }
                let next_pc = cpu.get_pc();
                if self.clock.is_past_limit(limit) {
                    self.limit = limit;
                    // Update bus devices once per frame
                    bus.frame_end(self.clock.as_timestamp());
                    if next_pc == next_bkpt {
                        pc = next_pc;
                        break
                    }
                    return (None,
                            self.clock.as_timestamp().saturating_sub(start_ts));
                }
                if next_pc >= next_bkpt || next_pc < pc {
                    pc = next_pc;
                    break
                }
                pc = next_pc
            }
        }
    }

    fn check_wrap_next_second<M>(&mut self, bus: &mut M)
        where M: BusDevice<Timestamp=Ts>
    {
        if self.clock.check_wrap_second() {
            let clock_hz = self.clock.clock_hz();
            self.limit -= clock_hz;
            bus.next_second(clock_hz);
        }
    }
}

/// Return a standalone prefix pseudo-mnemonic info
fn prefix_debug(pfx: Prefix, pc: u16) -> CpuDebug {
    let prefix = Some(pfx);
    let mut code = CpuDebugCode::new();
    code.push(pfx.to_code());
    CpuDebug {
        code,
        mnemonic: "NOP*",
        pc,
        prefix,
        args: CpuDebugArgs::Single(CpuDebugArg::Reg16(prefix, Reg16::HL))
    }
}

/// Update an IM-2 IRQ request pseudo-mnemonic info.
fn irq_debug_update<C: Cpu>(data: u8, pc: u16, cpu: &mut C, dbg: &mut Option<CpuDebug>) {
    if cpu.get_im() == InterruptMode::Mode2 {
        if let Some(deb) = dbg.as_mut() {
            let vector = u16::from_le_bytes([data, cpu.get_i()]);
            deb.pc = pc; /* original PC, to show where IRQ occured */
            deb.mnemonic = "IRQ#";
            deb.code.remove(0); /* remove JP opcode */
            deb.args = CpuDebugArgs::Single(CpuDebugArg::Imm16(vector));
        }
    }
}

/// Return whether a successful `CALL/CALL cc/RST` instruction was executed.
pub fn was_just_a_call<C: Cpu>(deb: &CpuDebug, cpu: &C) -> bool {
    use opconsts::*;
    match deb.code[0] {
        CALL_OPCODE => true,
        op if (op & RST_OPMASK) == RST_OPBASE => true,
        op if (op & CALL_CC_OPMASK) == CALL_CC_OPBASE => {
            Condition::from_code(op).is_satisfied(cpu.get_flags())
        }
        _ => false
    }
}

/// Return whether a successful `RET/RET cc/RETI/RETN` instruction was executed.
pub fn was_just_a_ret<C: Cpu>(deb: &CpuDebug, cpu: &C) -> bool {
    use opconsts::*;
    match deb.code[0] {
        RET_OPCODE => true,
        ED_PREFIX if (deb.code[1] & RETN_OP2_MASK) == RETN_OP2_BASE => true,
        op if (op & RET_CC_OPMASK) == RET_CC_OPBASE => {
            Condition::from_code(op).is_satisfied(cpu.get_flags())
        }
        _ => false
    }
}
