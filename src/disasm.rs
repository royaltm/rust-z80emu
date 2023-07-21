/*
    z80emu: ZiLOG Z80 microprocessor emulation library.
    Copyright (C) 2019-2023  Rafal Michalski

    For the full copyright notice, see the lib.rs file.
*/
//! Utilities for disassembling Z80 machine code.
use core::fmt;
use crate::*;

type TsClock = host::TsCounter<u32>;

#[derive(Clone, Debug, Default)]
struct Mem<'a>(u16, &'a [u8]);

impl Io for Mem<'_> {
    type Timestamp = u32;
    type WrIoBreak = ();
    type RetiBreak = ();
}

impl Memory for Mem<'_> {
    type Timestamp = u32;
    fn read_debug(&self, addr: u16) -> u8 {
        *self.1.get(addr.wrapping_sub(self.0) as usize).unwrap_or(&0xff)
    }
}
/// Interprets a given `memory` chunk as machine code instructions.
///
/// * `pc` - the address of the first byte of memory chunk given.
/// * `memory` - a chunk of memory to disassemble.
/// * `debug` - a function called for each interpreted instruction with a single argument: [CpuDebug].
///
/// After each instruction the address is increased by the number of bytes occupied by this
/// instruction until the end of memory is reached.
///
/// If `debug` closure returns `Err(E)` disassembling will stop and `Err(E)` will be returned immediately.
///
/// *NOTE*: `debug` is being invoked only for multi-byte instructions which entirely fit
/// in the provided memory.
pub fn disasm_memory<C: Cpu, F: FnMut(CpuDebug) -> Result<(), E>, E>(
            pc: u16,
            memory: &[u8],
            mut debug: F
        ) -> Result<(), E>
{
    let mut cpu = C::default();
    let mut cursor: usize = 0;
    let mut dbg: Option<CpuDebug> = None;
    let mut tsc = TsClock::default();
    while cursor < memory.len() {
        let pc = pc.wrapping_add(cursor as u16);
        cpu.set_pc(pc);
        let _ = cpu.execute_next(&mut Mem(pc, &memory[cursor..]), &mut tsc,
            Some(|deb: CpuDebug| {
                cursor += deb.code.len();
                if deb.prefix.is_some() {
                    cursor -= 1;
                }
                dbg = Some(deb);
            })
        );
        if cursor > memory.len() {
            break;
        }
        else if let Some(deb) = dbg.take() {
            debug(deb)?;
        }
        else if cpu.is_after_prefix() {
            cursor += 1;
        }
        else { // reset after halt
            cpu.reset();
        }
    }
    Ok(())
}

/// Instead of providing your own `debug` function to [disasm_memory], provide a [fmt::Write]
/// implementation which will get disassembled instructions written as lines of text.
pub fn disasm_memory_write_text<C: Cpu, W: fmt::Write>(
            pc: u16,
            mem: &[u8],
            mut f: W
        ) -> fmt::Result
{
    disasm_memory::<C,_,_>(pc, mem, |deb|
        writeln!(f, "{:x}", deb)
    )
}

/// Prints disassembled instructions to stdout as lines of text.
#[cfg(feature = "std")]
pub fn disasm_memory_print_text<C: Cpu>(pc: u16, mem: &[u8]) {
    let _ = disasm_memory::<C,_,()>(pc, mem, |deb| {
        println!("{:x}", deb);
        Ok(())
    });
}

#[cfg(test)]
mod tests {
    use super::*;
    use arrayvec::ArrayString;

    #[test]
    fn disasm_works() {
        let mem = [0x09, 0xdd, 0xfd, 0xdd, 0x09, 0x76, 0, 0xcb];
        let mut s = ArrayString::<128>::new();
        disasm_memory_write_text::<Z80NMOS,_>(0xfffe, &mem, &mut s).unwrap();
        assert_eq!(&s.as_ref(), &concat!(
            "fffeh ADD  HL, BC         [09]\n",
            "0001h ADD  IX, BC         [dd, 09]\n",
            "0003h HALT                [76]\n",
            "0004h NOP                 [00]\n"));
        let mem = [0xdd, 0xcb, 0x00, 0x00, 0xfd, 0x00];
        let mut s = ArrayString::<128>::new();
        disasm_memory_write_text::<Z80NMOS,_>(0xffff, &mem, &mut s).unwrap();
        assert_eq!(&s.as_ref(), &concat!(
            "ffffh RLC  (IX+00h), B    [dd, cb, 00, 00]\n",
            "0004h NOP                 [00]\n"));
    }
}