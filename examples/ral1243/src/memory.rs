/*
    ral1243: Emulator program as an example implementation for the z80emu library.
    Copyright (C) 2019-2024  Rafal Michalski

    For the full copyright notice, see the mod.rs file.
*/
#![allow(unused_imports)]
use core::ptr::NonNull;
use super::vec;
use super::vec::Vec;
use super::boxed::Box;
use super::bus::MemoryControl;
use super::clock::Ts;

#[allow(unused_imports)]
use log::{error, warn, info, debug, trace, Level};

const MAXRAMSIZE: usize = 0x10000 - BANK2 as usize;
const BANK_SIZE: u16 = 0x2000;
const BANK_MASK: u16 = BANK_SIZE - 1;
const BANK_SELECT: u16 = !BANK_MASK;
const BANK0: u16 = 0x0000;
const BANK1: u16 = BANK0 + BANK_SIZE;
const BANK2: u16 = BANK1 + BANK_SIZE;
const ROMSIZE: usize = BANK_SIZE as usize;

pub type Rom = Box<[u8;ROMSIZE]>;

pub struct Memory {
    rom: Rom,
    bank1: Option<NonNull<[u8;ROMSIZE]>>,
    ram0: Box<[u8;ROMSIZE]>,
    ram1: Box<[u8]>,
    exroms: Vec<Rom>,
    exrom_select: u8,
    bank1_is_ram: bool
}

#[inline(always)]
unsafe fn read_unaligned16(ptr: *const u8) -> u16 {
    ptr.cast::<u16>().read_unaligned().to_le()
}

impl Memory {
    pub const ROMSIZE: usize = ROMSIZE;
    pub const MAXRAM_KB: usize = MAXRAMSIZE / 1024;

    pub fn new(rom_in: &[u8], ramsizekilos: usize) -> Self {
        let ramsize = ramsizekilos * 1024;
        let rom = Self::make_rom(rom_in);
        let ram0 = Box::new([0;ROMSIZE]);
        let bank1 = None;
        assert!(ramsize <= MAXRAMSIZE);
        let ram1 = vec![0;ramsize].into_boxed_slice();
        Memory { rom, bank1, ram0, ram1, exroms: Vec::new(), exrom_select: 0, bank1_is_ram: false }
    }

    pub fn eject_exroms(&mut self) -> Vec<Rom> {
        if !self.bank1_is_ram {
            self.bank1 = None;
        }
        core::mem::take(&mut self.exroms)
    }

    pub fn make_rom(rom_in: &[u8]) -> Rom {
        assert!(rom_in.len() <= ROMSIZE);
        let mut rom = Box::new([u8::max_value();ROMSIZE]);
        rom[0..rom_in.len()].copy_from_slice(rom_in);
        rom
    }

    pub fn free_exrom_slots(&self) -> usize {
        u8::max_value() as usize - self.exroms.len()
    }

    pub fn attach_exroms<I>(&mut self, exroms: I)
        where I: IntoIterator<Item=Rom>,
              I::IntoIter: ExactSizeIterator
    {
        let exroms = exroms.into_iter();
        let max_exroms = exroms.len().min(self.free_exrom_slots());
        for exrom in exroms.take(max_exroms) {
            self.attach_exrom(exrom);
        }
    }

    pub fn attach_exrom(&mut self, exrom: Rom) -> u8 {
        if self.free_exrom_slots() == 0 {
            panic!("No free EXROM slots left!");
        }
        self.exroms.push(exrom);
        if !self.bank1_is_ram {
            self.exrom_to_bank1();
        }
        self.exroms.len() as u8
    }

    #[inline(always)]
    fn read_rom(&self, addr: u16) -> u8 {
        self.rom[(addr & BANK_MASK) as usize]
    }

    #[inline(always)]
    fn read_bank1(&self, addr: u16) -> u8 {
        match self.bank1 {
            // Safe as long as ram0 and exroms are not moved out or replaced in Memory
            Some(bank1) => unsafe {
                bank1.as_ref()[(addr & BANK_MASK) as usize]
            }
            None => u8::max_value()
        }
    }

    #[inline(always)]
    fn read_ram1(&self, addr: u16) -> u8 {
        match self.ram1.get(addr.wrapping_sub(BANK2) as usize) {
            Some(p) => *p,
            None => u8::max_value()
        }
    }

    #[inline(always)]
    fn exrom_to_bank1(&mut self) {
        self.bank1_is_ram = false;
        if let Some(bank) = self.exroms.get_mut(self.exrom_select as usize) {
            // trace!("swapped bank; {}", self.exrom_select);
            self.bank1 = NonNull::new(bank.as_mut());
        }
        else {
            // trace!("swapped empty bank");
            self.bank1 = None;
        }
    }
}

impl MemoryControl for Memory {
    fn read_ctrl(&self) -> u8 {
        // trace!("read bank: {}", self.exrom_select);
        self.exrom_select
    }

    fn write_ctrl(&mut self, no: u8) {
        // trace!("set memory: {} prev: {}", no, self.exrom_select);
        if self.exrom_select != no {
            self.exrom_select = no;
            // trace!("bank changed: {}", no);
            if !self.bank1_is_ram {
                self.exrom_to_bank1();
            }
        }
    }

    fn memory_debug(&self, addrs: core::ops::Range<u16>) -> Vec<u8> {
        addrs.map(|addr| z80emu::Memory::read_debug(self, addr)).collect()
    }
}

impl z80emu::Memory for Memory {
    type Timestamp = Ts;

    #[inline(always)]
    fn read_opcode(&mut self, pc: u16, _ir: u16, _ts: Ts) -> u8 {
        let pcbank = pc & BANK_SELECT;
        if pcbank == BANK0 {
            if !self.bank1_is_ram {
                self.bank1_is_ram = true;
                self.bank1 = NonNull::new(self.ram0.as_mut());
                // trace!("swapped ram bank: {:04x}", pc);
            }
            self.read_rom(pc)
        }
        else {
            if self.bank1_is_ram {
                self.exrom_to_bank1();
                // trace!("swapped rom bank: {:04x}", pc);
            }
            if pcbank == BANK1 {
                self.read_bank1(pc)
            }
            else {
                self.read_ram1(pc)
            }
        }
    }

    #[inline(always)]
    fn read_mem(&self, addr: u16, _ts: Ts) -> u8 {
        self.read_debug(addr)
    }

    #[inline(always)]
    fn read_mem16(&self, addr: u16, _ts: Ts) -> u16 {
        match addr & BANK_SELECT {
            BANK0 => if addr != BANK_MASK {
                // Safe: (addr & BANK_MASK) == addr
                unsafe { read_unaligned16(self.rom.as_ptr().add(addr as usize)) }
            }
            else {
                u16::from_le_bytes([self.read_rom(BANK_MASK), self.read_bank1(BANK1)])
            }
            BANK1 => if addr != (BANK1|BANK_MASK) {
                match self.bank1 {
                    // Safe as long as ram0 and exroms are not moved out or replaced in Memory
                    Some(bank1) => unsafe {
                        read_unaligned16(bank1.as_ptr().cast::<u8>().add((addr & BANK_MASK) as usize))
                    }
                    None => u16::max_value()
                }
            }
            else {
                u16::from_le_bytes([self.read_bank1(BANK1|BANK_MASK), self.read_ram1(BANK2)])
            }
            _ => { /* addr is now >= BANK2 */
                let offs = usize::from(addr - BANK2);
                if offs + 1 < self.ram1.len() {
                    // Safe: the length is checked against the 2nd byte address.
                    unsafe { read_unaligned16(self.ram1.as_ptr().add(offs)) }
                }
                else {
                    u16::from_le_bytes([
                        self.read_ram1(addr),
                        if addr == u16::MAX {
                            self.read_rom(0)
                        }
                        else {
                            u8::max_value()
                        }])
                }
            }
        }
    }

    #[inline(always)]
    fn write_mem(&mut self, addr: u16, data: u8, _ts: Ts) {
        match addr & BANK_SELECT {
            BANK0 => {}
            BANK1 => if self.bank1_is_ram {
                self.ram0[(addr & BANK_MASK) as usize] = data
            }
            _ => if let Some(p) = self.ram1.get_mut(addr.wrapping_sub(BANK2) as usize) {
                *p = data;
            }
        }
    }

    #[inline(always)]
    fn read_debug(&self, addr: u16) -> u8 {
        match addr & BANK_SELECT {
            BANK0 => self.read_rom(addr),
            BANK1 => self.read_bank1(addr),
            _ => self.read_ram1(addr)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use z80emu::Memory as _;

    #[test]
    fn memory_read16_works() {
        let mut rom_in = Vec::new();
        for i in 0..Memory::ROMSIZE {
            rom_in.push(u8::try_from(i & 0xFF).unwrap());
        }
        let mut mem = Memory::new(rom_in.as_slice(), 48);
        /* force RAM0 into bank 1 */
        mem.read_opcode(0, 0, 0);
        for addr in BANK1..=u16::MAX {
            mem.write_mem(addr, u8::try_from(addr & 0xFF).unwrap(), 0);
        }
        for addr in 0..=u16::MAX {
            let addr1 = addr.wrapping_add(1);
            let lo = mem.read_debug(addr);
            let hi = mem.read_debug(addr1);
            let r16 = mem.read_mem16(addr, 0);
            let t16 = u16::from_le_bytes([lo, hi]);
            let x16 = u16::from_le_bytes([
                u8::try_from(addr & 0xFF).unwrap(),
                u8::try_from(addr1 & 0xFF).unwrap()
            ]);
            assert_eq!(r16, t16);
            assert_eq!(r16, x16);
        }

        let mut mem = Memory::new(rom_in.as_slice(), 0);
        /* force RAM0 into bank 1 */
        mem.read_opcode(0, 0, 0);
        for addr in BANK1..=u16::MAX {
            mem.write_mem(addr, u8::try_from(addr & 0xFF).unwrap(), 0);
        }
        for addr in 0..BANK2-1 {
            let addr1 = addr.wrapping_add(1);
            let lo = mem.read_debug(addr);
            let hi = mem.read_debug(addr1);
            let r16 = mem.read_mem16(addr, 0);
            let t16 = u16::from_le_bytes([lo, hi]);
            let x16 = u16::from_le_bytes([
                u8::try_from(addr & 0xFF).unwrap(),
                u8::try_from(addr1 & 0xFF).unwrap()
            ]);
            assert_eq!(r16, t16);
            assert_eq!(r16, x16);
        }
        assert_eq!(mem.read_debug(BANK2-1), 0xFF);
        assert_eq!(mem.read_debug(BANK2), 0xFF);
        assert_eq!(mem.read_mem16(BANK2-1, 0), 0xFFFF);
        for addr in BANK2..u16::MAX {
            let addr1 = addr.wrapping_add(1);
            let lo = mem.read_debug(addr);
            let hi = mem.read_debug(addr1);
            let r16 = mem.read_mem16(addr, 0);
            let t16 = u16::from_le_bytes([lo, hi]);
            let x16 = u16::from_le_bytes([u8::MAX, u8::MAX]);
            assert_eq!(r16, t16);
            assert_eq!(r16, x16);
        }
        assert_eq!(mem.read_debug(u16::MAX), 0xFF);
        assert_eq!(mem.read_debug(0), 0);
        assert_eq!(mem.read_mem16(u16::MAX, 0), 0x00FF);
    }
}
