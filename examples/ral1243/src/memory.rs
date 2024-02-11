/*
    ral1243: Emulator program as an example implementation for the z80emu library.
    Copyright (C) 2019-2024  Rafal Michalski

    For the full copyright notice, see the mod.rs file.
*/
use core::ptr::NonNull;
use super::vec;
use super::vec::Vec;
use super::boxed::Box;
use super::bus::MemoryControl;
use super::clock::Ts;

#[allow(unused_imports)]
use log::{error, warn, info, debug, trace, Level};

const MAXRAMSIZE: usize = 0x10000 - BANK2 as usize;
const ROMSIZE: usize = 0x2000;
const BANK_MASK: u16 = 0xE000;
const BANK0: u16 = 0x0000;
const BANK1: u16 = 0x2000;
const BANK2: u16 = 0x4000;

pub type Rom = Box<[u8;ROMSIZE]>;

pub struct Memory {
    rom: Rom,
    bank1: Option<NonNull<u8>>,
    ram0: Box<[u8;ROMSIZE]>,
    ram1: Box<[u8]>,
    exroms: Vec<Rom>,
    exrom_select: u8,
    bank1_is_ram: bool
}

#[inline(always)]
unsafe fn read_unaligned16(ptr: *const u8) -> u16 {
    (ptr as *const u16).read_unaligned().to_le()
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
        Memory { rom, bank1, ram0, ram1, exroms: vec![], exrom_select: 0, bank1_is_ram: false }
    }

    pub fn make_rom(rom_in: &[u8]) -> Box<[u8;ROMSIZE]> {
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
        unsafe {
            *self.rom.as_ptr().add((addr & BANK1 - 1) as usize)
        }
    }

    #[inline(always)]
    fn read_bank1(&self, addr: u16) -> u8 {
        match self.bank1 {
            Some(bank1) => unsafe {
                *bank1.as_ptr().add((addr & BANK1 - 1) as usize)
            },
            None => u8::max_value()
        }
    }

    #[inline(always)]
    fn read_ram1(&self, addr: u16) -> u8 {
        match self.ram1.get((addr - BANK2) as usize) {
            Some(p) => *p,
            None => u8::max_value()
        }
    }

    #[inline(always)]
    fn write_ram0(&mut self, addr: u16, data: u8) {
        unsafe { *self.ram0.as_mut_ptr().add((addr & BANK1 - 1) as usize) = data }
    }

    #[inline(always)]
    fn write_ram1(&mut self, addr: u16, data: u8) {
        if let Some(p) = self.ram1.get_mut((addr - BANK2) as usize) {
            *p = data;
        }
    }

    #[inline(always)]
    fn exrom_to_bank1(&mut self) {
        if let Some(bank) = self.exroms.get_mut(self.exrom_select as usize) {
            // trace!("swapped bank; {}", self.exrom_select);
            self.bank1 = NonNull::new(bank.as_mut_ptr());
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
        let pcbank = pc & BANK_MASK;
        if pcbank == BANK0 {
            if !self.bank1_is_ram {
                self.bank1_is_ram = true;
                self.bank1 = NonNull::new(self.ram0.as_mut_ptr());
                // trace!("swapped ram bank: {:04x}", pc);
            }
            self.read_rom(pc)
        }
        else {
            if self.bank1_is_ram {
                self.bank1_is_ram = false;
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
        let addr1 = addr.wrapping_add(1);
        unsafe {
            match (addr & BANK_MASK, addr1 & BANK_MASK) {
                (BANK0, BANK0) => read_unaligned16(self.rom.as_ptr().add(addr as usize)),
                (BANK1, BANK1) => match self.bank1 {
                    Some(bank1) => read_unaligned16(bank1.as_ptr().add((addr & BANK1 - 1) as usize)),
                    None => u16::max_value()
                }
                _ if addr >= BANK2 && ((addr - BANK2 + 1) as usize) < self.ram1.len() => {
                    read_unaligned16(self.ram1.as_ptr().add((addr - BANK2) as usize))
                }
                _ => {
                    u16::from_le_bytes([self.read_debug(addr), self.read_debug(addr1)])
                }
            }
        }
    }

    #[inline(always)]
    fn write_mem(&mut self, addr: u16, data: u8, _ts: Ts) {
        match addr & BANK_MASK {
            BANK0 => {},
            BANK1 => if self.bank1_is_ram {
                self.write_ram0(addr, data)
            },
            _ => self.write_ram1(addr, data)
        }
    }

    #[inline(always)]
    fn read_debug(&self, addr: u16) -> u8 {
        match addr & BANK_MASK {
            BANK0 => self.read_rom(addr),
            BANK1 => self.read_bank1(addr),
            _ => self.read_ram1(addr)
        }
    }
}
