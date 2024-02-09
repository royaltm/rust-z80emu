/*
    ral1243: Emulator program as an example implementation for the z80emu library.
    Copyright (C) 2019-2024  Rafal Michalski

    For the full copyright notice, see the mod.rs file.
*/
use core::num::NonZeroU16;
use core::ops::Range;
use core::marker::PhantomData;
use z80emu::{Io, Memory};

pub struct Terminator<T: Copy>(PhantomData<T>);

pub const fn bit8(n: u32) -> u8 {
    1 << n
}

#[allow(dead_code)]
pub trait BusMemory {
    fn memory_debug(&self, addr: Range<u16>) -> Vec<u8>;
}

pub trait BusDevice {
    type Timestamp: Copy;
    type NextDevice: BusDevice;
    /// This is called on hard reset.
    fn reset(&mut self, ts: Self::Timestamp);
    /// This will be called whenever Memory::read_opcode is being called.
    fn m1(&mut self, ts: Self::Timestamp);
    /// Allows access to the next device in a daisy chain.
    fn next_device(&mut self) -> &mut Self::NextDevice;
    /// Decrease all stored timestamps by the provided interval.
    fn next_second(&mut self, delta: Self::Timestamp);
}

pub trait MemoryControl {
    fn read_ctrl(&self) -> u8;
    fn write_ctrl(&mut self, data: u8);
    fn memory_debug(&self, addr: Range<u16>) -> Vec<u8>;
}

pub struct Bus<D, M> {
    port_match_mask: u16,
    port_match_bits: u16,
    device: D,
    memory: M,
}

impl <T: Copy, D, M: MemoryControl> BusMemory for Bus<D, M>
where D: BusDevice<Timestamp=T>
{
    fn memory_debug(&self, addrs: Range<u16>) -> Vec<u8> {
        self.memory.memory_debug(addrs)
    }
}

impl <T: Copy, D, M> BusDevice for Bus<D, M>
where D: BusDevice<Timestamp=T>
{
    type Timestamp = T;
    type NextDevice = D;
    fn reset(&mut self, ts: T) {
        self.device.reset(ts)
    }
    fn m1(&mut self, ts: T) {
        self.device.m1(ts)
    }
    fn next_device(&mut self) -> &mut D {
        &mut self.device
    }
    fn next_second(&mut self, delta: T) {
        self.device.next_second(delta);
    }
}

impl<D, M> Bus<D, M> {
    pub fn new(device: D, memory: M) -> Self {
        Bus { port_match_mask: 0, port_match_bits: 0, device, memory }
    }

    pub fn with_port_bits(mut self, port_match_mask: u16, port_match_bits: u16) -> Self {
        assert_eq!(port_match_mask & port_match_bits, port_match_bits);
        self.port_match_mask = port_match_mask;
        self.port_match_bits = port_match_bits;
        self
    }
}

impl<T, D, M> Io for Bus<D, M>
where T: Copy,
      D: BusDevice + Io<Timestamp=T,WrIoBreak=(),RetiBreak=()>,
      M: Memory<Timestamp=T> + MemoryControl
{
    type Timestamp = T;
    type WrIoBreak = ();
    type RetiBreak = ();

    #[inline(always)]
    fn write_io(&mut self, port: u16, data: u8, ts: T) -> (Option<()>, Option<NonZeroU16>) {
        if port & self.port_match_mask == self.port_match_bits {
            self.memory.write_ctrl(data);
            (None, None)
        }
        else {
            self.device.write_io(port, data, ts)
        }
    }

    #[inline(always)]
    fn read_io(&mut self, port: u16, ts: T) -> (u8, Option<NonZeroU16>) {
        if port & self.port_match_mask == self.port_match_bits {
            (self.memory.read_ctrl(), None)
        }
        else {
            self.device.read_io(port, ts)
        }
    }

    #[inline(always)]
    fn is_irq(&mut self, ts: Self::Timestamp) -> bool {
        self.device.is_irq(ts)
    }

    #[inline(always)]
    fn irq_data(&mut self, pc: u16, ts: Self::Timestamp) -> (u8, Option<NonZeroU16>) {
        self.device.irq_data(pc, ts)
    }

    #[inline(always)]
    fn reti(&mut self, addr: u16, ts: Self::Timestamp) -> Option<()> {
        self.device.reti(addr, ts)
    }
}

impl<T, D, M> Memory for Bus<D, M>
where T: Copy,
      D: BusDevice<Timestamp=T> + Io<Timestamp=T>,
      M: Memory<Timestamp=T>
{
    type Timestamp = T;
    #[inline(always)]
    fn read_opcode(&mut self, pc: u16, ir: u16, ts: T) -> u8 {
        self.device.m1(ts);
        self.memory.read_opcode(pc, ir, ts)
    }
    #[inline(always)]
    fn read_mem(&self, addr: u16, ts: T) -> u8 {
        self.memory.read_mem(addr, ts)
    }
    #[inline(always)]
    fn read_mem16(&self, addr: u16, ts: T) -> u16 {
        self.memory.read_mem16(addr, ts)
    }
    #[inline(always)]
    fn write_mem(&mut self, addr: u16, data: u8, ts: T) {
        self.memory.write_mem(addr, data, ts);
    }
    #[inline(always)]
    fn read_debug(&self, addr: u16) -> u8 {
        self.memory.read_debug(addr)
    }
}

impl<T: Copy> BusDevice for Terminator<T> {
    type Timestamp = T;
    type NextDevice = Self;
    #[inline(always)]
    fn reset(&mut self, _ts: T) {}
    #[inline(always)]
    fn m1(&mut self, _ts: T) {}
    fn next_device(&mut self) -> &mut Self {
        self
    }
    fn next_second(&mut self, _delta: T) {}
}

impl<T: Copy> Terminator<T> {
    pub fn new() -> Self {
        Terminator(PhantomData)
    }
}

impl<T: Copy> Io for Terminator<T> {
    type Timestamp = T;
    type WrIoBreak = ();
    type RetiBreak = ();

    #[inline(always)]
    fn write_io(&mut self, _port: u16, _data: u8, _ts: T) -> (Option<()>, Option<NonZeroU16>) {
        (None, None)
    }
    #[inline(always)]
    fn read_io(&mut self, _port: u16, _ts: T) -> (u8, Option<NonZeroU16>) {
        (u8::max_value(), None)
    }
    #[inline(always)]
    fn is_irq(&mut self, _ts: Self::Timestamp) -> bool {
        false
    }
    #[inline(always)]
    fn irq_data(&mut self, _pc: u16, _ts: Self::Timestamp) -> (u8, Option<NonZeroU16>) {
        (u8::max_value(), None)
    }
    #[inline(always)]
    fn reti(&mut self, _addr: u16, _ts: Self::Timestamp) -> Option<()> {
        None
    }
}
