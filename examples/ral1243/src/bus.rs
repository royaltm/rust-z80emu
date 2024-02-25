/*
    ral1243: Emulator program as an example implementation for the z80emu library.
    Copyright (C) 2019-2024  Rafal Michalski

    For the full copyright notice, see the lib.rs file.
*/
//! System `BUS` organization traits and structs.
#[allow(unused_imports)]
use super::vec::Vec;
use core::num::NonZeroU16;
use core::ops::Range;
use core::marker::PhantomData;
use z80emu::{Io, Memory};

/// A terminator peripheral device.
///
/// Should be inserted as a last peripheral device in a daisy chain.
pub struct Terminator<T: Copy>(PhantomData<T>);

/// Return an 8-bit mask from a given bit number.
pub const fn bit8(n: u32) -> u8 {
    1 << n
}

/// A helper trait that allows debugging the memory content.
pub trait BusMemory {
    /// Return a copy of the system memory view of the given address range.
    fn memory_debug(&self, addr: Range<u16>) -> Vec<u8>;
}

/// A trait that all emulated devices must implement.
pub trait BusDevice {
    /// The timestamp type used.
    type Timestamp: Copy;
    /// The next device type in a daisy chain.
    type NextDevice: BusDevice;
    /// This method is called on hard reset.
    fn reset(&mut self, ts: Self::Timestamp);
    /// This method is called once at the end of an emulated frame,
    //  so devices can update themselves.
    fn frame_end(&mut self, ts: Self::Timestamp);
    /// This method will be called whenever [`Memory::read_opcode`]
    /// is being called from the `CPU` emulator.
    fn m1(&mut self, ts: Self::Timestamp);
    /// Provide a mutable access to the next device in a daisy chain.
    fn next_device(&mut self) -> &mut Self::NextDevice;
    /// Implementations should decrease all stored timestamps by the provided interval.
    fn next_second(&mut self, delta: Self::Timestamp);
}

/// A trait implementing `Ral1243` I/O memory control port.
pub trait MemoryControl {
    /// Read the current memory control data.
    fn read_ctrl(&self) -> u8;
    /// Write a control data into the memory I/O port.
    fn write_ctrl(&mut self, data: u8);
    /// Return a copy of the system memory view of the given address range.
    fn memory_debug(&self, addr: Range<u16>) -> Vec<u8>;
}

/// This struct organizes the peripheral device chain of the `Ral1243`.
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
    fn frame_end(&mut self, ts: Self::Timestamp) {
        self.device.frame_end(ts)
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
    /// Create a new instance of the device chain from the given `device` and
    /// `memory` instances.
    pub fn new(device: D, memory: M) -> Self {
        Bus { port_match_mask: 0, port_match_bits: 0, device, memory }
    }

    /// Configure the mask and the address of the memory control I/O port.
    pub fn with_port_bits(mut self, port_match_mask: u16, port_match_bits: u16) -> Self {
        assert_eq!(port_match_mask & port_match_bits, port_match_bits);
        self.port_match_mask = port_match_mask;
        self.port_match_bits = port_match_bits;
        self
    }
    /// Provide a reference to the instance of memory.
    pub fn memory_ref(&self) -> &M {
        &self.memory
    }
    /// Provide a mutable reference to the instance of memory.
    pub fn memory_mut(&mut self) -> &mut M {
        &mut self.memory
    }
    /// Destruct `Bus` and return a pair of a `device` chain and a `memory`.
    pub fn into_inner(self) -> (D, M) {
        (self.device, self.memory)
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
    fn frame_end(&mut self, _ts: Self::Timestamp) {}
    #[inline(always)]
    fn m1(&mut self, _ts: T) {}
    #[inline(always)]
    fn next_device(&mut self) -> &mut Self {
        self
    }
    #[inline(always)]
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
