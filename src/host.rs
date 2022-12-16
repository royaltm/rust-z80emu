/*
    z80emu: a minimalistic Z80 CPU emulation library.
    Copyright (C) 2019-2022  Rafal Michalski

    For the full copyright notice, see the lib.rs file.
*/
/*! This module contains traits that should be implemented by the builder of the host computer.

To complete the emulation of the Z80-based computer the following trait interfaces need to be implemented:

* [Clock] - to determine how the CPU cycles are being counted and when certain events take place.
* [Io] - to establish a communication with the I/O devices.
* [Memory] - to build a bridge between the CPU and the host's memory.

When the [Cpu][crate::cpu::Cpu] is executing instructions, it calls [Clock]'s methods to increase the cycle counter,
only hinting which cycle the currently executed instruction is at.
It is however up to the [Clock] implementation to actually increase the counter. Some of the [Clock]'s methods return
timestamps which in turn are being used when accessing peripherals or memory to mark certain actions on the time axis.

Please see [crate::host::cycles] module for the description of each emulated cycle.
!*/
#[cfg(feature = "std")] use std::error;
#[cfg(feature = "serde")] use serde::{Serialize, Deserialize};
use core::fmt;
#[allow(unused_imports)]
use core::num::{NonZeroU8, NonZeroU16, Wrapping};
use core::ops::{Add, AddAssign, Deref, DerefMut};

use super::opconsts::RST_38H_OPCODE;

/** This module defines constants indicating the number of T-states of each of the emulated [Cpu] cycle,
to help [Clock] implementations count those cycles.

Below are the diagrams showing timings for all CPU cycles. The diagrams include the information when the
[Io::is_irq] is being called, compared with the moment when the `INT` line is being sampled by the real CPU.

# M1 (opcode fetch)

M1 is indicated by a call to [Clock::add_m1]. The function should return a timestamp that is being passed
to the [Memory::read_opcode] function and should increase the internal counter by at least [M1_CYCLE_TS].
```text
       T1  T2  T3  T4
       _   _   _   _   _
      | |_| |_| |_| |_| |_|
      0               ^ M1_CYCLE_TS
A0-15 |---pc--|---ir--|
D0-7      -op-|
MREQ  --_______--___----
RD    --______----------
M1    -________---------
RFSH  ----------_______-
WAIT  ......--..........
      <===============> Memory::read_opcode
                      ^ Io::is_irq ¹
INT   ............___.. ¹
```

# Memory Read or Write.

This cycle is indicated by a call to [Clock::add_mreq]. The function should return a timestamp that is
being passed to one of: [Memory::read_mem], [Memory::read_mem16] or [Memory::write_mem] functions and should
increase the internal counter by at least [MEMRW_CYCLE_TS].
```text
       T1  T2  T3
       _   _   _   _  
      | |_| |_| |_| |_|
      0           ^ MEMRW_CYCLE_TS
A0-15 |--address--|
D0-7      ---data-|
MREQ  --_________--
RD    --_________-- (read)
WR    ------_____-- (write)
WAIT  .....--......
      <===========> Memory::read_mem/write_mem
                  ^ Io::is_irq ¹
INT   ........___.. ¹
```

# Input/Output.

This cycle is indicated by a call to [Clock::add_io]. The function should return a timestamp that is
being passed to one of: [Io::read_io] or [Io::write_io] functions and should increase the internal counter by
at least [IO_CYCLE_TS].
The aforementioned [Io] methods can in turn insert an additional number of wait states (visible here as `TW`)
into this cycle.
```text
       T1  T2  TW  T3
       _   _   _   _   _   
      | |_| |_| |_| |_| |_|
      0               ^ IO_CYCLE_TS
          ^ IO_IORQ_LOW_TS
A0-15 |-----port------|
D0-7      |----data---|
IORQ  -----__________--
RD    -----__________-- (read)
WR    -----__________-- (write)
WAIT  .........__......
      <===============> Io::read_io/write_io
                      ^ Io::is_irq ¹
INT   ............___.. ¹
```

# Interrupt Request/Acknowledge.

This cycle is indicated by a call to [Clock::add_irq]. The function should return a timestamp that is
being passed to the [Io::irq_data] function and should increase the internal counter by at least [IRQ_ACK_CYCLE_TS].
The [Io::irq_data] method can in turn insert an additional number of wait states (visible here as `TW`)
into this cycle.
```text
       T1  T2  TW  TW  T3  T4
       _   _   _   _   _   _
      | |_| |_| |_| |_| |_| |_|
      0                       ^ IRQ_ACK_CYCLE_TS
              ^ INT_IORQ_LOW_TS
A0-15 |------pc-------|---ir--|
D0-7          |--data-|
M1    -________________--------
MREQ  -------------------_____-
IORQ  -----------______--------
WAIT  .............--..........
      <=======================> Io::irq_data
```

# RETI

This is not a cycle but rather a special case for an instruction which is being used by the Z80 peripherals
to detect the end of the interrupt service routine. The currently active device in the daisy chain, with
the `IEI` line high, can deactivate its `IEO` line and let the device with lower priority take control over
the `INT` line. The diagram shows when the [Io::reti] is being called while executing the `RETI` instruction.
```text
       T1  T2  T3  T4  T1  T2  T3  T4  T1  T2  T3  T1  T2  T3
       _   _   _   _   _   _   _   _   _   _   _   _   _   _
      | |_| |_| |_| |_| |_| |_| |_| |_| |_| |_| |_| |_| |_| |_|
Clock |>>> add_m1 >>>>|>>> add_m1 >>>>|> add_mreq |> add_mreq |
A0-15 |---pc--|---ir--|--pc+1-|---ir--|-----sp----|----sp+1---|
D0-7      -ED-|       |   -4D-|           ----lo--|   ----hi--|
                                      ^ Io::reti(pc+2)        ^ Io::is_irq
INT   ....................................................___..
```
¹ The INT line is being probed with the rising edge of the final clock at the end of every instruction
when the interrupts are enabled, except after the `EI` instruction and prefixes: `0xDD`, `0xFD`.

 [Cpu]: crate::cpu::Cpu
**/
pub mod cycles {
    /// The minimum number of T-states for an `M1` cycle: opcode fetch, non-maskable interrupt and a `HALT` cycle.
    pub const M1_CYCLE_TS: u8 = 4;
    /// The minimum number of T-states for a memory read or write cycle.
    pub const MEMRW_CYCLE_TS: u8 = 3;
    /// The minimum number of T-states for an `I/O` cycle before the `IORQ` goes low
    /// and the earliest moment the value might be available on the bus.
    pub const IO_IORQ_LOW_TS: u8 = 1;
    /// The minimum number of T-states for an `I/O` cycle.
    pub const IO_CYCLE_TS: u8 = 4;
    /// The minimum number of T-states in a maskable interrupt request/acknowledge cycle before the `IORQ` goes low
    /// and the earliest moment the value might be put on the bus.
    pub const INT_IORQ_LOW_TS: u8 = 2;
    /// The number of T-states in a maskable interrupt request/acknowledge cycle.
    pub const IRQ_ACK_CYCLE_TS: u8 = 6;
}

use cycles::*;

/// This trait defines an interface to the system clock from the [Cpu] emulation perspective.
///
/// An implementation of this trait is responsible for counting T-states during various [Cpu] cycles.
///
/// It is however up to the implementation to determine how the counter is being represented and updated.
///
/// This trait can be used to emulate the [Cpu] contention by increasing the counter more than the required
/// [number of T-states][cycles].
///
/// [Cpu]: crate::cpu::Cpu
pub trait Clock {
    /// This type is being used for an arbitrary representation of the `limit` argument when executing
    /// instructions. See [Cpu::execute_with_limit](crate::cpu::Cpu::execute_with_limit) for an explanation.
    type Limit: Sized + Copy;
    /// This type is being used for timestamping the interactions between [Cpu], [Io] and [Memory]
    /// implementations.
    ///
    /// The implementations of `Clock`, [Io][Io::Timestamp] and [Memory][Memory::Timestamp] should
    /// have the same exact type assigned as their `Timestamp` associated type in order for the
    /// [Cpu] to execute instructions.
    ///
    /// Values of this type are returned by some of the methods in this trait.
    ///
    /// [Cpu]: crate::cpu::Cpu
    type Timestamp: Sized;
    /// Should return `true` if the [Clock] has reached the given `limit` otherwise should return `false`.
    fn is_past_limit(&self, limit: Self::Limit) -> bool;
    /// This method should increase the counter by at least [IRQ_ACK_CYCLE_TS] `6` T-states.
    /// The method should return the timestamp that may be passed to [Io::irq_data].
    /// It's being used at the beginning of the maskable interrupt request/acknowledge cycle.
    /// The `pc` is a value of the program counter when the interrupt was accepted.
    fn add_irq(&mut self, pc: u16) -> Self::Timestamp;
    /// This method should increase the counter by at least the value given in `add_ts`.
    /// It's being used by internal operations of the [Cpu](crate::cpu::Cpu) without any external access.
    /// The `address` given here is whatever was put on the address bus before.
    // fn add_no_mreq<const ADD_TS: u8>(&mut self, address: u16);
    fn add_no_mreq(&mut self, address: u16, add_ts: NonZeroU8);
    /// This method should increase the counter by at least [M1_CYCLE_TS] `4`
    /// and should return the timestamp that may be passed to [Memory::read_opcode].
    /// This method is also being used when the non-maskable interrupt is being accepted and while the
    /// `Cpu` is wasting cycles in the `halted` state.
    fn add_m1(&mut self, address: u16) -> Self::Timestamp;
    /// This method should increase the counter by at least the value given in [MEMRW_CYCLE_TS] `3`
    /// and should return the timestamp that may be passed to [Memory::read_mem],
    //  [Memory::read_mem16] or [Memory::write_mem].
    fn add_mreq(&mut self, address: u16) -> Self::Timestamp;
    /// This method should increase the counter by at least [IO_CYCLE_TS] `4` T-states
    /// and should return the timestamp that may be passed to [Io::read_io] or [Io::write_io].
    fn add_io(&mut self, port: u16) -> Self::Timestamp;
    /// This method should increase the counter by the value given in `wait_states`.
    /// A call to one of [Io::read_io], [Io::write_io] or [Io::irq_data] may request such additional
    /// number of wait states to be added.
    fn add_wait_states(&mut self, bus: u16, wait_states: NonZeroU16);
    /// Should return the current state of the [Clock] as a timestamp.
    fn as_timestamp(&self) -> Self::Timestamp;
}

/// This trait defines an interface for handling data provided to `IN` or from `OUT` instruction
/// families and maskable interrupts.
///
/// Please see also [cycles].
#[allow(unused_variables)]
pub trait Io {
    /// This type is being used for timestamping I/O operations. See also [Clock::Timestamp].
    type Timestamp: Sized;
    /// A value of this type is returned when a break is being requested by [Io::write_io].
    type WrIoBreak;
    /// A value of this type is returned when a break is being requested by [Io::reti].
    type RetiBreak;
    /// Called during the `IO_IORQ` cycle when executing one of the `IN` instructions.
    ///
    /// Should return a single byte from the emulated device at the given `port` and the optional
    /// number of wait states to be inserted during the `IO_IORQ` cycle.
    ///
    /// The `timestamp` given here has been previously returned from [Clock::add_io].
    ///
    /// This method is being used by the [Cpu](crate::cpu::Cpu) to read data from the I/O port.
    ///
    /// The default implementation returns `(u8::MAX, None)`.
    fn read_io(&mut self, port: u16, timestamp: Self::Timestamp) -> (u8, Option<NonZeroU16>) {
        (u8::max_value(), None)
    }
    /// Called during the `IO_IORQ` cycle when executing one of the `OUT` instructions.
    ///
    /// Should write a single `data` byte to the device at the given `port`.
    ///
    /// The `timestamp` given here has been previously returned from [Clock::add_io].
    ///
    /// Returning `Some(Self::WrIoBreak)` from this method is a request to break the execution after
    /// the currently executed instruction completes.
    /// See [Cpu::execute_with_limit](crate::cpu::Cpu::execute_with_limit).
    ///
    /// The returned tuple's second argument is an optional number of wait states to be inserted
    /// during the `IO_IORQ` cycle.
    ///
    /// This method is being used by the [Cpu](crate::cpu::Cpu) to write data to the I/O port.
    ///
    /// The default implementation returns `(None, None)`.
    fn write_io(&mut self, port: u16, data: u8, timestamp: Self::Timestamp) -> (Option<Self::WrIoBreak>, Option<NonZeroU16>) {
        (None, None)
    }
    /// Should return `true` if the interrupt request signal - the `INT` line - is active.
    ///
    /// The `timestamp` given here has been previously returned from the [Clock::as_timestamp] method.
    ///
    /// Look at the diagrams in the [cycles] module for the information when exactly, in the execution cycle,
    /// this method is being called.
    ///
    /// The default implementation returns `false`.
    fn is_irq(&mut self, timestamp: Self::Timestamp) -> bool {
        false
    }
    /// Called during the `INT_IORQ` cycle when the maskable interrupt has been requested and accepted.
    ///
    /// Depending on the interrupt mode this method should return a tuple with its first argument being:
    /// * [IM 0][crate::InterruptMode::Mode0]: an opcode of a command to execute.
    /// * [IM 1][crate::InterruptMode::Mode1]: irrelevant as it is just being ignored.
    /// * [IM 2][crate::InterruptMode::Mode2]: the lower half of an address of the vector jump table entry.
    /// The upper half of this address is being taken from the value of the register `I`.
    ///
    /// In reality, during the `INT_IORQ` cycle, a byte is being sampled from the data bus where an external
    /// device places it while requesting an interrupt.
    ///
    /// The returned tuple's second argument is an optional number of wait states to be inserted
    /// during the `INT_IORQ` cycle.
    ///
    /// The default implementation returns `(RST_38H_OPCODE, None)` equalizing mode 0 to mode 1.
    fn irq_data(&mut self, pc: u16, timestamp: Self::Timestamp) -> (u8, Option<NonZeroU16>) {
        (RST_38H_OPCODE, None)
    }
    /// When `RETI` instruction is being executed this method is being called to update the device
    /// implementation instance, so another interrupt signal can be set up if necessary.
    ///
    /// This method is being called in the middle of the instruction execution, before the returning
    /// address is popped from the machine stack.
    ///
    /// The given `address` is pointing immediately after the `RETI` instruction opcode.
    /// The given `timestamp` is taken after the `RETI` instruction opcode was read but before popping
    /// the return value from the stack. After calling this method the [Clock] counter will be
    /// further increased `2 x` by the [MEMRW][MEMRW_CYCLE_TS] cycles.
    ///
    /// Returning `Some(Self::RetiBreak)` from this method is a request to break the execution after
    /// the execution of `RETI` completes. See [Cpu::execute_with_limit](crate::cpu::Cpu::execute_with_limit).
    ///
    /// The default implementation returns `None`.
    fn reti(&mut self, address: u16, timestamp: Self::Timestamp) -> Option<Self::RetiBreak> {
        None
    }
}

/// This trait defines an interface for accessing the host's memory.
///
/// Please also see [cycles] module.
#[allow(unused_variables)]
pub trait Memory {
    /// This type is being used for timestamping memory operations. See also [Clock::Timestamp].
    type Timestamp: Sized;
    /// Should return a single byte read from the memory present at the given `address`.
    ///
    /// This method is being used by the [Cpu](crate::cpu::Cpu) to read data from memory
    /// during the `MEMRW` read cycle.
    ///
    /// The `timestamp` given here has been previously returned from [Clock::add_mreq].
    ///
    /// For reading data during `M1` cycles [Memory::read_opcode] is being used instead.
    ///
    /// The default implementation forwards call to [Memory::read_debug].
    fn read_mem(&self, address: u16, ts: Self::Timestamp) -> u8 {
        self.read_debug(address)
    }
    /// Should return the unaligned, 2 consecutive bytes from the memory present at the given
    /// `address` as a 16-bit unsigned integer in a `LE` (least significant byte first) order.
    ///
    /// This method is being used by the [Cpu](crate::cpu::Cpu) to read 16 bit values from memory
    /// during the `MEMRW` read cycle.
    ///
    /// The real CPU splits this read but we are cutting corners here slightly.
    ///
    /// The `timestamp` given here has been previously returned from [Clock::add_mreq].
    ///
    /// The default implementation composes a 16-bit value from two calls to [Memory::read_debug].
    fn read_mem16(&self, address: u16, ts: Self::Timestamp) -> u16 {
        u16::from_le_bytes([self.read_debug(address), self.read_debug(address.wrapping_add(1))])
    }
    /// Should return a single byte read from the memory present at the given `pc` address.
    ///
    /// This method is being used by the [Cpu](crate::cpu::Cpu) to read instruction opcodes
    /// from memory during the `M1` cycle and can be used e.g. for implementing ROM/instruction
    /// traps.
    ///
    /// Other, non `M1` related [Cpu](crate::cpu::Cpu) read operations, are performed
    /// via [Memory::read_mem] and [Memory::read_mem16] methods.
    ///
    /// * `pc` contains an address in memory from which the opcode should be read.
    /// * `ir` contains a memory refresh value that the real CPU would put on the bus during
    /// the memory refresh cycle.
    /// * `timestamp` has been previously returned from [Clock::add_m1].
    ///
    /// The default implementation forwards call to [Memory::read_debug].
    fn read_opcode(&mut self, pc: u16, ir: u16, ts: Self::Timestamp) -> u8 {
        self.read_debug(pc)
    }
    /// Should write a byte `value` into the memory at the given `address`.
    ///
    /// This method is being used by the [Cpu](crate::cpu::Cpu) to write data into memory
    /// during the `MEMRW` write cycle.
    ///
    /// The `timestamp` given here has been previously returned from [Clock::add_mreq].
    ///
    /// The default implementation is a no-op.
    fn write_mem(&mut self, address: u16, value: u8, ts: Self::Timestamp) {}
    /// Should return a single byte read from the memory present at the given `address`.
    ///
    /// This method is being used by the [Cpu](crate::cpu::Cpu) debugger to get a relative
    /// jump argument of a conditional instruction when a condition is not satisfied.
    /// If the debugger is not being used, this call should be optimized out by the compiler
    /// unless your implementation performs a volatile read here.
    ///
    /// The default implementation returns `u8::MAX`.
    fn read_debug(&self, address: u16) -> u8 {
        u8::max_value()
    }
}

/// An enum representing the execution break cause returned by various methods of the [Cpu](crate::cpu::Cpu)
/// trait.
#[derive(Debug)]
pub enum BreakCause<O, R> {
    /// The `HALT` instruction was executed.
    Halt,
    /// [Io::write_io] requested a break while one of the `OUT` family instructions was executed.
    WriteIo(O),
    /// [Io::reti] requested a break while `RETI` was executed.
    Reti(R)
}

/// The type returned from some of the [Cpu](crate::cpu::Cpu) trait methods.
pub type Result<O, R> = core::result::Result<(), BreakCause<O, R>>;

impl<O, R> From<BreakCause<O, R>> for &str {
    fn from(cause: BreakCause<O, R>) -> &'static str {
        (&cause).into()
    }
}

impl<O, R> From<&BreakCause<O, R>> for &str {
    fn from(cause: &BreakCause<O, R>) -> &'static str {
        match cause {
            BreakCause::Halt => "a HALT instruction was executed",
            BreakCause::WriteIo(_) => "an I/O write operation has requested a break",
            BreakCause::Reti(_) => "a break was requested at the end of an interrupt service routine",
        }
    }
}

impl<O, R> fmt::Display for BreakCause<O, R> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Into::<&str>::into(self).fmt(f)
    }
}

#[cfg(feature = "std")]
impl<O, R> error::Error for BreakCause<O, R> where O: fmt::Debug, R: fmt::Debug {
    fn description(&self) -> &str {
        self.into()
    }

    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

/// A simple T-states counter wrapping at 2^bitsize of T.
/// You may refer to it as a template for implementing [Clock] trait methods.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub struct TsCounter<T: Copy>(pub Wrapping<T>);

impl<T> Clock for TsCounter<T>
where T: Copy + PartialEq + PartialOrd + core::convert::From<u8> + core::convert::From<u16>,
      Wrapping<T>: AddAssign + Add<Output=Wrapping<T>>
{
    type Limit = T;
    type Timestamp = T;

    /// Returns `true` if `self` as `T` >= `limit`. Otherwise returns `false`.
    #[inline(always)]
    fn is_past_limit(&self, limit: Self::Limit) -> bool {
        (self.0).0 >= limit
    }
    /// Returns `self` after adding [INT_IORQ_LOW_TS] as `T`.
    /// Updates `self` by adding [IRQ_ACK_CYCLE_TS] to the previous value of `self`.
    #[inline]
    fn add_irq(&mut self, _addr: u16) -> T {
        let ts = (self.0 + Wrapping(INT_IORQ_LOW_TS.into())).0;
        self.0 += Wrapping(IRQ_ACK_CYCLE_TS.into());
        ts
    }
    /// Updates `self` by adding `add_ts` T-states to the previous value of `self`.
    #[inline(always)]
    fn add_no_mreq(&mut self, _addr: u16, add_ts: NonZeroU8) {
    // fn add_no_mreq<const ADD_TS: u8>(&mut self, _addr: u16) {
        self.0 += Wrapping(add_ts.get().into());
    }
    /// Returns `self` after adding [IO_IORQ_LOW_TS] as `T`.
    /// Updates `self` by adding [IO_CYCLE_TS] to the previous value of `self`.
    #[inline]
    fn add_io(&mut self, _port: u16) -> T {
        let ts = (self.0 + Wrapping(IO_IORQ_LOW_TS.into())).0;
        self.0 += Wrapping(IO_CYCLE_TS.into());
        ts
    }
    /// Updates `self` by adding [MEMRW_CYCLE_TS] to the previous value of `self`.
    /// Returns `self` after updating as `T`.
    #[inline(always)]
    fn add_mreq(&mut self, _addr: u16) -> T {
        self.0 += Wrapping(MEMRW_CYCLE_TS.into());
        (self.0).0
    }
    /// Updates `self` by adding [M1_CYCLE_TS] to the previous value of `self`.
    /// Returns `self` after updating as `T`.
    #[inline(always)]
    fn add_m1(&mut self, _addr: u16) -> T {
        self.0 += Wrapping(M1_CYCLE_TS.into());
        (self.0).0
    }
    /// Updates `self` by adding `wait_states` T-states to the previous value of `self`.
    #[inline]
    fn add_wait_states(&mut self, _bus: u16, wait_states: NonZeroU16) {
        self.0 += Wrapping(wait_states.get().into())
    }
    /// Returns a copy of `self` as `T`.
    #[inline(always)]
    fn as_timestamp(&self) -> T {
        (self.0).0
    }
}

impl<T: Copy> From<T> for TsCounter<T> {
    fn from(tsc: T) -> Self {
        TsCounter(Wrapping(tsc))
    }
}

impl<T: Copy> From<Wrapping<T>> for TsCounter<T> {
    fn from(tsc: Wrapping<T>) -> Self {
        TsCounter(tsc)
    }
}

impl<T: Copy> Deref for TsCounter<T> {
    type Target = Wrapping<T>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: Copy> DerefMut for TsCounter<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[cfg(test)]
mod tests {
    #[allow(unused_imports)]
    use super::*;

    #[test]
    fn default_host_impl() {
      #[derive(Default)]
      struct Ctrl;
      impl Io for Ctrl {
        type Timestamp = i32;
        type WrIoBreak = ();
        type RetiBreak = ();
      }
      impl Memory for Ctrl {
        type Timestamp = i32;
      }
      let mut ctrl = Ctrl::default();
      for val in 0..=u16::max_value() {
        assert_eq!(ctrl.read_io(val, 0), (0xFF, None));
        assert_eq!(ctrl.write_io(val, 0, 0), (None, None));
        assert_eq!(ctrl.is_irq(0), false);
        assert_eq!(ctrl.irq_data(val, 0), (RST_38H_OPCODE, None));
        assert_eq!(ctrl.reti(val, 0), None);
        assert_eq!(ctrl.read_mem(val, 0), 0xFF);
        assert_eq!(ctrl.read_mem16(val, 0), 0xFFFF);
        assert_eq!(ctrl.read_opcode(val, val, 0), 0xFF);
        assert_eq!(ctrl.read_debug(val), 0xFF);
      }
    }

    #[test]
    fn break_cause() {
      type BrkCause = BreakCause<(),()>;
      assert_eq!(BrkCause::Halt.to_string(), "a HALT instruction was executed");
      assert_eq!(BrkCause::WriteIo(()).to_string(), "an I/O write operation has requested a break");
      assert_eq!(BrkCause::Reti(()).to_string(), "a break was requested at the end of an interrupt service routine");
    }

    #[test]
    fn ts_counter() {
      let mut tc = TsCounter::<i32>::default();
      assert!(tc.is_past_limit(0));
      assert!(!tc.is_past_limit(1));
      tc.0.0 = 1;
      assert!(!tc.is_past_limit(2));
      assert!(tc.is_past_limit(1));
      assert!(tc.is_past_limit(0));
      tc.0 = Wrapping(0).into();
      assert_eq!(tc.add_irq(0), INT_IORQ_LOW_TS as i32);
      assert_eq!(tc.as_timestamp(), IRQ_ACK_CYCLE_TS as i32);
      tc = 0.into();
      tc.add_no_mreq(0, NonZeroU8::new(3).unwrap());
      assert_eq!(*tc, Wrapping(3i32));
      *tc = Wrapping(0).into();
      assert_eq!(tc.add_io(0), IO_IORQ_LOW_TS as i32);
      assert_eq!(tc.as_timestamp(), IO_CYCLE_TS as i32);
      tc = 0.into();
      assert_eq!(tc.add_mreq(0), MEMRW_CYCLE_TS as i32);
      assert_eq!(tc.as_timestamp(), MEMRW_CYCLE_TS as i32);
      tc.0 = Wrapping(0).into();
      assert_eq!(tc.add_m1(0), M1_CYCLE_TS as i32);
      assert_eq!(tc.as_timestamp(), M1_CYCLE_TS as i32);
      tc = 0.into();
      tc.add_wait_states(0, NonZeroU16::new(7).unwrap());
      assert_eq!(tc.as_timestamp(), 7i32);
    }

    #[cfg(feature = "serde")]
    #[test]
    fn tscounter_serde() {
        let tsc: TsCounter<i32> = serde_json::from_str("0").unwrap();
        assert_eq!(tsc, TsCounter::default());
        let tsc = TsCounter::from(-32);
        let sertsc = serde_json::to_string(&tsc).unwrap();
        assert_eq!(sertsc, "-32");
        let tsc_de: TsCounter<i32> = serde_json::from_str(&sertsc).unwrap();
        assert_eq!(tsc, tsc_de);
    }
}
