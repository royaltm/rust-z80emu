//! This module contains traits that should be implemented by the user.
//!
//! The [Cpu] emulation is only hinting on what kind of cycle is currently being performed by the current instruction.
//! It is up to the implementors to decide when on the emulated time axis the interaction with the peripherals or memory
//! takes place.
//!
//! Please see [cycles] module for the description of each emulated cycle.
use core::num::Wrapping;
use core::ops::{Add, AddAssign, Deref, DerefMut};

use super::opconsts::RST_38H_OPCODE;

/** These are constants indicating the number of T-states for the [Clock] to count for different [Cpu] cycles.

The diagrams below show each cycle timings. They also show for each cycle, when the [Io::is_irq] is being called
after each instruction compared with the moment when the `INT` line is being sampled by the real CPU.

# M1 (opcode fetch)

M1 is indicated by a call to [Clock::add_m1]. The function should return the timestamp for [Memory::read_opcode]
and should increase the internal counter by at least [M1_CYCLE_TS].
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

This cycle is indicated by a call to [Clock::add_mreq]. The function should return the timestamp for one of:
[Memory::read_mem], [Memory::read_mem16] or [Memory::write_mem] and should increase the internal counter by
at least [MEMRW_CYCLE_TS].
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

This cycle is indicated by a call to [Clock::add_io]. The function should return the timestamp for one of:
[Io::read_io] or [Io::write_io] and should increase the internal counter by at least [IO_CYCLE_TS].
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

This cycle is indicated by a call to [Clock::add_irq]. The function should return the timestamp for
[Io::irq_data] and should increase the internal counter by at least [IRQ_ACK_CYCLE_TS]
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

This is not a cycle but rather a special case for a whole instruction which is being used by the Z80 peripherals
to detect the end of the interrupt service routine. The currently active device in the daisy chain (with `IEI` high)
can deactivate its `IEO` and let the device with lower priority take control over the `INT` line.
The diagram shows when the [Io::reti] is being called while executing the `RETI` instruction.

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
¹ The INT line is being probed with the rising edge of the final clock at the end of every instruction,
except `EI` and `0xDD`, `0xFD` prefixes.

 [Cpu]: crate::cpu::Cpu
**/
pub mod cycles {
    /// A minimal number of T-states for an `M1` cycle: opcode fetch, non-maskable interrupt and a `HALT` cycle.
    pub const M1_CYCLE_TS: u8 = 4;
    /// A minimal number of T-states for a memory read or write cycle.
    pub const MEMRW_CYCLE_TS: u8 = 3;
    /// A minimal number of T-states for an `I/O` cycle before the `IORQ` goes low
    /// and the earlies moment the value might be available on the bus.
    pub const IO_IORQ_LOW_TS: u8 = 1;
    /// A minimal number of T-states for an `I/O` cycle.
    pub const IO_CYCLE_TS: u8 = 4;
    /// A minimal number of T-states in a maskable interrupt request/acknowledge cycle before the `IORQ` goes low
    /// and the earlies moment the value might be put on the bus.
    pub const INT_IORQ_LOW_TS: u8 = 2;
    /// A number of T-states in a maskable interrupt request/acknowledge cycle.
    pub const IRQ_ACK_CYCLE_TS: u8 = 6;
}

use cycles::*;

/// A trait responsible for counting T-states during various [Cpu] cycles.
/// The complete emulation depends on this trait to properly increase the counter.
///
/// It is however up to the implementation to determine how the counter is being represented.
/// The only limit is that the `Clock` type must implement [Copy] and be [Sized].
///
/// This trait can be used to emulate the [Cpu] contention by increasing the counter more than the required value.
///
/// [Cpu]: crate::cpu::Cpu
pub trait Clock: Sized + Copy {
    /// A type for an arbitrary representation of the `limit` when executing instructions.
    /// See [Cpu::execute_with_limit](crate::cpu::Cpu::execute_with_limit) for the explanation.
    type Limit: Sized + Copy;
    /// A type returned by some of methods in this trait that are passed later to the [Io] and [Memory] traits.
    /// The [Clock], [Io] and [Memory] traits needs this associated type to be the same.
    type Timestamp: Sized;
    /// Returns `true` if the [Clock] has reached the given `limit`.
    fn is_past_limit(&self, limit: Self::Limit) -> bool;
    /// This method should increase the counter by at least [IRQ_ACK_CYCLE_TS] `6` T-states.
    /// The method should return the timestamp that may be passed to [Io::irq_data].
    /// It's being used at the beginning of the maskable interrupt request/acknowledge cycle.
    /// The `pc` is a value of the program counter when the interrupt was accepted.
    fn add_irq(&mut self, pc: u16) -> Self::Timestamp;
    /// This method should increase the counter by at least the value given in `add_ts`.
    /// It's being used by internal operations of the [Cpu](crate::cpu::Cpu) without any external access.
    /// The address given is whatever was put on the address bus before.
    fn add_no_mreq(&mut self, address: u16, add_ts: u8);
    /// This method should increase the counter by at least [M1_CYCLE_TS] `4`.
    /// The method should return the timestamp that may be passed to [Memory::read_opcode].
    /// This method is also being used by the non-maskable interrupt and while the `Cpu` is in the `halted` state.
    fn add_m1(&mut self, address: u16) -> Self::Timestamp;
    /// This method should increase the counter by at least the value given in [MEMRW_CYCLE_TS] `3`.
    /// The method should return the timestamp that may be passed to [Memory::read_mem],
    //  [Memory::read_mem16] or [Memory::write_mem].
    fn add_mreq(&mut self, address: u16) -> Self::Timestamp;
    /// This method should increase the counter by at least [IO_CYCLE_TS] `4` T-states.
    /// The method should return the timestamp that may be passed to [Io::read_io] or [Io::write_io].
    fn add_io(&mut self, port: u16) -> Self::Timestamp;
    /// Should return a copy of self as a `Self::Timestamp`.
    fn as_timestamp(&self) -> Self::Timestamp;
}

/// This trait handles `IN`/`OUT` instruction family and maskable interrupts.
/// Please also see [cycles] module.
pub trait Io {
    /// A type used for timestamping I/O operations.
    type Timestamp: Sized;
    /// Should return the byte value from the device at the given `port`.
    ///
    /// This method is being used by the [Cpu](crate::cpu::Cpu) to read data from the I/O port.
    /// The `timestamp` given has previously been returned from [Clock::add_io].
    fn read_io(&mut self, port: u16, timestamp: Self::Timestamp) -> u8;
    /// Should write the byte `data` to the device at the given `port`.
    ///
    /// This method is being used by the [Cpu](crate::cpu::Cpu) to write data to the I/O port.
    /// The `timestamp` given has previously been returned from [Clock::add_io].
    ///
    /// Returning `true` from this method is a request to break the execution after the current
    /// instruction completes. See [Cpu::execute_with_limit](crate::cpu::Cpu::execute_with_limit).
    fn write_io(&mut self, port: u16, data: u8, timestamp: Self::Timestamp) -> bool;
    /// This method should return `true` if the interrupt request signal (`INT`) is active.
    /// The `timestamp` given has previously been returned from [Clock::as_timestamp] method.
    fn is_irq(&mut self, timestamp: Self::Timestamp) -> bool;
    /// Depending on the interrupt mode this should return the opcode of a command to execute
    /// (im 0) or a lower half of the address of the vector address jump table (im 2).
    ///
    /// In reality the value is obtained from the data bus where the external device places one byte
    /// while requesting the interrupt.
    /// The default implementation returns [RST_38H_OPCODE] equalizing mode 0 to mode 1.
    fn irq_data(&mut self, _pc: u16, _timestamp: Self::Timestamp) -> u8 { RST_38H_OPCODE }
    /// When `RETI` instruction is being executed. This method is being called to update the I/O instance,
    /// so another interrupt signal can be set up if necessary.
    ///
    /// This method is being called in the middle of the instruction execution, before the returning
    /// address is popped from the machine stack.
    /// The given address is pointing immediately after the `RETI` instruction opcode.
    /// The given timestamp is taken after the `RETI` instruction opcode was read but before popping
    /// the return value from the stack. After calling this method the [Clock] counter will be
    /// increased by at least `2 x` [MEMRW_CYCLE_TS].
    ///
    /// Returning `true` from this method is a request to break the execution after the execution of
    /// `RETI` completes. See [Cpu::execute_with_limit](crate::cpu::Cpu::execute_with_limit).
    /// The default implementation returns `false`.
    fn reti(&mut self, _address: u16, _timestamp: Self::Timestamp) -> bool { false }
}

/// An interface to the host memory. Please also see [cycles] module.
pub trait Memory {
    /// A type used for timestamping memory operations.
    type Timestamp: Sized;
    /// Should return the value of the byte from memory present at the given `address`.
    ///
    /// This method is being used by the [Cpu](crate::cpu::Cpu) to read data from memory.
    /// The `timestamp` given has previously been returned from [Clock::add_mreq].
    /// For the `M1` cycles [Memory::read_opcode] is used instead.
    fn read_mem(&self, address: u16, ts: Self::Timestamp) -> u8;
    /// Should return the unaligned 2 consecutive bytes from memory at present the given
    /// `address` as a 16-bit unsigned integer in LE order.
    ///
    /// This method is being used by the [Cpu](crate::cpu::Cpu) to read 16 bit values from memory.
    /// The real CPU splits this read but we are cutting corners here slightly.
    /// The `timestamp` given has previously been returned from [Clock::add_mreq].
    fn read_mem16(&self, address: u16, ts: Self::Timestamp) -> u16;
    /// Should return the byte value from memory present at the given `pc` address.
    ///
    /// Used by the [Cpu](crate::cpu::Cpu) during `M1` cycle for reading opcodes.
    /// Can be used for ROM or instruction traps etc.
    ///
    /// Other [Cpu](crate::cpu::Cpu) read operation are performed via [Memory::read_mem] and
    /// [Memory::read_mem16] methods.
    ///
    /// The `timestamp` given has previously been returned from [Clock::add_m1].
    /// `pc` contains an address in the memory from which the opcode should be read.
    /// `ir` contains a memory refresh value that the real CPU would put on the bus during memory refresh cycles.
    fn read_opcode(&mut self, pc: u16, ir: u16, ts: Self::Timestamp) -> u8;
    /// Should store a byte `value` at the given `address` in memory.
    ///
    /// This is used by the [Cpu](crate::cpu::Cpu) for writing to memory.
    /// The `timestamp` given has previously been returned from [Clock::add_mreq].
    fn write_mem(&mut self, address: u16, value: u8, ts: Self::Timestamp);
    /// Should return the value of the byte from memory present at the given `address`.
    ///
    /// Used by the [Cpu](crate::cpu::Cpu) debugger to get a conditional command argument.
    fn read_debug(&self, address: u16) -> u8;
}

/// A simple T-states counter wrapping at 2^bitsize of T.
/// Please refer to it as a template for implementing Clock trait methods.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub struct TsCounter<T: Copy>(pub Wrapping<T>);

impl<T> Clock for TsCounter<T>
where T: Copy + PartialEq + PartialOrd + core::convert::From<u8>,
      Wrapping<T>: AddAssign + Add<Output=Wrapping<T>>
{
    type Limit = T;
    type Timestamp = T;

    /// Returns true if self >= limit.
    #[inline]
    fn is_past_limit(&self, limit: Self::Limit) -> bool {
        (self.0).0 >= limit
    }
    /// Returns self (before addition of `IRQ_ACK_CYCLE_TS`) + [INT_IORQ_LOW_TS] as `T`.
    /// Adds [IRQ_ACK_CYCLE_TS] T-states to self.
    #[inline]
    fn add_irq(&mut self, _addr: u16) -> T {
        let ts = (self.0 + Wrapping(INT_IORQ_LOW_TS.into())).0;
        self.0 += Wrapping(IRQ_ACK_CYCLE_TS.into());
        ts
    }
    /// Adds `add_ts` T-states to self.
    #[inline]
    fn add_no_mreq(&mut self, _addr: u16, add_ts: u8) {
        self.0 += Wrapping(add_ts.into());
    }
    /// Returns self (before addition of `IO_CYCLE_TS`) + [IO_IORQ_LOW_TS] as `T`.
    /// Adds [IO_CYCLE_TS] T-states to self.
    #[inline]
    fn add_io(&mut self, _port: u16) -> T {
        let ts = (self.0 + Wrapping(IO_IORQ_LOW_TS.into())).0;
        self.0 += Wrapping(IO_CYCLE_TS.into());
        ts
    }
    /// Adds [MEMRW_CYCLE_TS] T-states to self.
    /// Returns self as `T`.
    #[inline]
    fn add_mreq(&mut self, _addr: u16) -> T {
        self.0 += Wrapping(MEMRW_CYCLE_TS.into());
        (self.0).0
    }
    /// Adds [MEMRW_CYCLE_TS] T-states to self.
    /// Returns self as `T`.
    #[inline]
    fn add_m1(&mut self, _addr: u16) -> T {
        self.0 += Wrapping(M1_CYCLE_TS.into());
        (self.0).0
    }
    /// Returns a copy of self as T.
    #[inline]
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
