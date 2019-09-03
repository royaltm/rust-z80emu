//! This module defines the interfaces between a host and the Cpu.
use core::num::Wrapping;
use core::ops::{Add, AddAssign, Deref, DerefMut};
use super::opconsts::RST_0H_OPCODE;
pub mod cycles {
    /// An op-code fetch, NMI and HALT cycle T-states.
    pub const M1_CYCLE: u8 = 4;
    /// A memory read/write cycle T-states.
    pub const MEMRW_CYCLE: u8 = 3;
    /// A minimum number of T-states in the I/O cycle before the value is being provided on the bus.
    pub const IO_PRE_OP_CYCLE: u8 = 1;
    /// A minimum number of T-states in the I/O cycle after the value is being available on the bus.
    pub const IO_POST_OP_CYCLE: u8 = 3;
    /// A total number of T-states for I/O cycle.
    pub const IO_CYCLE: u8 = IO_PRE_OP_CYCLE + IO_POST_OP_CYCLE;
    /// A maskable interrupt request cycle T-states.
    pub const IRQ_CYCLE: u8 = 6;
}
use cycles::*;

/// A trait responsible for advancing T-state counter during various Cpu cycles.
/// The Cpu instructions depend on this trait to properly increase the counter.
/// It is up to the implementation however to determine how the counter is represented.
/// The only limit is that the TstateCounter type must implement Copy and be Sized.
/// This trait can be used to emulate the Cpu contention by increasing the counter more than the required value.
pub trait TstateCounter: Sized + Copy {
    /// A type for arbitrary limit representation when executing code with a limit. See Cpu.execute_with_limit.
    type Limit: Sized + Copy;
    /// A type produced by some of this trait methods that are passed to the methods of Io and Memory traits.
    type Timestamp: Sized;
    /// If the T-states counter is at the given limit.
    fn is_at_limit(&self, limit: Self::Limit) -> bool;
    /// This method should add at least IRQ_CYCLE (6) T-states to self.
    /// It's being used by the maskable interrupt requests.
    /// The address is a value of the PC register when the interrupt was triggered
    /// before the instruction at the address was executed.
    fn add_irq(&mut self, address: u16);
    /// This method should add an arbitrary T-states, at least the value given in add_ts, to self.
    /// It's being used by internal operations of the Cpu without memory or I/O access.
    fn add_no_mreq(&mut self, addr: u16, add_ts: u8);
    /// This method should add an arbitrary T-states, at least the value given in add_ts, to self.
    /// It should return the value of T-states after the addition as Self::Timestamp.
    /// The returned value may be passed later to one of the Memory methods.
    /// The Cpu calls this method with MEMRW_CYCLE (3) or M1_CYCLE (4).
    /// This method is also being used by the NMI invocation and while the Cpu is in halted state.
    fn add_mreq(&mut self, addr: u16, add_ts: u8) -> Self::Timestamp;
    /// This method should add at least IO_CYCLE (4) T-states to self.
    /// This method should return the value of T-states before increasing + IO_PRE_OP_CYCLE
    /// (or more) as Self::Timestamp.
    /// The returned value will be passed to one of Io::write_io or Io:read_io methods.
    fn add_io(&mut self, port: u16) -> Self::Timestamp;
    /// Should return a copy of self as a Self::Timestamp.
    fn as_timestamp(&self) -> Self::Timestamp;
}

/// I/O operations.
pub trait Io {
    /// A type used for timestamping I/O operations.
    type Timestamp: Sized;
    /// Used by the Cpu to read data from the I/O port.
    /// The T-states counter should be advanced by at least host::IO_CYCLE (4) by this method implementaiton.
    /// The implementation may choose to use the TstateCounter::add_io method for that purpose.
    fn read_io(&mut self, port: u16, ts: Self::Timestamp) -> u8;
    /// Used by the Cpu to write data to the I/O port.
    /// The T-states counter should be advanced by at least host::IO_CYCLE (4) by this method implementaiton.
    /// The implementation may choose to use the TstateCounter::add_io method for that purpose.
    fn write_io(&mut self, port: u16, data: u8, ts: Self::Timestamp) -> bool;
    /// Should return true if the IRQ signal is active.
    fn is_irq(&self, ts: Self::Timestamp) -> bool;
    /// This is used by the Interrupt Mode 0 to get the command code to execute.
    /// In reality this is obtained from the data bus where the external device places the data while requesting the interrupt.
    fn irq_data(&mut self, _pc: u16, _ts: Self::Timestamp) -> u8 { RST_0H_OPCODE }
    /// When a RETI instruction is being executed. This method is being called to update controller,
    /// so another interrupt signal can be set up if necessary.
    /// This is called before the returning address is popped from the machine stack and the address given
    /// is pointing immediately after the RETI instruction opcode.
    /// The Marker value given here is after advancing the M1 cycle (op-code fetch) but before the pop stack operation.
    fn reti(&mut self, _address: u16, _ts: Self::Timestamp) {}
}

/// An interface to the memory.
pub trait Memory {
    /// A type used for timestamping memory operations.
    type Timestamp: Sized;
    /// Used by the Cpu to read from the memory.
    /// For the M1 cycles (op-code fetches) read_opcode is used instead.
    fn read_mem(&self, addr: u16, ts: Self::Timestamp) -> u8;
    /// Used by the Cpu to read 2 bytes of memory in LE order.
    /// Real CPU splits this read but we are cutting corners here slightly.
    fn read_mem16(&self, addr: u16, ts: Self::Timestamp) -> u16;
    /// Used by the Cpu during M1 cycle for reading op-code.
    /// Can be used for ROM traps etc.
    /// Other Cpu read operation are performed via read and read16 methods.
    fn read_opcode(&mut self, pc: u16, ir: u16, ts: Self::Timestamp) -> u8;
    /// This is used by the Cpu for writing to the memory.
    fn write_mem(&mut self, addr: u16, value: u8, ts: Self::Timestamp);
    /// Used by the Cpu debugger to get conditional command argument (DJNZ/JR when not jumping). No timestamp.
    fn read_debug(&self, addr: u16) -> u8;
}

/// A simple T-states counter wrapping at 2^bitsize of T.
/// Please refer to it as a template for implementing TstateCounter trait methods.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub struct TsCounter<T: Copy>(pub Wrapping<T>);

impl<T> TstateCounter for TsCounter<T>
where T: Copy + PartialEq + PartialOrd + std::convert::From<u8>,
      Wrapping<T>: AddAssign + Add<Output=Wrapping<T>>
{
    type Limit = T;
    type Timestamp = T;

    /// Returns true if self >= limit.
    #[inline]
    fn is_at_limit(&self, limit: Self::Limit) -> bool {
        (self.0).0 >= limit
    }
    /// Adds IRQ_CYCLE (6) T-states to self.
    #[inline]
    fn add_irq(&mut self, _addr: u16) {
        self.0 += Wrapping(IRQ_CYCLE.into());
    }
    /// Adds add_ts T-states to self.
    #[inline]
    fn add_no_mreq(&mut self, _addr: u16, add_ts: u8) {
        self.0 += Wrapping(add_ts.into());
    }
    /// Returns T-state value after adding IO_PRE_OP_CYCLE. Adds IO_CYCLE (4) T-states to self. 
    #[inline]
    fn add_io(&mut self, _port: u16) -> T {
        let ts = (self.0 + Wrapping(IO_PRE_OP_CYCLE.into())).0;
        self.0 += Wrapping(IO_CYCLE.into());
        ts
    }
    /// Adds MEMRW_CYCLE (3) T-states to self.
    #[inline]
    fn add_mreq(&mut self, _addr: u16, add_ts: u8) -> T {
        self.0 += Wrapping(add_ts.into());
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
