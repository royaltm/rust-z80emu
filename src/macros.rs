/*
        z80emu: ZiLOG Z80 microprocessor emulation library.
        Copyright (C) 2019-2024  Rafal Michalski

        For the full copyright notice, see the lib.rs file.
*/
/*! Helper macros when implementing [`Io`], [`Memory`] and [`Clock`] for wrapper types.

```
use z80emu::{*, host::TsCounter};

struct Ticks<T: Copy>(TsCounter<T>);

struct BusWrap<B>(B);

impl Clock for Ticks<i32> {
    forward_host_clock_types!{ @ => TsCounter<i32> }
    forward_host_clock_methods!{ @ => |c| c.0 }
}

impl<B: Io> Io for BusWrap<B> {
    forward_host_io_types!{ @ => B }
    forward_host_io_methods!{ @ => |b| b.0 }
}

impl<B: Memory> Memory for BusWrap<B> {
    forward_host_memory_types!{ @ => B }
    forward_host_memory_methods!{ @ => |b| b.0 }
}
```

One practical scenario is that the wrapper requires only a few methods with custom behaviour 
while forwarding the rest.

```
use core::num::NonZeroU16;
use z80emu::*;

struct DebugBus<'a, B> {
    bus: &'a mut B,
    irq_data: Option<u8>
}

// To illustrate an example with accessors:
impl<'a, B> DebugBus<'a, B> {
    fn bus_mut(&mut self) -> &mut B {
        &mut *self.bus
    }
    fn bus_ref(&self) -> &B {
        &*self.bus
    }
}

impl<B: Io> Io for DebugBus<'_, B> {
    // while we should use just this:
    // forward_host_io_types!{ @ => B }

    // here we illustrate how to forward only selected types:
    type Timestamp = <B as Io>::Timestamp;
    forward_host_io_types! { (WrIoBreak RetiBreak) => B }

    /// Capture data from an interrupting device.
    fn irq_data(&mut self, pc: u16, ts: Self::Timestamp) -> (u8, Option<NonZeroU16>) {
        let (data, delay) = self.bus.irq_data(pc, ts);
        self.irq_data = Some(data);
        (data, delay)
    }

    // forward remaining methods
    forward_host_io_methods! {
        (write_io, read_io, is_irq, reti) => |me| me.bus_mut() }

    // be carefull if you don't forward a method, a default implementation
    // will be used for that method instead
}

impl<B: Memory> Memory for DebugBus<'_, B> {
    forward_host_memory_types!{ @ => B }
    // here we can split implementations between methods that access &self:
    forward_host_memory_methods!{ @ref => |me| me.bus_ref() }
    // from methods accessing &mut self
    forward_host_memory_methods!{ @mut => |me| me.bus_mut() }
}
```

[`Clock`]: crate::Clock
[`Io`]: crate::Io
[`Memory`]: crate::Memory
*/
/// Forward associated types of a [`Clock`] trait to another type.
///
/// See [`macros`] for more information.
///
/// [`Clock`]: crate::Clock
/// [`macros`]: crate::macros
#[macro_export] macro_rules! forward_host_clock_types {
    (@ => $fwd:ty) => {
        forward_host_clock_types! {
            (Limit, Timestamp) => $fwd
        }
    };

    (($($ty:ident)*) => $fwd:ty) => {
        $( forward_host_clock_types! { $ty => $fwd } )*
    };

    (($($ty:ident),*) => $fwd:ty) => {
        $( forward_host_clock_types! { $ty => $fwd } )*
    };

    ($ty:ident => $fwd:ty) => {
        type $ty = <$fwd as $crate::host::Clock>::$ty;
    };
}

/// Forward associated types of a [`Io`] trait to another type.
///
/// See [`macros`] for more information.
///
/// [`Io`]: crate::Io
/// [`macros`]: crate::macros
#[macro_export] macro_rules! forward_host_io_types {
    (@ => $fwd:ty) => {
        forward_host_io_types! {
            (Timestamp, WrIoBreak, RetiBreak) => $fwd
        }
    };

    (($($ty:ident)*) => $fwd:ty) => {
        $( forward_host_io_types! { $ty => $fwd } )*
    };

    (($($ty:ident),*) => $fwd:ty) => {
        $( forward_host_io_types! { $ty => $fwd } )*
    };

    ($ty:ident => $fwd:ty) => {
        type $ty = <$fwd as $crate::host::Io>::$ty;
    };
}

/// Forward associated types of a [`Memory`] trait to another type.
///
/// See [`macros`] for more information.
///
/// [`Memory`]: crate::Memory
/// [`macros`]: crate::macros
#[macro_export] macro_rules! forward_host_memory_types {
    (@ => $fwd:ty) => {
        forward_host_memory_types! {
            (Timestamp) => $fwd
        }
    };

    (($($ty:ident)*) => $fwd:ty) => {
        $( forward_host_memory_types! { $ty => $fwd } )*
    };

    (($($ty:ident),*) => $fwd:ty) => {
        $( forward_host_memory_types! { $ty => $fwd } )*
    };

    ($ty:ident => $fwd:ty) => {
        type $ty = <$fwd as $crate::host::Memory>::$ty;
    };
}

/// Forward methods of [`Clock`] to a wrapped instance.
///
/// See [`macros`] for more information.
///
/// [`Clock`]: crate::Clock
/// [`macros`]: crate::macros
#[macro_export] macro_rules! forward_host_clock_methods {
    (@ => |$this:ident| $fwd:expr) => {
        forward_host_clock_methods! {
                (is_past_limit
                 add_irq
                 add_no_mreq
                 add_m1
                 add_mreq
                 add_io
                 add_wait_states
                 as_timestamp) => |$this| $fwd
        }
    };

    (@ref => |$this:ident| $fwd:expr) => {
        forward_host_clock_methods! {
                (is_past_limit, as_timestamp) => |$this| $fwd
        }
    };

    (@mut => |$this:ident| $fwd:expr) => {
        forward_host_clock_methods! {
                (add_irq
                 add_no_mreq
                 add_m1
                 add_mreq
                 add_io
                 add_wait_states) => |$this| $fwd
        }
    };

    (($($func:ident)*) => |$this:ident| $fwd:expr) => {
        $( forward_host_clock_methods! { $func => |$this| $fwd } )*
    };

    (($($func:ident),*) => |$this:ident| $fwd:expr) => {
        $( forward_host_clock_methods! { $func => |$this| $fwd } )*
    };

    (is_past_limit => |$this:ident| $fwd:expr) => {
        #[inline]
        fn is_past_limit(&self, limit: Self::Limit) -> bool {
            let $this = self;
            ($fwd).is_past_limit(limit)
        }
    };

    (add_irq => |$this:ident| $fwd:expr) => {
        #[inline]
        fn add_irq(&mut self, pc: u16) -> Self::Timestamp {
            let $this = self;
            ($fwd).add_irq(pc)
        }
    };

    (add_no_mreq => |$this:ident| $fwd:expr) => {
        #[inline]
        fn add_no_mreq(&mut self, address: u16, add_ts: core::num::NonZeroU8) {
            let $this = self;
            ($fwd).add_no_mreq(address, add_ts)
        }
    };

    (add_m1 => |$this:ident| $fwd:expr) => {
        #[inline]
        fn add_m1(&mut self, address: u16) -> Self::Timestamp {
            let $this = self;
            ($fwd).add_m1(address)
        }
    };

    (add_mreq => |$this:ident| $fwd:expr) => {
        #[inline]
        fn add_mreq(&mut self, address: u16) -> Self::Timestamp {
            let $this = self;
            ($fwd).add_mreq(address)
        }
    };

    (add_io => |$this:ident| $fwd:expr) => {
        #[inline]
        fn add_io(&mut self, port: u16) -> Self::Timestamp {
            let $this = self;
            ($fwd).add_io(port)
        }
    };

    (add_wait_states => |$this:ident| $fwd:expr) => {
        #[inline]
        fn add_wait_states(&mut self, bus: u16, wait_states: core::num::NonZeroU16) {
            let $this = self;
            ($fwd).add_wait_states(bus, wait_states)
        }
    };

    (as_timestamp => |$this:ident| $fwd:expr) => {
        #[inline]
        fn as_timestamp(&self) -> Self::Timestamp {
            let $this = self;
            ($fwd).as_timestamp()
        }
    };
}

/// Forward methods of [`Io`] to a wrapped instance.
///
/// See [`macros`] for more information.
///
/// [`Io`]: crate::Io
/// [`macros`]: crate::macros
#[macro_export] macro_rules! forward_host_io_methods {
    (@ => |$this:ident| $fwd:expr) => {
        forward_host_io_methods! {
            (write_io, read_io, is_irq, irq_data, reti) => |$this| $fwd
        }
    };

    (($($func:ident)*) => |$this:ident| $fwd:expr) => {
        $( forward_host_io_methods! { $func => |$this| $fwd } )*
    };

    (($($func:ident),*) => |$this:ident| $fwd:expr) => {
        $( forward_host_io_methods! { $func => |$this| $fwd } )*
    };

    (write_io => |$this:ident| $fwd:expr) => {
        #[inline]
        fn write_io(&mut self, port: u16, data: u8, ts: Self::Timestamp) -> (Option<Self::WrIoBreak>, Option<core::num::NonZeroU16>)
        {
            let $this = self;
            ($fwd).write_io(port, data, ts)
        }
    };

    (read_io => |$this:ident| $fwd:expr) => {
        #[inline]
        fn read_io(&mut self, port: u16, ts: Self::Timestamp) -> (u8, Option<core::num::NonZeroU16>)
        {
            let $this = self;
            ($fwd).read_io(port, ts)
        }
    };

    (is_irq => |$this:ident| $fwd:expr) => {
        #[inline]
        fn is_irq(&mut self, ts: Self::Timestamp) -> bool
        {
            let $this = self;
            ($fwd).is_irq(ts)
        }
    };

    (irq_data => |$this:ident| $fwd:expr) => {
        #[inline]
        fn irq_data(&mut self, pc: u16, ts: Self::Timestamp) -> (u8, Option<core::num::NonZeroU16>)
        {
            let $this = self;
            ($fwd).irq_data(pc, ts)
        }
    };

    (reti => |$this:ident| $fwd:expr) => {
        #[inline]
        fn reti(&mut self, addr: u16, ts: Self::Timestamp) -> Option<Self::RetiBreak>
        {
            let $this = self;
            ($fwd).reti(addr, ts)
        }
    };
}

/// Forward methods of [`Memory`] to a wrapped instance.
///
/// See [`macros`] for more information.
///
/// [`Memory`]: crate::Memory
/// [`macros`]: crate::macros
#[macro_export] macro_rules! forward_host_memory_methods {
    (@ => |$this:ident| $fwd:expr) => {
        forward_host_memory_methods! {
                (read_opcode, read_mem, read_mem16, write_mem, read_debug) => |$this| $fwd
        }
    };

    (@ref => |$this:ident| $fwd:expr) => {
        forward_host_memory_methods! {
                (read_mem, read_mem16, read_debug) => |$this| $fwd
        }
    };

    (@mut => |$this:ident| $fwd:expr) => {
        forward_host_memory_methods! {
                (read_opcode, write_mem) => |$this| $fwd
        }
    };

    (($($func:ident)*) => |$this:ident| $fwd:expr) => {
        $( forward_host_memory_methods! { $func => |$this| $fwd } )*
    };

    (($($func:ident),*) => |$this:ident| $fwd:expr) => {
        $( forward_host_memory_methods! { $func => |$this| $fwd } )*
    };

    (read_opcode => |$this:ident| $fwd:expr) => {
        #[inline]
        fn read_opcode(&mut self, pc: u16, ir: u16, ts: Self::Timestamp) -> u8 {
            let $this = self;
            ($fwd).read_opcode(pc, ir, ts)
        }
    };

    (read_mem => |$this:ident| $fwd:expr) => {
        #[inline]
        fn read_mem(&self, addr: u16, ts: Self::Timestamp) -> u8 {
            let $this = self;
            ($fwd).read_mem(addr, ts)
        }
    };

    (read_mem16 => |$this:ident| $fwd:expr) => {
        #[inline]
        fn read_mem16(&self, addr: u16, ts: Self::Timestamp) -> u16 {
            let $this = self;
            ($fwd).read_mem16(addr, ts)
        }
    };

    (write_mem => |$this:ident| $fwd:expr) => {
        #[inline]
        fn write_mem(&mut self, addr: u16, data: u8, ts: Self::Timestamp) {
            let $this = self;
            ($fwd).write_mem(addr, data, ts)
        }
    };

    (read_debug => |$this:ident| $fwd:expr) => {
        #[inline]
        fn read_debug(&self, addr: u16) -> u8 {
            let $this = self;
            ($fwd).read_debug(addr)
        }
    };
}

#[cfg(test)]
mod test {
    
    #[derive(Default)]
    struct Bus<T>{ clock: T, io: u8, byte: u8, mem: u16, ir: u16 }

    struct Ticks<T: Copy>(crate::host::TsCounter<T>);

    struct RTicks<T: Copy>{ cl: Ticks<T> }

    impl<T: Copy> RTicks<T> {
        fn clock_ref(&self) -> &Ticks<T> { &self.cl }
        fn clock_mut(&mut self) -> &mut Ticks<T> { &mut self.cl }
    }

    impl crate::Clock for Ticks<i32> {
        forward_host_clock_types!{ @ => crate::host::TsCounter<i32> }
        forward_host_clock_methods!{ @ => |c| c.0 }
    }

    impl crate::Clock for RTicks<i32> {
        forward_host_clock_types!{ @ => Ticks<i32> }
        forward_host_clock_methods!{ @ref => |c| c.clock_ref() }
        forward_host_clock_methods!{ @mut => |c| c.clock_mut() }
    }

    struct RBus<'a, B> {
        bus: &'a mut B
    }

    struct NBus<'a, B>(RBus<'a, B>);

    impl<'a, B> RBus<'a, B> {
        fn bus_mut(&mut self) -> &mut B {
            &mut *self.bus
        }
        fn bus_ref(&self) -> &B {
            &*self.bus
        }
    }

    impl<'a, B> NBus<'a, B> {
        fn bus_mut(&mut self) -> &mut RBus<'a, B> {
            &mut self.0
        }
        fn bus_ref(&self) -> &RBus<'a, B> {
            &self.0
        }
    }

    impl<'a, B: crate::Io> crate::Io for NBus<'a, B> {
        forward_host_io_types!{ @ => RBus<'a, B> }
        forward_host_io_methods!{ @ => |m| m.0 }
    }

    impl<B: crate::Io> crate::Io for RBus<'_, B> {
        forward_host_io_types!{ @ => B }
        forward_host_io_methods!{ @ => |m| m.bus }
    }

    impl<'a, B: crate::Memory> crate::Memory for NBus<'a, B> {
        forward_host_memory_types!{ @ => RBus<'a, B> }
        forward_host_memory_methods!{ @ref => |m| m.bus_ref() }
        forward_host_memory_methods!{ @mut => |m| m.bus_mut() }
    }

    impl<B: crate::Memory> crate::Memory for RBus<'_, B> {
        forward_host_memory_types!{ @ => B }
        forward_host_memory_methods!{ @ref => |m| m.bus_ref() }
        forward_host_memory_methods!{ @mut => |m| m.bus_mut() }
    }

    impl<T> crate::Io for Bus<T> {
        type Timestamp = T;
        type WrIoBreak = u8;
        type RetiBreak = u16;
        fn write_io(&mut self, port: u16, data: u8, ts: Self::Timestamp) -> (Option<Self::WrIoBreak>, Option<core::num::NonZeroU16>) {
            self.io = data;
            self.clock = ts;
            (Some(data), core::num::NonZeroU16::new(port))
        }
        fn read_io(&mut self, port: u16, ts: Self::Timestamp) -> (u8, Option<core::num::NonZeroU16>) {
            self.clock = ts;
            (self.io, core::num::NonZeroU16::new(port))
        }
        fn is_irq(&mut self, ts: Self::Timestamp) -> bool {
            self.clock = ts;
            true
        }
        fn irq_data(&mut self, pc: u16, ts: Self::Timestamp) -> (u8, Option<core::num::NonZeroU16>) {
            self.clock = ts;
            (self.io, core::num::NonZeroU16::new(pc))
        }
        fn reti(&mut self, addr: u16, ts: Self::Timestamp) -> Option<Self::RetiBreak> {
            self.clock = ts;
            Some(addr)
        }
    }

    impl<T> crate::Memory for Bus<T> {
        type Timestamp = T;
        fn read_mem(&self, _address: u16, _ts: Self::Timestamp) -> u8 {
            self.byte
        }
        fn read_mem16(&self, address: u16, _ts: Self::Timestamp) -> u16 {
            address
        }
        fn read_opcode(&mut self, pc: u16, ir: u16, ts: Self::Timestamp) -> u8 {
            self.clock = ts;
            self.mem = pc;
            self.ir = ir;
            self.byte
        }
        fn write_mem(&mut self, address: u16, value: u8, ts: Self::Timestamp) {
            self.clock = ts;
            self.mem = address;
            self.byte = value;
        }
        fn read_debug(&self, _address: u16) -> u8 {
            self.byte
        }
    }

    #[test]
    fn forward_host_clock_works() {
        use core::num::Wrapping;
        use crate::{Clock, host::TsCounter};
        let mut ticks = Ticks(TsCounter(Wrapping(100i32)));

        assert_eq!(ticks.as_timestamp(), 100);
        assert!(ticks.is_past_limit(99));
        assert!(ticks.is_past_limit(100));
        assert!(!ticks.is_past_limit(101));
        assert_eq!(ticks.add_irq(0), 102);
        assert_eq!(ticks.as_timestamp(), 106);
        ticks.add_no_mreq(0, core::num::NonZeroU8::new(1).unwrap());
        assert_eq!(ticks.as_timestamp(), 107);
        assert_eq!(ticks.add_m1(0), 111);
        assert_eq!(ticks.add_mreq(0), 114);
        assert_eq!(ticks.as_timestamp(), 114);
        assert_eq!(ticks.add_io(0), 115);
        assert_eq!(ticks.as_timestamp(), 118);
        ticks.add_wait_states(0, core::num::NonZeroU16::new(2).unwrap());
        assert_eq!(ticks.as_timestamp(), 120);
        assert_eq!(ticks.as_timestamp(), ((ticks.0).0).0);
    }

    #[test]
    fn forward_host_io_works() {
        use crate::Io;
        let mut bus: Bus<u32> = Bus{ clock: 0, io: 42, ..Default::default() };
        let mut rbus = RBus { bus: &mut bus };
        assert_eq!(rbus.write_io(0xbaca, 33, 420), (Some(33), core::num::NonZeroU16::new(0xbaca)));
        assert_eq!(rbus.bus_ref().clock, 420);
        assert_eq!(rbus.bus.io, 33);
        assert_eq!(rbus.read_io(0xbeee, 696969), (33, core::num::NonZeroU16::new(0xbeee)));
        assert_eq!(rbus.bus_ref().clock, 696969);
        assert_eq!(rbus.is_irq(12345678), true);
        assert_eq!(rbus.bus.clock, 12345678);
        assert_eq!(rbus.irq_data(0x1243, 99999), (33, core::num::NonZeroU16::new(0x1243)));
        assert_eq!(rbus.bus.clock, 99999);
        assert_eq!(rbus.reti(0xfeed, 1701), Some(0xfeed));
        assert_eq!(rbus.bus.clock, 1701);

        let mut nbus = NBus(rbus);
        assert_eq!(nbus.write_io(0xbaca, 33, 420), (Some(33), core::num::NonZeroU16::new(0xbaca)));
        assert_eq!(nbus.0.bus_ref().clock, 420);
        assert_eq!(nbus.0.bus.io, 33);
        assert_eq!(nbus.read_io(0xbeee, 696969), (33, core::num::NonZeroU16::new(0xbeee)));
        assert_eq!(nbus.bus_ref().bus_ref().clock, 696969);
        assert_eq!(nbus.is_irq(12345678), true);
        assert_eq!(nbus.bus_ref().bus.clock, 12345678);
        assert_eq!(nbus.irq_data(0x1243, 99999), (33, core::num::NonZeroU16::new(0x1243)));
        assert_eq!(nbus.0.bus.clock, 99999);
        assert_eq!(nbus.reti(0x11cc, 1701), Some(0x11cc));
        assert_eq!(nbus.bus_ref().bus_ref().clock, 1701);
    }

    #[test]
    fn forward_host_memory_works() {
        use crate::Memory;
        let mut bus: Bus<u32> = Bus { clock: 0, byte: 77, mem: 0xaaaa, ir: 0x8888, ..Default::default() };
        let mut rbus = RBus { bus: &mut bus };
        rbus.write_mem(0xabcd, 44, 13);
        assert_eq!(rbus.bus.clock, 13);
        assert_eq!(rbus.bus_ref().mem, 0xabcd);
        assert_eq!(rbus.bus_ref().byte, 44);
        assert_eq!(rbus.read_mem(0, 0), 44);
        assert_eq!(rbus.read_mem16(0xd00f, 0), 0xd00f);
        assert_eq!(rbus.read_opcode(0x7654, 0x4444, u32::MAX), 44);
        assert_eq!(rbus.bus_ref().clock, u32::MAX);
        assert_eq!(rbus.bus_ref().mem, 0x7654);
        assert_eq!(rbus.bus_ref().ir, 0x4444);
        assert_eq!(rbus.read_debug(0), 44);

        let mut nbus = NBus(rbus);
        nbus.write_mem(0xabcd, 44, 13);
        assert_eq!(nbus.0.bus.clock, 13);
        assert_eq!(nbus.bus_ref().bus.mem, 0xabcd);
        assert_eq!(nbus.bus_ref().bus.byte, 44);
        assert_eq!(nbus.read_mem(0, 0), 44);
        assert_eq!(nbus.read_mem16(0xd00f, 0), 0xd00f);
        assert_eq!(nbus.read_opcode(0x7654, 0x4444, u32::MAX), 44);
        assert_eq!(nbus.0.bus_ref().clock, u32::MAX);
        assert_eq!(nbus.bus_ref().bus_ref().mem, 0x7654);
        assert_eq!(nbus.0.bus.ir, 0x4444);
        assert_eq!(nbus.read_debug(0), 44);
    }
}
