/*
    ral1243: Emulator program as an example implementation for the z80emu library.
    Copyright (C) 2019-2024  Rafal Michalski

    For the full copyright notice, see the mod.rs file.
*/
/*
 Emulation runner.

 Emulates in frame pulses.

|<-      1/TIME_FRAME_HZ s.     ->|
t1       t2                      t3
<- work -><======= sleep =========>
*/
use core::time::{Duration};
use super::clock::{TClock, Ts};
use super::bus::{BusMemory, BusDevice};

use z80emu::{Cpu, Memory, Io, Clock, BreakCause};

#[allow(unused_imports)]
use log::{error, warn, info, debug, trace, Level};

pub struct FrameRunner<const EXT_CLOCK_HZ: u32, const TIME_FRAME_HZ: u32> {
    clock: TClock,
    limit: Ts,
    frame_tstates: Ts,
}

/// Return a duration of a single running frame.
pub const fn frame_duration(frame_hz: u32) -> core::time::Duration {
    Duration::from_nanos(1e9 as u64 / frame_hz as u64)
}

impl<const EXT_HZ: u32, const FRAME_HZ: u32> FrameRunner<EXT_HZ, FRAME_HZ> {
    /// External clock frequency.
    pub const EXT_CLOCK_HZ: u32 = EXT_HZ;
    /// How many frames / second.
    pub const TIME_FRAME_HZ: u32 = FRAME_HZ;
    /// A duration of a sinle frame.
    pub const FRAME_DURATION: Duration = frame_duration(FRAME_HZ);

    pub const fn clock_is_valid(clock_hz: Ts) -> bool {
        clock_hz % (EXT_HZ * 2) == 0 && clock_hz >= EXT_HZ * 10
    }

    pub fn external_clock_tstates(&self) -> u32 {
        self.clock.clock_hz() / EXT_HZ
    }

    /// `clock_hz`: CPU clock in T-states / second (modify at will as long as its divisible by EXT_HZ * 2)
    pub fn new(clock_hz: Ts) -> Self {
        assert!(Self::clock_is_valid(clock_hz));
        let clock = TClock::new(clock_hz);
        // How many T-states per each frame.
        let frame_tstates: u32 = clock.clock_hz() / FRAME_HZ;
        info!("frame: {} T-states", frame_tstates);
        FrameRunner { clock, limit: 0, frame_tstates }
    }

    pub fn frame_duration() -> Duration {
        Self::FRAME_DURATION
    }

    pub fn start<M, C>(&mut self, cpu: &mut C, bus: &mut M)
        where M: Memory<Timestamp=Ts> + Io<Timestamp=Ts> + BusDevice<Timestamp=Ts> + BusMemory,
              C: Cpu
    {
        self.clock.reset();
        cpu.reset();
        bus.reset(self.clock.as_timestamp());
        self.limit = self.clock.as_timestamp();
    }

    /// Run emulation step, return frame duration in T-states.
    pub fn step<M, C>(&mut self, cpu: &mut C, bus: &mut M) -> Ts
        where M: Memory<Timestamp=Ts> + Io<Timestamp=Ts> + BusDevice<Timestamp=Ts> + BusMemory,
              C: Cpu
    {
        if self.clock.check_wrap_second() {
            let clock_hz = self.clock.clock_hz();
            self.limit -= clock_hz;
            bus.next_second(clock_hz);
            // debug!("seconds: {} ts: {} limit: {} ext_ts: {} pc: {:04x} sp: {:04x} i: {:02x} {:x?}",
            //     self.tp.seconds(), self.clock.as_timestamp(), limit, ext_ts, cpu.get_pc(), cpu.get_sp(), cpu.get_i(),
            //     bus.memory_debug(0x2000..0x200A));
        }
        let start_ts = self.clock.as_timestamp();
        self.limit += self.frame_tstates;
        loop {
            match cpu.execute_with_limit(bus, &mut self.clock, self.limit) {
                Ok(()) => break,
                Err(BreakCause::Halt) => { 
                    // trace!("HALT");
                }
                Err(cause) => {
                    panic!("no break request was expected: {}", cause);
                }
            }
        }
        // Update bus devices once per frame
        bus.frame_end(self.clock.as_timestamp());
        self.clock.as_timestamp().wrapping_sub(start_ts)
    }

    pub fn reset<C, M>(&mut self, cpu: &mut C, bus: &mut M)
        where M: Memory<Timestamp=Ts> + Io<Timestamp=Ts> + BusDevice<Timestamp=Ts> + BusMemory,
              C: Cpu
    {
        cpu.reset();
        bus.reset(self.clock.as_timestamp());
    }

    pub fn nmi<C, M>(&mut self, cpu: &mut C, bus: &mut M) -> bool
        where M: Memory<Timestamp=Ts> + Io<Timestamp=Ts> + BusDevice<Timestamp=Ts> + BusMemory,
              C: Cpu
    {
        cpu.nmi(bus, &mut self.clock)
    }
}
