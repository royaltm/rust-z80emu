/*
    ral1243: Emulator program as an example implementation for the z80emu library.
    Copyright (C) 2019-2024  Rafal Michalski

    For the full copyright notice, see the lib.rs file.
*/
use core::time::{Duration};
use super::clock::{TClock, Ts};
use super::bus::BusDevice;

use z80emu::{Cpu, Memory, Io, Clock, BreakCause};

#[allow(unused_imports)]
use log::{error, warn, info, debug, trace, Level};

/// A helper for running emulated frames.
///
/// ```text
/// FRAME_PERIOD = 1 / TIME_FRAME_HZ
///
/// |<-        FRAME_PERIOD         ->|
/// t1          t2                   t3
/// <- emulate -><====== sleep =======>
/// ```
///
/// The `FrameRunner` keeps track of and manages the emulated time.
/// It holds the instance of [`TClock`] and a current frame limit.
/// 
/// The implementation methods running the emulation expect an instance
/// of a Z80 emulator implementing the [`Cpu`] trait and the system bus
/// emulator implementing [`Memory`], [`Io`] and [`BusDevice`] traits.
///
/// * `EXT_CLOCK_HZ`: an external clock frequency in hertz (NOT THE CPU CLOCK).
/// * `TIME_FRAME_HZ`: how many frames per second the emulation will run.
pub struct FrameRunner<const EXT_CLOCK_HZ: u32, const TIME_FRAME_HZ: u32> {
    pub(crate) clock: TClock,
    pub(crate) limit: Ts,
    pub(crate) frame_tstates: Ts,
}

/// Return a real-time duration of a running frame from a given frames frequency.
pub const fn frame_duration(frame_hz: u32) -> core::time::Duration {
    Duration::from_nanos(1e9 as u64 / frame_hz as u64)
}

impl<const EXT_HZ: u32, const FRAME_HZ: u32> FrameRunner<EXT_HZ, FRAME_HZ> {
    /// The external clock frequency used by peripherals.
    pub const EXT_CLOCK_HZ: u32 = EXT_HZ;
    /// Emulation frames frequency.
    pub const TIME_FRAME_HZ: u32 = FRAME_HZ;
    /// A real-time duration of a single frame.
    pub const FRAME_DURATION: Duration = frame_duration(FRAME_HZ);

    /// Return whether the `clock_hz` is a valid CPU clock for this runner.
    pub const fn clock_is_valid(clock_hz: Ts) -> bool {
        clock_hz % (EXT_HZ * 2) == 0 && clock_hz >= EXT_HZ * 10
    }

    /// Return the external clock period in the number of T-states.
    ///
    /// The external clock drives some peripherals.
    pub fn external_clock_tstates(&self) -> u32 {
        self.clock.clock_hz() / EXT_HZ
    }

    /// Create a new runner.
    ///
    /// `clock_hz`: a `CPU` clock in T-states per second.
    /// It must be divisible by [`Self::EXT_CLOCK_HZ`] * 2.
    ///
    /// **Panics** if the clock is invalid.
    pub fn new(clock_hz: Ts) -> Self {
        assert!(Self::clock_is_valid(clock_hz));
        let clock = TClock::new(clock_hz);
        // How many T-states per each frame.
        let frame_tstates: u32 = clock.clock_hz() / FRAME_HZ;
        info!("frame: {} T-states", frame_tstates);
        FrameRunner { clock, limit: 0, frame_tstates }
    }

    /// A real-time duration of a single running frame.
    pub fn frame_duration() -> Duration {
        Self::FRAME_DURATION
    }

    /// Reset everything including clock and the frame limit counter.
    /// 
    /// This function should be called before very first step.
    pub fn start<C: Cpu, M>(&mut self, cpu: &mut C, bus: &mut M)
        where M: BusDevice<Timestamp=Ts>
    {
        self.clock.reset();
        cpu.reset();
        bus.reset(self.clock.as_timestamp());
        self.limit = self.clock.as_timestamp();
    }

    /// Run the emulation for a period of a single frame and return the
    /// frame duration in emulated T-states.
    pub fn step<C: Cpu, M>(&mut self, cpu: &mut C, bus: &mut M) -> Ts
        where M: Memory<Timestamp=Ts>
              + Io<Timestamp=Ts>
              + BusDevice<Timestamp=Ts>
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

    /// Reset the `cpu` and `bus` devices.
    pub fn reset<C: Cpu, M>(&mut self, cpu: &mut C, bus: &mut M)
        where M: BusDevice<Timestamp=Ts>
    {
        cpu.reset();
        bus.reset(self.clock.as_timestamp());
    }

    /// Trigger a non-maskable interrupt and return a number of T-states that
    /// it took on success.
    pub fn nmi<C: Cpu, M>(&mut self, cpu: &mut C, bus: &mut M) -> Option<Ts>
        where M: Memory<Timestamp=Ts> + Io<Timestamp=Ts>
    {
        let start_ts = self.clock.as_timestamp();
        if cpu.nmi(bus, &mut self.clock) {
            return Some(self.clock.as_timestamp().wrapping_sub(start_ts))
        }
        None
    }
}
