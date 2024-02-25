/*
    ral1243: Emulator program as an example implementation for the z80emu library.
    Copyright (C) 2019-2024  Rafal Michalski

    For the full copyright notice, see the lib.rs file.
*/
//! [`Clock`] implementation for `Ral1243`.
use core::num::{NonZeroU8, NonZeroU16};
use core::ops::{Deref, DerefMut};
use core::num::Wrapping;
use z80emu::host::{cycles, Clock};
use cycles::*;

/// The type used for `Timestamps`.
pub type Ts = u32;

/// The clock of `Ral1243`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TClock { cur: Wrapping<Ts>, clock_hz: Ts }

impl TClock {
    /// Return a new instance of the clock from a given number of T-states per second.
    pub fn new(clock_hz: Ts) -> Self {
        TClock { cur: Wrapping(clock_hz), clock_hz }
    }

    /// Reset the internal counter.
    pub fn reset(&mut self) {
        self.cur = Wrapping(self.clock_hz);
    }

    /// Return the `CPU` clock frequency in T-states per second.
    pub fn clock_hz(&self) -> Ts {
        self.clock_hz
    }

    /// Return a duraction of a single T-state in nanoseconds.
    pub fn ts_duration_nanos(&self) -> u32 {
        1e9 as u32 / self.clock_hz
    }

    /// Check if the internal clock counter exceeds the value of an emulated second
    /// and decrease the clock accordingly. Return whether the counter was wrapped.
    pub fn check_wrap_second(&mut self) -> bool {
        if self.cur.0 > 2*self.clock_hz {
            self.cur -= Wrapping(self.clock_hz);
            true
        }
        else {
            false
        }
    }
}

impl Clock for TClock {
    type Limit = Ts;
    type Timestamp = Ts;

    #[inline]
    fn is_past_limit(&self, limit: Self::Limit) -> bool {
        self.cur.0 >= limit
    }

    #[inline]
    fn add_irq(&mut self, _addr: u16) -> Ts {
        self.cur += Wrapping(IRQ_ACK_CYCLE_TS.into());
        self.cur.0
    }

    #[inline]
    fn add_no_mreq(&mut self, _addr: u16, add_ts: NonZeroU8) {
        self.cur += Wrapping(add_ts.get().into());
    }

    #[inline]
    fn add_io(&mut self, _port: u16) -> Ts {
        self.cur += Wrapping(IO_CYCLE_TS.into());
        self.cur.0
    }

    #[inline]
    fn add_mreq(&mut self, _addr: u16) -> Ts {
        self.cur += Wrapping(MEMRW_CYCLE_TS.into());
        self.cur.0
    }

    #[inline]
    fn add_m1(&mut self, _addr: u16) -> Ts {
        self.cur += Wrapping(M1_CYCLE_TS.into());
        self.cur.0
    }

    #[inline]
    fn add_wait_states(&mut self, _bus: u16, wait_states: NonZeroU16) {
        self.cur += Wrapping(wait_states.get().into());
    }

    #[inline]
    fn as_timestamp(&self) -> Ts {
        self.cur.0
    }
}

impl Deref for TClock {
    type Target = Wrapping<Ts>;

    fn deref(&self) -> &Self::Target {
        &self.cur
    }
}

impl DerefMut for TClock {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.cur
    }
}
