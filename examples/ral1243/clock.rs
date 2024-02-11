/*
    ral1243: Emulator program as an example implementation for the z80emu library.
    Copyright (C) 2019-2024  Rafal Michalski

    For the full copyright notice, see the mod.rs file.
*/
use core::num::{NonZeroU8, NonZeroU16};
use core::ops::{Deref, DerefMut};
use core::num::Wrapping;
use z80emu::host::{cycles, Clock};
use cycles::*;

pub type Ts = u32;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TClock { cur: Wrapping<Ts>, clock_hz: Ts }

impl TClock {
    pub fn new(clock_hz: Ts) -> Self {
        TClock { cur: Wrapping(0), clock_hz }
    }

    pub fn reset(&mut self) {
        self.cur = Wrapping(0);
    }

    /// CPU clock in T-states / second
    pub fn clock_hz(&self) -> Ts {
        self.clock_hz
    }

    /// Nanoseconds / T-state
    pub fn ts_duration_nanos(&self) -> Ts {
        1e9 as Ts/self.clock_hz
    }

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
