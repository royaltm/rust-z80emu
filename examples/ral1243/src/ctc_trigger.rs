/*
    ral1243: Emulator program as an example implementation for the z80emu library.
    Copyright (C) 2019-2024  Rafal Michalski

    For the full copyright notice, see the mod.rs file.
*/
use core::cell::Cell;
#[allow(unused_imports)]
use super::rc::Rc;
use super::ctc::CtcTrigger;
use super::clock::Ts;

/// Pulse collector.
///
/// `N`: is max pulse capacity.
/// Determines how many pulses can be captured before the component panics.
pub struct CtcPulse<const N: usize> {
    // pulse timestamp shift register, pulses never overlaps
    ts: [Cell<Ts>;N],
    // current number of pulses stored
    pulses: Cell<u32>
}

impl<const N: usize> Default for CtcPulse<N> {
    fn default() -> Self {
        let ts = [0;N].map(Cell::new);
        let pulses = Cell::default();
        CtcPulse { ts, pulses }
    }
}

/// Active CTC component with external clock.
/// Chains ZC/TO with the next CTC channel's CLK/TRG.
///
/// For `N` see [CtcPulse].
pub struct CtcActive<const N: usize> {
    /// External clock half-period in T-states.
    ///
    /// If clock is 10kHz this should be 5000.
    ext_hclock: Ts,
    /// Previous half-period begin timestamp.
    period_ts: Ts,
    /// Whether the clock line is high.
    edge_high: bool,
    /// The CTC pulse collector.
    next: Rc<CtcPulse<N>>
}

/// Passive CTC component chained to another components ZC/TO line.
///
/// For `N` see [CtcPulse].
pub struct CtcPassive<const N: usize> {
    pulse: Rc<CtcPulse<N>>
}

impl<const N: usize> CtcActive<N> {
    /// Create active CTC component from external clock period in T-states.
    /// Panics if `ext_clock` is not even.
    pub fn new(ext_clock: Ts) -> Self {
        assert_eq!(ext_clock & 1, 0);
        CtcActive {
            ext_hclock: ext_clock / 2,
            period_ts: 0,
            edge_high: false,
            next: Rc::new(CtcPulse::default())
        }
    }
    /// Create a ZC/TO to CLK/TRG connected passive CTC.
    pub fn new_passive(&self) -> CtcPassive<N> {
        CtcPassive {
            pulse: Rc::clone(&self.next)
        }
    }
}

impl<const N: usize> CtcTrigger for CtcActive<N> {
    type Timestamp = Ts;

    fn next_clk_trg(&mut self, rising_edge: bool, timestamp: Ts) -> Result<Ts, Ts> {
        let next_ts = self.period_ts + self.ext_hclock;
        if next_ts <= timestamp {
            let edge = !self.edge_high;
            if edge == rising_edge {
                self.edge_high = edge;
                self.period_ts = next_ts;
                return Ok(next_ts)
            }
            else {
                let next_next_ts = next_ts + self.ext_hclock;
                if next_next_ts <= timestamp {
                    self.period_ts = next_next_ts;
                    return Ok(next_next_ts)
                }
                self.edge_high = edge;
                self.period_ts = next_ts;
                return Err(next_next_ts);
            }
        }
        Err(next_ts)
    }

    fn purge_clk_trg(&mut self, timestamp: Ts) -> Ts {
        let next_ts = self.period_ts + self.ext_hclock;
        if next_ts <= timestamp {
            self.period_ts = next_ts;
            self.edge_high = !self.edge_high;
            next_ts + self.ext_hclock
        }
        else {
            next_ts
        }
    }

    fn zc_to_pulse(&mut self, timestamp: Ts) {
        let npulses = self.next.pulses.get();
        if npulses as usize >= N {
            panic!("too many pulses: {:?} {}", self.next.ts, timestamp);
        }
        let newcount = npulses + 1;
        self.next.ts[npulses as usize].set(timestamp);
        self.next.pulses.set(newcount);
    }

    fn next_second(&mut self, delta: Ts) {
        self.period_ts = self.period_ts.saturating_sub(delta);
    }
}


impl<const N: usize> CtcPulse<N> {
    fn shift(&self, index: u32) {
        let count = (N - 1).min(index as usize);
        for i in 0..count {
            self.ts[i].set(self.ts[i + 1].get());
        }
        self.pulses.set(count as u32);
    }
}

impl<const N: usize> CtcTrigger for CtcPassive<N> {
    type Timestamp = Ts;
    fn next_clk_trg(&mut self, rising_edge: bool, timestamp: Ts) -> Result<Ts, Ts> {
        let npulses = self.pulse.pulses.get();
        if npulses != 0 {
            let next_ts = self.pulse.ts[0].get();
            if next_ts <= timestamp {
                if rising_edge {
                    self.pulse.shift(npulses - 1);
                    return Ok(next_ts)
                }
                else if next_ts + 2 <= timestamp {
                    self.pulse.shift(npulses - 1);
                    return Ok(next_ts + 2)
                }
                else {
                    return Err(next_ts + 2)
                }
            }
            return Err(next_ts)
        }
        Err(Ts::MAX)
    }

    fn purge_clk_trg(&mut self, timestamp: Ts) -> Ts {
        let mut npulses = self.pulse.pulses.get();
        while npulses != 0 {
            let next_ts = self.pulse.ts[0].get();
            if next_ts > timestamp {
                return next_ts;
            }
            else if next_ts + 2 > timestamp {
                return next_ts + 2;
            }
            npulses -= 1;
            self.pulse.shift(npulses);
        }
        Ts::MAX
    }

    fn zc_to_pulse(&mut self, _timestamp: Ts) {}

    fn next_second(&mut self, delta: Ts) {
        let count = self.pulse.pulses.get() as usize;
        for cell in &self.pulse.ts[0..count] {
            cell.set(cell.get().saturating_sub(delta));
        }
    }
}
