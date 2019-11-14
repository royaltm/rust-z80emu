/*
 Emulation runner.

 Emulates in frame pulses.

|<- 1/TIME_FRAME_HZ s.->|
t1                      t2                       t3
<======= sleep =========><- work -><=== sleep ===>
*/
use std::convert::TryFrom;
use std::thread;
use std::sync::mpsc::{Receiver, TryRecvError};
use std::cell::Cell;
use std::rc::Rc;
use std::time::{Duration, Instant};
use super::clock::{TClock, Ts};
use super::bus::{BusMemory, BusDevice};
use super::ctc_trigger::Triggers;
use z80emu::{Cpu, Memory, Io, Clock, BreakCause};

#[allow(unused_imports)]
use log::{error, warn, info, debug, trace, Level};

/// How many frames / second.
const TIME_FRAME_HZ: u32 = 1000;
/// External clock frequency.
const EXT_CLOCK_HZ: u32 = 10_000;
/// Max CPU clock frequency.
const MAX_CLOCK_HZ: u32 = 40_000_000;

pub enum RunnerMsg {
    Terminate,
    Reset,
    Nmi
}

pub struct TimeProxy {
    epoch: Cell<Instant>,
    seconds: Cell<u64>,
    clock_hz: Ts,
    ts_duration_nanos: Ts
}

pub struct FrameRunner {
    tp: Rc<TimeProxy>,
    rx: Receiver<RunnerMsg>,
    clock: TClock
}

impl FrameRunner {
    /// `clock_hz`: CPU clock in T-states / second (modify at will as long as its divisible by 10000)
    pub fn new(rx: Receiver<RunnerMsg>, clock_hz: Ts) -> Self {
        assert!(clock_hz % EXT_CLOCK_HZ == 0 && clock_hz >= EXT_CLOCK_HZ && clock_hz <= MAX_CLOCK_HZ);
        let clock = TClock::new(clock_hz);
        let tp = Rc::new(TimeProxy::new(&clock));
        FrameRunner { tp, rx, clock }
    }

    pub fn time_proxy(&self) -> Rc<TimeProxy> {
        Rc::clone(&self.tp)
    }

    pub fn run<M, C>(&mut self, cpu: &mut C, bus: &mut M, vec_of_triggers: Vec<Triggers>)
        where M: Memory<Timestamp=Ts> + Io<Timestamp=Ts> + BusDevice<Timestamp=Ts> + BusMemory,
              C: Cpu
    {
        const FRAME_DURATION: Duration = Duration::from_nanos(1e9 as u64 / TIME_FRAME_HZ as u64);
        // How many T-states per each frame.
        let frame_tstates: u32 = self.clock.clock_hz() / TIME_FRAME_HZ;
        // How many T-states per each external clock period.
        let ext_clock_period_tstates: u32 = self.clock.clock_hz() / EXT_CLOCK_HZ;

        self.clock.reset();
        cpu.reset();
        bus.reset(self.clock.as_timestamp());
        for triggers in vec_of_triggers.iter() {
            triggers.borrow_mut().clear();
        }
        let mut limit = self.clock.as_timestamp();
        let mut ext_ts = limit;
        self.tp.start();
        let mut time = self.tp.epoch();
        loop {
            limit += frame_tstates;
            { // emulate external clock independent from CPU frequency
                let seconds = self.tp.seconds();
                while ext_ts <= limit {
                    ext_ts += ext_clock_period_tstates;
                    for triggers in vec_of_triggers.iter() {
                        let mut triggers = triggers.borrow_mut();
                        triggers.push_back((seconds, ext_ts - ext_clock_period_tstates/2, false));
                        triggers.push_back((seconds, ext_ts, true));
                    }
                }
            }
            if let Some(duration) = FRAME_DURATION.checked_sub(time.elapsed()) {
                thread::sleep(duration);
            }
            time += FRAME_DURATION;
            loop {
                match cpu.execute_with_limit(bus, &mut self.clock, limit) {
                    Ok(()) => break,
                    Err(BreakCause::Halt) => { 
                        // trace!("HALT");
                    }
                    Err(cause) => {
                        panic!("no break request was expected: {}", cause);
                    }
                }
            }
            match self.rx.try_recv() {
                Ok(RunnerMsg::Terminate) => break,
                Ok(RunnerMsg::Reset) => {
                    cpu.reset();
                    bus.reset(self.clock.as_timestamp());                    
                }
                Ok(RunnerMsg::Nmi) => {
                    if !cpu.nmi(bus, &mut self.clock) {
                        warn!("NMI failed");
                    }
                    else {
                        trace!("NMI request");
                    }
                }
                Err(TryRecvError::Empty) => {},
                Err(TryRecvError::Disconnected) => break,
            }
            if self.clock.check_wrap_second() {
                let clock_hz = self.clock.clock_hz();
                limit -= clock_hz;
                ext_ts -= clock_hz;
                bus.next_second(clock_hz);
                self.tp.next_second();
                // debug!("seconds: {} ts: {} limit: {} ext_ts: {} pc: {:04x} sp: {:04x} i: {:02x} {:x?}",
                //     self.tp.seconds(), self.clock.as_timestamp(), limit, ext_ts, cpu.get_pc(), cpu.get_sp(), cpu.get_i(),
                //     bus.memory_debug(0x2000..0x200A));
            }
        }
    }
}

impl TimeProxy {
    fn new(clock: &TClock) -> Self {
        let epoch = Cell::new(Instant::now());
        let seconds = Cell::new(0);
        let clock_hz = clock.clock_hz();
        let ts_duration_nanos = clock.ts_duration_nanos();
        TimeProxy { epoch, seconds, clock_hz, ts_duration_nanos }
    }

    fn start(&self) {
        self.epoch.set(Instant::now());
        self.seconds.set(0);
    }

    fn next_second(&self) {
        self.seconds.set(self.seconds.get() + 1);
    }

    pub fn epoch(&self) -> Instant {
        self.epoch.get()
    }

    pub fn seconds(&self) -> u64 {
        self.seconds.get()
    }

    pub fn normalized_timestamp_now(&self, seconds: u64, ts: Ts) -> Ts {
        if let Some(diff) = self.seconds.get().checked_sub(seconds) {
             if let Some(ts) = ts.checked_sub(diff as Ts * self.clock_hz) {
                ts
             }
            else {
                panic!("the timestamp given was too old");
            }
        }
        else {
            panic!("the seconds provided are in the future");
        }
    }

    pub fn timestamp_to_instant(&self, ts: Ts) -> Instant {
        let nanos = ts as u64 * self.ts_duration_nanos as u64 + self.seconds.get() * self.clock_hz as u64;
        self.epoch.get().checked_add(Duration::from_nanos(nanos)).unwrap()
    }

    pub fn instant_to_timestamp(&self, time: Instant) -> Ts {
        let dur = time.duration_since(self.epoch.get());
        if let Some(dur) = dur.checked_sub(Duration::from_secs(self.seconds.get())) {
            Ts::try_from(dur.as_secs() * self.clock_hz as u64 + (dur.subsec_nanos() / self.ts_duration_nanos) as u64)
            .unwrap_or(Ts::max_value())
        }
        else {
            Ts::min_value()
        }
    }
}
