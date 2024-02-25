/*
    ral1243: Emulator program as an example implementation for the z80emu library.
    Copyright (C) 2019-2024  Rafal Michalski

    For the full copyright notice, see the lib.rs file.
*/
//! `CTC Z8430` emulator.
use core::num::{NonZeroU16, Wrapping};
use core::ops::{Add, Sub};
use z80emu::Io;
use super::bus::{BusDevice, bit8};

#[allow(unused_imports)]
use log::{error, warn, info, debug, trace, Level};

/// An interface for triggering lines: `CLK/TRG` in and `ZC/TO` out.
pub trait CtcTrigger {
    type Timestamp: Copy;
    /// Should return `Ok(Timestamp)` of the first `CLK/TRG` edge mathing
    /// the `rising_edge` before or at the `ts`. Otherwise return the next
    /// future timestamp hint as `Err(Timestamp)`.
    fn next_clk_trg(&mut self, rising_edge: bool, ts: Self::Timestamp) -> Result<Self::Timestamp, Self::Timestamp>;
    /// Purge all `CLK/TRG` events before `ts`, return the next future timestamp hint.
    fn purge_clk_trg(&mut self, ts: Self::Timestamp) -> Self::Timestamp;
    /// A pulse on `ZC/TO` is being triggered with the given `ts`.
    /// The pulse is high (rising) and lasts 2 T-states.
    fn zc_to_pulse(&mut self, ts: Self::Timestamp);
    /// Wrap the internal counters by the given `delta` T-states.
    fn next_second(&mut self, delta: Self::Timestamp);
}

#[allow(non_camel_case_types)]
#[derive(Debug,Clone,Copy)]
#[repr(u8)]
enum Prescaler {
    x256 = 1,
    x16 = 16,
}

impl Prescaler {
    #[inline(always)]
    fn to_ts_factor(self) -> u32 {
        self as u32
    }
}

#[derive(PartialEq, Eq, Debug)]
enum ChannelMode {
    Timer, Counter
}

struct Channel<T: Copy, R: CtcTrigger<Timestamp=T>> {
    prescaler: Prescaler,
    mode: ChannelMode,
    next_ts_hint: T, // Predict the future, 0 has special meaning, forcing all channels to process
    timer_last_ts: Option<T>, // None -> waiting for trigger, Some(T) Timer is ticking
    ext_trigger_ts: Option<T>, // None -> no ext loaded when timer_ext_trigger==true
    timer_counter_internal: u16, // starts with 256; on x16 each ts decrements by 16 on x256 each ts decrements 1
    restart: Option<u8>, // None -> suspended counting/timer (was reset), waiting for the Time Constant
    current: Wrapping<u8>, // current counter/timer value (will be read by I/O)
    awaiting_time_constant: bool, // next word is Time Constant
    timer_ext_trigger: bool,
    ints_enabled: bool,
    int_active: bool,
    triggering_rising_edge: bool,
    trigger: R
}

/// Emulator of the `CTC Z8430`.
///
/// `R0` to `R3` are types implementing [`CtcTrigger`] representing
/// `CTC` channels 0 to 3.
pub struct Ctc<T: Copy,
        R0: CtcTrigger<Timestamp=T>,
        R1: CtcTrigger<Timestamp=T>,
        R2: CtcTrigger<Timestamp=T>,
        R3: CtcTrigger<Timestamp=T>,
        D> {
    next_ts_hint: T,
    port_match_mask: u16,
    port_match_bits: u16,
    port_cs0_mask: u16,
    port_cs1_mask: u16,
    vector: u8,
    channel0: Channel<T, R0>,
    channel1: Channel<T, R1>,
    channel2: Channel<T, R2>,
    channel3: Channel<T, R3>,
    ieo: bool,
    daisy_chained: D
}

#[allow(dead_code)]
impl<T, R0, R1, R2, R3, D> Ctc<T, R0, R1, R2, R3, D>
where T: Copy + Default,
      R0: CtcTrigger<Timestamp=T>,
      R1: CtcTrigger<Timestamp=T>,
      R2: CtcTrigger<Timestamp=T>,
      R3: CtcTrigger<Timestamp=T>
{
    /// Return a new instance of the [`Ctc`] peripheral.
    ///
    /// Provide the [`CtcTrigger`] implementations for each `CTC` channel
    /// and the remaining daisy-chained devices or a [Terminator](crate::bus::Terminator).
    pub fn new(
            ctc_trigger0: R0,
            ctc_trigger1: R1,
            ctc_trigger2: R2,
            ctc_trigger3: R3,
            daisy_chained: D
        ) -> Self
    {
        Ctc {
            next_ts_hint: T::default(),
            port_match_mask: 0,
            port_match_bits: 0,
            port_cs0_mask: 1,
            port_cs1_mask: 2,
            vector: 0,
            channel0: Channel::new(ctc_trigger0),
            channel1: Channel::new(ctc_trigger1),
            channel2: Channel::new(ctc_trigger2),
            channel3: Channel::new(ctc_trigger3),
            ieo: false,
            daisy_chained
        }
    }
    /// Configure the `CPU` [`Io`] port interface for this `CTC` instance.
    ///
    /// * `port_match_mask` should contain the base port mask.
    /// * `port_match_bits` should contain the base port address.
    /// * `channel_select1_bit` should contain the number of the port address MSB bit
    ///   that selects the CTC channel.
    /// * `channel_select0_bit` should contain the number of the port address LSB bit
    ///   that selects the CTC channel.
    ///
    /// # Example
    ///
    /// ```text
    /// I/O port address bits:  xxxxxx10_MxxxxxxL
    /// ```
    /// where `x` bits are ignored and `M` and `L` bits select the channel such as:
    /// ```text
    /// ML = 00 - channel 0, 01 - channel 1, 10 - channel 2, 11 - channel 3.
    /// ```
    /// the arguments should be:
    ///
    /// * `port_match_mask` = `0b11_00000000`
    /// * `port_match_bits` = `0b10_00000000`
    /// * `channel_select1_bit` = 7
    /// * `channel_select0_bit` = 0
    pub fn with_port_bits(
            mut self,
            port_match_mask: u16,
            port_match_bits: u16,
            channel_select1_bit: u32,
            channel_select0_bit: u32
        ) -> Self
    {
        assert_ne!(channel_select1_bit, channel_select0_bit);
        assert!(channel_select1_bit < 16);
        assert!(channel_select0_bit < 16);
        assert_eq!(port_match_mask & port_match_bits, port_match_bits);
        assert_eq!(port_match_mask & (1 << channel_select1_bit), 0);
        assert_eq!(port_match_mask & (1 << channel_select0_bit), 0);
        self.port_match_mask = port_match_mask;
        self.port_match_bits = port_match_bits;
        self.port_cs0_mask = 1 << channel_select0_bit;
        self.port_cs1_mask = 1 << channel_select1_bit;
        self
    }
    /// Mutably access the channel 0 trigger implementation.
    pub fn ctc0_trigger(&mut self) -> &mut R0 {
        &mut self.channel0.trigger
    }

    /// Mutably access the channel 1 trigger implementation.
    pub fn ctc1_trigger(&mut self) -> &mut R1 {
        &mut self.channel1.trigger
    }

    /// Mutably access the channel 2 trigger implementation.
    pub fn ctc2_trigger(&mut self) -> &mut R2 {
        &mut self.channel2.trigger
    }

    /// Mutably access the channel 3 trigger implementation.
    pub fn ctc3_trigger(&mut self) -> &mut R3 {
        &mut self.channel3.trigger
    }
    /// Destruct `CTC` and return all triggers and a daisy-chained device.
    pub fn into_inner(self) -> (R0, R1, R2, R3, D) {
        let Ctc {
            channel0,
            channel1,
            channel2,
            channel3,
            daisy_chained,
            ..
        } = self;
        (channel0.trigger,
         channel1.trigger,
         channel2.trigger,
         channel3.trigger,
         daisy_chained)
    }
}

impl<T, R0, R1, R2, R3, D> Ctc<T, R0, R1, R2, R3, D>
where T: Copy + Eq + Ord + Add<T, Output=T> + Sub<T, Output=T> + From<u32>, u32: TryFrom<T>,
      R0: CtcTrigger<Timestamp=T>,
      R1: CtcTrigger<Timestamp=T>,
      R2: CtcTrigger<Timestamp=T>,
      R3: CtcTrigger<Timestamp=T>
{
    fn process_channels(&mut self, ts: T) {
        let hint_ts = self.next_ts_hint;
        if ts >= hint_ts {
            let force = hint_ts == T::from(0);
            macro_rules! process_channel {
                ($channel:ident) => {
                    if ts >= self.$channel.next_ts_hint || force {
                        self.$channel.process(ts);
                    }
                };
            }
            process_channel!(channel0);
            process_channel!(channel1);
            process_channel!(channel2);
            process_channel!(channel3);
            self.next_ts_hint = self.channel0.next_ts_hint
                .min(self.channel1.next_ts_hint)
                .min(self.channel2.next_ts_hint)
                .min(self.channel3.next_ts_hint);
        }
    }
}

impl<T, R0, R1, R2, R3, D> BusDevice for Ctc<T, R0, R1, R2, R3, D>
where T: Copy + Eq + Ord + Add<T, Output=T> + Sub<T, Output=T> + From<u32>, u32: TryFrom<T>,
      R0: CtcTrigger<Timestamp=T>,
      R1: CtcTrigger<Timestamp=T>,
      R2: CtcTrigger<Timestamp=T>,
      R3: CtcTrigger<Timestamp=T>,
      D: BusDevice<Timestamp=T>
{
    type Timestamp = T;
    type NextDevice = D;

    fn frame_end(&mut self, ts: T) {
        self.process_channels(ts);
        self.daisy_chained.frame_end(ts);
    }

    fn m1(&mut self, ts: T) {
        self.process_channels(ts);
        self.daisy_chained.m1(ts);
    }

    fn reset(&mut self, ts: T) {
        self.ieo = false;
        self.next_ts_hint = T::from(0);
        self.channel0.reset(ts);
        self.channel1.reset(ts);
        self.channel2.reset(ts);
        self.channel3.reset(ts);
        self.daisy_chained.reset(ts);
    }

    fn next_device(&mut self) -> &mut D {
        &mut self.daisy_chained
    }

    fn next_second(&mut self, delta: T) {
        let ts = self.next_ts_hint;
        self.next_ts_hint = if ts >= delta { ts - delta } else { T::from(0) };
        self.channel0.next_second(delta);
        self.channel1.next_second(delta);
        self.channel2.next_second(delta);
        self.channel3.next_second(delta);        
        self.daisy_chained.next_second(delta);
    }
}

impl<T, R0, R1, R2, R3, D> Io for Ctc<T, R0, R1, R2, R3, D>
where T: Copy + Eq + Ord + Add<T, Output=T> + Sub<T, Output=T> + From<u32>, u32: TryFrom<T>,
      R0: CtcTrigger<Timestamp=T>,
      R1: CtcTrigger<Timestamp=T>,
      R2: CtcTrigger<Timestamp=T>,
      R3: CtcTrigger<Timestamp=T>,
      D: BusDevice + Io<Timestamp=T,WrIoBreak=(),RetiBreak=()>
{
    type Timestamp = T;
    type WrIoBreak = ();
    type RetiBreak = ();

    fn read_io(&mut self, port: u16, ts: Self::Timestamp) -> (u8, Option<NonZeroU16>) {
        if port & self.port_match_mask == self.port_match_bits {
            // debug!("read ctc io: {:04x}", port);
            let data = match (port & self.port_cs1_mask, port & self.port_cs0_mask) {
                (0, 0) => self.channel0.read_counter(ts),
                (0, _) => self.channel1.read_counter(ts),
                (_, 0) => self.channel2.read_counter(ts),
                (_, _) => self.channel3.read_counter(ts),
            };
            (data, None)
        }
        else {
            self.daisy_chained.read_io(port, ts)
        }
    }

    fn write_io(&mut self, port: u16, data: u8, ts: Self::Timestamp) -> (Option<()>, Option<NonZeroU16>) {
        if port & self.port_match_mask == self.port_match_bits {
            // debug!("written ctc io: {:04x} = {:02x} ({:02x},{:02x})", port, data, port & self.port_cs1_mask, port & self.port_cs0_mask);
            let was_control_word = match (port & self.port_cs1_mask, port & self.port_cs0_mask) {
                (0, 0) => self.channel0.write_control(data, ts),
                (0, _) => self.channel1.write_control(data, ts),
                (_, 0) => self.channel2.write_control(data, ts),
                (_, _) => self.channel3.write_control(data, ts),
            };
            if !was_control_word {
                // debug!("write ctc vector: {:04x} = {:02x}", port, data);
                self.vector = data & 0b11111000;
            }
            (None, None)
        }
        else {
            self.daisy_chained.write_io(port, data, ts)
        }
    }

    fn is_irq(&mut self, ts: Self::Timestamp) -> bool {
        if self.ieo {
            false
        } 
        else {
            self.process_channels(ts);
            macro_rules! check_channel {
                ($channel:ident) => {
                    {
                        if self.$channel.int_active {
                            // trace!("is_irq: {}", stringify!($channel));
                            return true;
                        }
                    }
                };
            }
            check_channel!(channel0);
            check_channel!(channel1);
            check_channel!(channel2);
            check_channel!(channel3);
            self.daisy_chained.is_irq(ts)
        }
    }

    fn irq_data(&mut self, pc: u16, ts: Self::Timestamp) -> (u8, Option<NonZeroU16>) {
        self.ieo = true;
        if self.channel0.int_active {
            // trace!("irq_data: channel0");
            self.channel0.int_active = false;
            #[allow(clippy::identity_op)]
            (self.vector | 0 << 1, None)
        }
        else if self.channel1.int_active {
            // trace!("irq_data: channel1");
            self.channel1.int_active = false;
            (self.vector | 1 << 1, None)
        }
        else if self.channel2.int_active {
            // trace!("irq_data: channel2");
            self.channel2.int_active = false;
            (self.vector | 2 << 1, None)
        }
        else if self.channel3.int_active {
            // trace!("irq_data: channel3");
            self.channel3.int_active = false;
            (self.vector | 3 << 1, None)
        }
        else {
            self.ieo = false;
            self.daisy_chained.irq_data(pc, ts)
        }
    }

    fn reti(&mut self, pc: u16, ts: Self::Timestamp) -> Option<()> {
        if self.ieo {
            self.ieo = false;
            None
        }
        else {
            self.daisy_chained.reti(pc, ts)
        }
    }
}

impl<T, R> Channel<T, R>
where T: Copy + Default, R: CtcTrigger<Timestamp=T>
{
    fn new(trigger: R) -> Self {
        Channel {
            prescaler: Prescaler::x256,
            mode: ChannelMode::Timer,
            next_ts_hint: T::default(),
            timer_last_ts: None,
            ext_trigger_ts: None,
            timer_counter_internal: 256,
            restart: None,
            current: Wrapping(0),
            awaiting_time_constant: false,
            timer_ext_trigger: false,
            ints_enabled: false,
            int_active: false,
            triggering_rising_edge: false,
            trigger
        }
    }
}

impl<T, R> Channel<T, R>
where T: Copy + From<u32>, R: CtcTrigger<Timestamp=T>
{
    fn set_int_active(&mut self) {
        if self.ints_enabled {
            // trace!("int set: {}", self.int_active);
            self.int_active = true;
        }
        self.next_ts_hint = T::from(0);
    }
}

impl<T, R> Channel<T, R>
where T: Copy + Eq + Ord + Add<T, Output=T> + Sub<T, Output=T> + From<u32>, u32: TryFrom<T>,
      R: CtcTrigger<Timestamp=T>
{
    fn reset(&mut self, ts: T) {
        self.process(ts);
        self.next_ts_hint = T::from(0);
        self.restart = None;
        self.timer_last_ts = None;
        self.ext_trigger_ts = None;
        self.timer_counter_internal = 256;
        self.mode = ChannelMode::Timer;
        self.awaiting_time_constant = false;
        self.ints_enabled = false;
        self.int_active = false;
    }

    fn next_second(&mut self, delta: T) {
        if let Some(ts) = self.timer_last_ts {
            self.timer_last_ts = if ts >= delta {
                Some(ts - delta)
            }
            else {
                panic!("next second failed for CTC timer");
            }
        }
        if let Some(ts) = self.ext_trigger_ts {
            self.ext_trigger_ts = Some(if ts >= delta { ts - delta } else { T::from(0) });
        }
        let ts = self.next_ts_hint;
        self.next_ts_hint = if ts >= delta { ts - delta } else { T::from(0) };
        self.trigger.next_second(delta);
    }

    fn read_counter(&mut self, ts: T) -> u8 {
        self.process(ts);
        self.current.0
    }

    /// Returns false if data is a vector word.
    fn write_control(&mut self, data: u8, ts: T) -> bool {
        self.process(ts);
        if self.awaiting_time_constant { // Time Constant
            self.awaiting_time_constant = false;
            if self.restart.is_none() {
                self.current = Wrapping(data);
            }
            self.restart = Some(data);
            match self.mode {
                ChannelMode::Timer => {
                    if self.timer_last_ts.is_none() {
                        if self.timer_ext_trigger {
                            self.ext_trigger_ts = Some(ts);
                        }
                        else {
                            self.start_timer(ts);
                        }
                    }
                }
                ChannelMode::Counter => {}
            }
            true
        }
        else if data & bit8(0) != 0 { // Control Word
            if data & bit8(1) != 0 { // RESET
                self.restart = None;
                self.timer_last_ts = None;
                self.ext_trigger_ts = None;
                self.timer_counter_internal = 256;
            }
            if data & bit8(2) != 0 {
                self.awaiting_time_constant = true;
            }
            self.prescaler = if data & bit8(5) != 0 {
                Prescaler::x256
            }
            else {
                Prescaler::x16
            };
            self.ints_enabled = if data & bit8(7) != 0 {
                true
            }
            else {
                self.int_active = false;
                false
            };
            if data & bit8(6) != 0 { // COUNTER mode
                if self.mode == ChannelMode::Timer {
                    self.timer_last_ts = None;
                    self.ext_trigger_ts = None;
                    self.mode = ChannelMode::Counter;
                }
            }
            else { // TIMER mode
                if self.mode == ChannelMode::Counter {
                    if self.restart.is_some() && !self.awaiting_time_constant {
                        if data & bit8(3) != 0 { // TIMER CLK/TRG trigger
                            self.ext_trigger_ts = Some(ts);
                        }
                        else { // TIMER automatic trigger
                            self.start_timer(ts); // ??? should we also reset prescaler?
                        }
                    }
                    self.mode = ChannelMode::Timer;
                }
            }
            if self.mode == ChannelMode::Timer {
                if data & bit8(3) != 0 { // TIMER CLK/TRG trigger
                    self.timer_ext_trigger = true;
                }
                else if self.timer_ext_trigger { // TIMER automatic trigger
                    if self.timer_last_ts.is_none() && self.restart.is_some() && !self.awaiting_time_constant {
                        self.start_timer(ts);
                    }
                    self.ext_trigger_ts = None;
                    self.timer_ext_trigger = false;
                }
            }
            let rising_edge = data & bit8(4) != 0;
            if self.triggering_rising_edge != rising_edge {
                self.triggering_rising_edge = rising_edge;
                self.clk_trg_edge_changed(ts);
            }
            true
        }
        else { // Interrupt Vector
            false
        }
    }

    #[inline(always)]
    fn downcount_by_one(&mut self, restart: u8, ts: T) -> bool {
        self.current -= Wrapping(1);
        if self.current.0 == 0 {
            self.current = Wrapping(restart);
            self.trigger.zc_to_pulse(ts);
            self.set_int_active();
            return true
        }
        false
    }

    #[inline(always)]
    fn start_timer(&mut self, ts: T) {
        let start = ts + T::from(1);
        self.timer_last_ts = Some(start);
        self.next_ts_hint = start;
    }

    fn clk_trg_edge_changed(&mut self, ts: T) {
        if let Some(restart) = self.restart {
            match self.mode {
                ChannelMode::Counter => {
                    self.downcount_by_one(restart, ts);
                }
                ChannelMode::Timer => {
                    if self.timer_last_ts.is_none() {
                        if let Some(trig_ts) = self.ext_trigger_ts {
                            if ts >= trig_ts {
                                self.start_timer(ts);
                            }
                        }
                    }
                }
            }
        }
    }

    fn process(&mut self, ts: T) {
        // trace!("proc: {} >= {}", u32::try_from(ts).unwrap_or(0), u32::try_from(self.next_ts_hint).unwrap_or(0));
        if let Some(restart) = self.restart {
            match self.mode {
                ChannelMode::Counter => loop {
                    match self.trigger.next_clk_trg(self.triggering_rising_edge, ts) {
                        Ok(tgt) => if self.downcount_by_one(restart, tgt) {
                            return /* we triggered */
                        },
                        Err(hint) => {
                            self.next_ts_hint = hint;
                            return
                        }
                    }
                }
                ChannelMode::Timer => {
                    if let Some(last_ts) = self.timer_last_ts {
                        self.trigger.purge_clk_trg(ts);
                        if ts > last_ts {
                            let delta_ts = match u32::try_from(ts - last_ts) {
                                Ok(dt) => dt,
                                Err(_) => panic!("Delta T-states out of range!")
                            };
                            let factor = self.prescaler.to_ts_factor(); // (x256: 1, x16: 16)
                            let mut counter_delta = delta_ts * factor;
                            let mut timer_counter =
                                (u32::from((self.current - Wrapping(1)).0) << 8) +
                                 u32::from(self.timer_counter_internal);
                            // let mut timer_counter = (u32::from(match self.current.0 {
                            //     0 => 255,
                            //     t => t - 1
                            // }) << 8) + u32::from(self.timer_counter_internal);
                            let mut pulse_ts = last_ts;
                            let restart: u32 = match restart {
                                0 => 256,
                                t => u32::from(t)
                            } << 8;
                            while counter_delta >= timer_counter {
                                counter_delta -= timer_counter;
                                pulse_ts = pulse_ts + T::from(timer_counter / factor);
                                self.trigger.zc_to_pulse(pulse_ts);
                                self.set_int_active();
                                timer_counter = restart;
                            }
                            timer_counter -= counter_delta;
                            debug_assert_ne!(timer_counter, 0);
                            // trace!("delta: {} timer: {}", delta_ts, timer_counter);
                            self.next_ts_hint = if pulse_ts != last_ts {
                                T::from(0)
                            }
                            else {
                                ts + T::from(timer_counter / factor)
                            };
                            match timer_counter & 255 {
                                0 => {
                                    self.timer_counter_internal = 256;
                                    self.current = Wrapping((timer_counter >> 8) as u8);
                                }
                                tci => {
                                    self.timer_counter_internal = tci as u16;
                                    self.current = Wrapping((timer_counter >> 8) as u8) + Wrapping(1);
                                }
                            }
                            self.timer_last_ts = Some(ts);
                            return
                        }
                        self.next_ts_hint = last_ts + T::from(1);
                        return
                    }
                    else if let Some(trig_ts) = self.ext_trigger_ts {
                        loop {
                            match self.trigger.next_clk_trg(self.triggering_rising_edge, ts) {
                                Ok(tgt) => if tgt >= trig_ts {
                                    self.start_timer(tgt);
                                    // self.trigger.purge_clk_trg(ts);
                                    return
                                }
                                Err(hint) => {
                                    self.next_ts_hint = hint;
                                    return
                                }
                            }
                        }
                    }
                    // timer mode awaiting external trigger
                }
            }
        }
        // waiting for constant to be uploaded, while purging all previous CLK/TRG events
        self.next_ts_hint = self.trigger.purge_clk_trg(ts);
    }
}
