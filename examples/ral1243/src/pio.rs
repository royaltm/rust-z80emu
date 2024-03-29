/*
    ral1243: Emulator program as an example implementation for the z80emu library.
    Copyright (C) 2019-2024  Rafal Michalski

    For the full copyright notice, see the lib.rs file.
*/
//! `PIO Z8420` emulator.
//!
//! The emulator does not implement the bi-directional `PIO` channel mode.
use core::num::NonZeroU16;
use core::ops::Add;
use z80emu::Io;
use super::bus::{BusDevice, bit8};

#[allow(unused_imports)]
use log::{error, warn, info, debug, trace, Level};

/// A device connected to each channel of PIO Z8420.
pub trait PioDevice {
    type Timestamp: Copy;
    /// Called when the input mode was selected for this channel.
    ///
    /// The implementation should raise the `ready` flag.
    fn set_floating_bus(&mut self, _ts: Self::Timestamp) {}
    /// Called only in input mode when the `READY` line goes high
    /// after a `CPU` read data from a device.
    ///
    /// The implementation should raise the `ready` flag.
    fn ready(&mut self, _ts: Self::Timestamp) {}
    /// Called in input mode asking if the channel wants to activate `STROBE` line
    /// singalling sampling of a new data on the bus.
    ///
    /// The implementation should provide data for sampling only if the `ready`
    /// flag was raised and should clear the `ready` flag only if data is sampled.
    fn try_sample_data(&mut self, _ts: Self::Timestamp) -> Option<u8> { None }
    /// Called when the output mode was selected for this channel.
    ///
    /// The `data` is whatever was on the data bus prior to when the mode was selected.
    fn set_bus_data(&mut self, _data: u8, _ts: Self::Timestamp) {}
    /// Called only in output mode when the `READY` line goes high on `CPU` write.
    ///
    /// The implementation should take `data` and use it or store it as pending.
    fn ready_with_data(&mut self, _data: u8, _ts: Self::Timestamp) {}
    /// Called only in output mode asking if the `STROBE` line signalled the device is
    /// ready for more writes.
    ///
    /// The implementation should attempt to use any pending data and report
    /// a success or return `true` if no data is pending.
    fn was_strobe(&mut self, _ts: Self::Timestamp) -> bool { false }
}

#[derive(Debug, PartialEq, Eq)]
enum ChannelMode {
    Input,
    Output,
    // Bidirectional,
    // Control
}

struct Channel<T: Copy, D: PioDevice<Timestamp=T>> {
    mode: ChannelMode,
    want_ints_enabled: bool,
    ints_enabled: bool,
    int_active: bool,
    ready_active: bool,
    data_in: u8,
    data_out: u8,
    vector: u8,
    device: D
}

/// Emulator of the `PIO Z8420`.
///
/// `A` and `B` are types implementing [`PioDevice`] representing `PIO` channels.
pub struct Pio<T: Copy, A: PioDevice<Timestamp=T>, B: PioDevice<Timestamp=T>, D> {
    port_match_mask: u16,
    port_match_bits: u16,
    port_control_mask: u16,
    port_channel_mask: u16,
    channel_a: Channel<T, A>,
    channel_b: Channel<T, B>,
    ieo: bool,
    daisy_chained: D
}

#[allow(dead_code)]
impl<T, A, B, D> Pio<T, A, B, D>
where T: Copy,
      A: PioDevice<Timestamp=T>,
      B: PioDevice<Timestamp=T>,
      D: BusDevice + Io<Timestamp=T>
{
    /// Return a new instance of the [`Pio`] peripheral.
    ///
    /// Provide the [`PioDevice`] implementations for channels `A` and `B`
    /// and the remaining daisy-chained devices or a [Terminator](crate::bus::Terminator).
    pub fn new(pio_device_a: A, pio_device_b: B, daisy_chained: D) -> Self {
        Pio {
            port_match_mask: 0,
            port_match_bits: 0,
            port_control_mask: 1,
            port_channel_mask: 2,
            channel_a: Channel::new(pio_device_a),
            channel_b: Channel::new(pio_device_b),
            ieo: false,
            daisy_chained
        }
    }
    /// Configure the `CPU` [`Io`] port interface for this `PIO` instance.
    ///
    /// * `port_match_mask` should contain the base port mask.
    /// * `port_match_bits` should contain the base port address.
    /// * `channel_select_bit` should contain the number of the port address bit
    ///   that selects the PIO channel.
    /// * `control_select_bit` should contain the number of the port address bit
    ///   that selects write access between `PIO` device `data` output and the
    ///   `PIO` channel `control` word.
    ///
    /// # Example
    ///
    /// ```text
    /// I/O port address bits:  xxxxxx01_xPxxxxDx
    /// ```
    /// where `x` bits are ignored, `P` bit selects between `A`(=0) and `B`(=1)
    /// channel and `D` bit selects between writing to `data`(=0) or `control`(=1) word,
    /// the arguments should be:
    ///
    /// * `port_match_mask` = `0b11_00000000`
    /// * `port_match_bits` = `0b01_00000000`
    /// * `channel_select_bit` = 6
    /// * `control_select_bit` = 1
    pub fn with_port_bits(
            mut self,
            port_match_mask: u16,
            port_match_bits: u16,
            channel_select_bit: u32,
            control_select_bit: u32
        ) -> Self
    {
        assert_ne!(channel_select_bit, control_select_bit);
        assert!(channel_select_bit < 16);
        assert!(control_select_bit < 16);
        assert_eq!(port_match_mask & port_match_bits, port_match_bits);
        assert_eq!(port_match_mask & (1 << channel_select_bit), 0);
        assert_eq!(port_match_mask & (1 << control_select_bit), 0);
        self.port_match_mask = port_match_mask;
        self.port_match_bits = port_match_bits;
        self.port_channel_mask = 1 << channel_select_bit;
        self.port_control_mask = 1 << control_select_bit;
        self
    }
    /// Mutably access the channel A `PIO` device implementation.
    pub fn pio_device_a(&mut self) -> &mut A {
        &mut self.channel_a.device
    }
    /// Mutably access the channel B `PIO` device implementation.
    pub fn pio_device_b(&mut self) -> &mut B {
        &mut self.channel_b.device
    }
    /// Destruct `PIO` and return channels A and B `PIO` devices and a
    /// daisy-chained device.
    pub fn into_inner(self) -> (A, B, D) {
        (self.channel_a.device, self.channel_b.device, self.daisy_chained)
    }
}

impl<T, A, B, D> BusDevice for Pio<T, A, B, D>
where T: Copy,
      A: PioDevice<Timestamp=T>,
      B: PioDevice<Timestamp=T>,
      D: BusDevice<Timestamp=T>
{
    type Timestamp = T;
    type NextDevice = D;

    fn frame_end(&mut self, ts: T) {
        self.channel_a.check_int(ts);
        self.channel_b.check_int(ts);
        self.daisy_chained.frame_end(ts);
    }

    fn m1(&mut self, ts: T) {
        self.channel_a.m1(ts);
        self.channel_b.m1(ts);
        self.daisy_chained.m1(ts);
    }

    fn reset(&mut self, ts: T) {
        self.ieo = false;
        self.channel_a.reset(ts);
        self.channel_b.reset(ts);
        self.daisy_chained.reset(ts);
    }

    fn next_device(&mut self) -> &mut D {
        &mut self.daisy_chained
    }

    fn next_second(&mut self, delta: T) {
        self.daisy_chained.next_second(delta);
    }
}

impl<T, A, B, D> Io for Pio<T, A, B, D>
where T: Copy + Add<T, Output=T> + From<u8>,
      A: PioDevice<Timestamp=T>,
      B: PioDevice<Timestamp=T>,
      D: BusDevice + Io<Timestamp=T,WrIoBreak=(),RetiBreak=()>
{
    type Timestamp = T;
    type WrIoBreak = ();
    type RetiBreak = ();

    fn read_io(&mut self, port: u16, ts: Self::Timestamp) -> (u8, Option<NonZeroU16>) {
        if port & self.port_match_mask == self.port_match_bits {
            // trace!("read pio port {:04x}", port);
            let data = match port & self.port_channel_mask {
                0 => self.channel_a.read_io(ts),
                _ => self.channel_b.read_io(ts)
            };
            (data, None)
        }
        else {
            self.daisy_chained.read_io(port, ts)
        }
    }

    fn write_io(&mut self, port: u16, data: u8, ts: Self::Timestamp) -> (Option<()>, Option<NonZeroU16>) {
        if port & self.port_match_mask == self.port_match_bits {
            // trace!("write pio port {:04x}={:02x}", port, data);
            match (port & self.port_channel_mask, port & self.port_control_mask) {
                (0, 0) => self.channel_a.write_io_data(data, ts),
                (0, _) => self.channel_a.write_io_control(data, ts),
                (_, 0) => self.channel_b.write_io_data(data, ts),
                (_, _) => self.channel_b.write_io_control(data, ts),
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
        else if self.channel_a.check_int(ts) ||
                self.channel_b.check_int(ts)
        {
            true
        }
        else {
            self.daisy_chained.is_irq(ts)
        }
    }

    fn irq_data(&mut self, pc: u16, ts: Self::Timestamp) -> (u8, Option<NonZeroU16>) {
        if self.channel_a.int_active {
            self.ieo = true;
            self.channel_a.int_active = false;
            (self.channel_a.vector, None)
        }
        else if self.channel_b.int_active {
            self.ieo = true;
            self.channel_b.int_active = false;
            (self.channel_b.vector, None)
        }
        else {
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

impl<T, D> Channel<T, D>
where T: Copy, D: PioDevice<Timestamp=T>
{
    fn new(device: D) -> Self {
        Channel {
            mode: ChannelMode::Input,
            want_ints_enabled: false,
            ints_enabled: false,
            int_active: false,
            ready_active: true,
            data_in: 0,
            data_out: 0,
            vector: 0,
            device
        }
    }

    fn reset(&mut self, ts: T) {
        self.mode = ChannelMode::Input;
        self.want_ints_enabled = false;
        self.ints_enabled = false;
        self.int_active = false;
        self.ready_active = true;
        self.data_in = 0;
        self.data_out = 0;
        // self.vector = 0;
        self.device.set_floating_bus(ts);
    }

    fn set_int_active(&mut self) {
        if self.ints_enabled {
            self.int_active = true;
        }
    }

    fn m1(&mut self, _ts: T) {
        if self.want_ints_enabled {
            self.ints_enabled = true;
            self.want_ints_enabled = false;
        }
        // self.check_int(ts);
    }

    fn check_int(&mut self, ts: T) -> bool {
        if self.ready_active {
            match self.mode {
                ChannelMode::Input => {
                    if let Some(data) = PioDevice::try_sample_data(&mut self.device, ts) {
                        self.data_in = data;
                        self.ready_active = false;
                        self.set_int_active();
                    }
                }
                ChannelMode::Output => {
                    if PioDevice::was_strobe(&mut self.device, ts) {
                        self.ready_active = false;
                        self.set_int_active();
                    }
                }
            }
        }
        self.int_active
    }

    fn read_io(&mut self, ts: T) -> u8
    where T: Add<T, Output=T> + From<u8>
    {
        match self.mode {
            ChannelMode::Input => {
                if self.ready_active {
                    PioDevice::ready(&mut self.device, ts + T::from(3));
                }
                else {
                    self.ready_active = true;
                    PioDevice::ready(&mut self.device, ts + T::from(1));
                }
                self.data_in
            }
            ChannelMode::Output => {
                self.data_out
            }
        }
    }

    fn write_io_data(&mut self, data: u8, ts: T)
    where T: Add<T, Output=T> + From<u8>
    {
        // trace!("write io data: {:02x}", data);
        self.data_out = data;
        if self.mode == ChannelMode::Output {
            let ts = if self.ready_active {
                if self.device.was_strobe(ts) {
                    self.set_int_active();
                    ts + T::from(1)
                }
                else {
                    ts + T::from(3)
                }
            }
            else {
                self.ready_active = true;
                ts + T::from(1)
            };
            self.device.ready_with_data(data, ts);
        }
    }

    fn write_io_control(&mut self, data: u8, ts: T) {
        if data & 1 == 0 {
            // trace!("write io vector: {:02x}", data);
            self.vector = data;
        }
        else {
            match data & 0b1111 {
                0b1111 => { // mode select
                    self.mode = match data >> 6 {
                        0 => {
                            self.ready_active = false;
                            self.device.set_bus_data(self.data_out, ts);
                            ChannelMode::Output
                        }
                        1 => {
                            self.ready_active = true;
                            self.device.set_floating_bus(ts);
                            ChannelMode::Input
                        }
                        _ => unimplemented!("unsupported mode")
                    };
                }
                0b0111 => {
                    unimplemented!("unsupported interrupt control")
                }
                0b0011 => {
                    if data & bit8(7) != 0 {
                        if !self.ints_enabled {
                            // trace!("want int enabled");
                            self.want_ints_enabled = true;
                        }
                    }
                    else {
                        // trace!("int disabled");
                        self.want_ints_enabled = false;
                        self.ints_enabled = false;
                        self.int_active = false;
                    }
                }
                _ => {
                    // warn!("unrecognized control word: {:02x}", data);
                }
            }
        }
    }
}
