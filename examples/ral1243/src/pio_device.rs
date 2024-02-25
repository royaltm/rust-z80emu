/*
    ral1243: Emulator program as an example implementation for the z80emu library.
    Copyright (C) 2019-2024  Rafal Michalski

    For the full copyright notice, see the lib.rs file.
*/
//! `PIO` channel devices.
use super::clock::Ts;
use super::pio::PioDevice;

#[allow(unused_imports)]
use log::{error, warn, info, debug, trace, Level};

/// Implement this trait to receive data from [`PioOutput`].
pub trait PioSink {
    /// Attempt to flush the next byte, return whether flushing succeeds.
    fn flush(&mut self, data: u8) -> bool;
}

/// Implement this trait to provide data for [`PioInput`].
pub trait PioStream {
    /// Attempt to slurp the next byte from the stream.
    fn slurp(&mut self) -> Option<u8>;
}

/// Input-only `PIO` channel.
pub struct PioInput<T> {
    source: T,
    ready: bool
}

/// Output-only `PIO` channel.
pub struct PioOutput<T> {
    sink: T,
    pending: Option<u8>
}

impl<T: PioStream> PioInput<T> {
    /// Return a new instance.
    ///
    /// Provide the input stream `source` instance implementing [`PioStream`].
    pub fn new(source: T) -> PioInput<T> {
        PioInput { source, ready: false }
    }
    /// Destruct `self` and return a stream `source`.
    pub fn into_inner(self) -> T {
        self.source
    }
}

impl<T: PioSink> PioOutput<T> {
    /// Return a new instance.
    ///
    /// Provide the output `sink` instance implementing [`PioSink`].
    pub fn new(sink: T) -> PioOutput<T> {
        PioOutput { sink, pending: None }
    }
    /// Destruct `self` and return an output `sink`.
    pub fn into_inner(self) -> T {
        self.sink
    }
}

impl<T: PioStream> PioDevice for PioInput<T> {
    type Timestamp = Ts;

    fn set_floating_bus(&mut self, _ts: Self::Timestamp) {
        self.ready = true;
    }

    fn set_bus_data(&mut self, _data: u8, _ts: Self::Timestamp) {
        self.ready = false;
    }

    fn ready(&mut self, _ts: Self::Timestamp) {
        self.ready = true;
    }

    fn try_sample_data(&mut self, _ts: Self::Timestamp) -> Option<u8> {
        if self.ready {
            if let Some(data) = self.source.slurp() {
                self.ready = false;
                return Some(data)
            }
        }
        None
    }
}

impl<T: PioSink> PioDevice for PioOutput<T> {
    type Timestamp = Ts;

    fn set_floating_bus(&mut self, _ts: Self::Timestamp) {
        self.pending = None;
    }

    fn set_bus_data(&mut self, _data: u8, _ts: Self::Timestamp) {}

    fn ready_with_data(&mut self, data: u8, _ts: Self::Timestamp) {
        // trace!("ready_with_data: {:02x}", data);
        if !self.sink.flush(data) {
            self.pending = Some(data);
        }
    }

    fn was_strobe(&mut self, _ts: Self::Timestamp) -> bool {
        if let Some(data) = self.pending.take() {
            if !self.sink.flush(data) {
                self.pending = Some(data);
                return false;
            }
        }
        true
    }
}
