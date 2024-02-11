/*
    ral1243: Emulator program as an example implementation for the z80emu library.
    Copyright (C) 2019-2024  Rafal Michalski

    For the full copyright notice, see the mod.rs file.
*/
use super::clock::Ts;
use super::pio::PioDevice;

#[allow(unused_imports)]
use log::{error, warn, info, debug, trace, Level};

/// Implement this sink to receive data from [PioOutput].
pub trait PioSink {
    /// Attempt to flush the next byte, return whether flushing succeeds.
    fn flush(&mut self, data: u8) -> bool;
}

/// Implement this stream to provide data for [PioInput].
pub trait PioStream {
    /// Attempt to slurp the next byte from the stream.
    fn slurp(&mut self) -> Option<u8>;
}

/// Implements [PioDevice] as an input-only channel.
pub struct PioInput<T> {
    source: T,
    ready: bool
}

/// Implements [PioDevice] as an output-only channel.
pub struct PioOutput<T> {
    sink: T,
    pending: Option<u8>
}

impl<T: PioStream> PioInput<T> {
    pub fn new(source: T) -> PioInput<T> {
        PioInput { source, ready: false }
    }

}

impl<T: PioSink> PioOutput<T> {
    pub fn new(sink: T) -> PioOutput<T> {
        PioOutput { sink, pending: None }
    }
}

impl<T: PioStream> PioDevice for PioInput<T> {
    type Timestamp = Ts;
    /// input mode was set
    fn set_floating_bus(&mut self, _ts: Self::Timestamp) {
        self.ready = true;
    }
    /// output mode was set
    fn set_bus_data(&mut self, _data: u8, _ts: Self::Timestamp) {
        self.ready = false;
    }
    /// The READY goes high on read in input mode.
    fn ready(&mut self, _ts: Self::Timestamp) {
        self.ready = true;
    }
    /// If the STROBE had signalled sampling of any new data on the bus.
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
    /// input mode was set
    fn set_floating_bus(&mut self, _ts: Self::Timestamp) {
        self.pending = None;
    }
    /// output mode was set
    fn set_bus_data(&mut self, _data: u8, _ts: Self::Timestamp) {}
    /// The READY goes high on write in output mode.
    fn ready_with_data(&mut self, data: u8, _ts: Self::Timestamp) {
        // trace!("ready_with_data: {:02x}", data);
        if !self.sink.flush(data) {
            self.pending = Some(data);
        }
    }
    /// If the STROBE signalled the device is ready for more writes.
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
