/*
    ral1243: Emulator program as an example implementation for the z80emu library.
    Copyright (C) 2019-2020  Rafal Michalski

    For the full copyright notice, see the mod.rs file.
*/
use std::rc::Rc;
use std::time::Instant;
use std::sync::mpsc::{SyncSender, Receiver, TrySendError};
use super::clock::Ts;
use super::run::TimeProxy;
use super::pio::PioDevice;

#[allow(unused_imports)]
use log::{error, warn, info, debug, trace, Level};

pub struct PioMessage {
    pub time: Instant,
    pub data: u8
}

pub struct PioInput {
    tp: Rc<TimeProxy>,
    rx: Receiver<PioMessage>,
    pending: Option<PioMessage>,
}

pub struct PioOutput {
    tp: Rc<TimeProxy>,
    tx: SyncSender<PioMessage>,
    pending: Option<PioMessage>,
}

impl PioMessage {
    pub fn new(data: u8) -> Self {
        let time = Instant::now();
        PioMessage { time, data }
    }
}

impl PioInput {
    pub fn new(rx: Receiver<PioMessage>, tp: Rc<TimeProxy>) -> PioInput {
        PioInput { tp, rx, pending: None }
    }

    fn fetch_next(&mut self) -> Option<PioMessage> {
        self.rx.try_recv().ok()
    }
}

impl PioDevice for PioInput {
    type Timestamp = Ts;

    fn set_floating_bus(&mut self, _ts: Self::Timestamp) {}

    fn ready(&mut self, _ts: Self::Timestamp) {
        if self.pending.is_none() {
            self.pending = self.fetch_next();
        }
    }

    fn try_sample_data(&mut self, timestamp: Self::Timestamp) -> Option<u8> {
        if let Some(msg) = self.pending.take().or_else(|| self.fetch_next()) {
            let ts = self.tp.instant_to_timestamp(msg.time);
            if ts <= timestamp {
               return Some(msg.data)
            }
            else {
                self.pending = Some(msg);
            }
        }
        None
    }

    fn set_bus_data(&mut self, _data: u8, _ts: Self::Timestamp) {
        warn!("output not supported {}", _data);
    }
    fn ready_with_data(&mut self, _data: u8, _ts: Self::Timestamp) {}
    fn was_strobe(&mut self, _ts: Self::Timestamp) -> bool { false }
}

impl PioOutput {
    pub fn new(tx: SyncSender<PioMessage>, tp: Rc<TimeProxy>) -> PioOutput {
        PioOutput { tp, tx, pending: None }
    }

    pub fn push_pending(&mut self) -> bool {
        if let Some(msg) = self.pending.take() {
            match self.tx.try_send(msg){
                Ok(()) => true,
                Err(TrySendError::Full(msg))|
                Err(TrySendError::Disconnected(msg)) => {
                    self.pending = Some(msg);
                    false
                }
            }
        }
        else {
            true
        }
    }
}

impl PioDevice for PioOutput {
    type Timestamp = Ts;
    fn set_floating_bus(&mut self, _ts: Self::Timestamp) {
        warn!("input not supported");
    }
    fn ready(&mut self, _ts: Self::Timestamp) {}
    fn try_sample_data(&mut self, _ts: Self::Timestamp) -> Option<u8> { None }
    fn set_bus_data(&mut self, _data: u8, _ts: Self::Timestamp) {}
    fn ready_with_data(&mut self, data: u8, timestamp: Self::Timestamp) {
        // trace!("ready_with_data: {:02x}", data);
        if self.pending.is_none() {
            let time = self.tp.timestamp_to_instant(timestamp);
            self.pending = Some(PioMessage { time, data });
            self.push_pending();
        }
        else {
            warn!("pio output dropped");
        }
    }
    fn was_strobe(&mut self, _ts: Self::Timestamp) -> bool {
        self.push_pending()
    }
}
