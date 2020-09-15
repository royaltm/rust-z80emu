/*
    ral1243: Emulator program as an example implementation for the z80emu library.
    Copyright (C) 2019-2020  Rafal Michalski

    For the full copyright notice, see the mod.rs file.
*/
use std::collections::VecDeque;
use std::cell::RefCell;
use std::rc::Rc;
use super::ctc::CtcTrigger;
use super::clock::Ts;
use super::run::TimeProxy;

pub type Triggers = Rc<RefCell<VecDeque<(u64,Ts,bool)>>>;

/// A struct to chain ZC/TO with the next Ctc channel's CLK/TRG
pub struct CtcChain {
    tp: Rc<TimeProxy>,
    triggers: Triggers,
    next: Option<Triggers>
}

impl CtcTrigger for CtcChain {
    type Timestamp = Ts;
    fn was_clk_trg(&mut self, rising_edge: bool, timestamp: Ts) -> Option<Ts> {
        let mut triggers = self.triggers.borrow_mut();
        while let Some(&(secs, ts, edge)) = triggers.front() {
            let ts = self.tp.normalized_timestamp_now(secs, ts);
            if ts <= timestamp {
                triggers.pop_front();
                if rising_edge == edge {
                    return Some(ts);
                }
            }
            else {
                break;
            }
        }
        None
    }

    fn purge_clk_trg(&mut self, timestamp: Ts) {
        self.triggers.borrow_mut().retain(|&(secs, ts, _)|
            self.tp.normalized_timestamp_now(secs, ts) > timestamp);
    }

    fn zc_to_pulse(&mut self, timestamp: Ts) {
        if let Some(next) = self.next.as_ref() {
            let mut triggers = next.borrow_mut();
            let seconds = self.tp.seconds();
            triggers.push_back((seconds, timestamp, true));
            triggers.push_back((seconds, timestamp + 2, false));
        }
    }
}

impl CtcChain {
    /// Returns a copy of reference to triggers.
    pub fn triggers(&self) -> Triggers {
        Rc::clone(&self.triggers)
    }

    /// Creates new CtcChain connecting its CLK/TRG line to ZT/CO output of self.
    pub fn new_chained(&mut self, tp: Rc<TimeProxy>) -> Self {
        let ctc_chain = Self::new(tp);
        assert!(self.next.is_none());
        self.next = Some(ctc_chain.triggers());
        ctc_chain
    }

    /// Creates new CtcChain.
    pub fn new(tp: Rc<TimeProxy>) -> Self {
        let triggers = Rc::new(RefCell::new(VecDeque::new()));
        CtcChain { tp, triggers, next: None }
    }
}
