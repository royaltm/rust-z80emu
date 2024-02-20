/*
    ral1243: Emulator program as an example implementation for the z80emu library.
    Copyright (C) 2019-2024  Rafal Michalski

    For the full copyright notice, see the mod.rs file.
*/
//! Std thread runner for Ral1243.
use std::io::Write;
use std::time::{Duration, Instant};
use std::thread::{spawn, sleep, JoinHandle};
use std::sync::mpsc::{SyncSender, Receiver, TryRecvError};
use log::{debug};
use super::*;
use super::debug::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RunnerMsg {
    Terminate,
    Reset,
    Nmi,
    DebugNext,
    DebugRunIrq,
    Continue
}

impl PioStream for Receiver<u8> {
    /// Attempt to slurp the next byte from the stream.
    fn slurp(&mut self) -> Option<u8> {
        self.try_recv().ok()
    }
}

impl PioSink for SyncSender<u8> {
    /// Attempt to flush the next byte, return whether flushing succeeds.
    fn flush(&mut self, data: u8) -> bool {
        self.try_send(data).is_ok()
    }
}


impl<F: Flavour, const EXT_HZ: u32, const FRAME_HZ: u32>
    Ral1243<F, Receiver<u8>, SyncSender<u8>, EXT_HZ, FRAME_HZ>
{
    pub fn start_thread<T>(ramsizekb: usize, clock_hz: Ts, exroms: Option<T>,
                run_rx: Receiver<RunnerMsg>,
                pio_stream: Receiver<u8>, pio_sink: SyncSender<u8>) -> JoinHandle<()>
        where T: IntoIterator<Item=Rom> + Send + 'static,
              T::IntoIter: ExactSizeIterator
    {
        thread::spawn(move || {
            let mut computer = Self::new(ramsizekb, clock_hz, exroms, pio_stream, pio_sink);
            computer.run(run_rx);
        })
    }

    pub fn run(&mut self, run_rx: Receiver<RunnerMsg>) {
        self.start();
        let frame_duration: Duration = FrameRunner::<EXT_HZ, FRAME_HZ>::frame_duration();

        let mut nmi_request = false;

        let mut time = Instant::now();

        let mut total_ts = 0u64;
        let mut duration = Duration::ZERO;
        let mut frame_count = 0;

        loop {
            if nmi_request && self.nmi().is_some() {
                nmi_request = false;
            }

            let mtime = Instant::now();
            let delta_ts = self.step();
            let elapsed = mtime.elapsed();

            duration += elapsed;
            total_ts += u64::from(delta_ts);

            match run_rx.try_recv() {
                Ok(RunnerMsg::Terminate) => break,
                Ok(RunnerMsg::Reset) => {
                    nmi_request = false;
                    self.reset();
                }
                Ok(RunnerMsg::Nmi) => {
                    nmi_request = self.nmi().is_none();
                }
                Ok(RunnerMsg::DebugNext|RunnerMsg::DebugRunIrq) => {
                    if self.debug(&run_rx, &mut nmi_request) == RunnerMsg::Terminate {
                        break
                    }
                    duration = Duration::ZERO;
                    total_ts = 0;
                    frame_count = 0;
                    time = Instant::now();
                    continue
                }
                Ok(RunnerMsg::Continue)|
                Err(TryRecvError::Empty) => {},
                Err(TryRecvError::Disconnected) => break,
            }

            if let Some(duration) = frame_duration.checked_sub(time.elapsed()) {
                thread::sleep(duration);
            }
            time += frame_duration;

            frame_count += 1;
            if frame_count == FRAME_HZ * 5 {
                debug!("emulation max {:.4} MHz", total_ts as f64 / duration.as_secs_f64() / 1e6);
                duration = Duration::ZERO;
                total_ts = 0;
                frame_count = 0;
            }
        }
    }

    pub fn debug(&mut self, run_rx: &Receiver<RunnerMsg>, nmi_request: &mut bool) -> RunnerMsg {
        let stdout = io::stdout();
        let mut header_lines = u8::MAX;
        let mut last_ts = 0;
        loop {
            if *nmi_request {
                if let Some(ts) = self.nmi() {
                    last_ts = ts;
                    *nmi_request = false;
                }
            }

            let deb = self.debug_preview();
            {
                let mut handle = stdout.lock();
                let _ = write!(handle, "{}T: +{}        \r", Preview::of(&deb), last_ts);
                handle.flush().unwrap();
            }

            let (deb, ts) = match run_rx.recv() {
                Ok(RunnerMsg::Continue) => return RunnerMsg::Continue,
                Ok(RunnerMsg::Terminate)|
                Err(..) => return RunnerMsg::Terminate,
                Ok(RunnerMsg::Reset) => {
                    *nmi_request = false;
                    self.reset();
                    continue
                }
                Ok(RunnerMsg::Nmi) => {
                    *nmi_request = true;
                    continue
                }
                Ok(RunnerMsg::DebugRunIrq) => self.debug_runto_int(EXT_HZ),
                Ok(RunnerMsg::DebugNext) => self.debug_step(),
            };

            last_ts = ts;

            if let Some(deb) = deb {
                let mut handle = stdout.lock();
                if header_lines > 10 {
                    let _ = writeln!(handle, "{}", Header);
                    header_lines = 0;
                }
                else {
                    header_lines += 1;
                }
                let _ = writeln!(handle, "{}", Debugger::of(&deb, &self.cpu));
            }
        }
    }
}
