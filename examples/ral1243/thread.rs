//! Std thread runner for Ral1243.
use std::time::{Duration, Instant};
use std::thread::{spawn, sleep, JoinHandle};
use std::sync::mpsc::{SyncSender, Receiver, TryRecvError};
use super::Ral1243;
use super::runner::FrameRunner;
use super::*;
pub use pio_device::{PioStream, PioSink};

pub enum RunnerMsg {
    Terminate,
    Reset,
    Nmi
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


impl<C: Cpu + Default, const EXT_HZ: u32, const FRAME_HZ: u32>
    Ral1243<C, Receiver<u8>, SyncSender<u8>, EXT_HZ, FRAME_HZ>
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

        let mut time = Instant::now();

        loop {
            let _delta_ts = self.step();
            if let Some(duration) = frame_duration.checked_sub(time.elapsed()) {
                thread::sleep(duration);
            }
            time += frame_duration;
            match run_rx.try_recv() {
                Ok(RunnerMsg::Terminate) => break,
                Ok(RunnerMsg::Reset) => {
                    self.reset();
                }
                Ok(RunnerMsg::Nmi) => {
                    self.nmi(); // TODO
                }
                Err(TryRecvError::Empty) => {},
                Err(TryRecvError::Disconnected) => break,
            }
        }
    }
}
