/*
    ral1243: Emulator program as an example implementation for the z80emu library.
    Copyright (C) 2019-2024  Rafal Michalski

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

    Author contact information: see Cargo.toml file, section [package.authors].
*/
//! RAL 1243 is a fictional computer brought into its virtual existence to provide
//! an example of how to implement emulators based on the [Z80 CPU emulator].
//!
//! [Z80 CPU emulator]: https://github.com/royaltm/rust-z80emu
#![cfg_attr(not(feature = "std"), no_std)]
pub mod bus;
pub mod clock;
pub mod ctc;
pub mod ctc_trigger;
pub mod debug;
pub mod memory;
pub mod pio;
pub mod pio_device;
mod runner;
#[cfg(feature = "std")]
mod thread;

#[cfg(not(feature = "std"))]
extern crate alloc;

#[cfg(feature = "std")]
pub(crate) use std::{boxed, rc, vec};

#[cfg(not(feature = "std"))]
pub(crate) use alloc::{boxed, rc, vec};

#[cfg(feature = "std")]
use std::path::Path;
#[cfg(feature = "std")]
use std::{io, fs};
#[cfg(feature = "std")]
use io::Read;

use z80emu::{Z80, z80::Flavour, CpuDebug};

use bus::{Bus, Terminator};
use ctc::Ctc;
use ctc_trigger::{CtcActive, CtcPassive};
use memory::{Rom, Memory};
use pio::Pio;
use pio_device::{PioInput, PioOutput};

pub use clock::Ts;
pub use pio_device::{PioStream, PioSink};
pub use runner::FrameRunner;
#[cfg(feature = "std")]
pub use thread::RunnerMsg;

const MAX_PULSES: usize = 4;

/// `PIO Z8420` device definition for `Ral1243`.
pub type PioT<I, O> = Pio<Ts, PioInput<I>, PioOutput<O>, Terminator<Ts>>;
/// `CTC Z8430` device definition for `Ral1243`.
pub type CtcT<I, O> = Ctc<Ts,
    CtcActive<MAX_PULSES>,
    CtcPassive<MAX_PULSES>,
    CtcActive<MAX_PULSES>,
    CtcPassive<MAX_PULSES>,
    PioT<I, O>>;
/// `Ral1243` memory and peripherals.
pub type BusT<I, O> = Bus<CtcT<I, O>, Memory>;

/// Memory control I/O port mask.
pub const MEMORY_PORT_MASK: u16 = 0b11111111;
/// Memory control I/O port address.
pub const MEMORY_PORT_BITS: u16 = 124;
/// PIO peripheral I/O port mask.
pub const PIO_PORT_MASK: u16 = 0b11111100;
/// PIO peripheral I/O port address.
pub const PIO_PORT_BITS: u16 = 0b00001000;    // 8,9,10,11
/// PIO peripheral I/O port mask bit number selecting a PIO channel.
pub const PIO_PORT_CHANNEL_SELECT: u32 = 1;
/// PIO peripheral I/O port mask bit number selecting between data and control access.
pub const PIO_PORT_CONTROL_SELECT: u32 = 0;
/// CTC peripheral I/O port mask.
pub const CTC_PORT_MASK: u16 = 0b11111100;
/// PIO peripheral I/O port address.
pub const CTC_PORT_BITS: u16 = 0b00000100;    // 4,5,6,7
/// PIO peripheral I/O port mask MSB bit number selecting a CTC channel.
pub const CTC_PORT_CHANNEL_SELECT1: u32 = 1;
/// PIO peripheral I/O port mask LSB bit number selecting a CTC channel.
pub const CTC_PORT_CHANNEL_SELECT0: u32 = 0;

/// `Ral1243` ROM binary.
pub const ROM: &[u8] = include_bytes!("../rom/rom.bin");
/// `Ral1243` EX-ROM 1 binary.
pub const EX_ROM001: &[u8] = include_bytes!("../exroms/exrom001.bin");
/// `Ral1243` EX-ROM 2 binary.
pub const EX_ROM002: &[u8] = include_bytes!("../exroms/exrom002.bin");

/// The `Ral1243` computer.
///
/// Requires one of `Z80` [`Flavour`]s and implementations of [PioStream] and [PioSink].
///
/// * `EXT_HZ`: external clock frequency (for `CTC`) in Hz.
/// * `FRAME_HZ`: in how many frames per second the emulation will run.
pub struct Ral1243<F: Flavour, I: PioStream, O: PioSink,
        const EXT_HZ: u32 = 10_000,
        const FRAME_HZ: u32 = 500> {
    runner: FrameRunner<EXT_HZ, FRAME_HZ>,
    cpu: Z80<F>,
    bus: BusT<I, O>,
}

/// Read `EX-ROMS` from a directory. `std` only.
#[cfg(feature = "std")]
pub fn read_exroms<P: AsRef<Path>>(dir: P) -> io::Result<Vec<Rom>> {
    let mut vec = Vec::new();
    let mut buf = Vec::with_capacity(Memory::ROMSIZE);
    for entry in fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_file() {
            if let Some(name) = entry.file_name().to_str() {
                if name.len() == 12 && name.starts_with("exrom") && name.ends_with(".bin") {
                    let mut f = fs::File::open(&path)?;
                    let metadata = f.metadata()?;
                    if metadata.len() > Memory::ROMSIZE as u64 {
                        return Err(io::Error::new(io::ErrorKind::Other, format!("Exrom is too big: {:?}", path)));
                    }
                    buf.clear();
                    f.read_to_end(&mut buf)?;
                    vec.push(Memory::make_rom(&buf));
                }
            }
        }
    }
    Ok(vec)
}

/// Create an `EX-ROM` from a slice.
pub fn exrom_from_slice(exrom: &[u8]) -> Rom {
    Memory::make_rom(exrom)
}

impl<F: Flavour, I: PioStream, O: PioSink,
     const EXT_HZ: u32, const FRAME_HZ: u32> Ral1243<F, I, O, EXT_HZ, FRAME_HZ>
{
    /// A real-time duration of a single emulated frame.
    pub fn frame_duration() -> core::time::Duration {
        FrameRunner::<EXT_HZ, FRAME_HZ>::frame_duration()
    }
    /// Validate whether a given `CPU` clock frequency `clock_hz` can be used with
    /// the emulator. `max_clock_hz` provides an upper limit of the clock.
    /// Returns an error with a message if the check fails.
    /// 
    /// While the upper limit depends on how fast the `Z80` can be emulated on a given
    /// hardware, `EX-ROM 1` program will report wrong `CPU` frequency if it's above
    /// `40_000_000`.
    pub fn check_clock(clock_hz: Ts, max_clock_hz: Ts) -> Result<(), &'static str> {
        if !FrameRunner::<EXT_HZ, FRAME_HZ>::clock_is_valid(clock_hz) ||
            clock_hz > max_clock_hz
        {
            return Err("please specify clock within the acceptable range");
        }
        Ok(())
    }

    /// Validate whether a given RAM size in kilobytes can be used with the emulator.
    /// Returns an error with a message if the check fails.
    pub fn check_ram_size(ramsizekb: usize) -> Result<(), &'static str> {
        if !(1..=Memory::MAXRAM_KB).contains(&ramsizekb) {
            return Err("please specify RAM between 1 and 48");
        }
        Ok(())
    }
    /// Return a new instance of the `Ral1243` computer.
    /// 
    /// Provide the user RAM size in kilobytes and the `CPU` clock frequency.
    /// The frequency must be divisible by `EXT_HZ * 2`.
    /// 
    /// Optionally an iterator that yields `EX-ROMS` as [`Rom`] instances can be
    /// given. If not provided `EX_ROM001` and `EX_ROM002` will be inserted by default.
    /// 
    /// * `pio_stream` should be an instance implementing [`PioStream`].
    /// * `pio_sink` should be an instance implementing [`PioSink`].
    /// 
    /// **Panics** if `ramsizekb` or `clock_hz` are out of range or otherwise invalid.
    pub fn new<T>(
            ramsizekb: usize,
            clock_hz: Ts,
            exroms: Option<T>,
            pio_stream: I,
            pio_sink: O
        ) -> Self
        where T: IntoIterator<Item=Rom>,
              T::IntoIter: ExactSizeIterator
    {
        assert!((1..=Memory::MAXRAM_KB).contains(&ramsizekb));
        assert!(FrameRunner::<EXT_HZ, FRAME_HZ>::clock_is_valid(clock_hz));
        let mut cpu = Z80::default();
        let mut memory = Memory::new(ROM, ramsizekb);
        match exroms {
            Some(exroms) => memory.attach_exroms(exroms),
            None => {
                memory.attach_exrom(Memory::make_rom(EX_ROM001));
                memory.attach_exrom(Memory::make_rom(EX_ROM002));
            }
        }

        let mut runner = FrameRunner::new(clock_hz);

        let pio_input = PioInput::new(pio_stream);
        let pio_output = PioOutput::new(pio_sink);
        let pio = Pio::new(pio_input, pio_output, Terminator::new())
                .with_port_bits(
                    PIO_PORT_MASK,
                    PIO_PORT_BITS,
                    PIO_PORT_CHANNEL_SELECT,
                    PIO_PORT_CONTROL_SELECT);

        let ctc_chain0 = CtcActive::new(runner.external_clock_tstates());
        let ctc_chain1 = ctc_chain0.new_passive();
        let ctc_chain2 = CtcActive::new(runner.external_clock_tstates());
        let ctc_chain3 = ctc_chain2.new_passive();
        let ctc = Ctc::new(ctc_chain0, ctc_chain1, ctc_chain2, ctc_chain3, pio)
                .with_port_bits(
                    CTC_PORT_MASK,
                    CTC_PORT_BITS,
                    CTC_PORT_CHANNEL_SELECT1,
                    CTC_PORT_CHANNEL_SELECT0);

        let mut bus = Bus::new(ctc, memory).with_port_bits(
                    MEMORY_PORT_MASK, MEMORY_PORT_BITS);

        runner.start(&mut cpu, &mut bus);
        Ral1243 { runner, cpu, bus }
    }

    /// Resets all components and begins a new emulation.
    pub fn start(&mut self) {
        self.runner.start(&mut self.cpu, &mut self.bus);
    }

    /// Run emulation for a period of a single frame.
    /// Return the duration of a frame in T-states.
    pub fn step(&mut self) -> Ts {
        self.runner.step(&mut self.cpu, &mut self.bus)
    }

    /// See [`FrameRunner::debug_preview`].
    pub fn debug_preview(&self) -> CpuDebug {
        self.runner.debug_preview(&self.cpu, &self.bus)
    }

    /// See [`FrameRunner::debug_step`].
    pub fn debug_step(&mut self) -> (Option<CpuDebug>, Ts) {
        self.runner.debug_step(&mut self.cpu, &mut self.bus)
    }

    /// See [`FrameRunner::debug_runto_int`].
    pub fn debug_runto_int(&mut self) -> (Option<CpuDebug>, Ts) {
        self.runner.debug_runto_int(&mut self.cpu, &mut self.bus)
    }

    /// See [`FrameRunner::debug_runto_ret`].
    pub fn debug_runto_ret(&mut self) -> (Option<CpuDebug>, Ts) {
        self.runner.debug_runto_ret(&mut self.cpu, &mut self.bus)
    }

    /// See [`FrameRunner::run_until_brkpt`].
    pub fn run_until_brkpt(&mut self, brkpts: &[u16]) -> (Option<usize>, Ts) {
        self.runner.run_until_brkpt(&mut self.cpu, &mut self.bus, brkpts)
    }

    /// Reset the computer.
    pub fn reset(&mut self) {
        self.runner.reset(&mut self.cpu, &mut self.bus)
    }

    /// Trigger non-maskable interrupt and return a number of T-states
    /// that it took on success.
    pub fn nmi(&mut self) -> Option<Ts> {
        self.runner.nmi(&mut self.cpu, &mut self.bus)
    }
}
