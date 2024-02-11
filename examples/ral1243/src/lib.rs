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
//! an example on how to implement emulators based on [z80emu](https://github.com/royaltm/rust-z80emu)
//! Z80 CPU emulator.
#![cfg_attr(not(feature = "std"), no_std)]
pub mod bus;
pub mod clock;
pub mod ctc;
pub mod ctc_trigger;
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

use z80emu::Cpu;

use bus::{Bus, Terminator};
use ctc::Ctc;
use ctc_trigger::{CtcActive, CtcPassive};
use memory::{Rom, Memory};
use pio::Pio;
use pio_device::{PioInput, PioOutput};

/// For implementations.
pub use clock::Ts;
pub use pio_device::{PioStream, PioSink};
pub use runner::FrameRunner;
#[cfg(feature = "std")]
pub use thread::RunnerMsg;

const MAX_PULSES: usize = 4;

type PioT<I, O> = Pio<Ts, PioInput<I>, PioOutput<O>, Terminator<Ts>>;
type CtcT<I, O> = Ctc<Ts,
    CtcActive<MAX_PULSES>,
    CtcPassive<MAX_PULSES>,
    CtcActive<MAX_PULSES>,
    CtcPassive<MAX_PULSES>,
    PioT<I, O>>;
type BusT<I, O> = Bus<CtcT<I, O>, Memory>;

const MEMORY_PORT_MASK: u16 = 0b11111111;
const MEMORY_PORT_BITS: u16 = 124;
const PIO_PORT_MASK: u16 = 0b11111100;
const PIO_PORT_BITS: u16 = 0b00001000;    // 8,9,10,11
const PIO_PORT_CHANNEL_SELECT: u32 = 1;
const PIO_PORT_CONTROL_SELECT: u32 = 0;
const CTC_PORT_MASK: u16 = 0b11111100;
const CTC_PORT_BITS: u16 = 0b00000100;    // 4,5,6,7
const CTC_PORT_CHANNEL_SELECT1: u32 = 1;
const CTC_PORT_CHANNEL_SELECT0: u32 = 0;

const ROM: &[u8] = include_bytes!("../rom/rom.bin");
/// For implementations.
pub const EX_ROM001: &[u8] = include_bytes!("../exroms/exrom001.bin");
/// For implementations.
pub const EX_ROM002: &[u8] = include_bytes!("../exroms/exrom002.bin");

/// The computer.
///
/// Require Cpu and implementations of [PioStream] and [PioSink].
///
/// * `EXT_HZ`: external clock frequency (for CTC) in Hz.
/// * `FRAME_HZ`: how many frames per second will be run.
pub struct Ral1243<C: Cpu, I: PioStream, O: PioSink,
        const EXT_HZ: u32 = 10_000,
        const FRAME_HZ: u32 = 500> {
    runner: FrameRunner<EXT_HZ, FRAME_HZ>,
    cpu: C,
    bus: BusT<I, O>,
}

/// Read EX-ROMS from a directory.
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

/// Read EX-ROM from a slice.
pub fn exrom_from_slice(exrom: &[u8]) -> Rom {
    Memory::make_rom(exrom)
}

impl<C: Cpu + Default, I: PioStream, O: PioSink,
     const EXT_HZ: u32, const FRAME_HZ: u32> Ral1243<C, I, O, EXT_HZ, FRAME_HZ>
{
    /// A single run frame duration.
    pub fn frame_duration() -> core::time::Duration {
        FrameRunner::<EXT_HZ, FRAME_HZ>::frame_duration()
    }
    /// Handy tool to validate CPU clock frequency.
    pub fn check_clock(clock_hz: Ts, max_clock_hz: Ts) -> Result<(), &'static str> {
        if !FrameRunner::<EXT_HZ, FRAME_HZ>::clock_is_valid(clock_hz) ||
            clock_hz > max_clock_hz
        {
            return Err("please specify clock within the acceptable range");
        }
        Ok(())
    }

    /// Handy tool to validate RAM size in kilobytes.
    pub fn check_ram_size(ramsizekb: usize) -> Result<(), &'static str> {
        if !(1..=Memory::MAXRAM_KB).contains(&ramsizekb) {
            return Err("please specify RAM between 1 and 48");
        }
        Ok(())
    }

    /// Return a new instance of the computer.
    ///
    /// Panics if `ramsizekb` or `clock_hz` are out of range or otherwise invalid.
    pub fn new<T>(ramsizekb: usize, clock_hz: Ts, exroms: Option<T>,
                  pio_stream: I, pio_sink: O) -> Self
        where T: IntoIterator<Item=Rom>,
              T::IntoIter: ExactSizeIterator
    {
        assert!((1..=Memory::MAXRAM_KB).contains(&ramsizekb));
        assert!(FrameRunner::<EXT_HZ, FRAME_HZ>::clock_is_valid(clock_hz));
        let cpu = C::default();
        let mut memory = Memory::new(ROM, ramsizekb);
        match exroms {
            Some(exroms) => memory.attach_exroms(exroms),
            None => {
                memory.attach_exrom(Memory::make_rom(EX_ROM001));
                memory.attach_exrom(Memory::make_rom(EX_ROM002));
            }
        }

        let runner = FrameRunner::new(clock_hz);

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

        let bus = Bus::new(ctc, memory).with_port_bits(
                    MEMORY_PORT_MASK, MEMORY_PORT_BITS);

        Ral1243 { runner, cpu, bus }
    }

    /// Resets all components and begins emulation.
    pub fn start(&mut self) {
        self.runner.start(&mut self.cpu, &mut self.bus);
    }

    /// Execute single step.
    pub fn step(&mut self) -> Ts {
        self.runner.step(&mut self.cpu, &mut self.bus)
    }

    /// Reset computer.
    pub fn reset(&mut self) {
        self.runner.reset(&mut self.cpu, &mut self.bus)
    }

    /// Trigger NMI, return whether succeeded.
    pub fn nmi(&mut self) -> bool {
        self.runner.nmi(&mut self.cpu, &mut self.bus)
    }
}
