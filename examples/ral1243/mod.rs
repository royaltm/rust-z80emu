/*
    ral1243: Emulator program as an example implementation for the z80emu library.
    Copyright (C) 2019-2020  Rafal Michalski

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
mod clock;
mod bus;
mod memory;
mod run;
mod ctc;
mod ctc_trigger;
mod pio;
mod pio_device;

use std::path::Path;
use std::thread::{self, JoinHandle};
use std::sync::mpsc::{SyncSender, Receiver};
use std::{io, fs};
use io::Read;

use ctc_trigger::CtcChain;
pub use clock::Ts;
use run::{FrameRunner};
use bus::{Bus, BusDevice, Terminator};
use memory::{Rom, Memory};
use pio::Pio;
use pio_device::{PioInput, PioOutput};
use ctc::Ctc;
use z80emu::Cpu;

pub use run::RunnerMsg;
pub use pio_device::PioMessage;

type PioT = Pio<Ts, PioInput, PioOutput, Terminator<Ts>>;
type CtcT = Ctc<Ts, CtcChain, CtcChain, CtcChain, CtcChain, PioT>;

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

const ROM: &[u8] = include_bytes!("rom/rom.bin");
const EX_ROM001: &[u8] = include_bytes!("exroms/exrom001.bin");
const EX_ROM002: &[u8] = include_bytes!("exroms/exrom002.bin");

pub struct Ral1243<C: Cpu> {
    runner: FrameRunner,
    cpu: C,
    bus: Bus<CtcT, Memory>,
}

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

impl<C: Cpu + Default> Ral1243<C> {
    pub fn start_thread(ramsizekb: usize, clock_hz: Ts, mut exroms: Option<Vec<Rom>>,
                run_rx: Receiver<RunnerMsg>,
                pio_in: Receiver<PioMessage>, pio_out: SyncSender<PioMessage>) -> JoinHandle<()> {
        thread::spawn(move || {
            let mut computer = Self::new(ramsizekb, clock_hz, exroms.as_mut(), run_rx, pio_in, pio_out);
            computer.run();
        })
    }

    pub fn new(ramsizekb: usize, clock_hz: Ts, exroms: Option<&mut Vec<Rom>>,
                run_rx: Receiver<RunnerMsg>,
                pio_in: Receiver<PioMessage>, pio_out: SyncSender<PioMessage>) -> Self {
        let cpu = C::default();
        let mut memory = Memory::new(ROM, ramsizekb);
        match exroms {
            Some(exroms) => memory.attach_exroms(exroms),
            None => {
                memory.attach_exrom(Memory::make_rom(EX_ROM001));
                memory.attach_exrom(Memory::make_rom(EX_ROM002));
            }
        }

        let runner = FrameRunner::new(run_rx, clock_hz);

        let pio_input = PioInput::new(pio_in, runner.time_proxy());
        let pio_output = PioOutput::new(pio_out, runner.time_proxy());
        let pio = Pio::new(pio_input, pio_output, Terminator::new())
                .with_port_bits(PIO_PORT_MASK, PIO_PORT_BITS, PIO_PORT_CHANNEL_SELECT, PIO_PORT_CONTROL_SELECT);

        let mut ctc_chain0 = CtcChain::new(runner.time_proxy());
        let ctc_chain1 = ctc_chain0.new_chained(runner.time_proxy());
        let mut ctc_chain2 = CtcChain::new(runner.time_proxy());
        let ctc_chain3 = ctc_chain2.new_chained(runner.time_proxy());
        let ctc = Ctc::new(ctc_chain0, ctc_chain1, ctc_chain2, ctc_chain3, pio)
                .with_port_bits(CTC_PORT_MASK, CTC_PORT_BITS, CTC_PORT_CHANNEL_SELECT1, CTC_PORT_CHANNEL_SELECT0);

        let bus = Bus::new(ctc, memory).with_port_bits(MEMORY_PORT_MASK, MEMORY_PORT_BITS);

        Ral1243 { runner, cpu, bus }
    }

    pub fn run(&mut self) {
        let ctc_device = self.bus.next_device();
        let triggers0 = ctc_device.ctc0_trigger().triggers();
        let triggers2 = ctc_device.ctc2_trigger().triggers();
        let vec_of_triggers = vec![triggers0, triggers2];
        self.runner.run(&mut self.cpu, &mut self.bus, vec_of_triggers);
    }
}
