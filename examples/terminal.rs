/*
    terminal: Example program for the z80emu library.
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
#[macro_use]
extern crate clap;

use std::str::FromStr;
use std::time::Duration;
use std::sync::mpsc::{sync_channel, channel, Receiver, SyncSender, RecvTimeoutError};
#[allow(unused_imports)]
use std::fs::File;
#[allow(unused_imports)]
use log::{error, warn, info, debug, trace};
use simplelog::*;
use clap::App;
use arrayvec::ArrayVec;
use pancurses::{initscr, endwin, Input, noecho, curs_set, raw, resize_term, beep};
use z80emu::z80::NMOS;
use ral1243::{read_exroms, RunnerMsg, Ts};

const DEFAULT_CLOCK_KHZ: Ts = 4_000;
/// How many frames / second.
const TIME_FRAME_HZ: u32 = 1000;
/// External clock frequency.
const EXT_CLOCK_HZ: u32 = 10_000;
/// Max CPU clock frequency.
const MAX_CLOCK_KHZ: u32 = 40_000_000;

type Ral1243 = ral1243::Ral1243<NMOS,
                        Receiver<u8>,
                        SyncSender<u8>,
                        EXT_CLOCK_HZ,
                        TIME_FRAME_HZ>;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    if cfg!(windows) {
        if let Err(_) = TermLogger::init(
                            LevelFilter::Debug,
                            Config::default(),
                            TerminalMode::Stdout,
                            ColorChoice::Auto)
        {
            SimpleLogger::init(LevelFilter::Debug, Config::default())?
        };
    }
    else {
        WriteLogger::init(LevelFilter::Info, Config::default(), File::create("terminal.log")?)?;
    }

    let matches = App::new("RAL1243 Terminal")
                  .version("2.0")
                  .author(crate_authors!())
                  .about("A terminal for RAL1243, a Z80 CPU based demonstration computer.")
                  .arg_from_usage("[exromdir]     'A path to a directory containing EX-ROM files'")
                  .arg_from_usage("-m, --ram=[kb] 'How many kilobytes of RAM'")
                  .arg_from_usage("-c, --clock=[kHz] 'CPU clock frequency in kHz'")
                  .get_matches();
    let ramsizekb: usize = matches.value_of("ram").map(usize::from_str).transpose()?.unwrap_or(16);
    let clock_hz: Ts = 1_000 * matches.value_of("clock").map(Ts::from_str).transpose()?.unwrap_or(DEFAULT_CLOCK_KHZ);
    let exroms = matches.value_of("exromdir").map(read_exroms).transpose()?;

    Ral1243::check_ram_size(ramsizekb)?;
    Ral1243::check_clock(clock_hz, MAX_CLOCK_KHZ)?;

    let window = initscr();
    window.keypad(true);
    // cbreak();
    noecho();
    raw();
    curs_set(0);
    window.printw(r#"
F1  - generates NMI signal
F4  - generates RESET signal

F5  - DEBUG next
F6  - DEBUG run to completion
F7  - DEBUG run to IRQ
F8  - RUN (exit DEBUG)

F10 - exit

press any key to start..."#);
    window.refresh();
    window.mv(window.get_cur_y(), 0);
    window.nodelay(false);
    window.getch();
    window.printw("Waiting for the system to boot up...\n");
    window.refresh();
    // window.draw_box(0,0);
    window.nodelay(true);
    let (runner_tx, runner_rx) = channel();
    let (pio_in_tx, pio_in_rx) = channel();
    let (pio_out_tx, pio_out_rx) = sync_channel(1);
    let mut need_refresh = false;

    let runner_handle = Ral1243::start_thread(runner_rx, ramsizekb, clock_hz, exroms, pio_in_rx, pio_out_tx);

    let mut control_multichar: Option<u8> = None;
    let mut collected: ArrayVec<u8,2> = ArrayVec::new();
    loop {
        match pio_out_rx.recv_timeout(Duration::from_micros(100)) {
            Ok(data) => {
                need_refresh = true;
                match control_multichar {
                    Some(0x10) => {
                        if collected.len() == 1 {
                            let x = data;
                            let y = collected.pop().unwrap();
                            control_multichar = None;
                            window.mv(y as i32, x as i32);
                        }
                        else {
                            collected.push(data);
                        }
                    }
                    Some(0x15) => {
                        control_multichar = None;
                        curs_set(data as i32);
                    }
                    _ => match data {
                        0x07 => { beep(); }
                        0x0C => { window.erase(); }
                        0x0D => { window.mv(window.get_cur_y(), window.get_beg_x()); }
                        c@0x10|c@0x15 => { control_multichar = Some(c) }
                        0x11 => { window.mv(window.get_cur_y().saturating_sub(1), window.get_cur_x()); }
                        0x12 => { window.mv(window.get_cur_y(), window.get_cur_x().saturating_sub(1)); }
                        0x0A|0x13 => { window.mv(window.get_cur_y()+1, window.get_cur_x()); }
                        0x14 => { window.mv(window.get_cur_y(), window.get_cur_x()+1); }
                        ch => { window.addch(ch as char); }
                    }
                }
            }
            Err(RecvTimeoutError::Timeout) => {
                if need_refresh {
                    window.refresh();
                    need_refresh = false;
                }
            }
            Err(RecvTimeoutError::Disconnected) => break
        }

        match window.getch() {
            Some(Input::Character(c)) => {
                debug!("{:?}", c);
                if c.is_ascii() {
                    if pio_in_tx.send(c as u8).is_err() { break; }
                }
            },
            Some(Input::KeyResize) => { resize_term(0, 0); window.refresh(); }
            Some(Input::KeyF10) => {
                info!("key: EXIT");
                break;
            }
            Some(Input::KeyF8) => {
                info!("key: RUN");
                if runner_tx.send(RunnerMsg::Continue).is_err() { break; }
            }
            Some(Input::KeyF7) => {
                info!("key: RUN IRQ");
                if runner_tx.send(RunnerMsg::DebugRunIrq).is_err() { break; }
            }
            Some(Input::KeyF6) => {
                info!("key: RUN to completion");
                if runner_tx.send(RunnerMsg::DebugCompletion).is_err() { break; }
            }
            Some(Input::KeyF5) => {
                if runner_tx.send(RunnerMsg::DebugNext).is_err() { break; }
            }
            Some(Input::KeyF4) => {
                info!("key: RESET");
                control_multichar = None;
                collected.clear();
                if runner_tx.send(RunnerMsg::Reset).is_err() { break; }
            }
            Some(Input::KeyF1) => {
                info!("key: NMI");
                control_multichar = None;
                collected.clear();
                if runner_tx.send(RunnerMsg::Nmi).is_err() { break; }
            }
            Some(input) => {
                if let Some(c) = match input {
                    Input::KeyPPage => Some(1),
                    Input::KeyHome  => Some(2),
                    Input::KeyEnd   => Some(3),
                    Input::KeyNPage => Some(4),
                    Input::KeyIC    => Some(5),
                    Input::KeyUp    => Some(17),
                    Input::KeyLeft  => Some(18),
                    Input::KeyDown  => Some(19),
                    Input::KeyRight => Some(20),
                    Input::KeyDC    => Some(127),
                    _ => {
                        debug!("{:?}", input);
                        None
                    }
                }
                {
                    if pio_in_tx.send(c).is_err() { break; }
                }
            }
            _ => {}
        }
    }

    if runner_tx.send(RunnerMsg::Terminate).is_ok() {
        runner_handle.join().unwrap();
    }

    endwin();
    Ok(())
}
