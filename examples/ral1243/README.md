RAL 1243
========

`RAL 1243` is a fictional computer brought into its virtual existence to provide an example on how to implement emulators based on [z80emu][z80emu repo] Z80 CPU emulator.

The computer has no graphics or a keyboard interface but instead communicates with the user via the PIO onboard chipset.


Library
-------

The `RAL 1243` computer is a library. Ready for embedded.


Usage
-----

An example using this library can be found in [z80emu/examples][z80emu examples], called `terminal`.


Memory
------

The memory map of `RAL 1243`:

* `0x0000`-`0x1FFF` occupies the `ROM` kernel.
* `0x2000`-`0x3FFF` occupies a swappable `EX-ROM` cardridges with user programs or a kernel only accessible `RAM` page.
* `0x4000`-`RAMTOP` Random Access Memory available for user programs.

Traps:

If the Program Counter is between `0x0000` and `0x1FFF` (inclusive) the `RAM` page is being swapped into memory page `0x2000`-`0x3FFF`. Otherwise, the currently swapped cartridge `ROM` is available on this memory page.


I/O
---

I/O is handled by the daisy-chained devices on the system `Bus` where the following peripherals are present:

### Memory Controller

* `IN (124)` - Reads the currently swapped in cartridge number.
* `OUT (124)` - Selects one of 256 swappable cartridges to be mapped at the memory page `0x2000`-`0x3FFF`. If the cartridge doesn't exist a `0xFF` byte-filled page appears instead.

### PIO Z8420

A single `PIO Z8420` chip controls the terminal input connected to its `Channel A` and the output from its `Channel B`.

__NOTE__: The implementation of the PIO chip emulates only `input` and `output` channel modes. The bi-directional and control modes are not currently supported.

* `IN (8)` - Reads a character from the terminal via `Channel` `A`.
* `OUT (9)` - `PIO` `Channel` `A` control.
* `OUT (10)` - Outputs a character to the terminal via `Channel` `B`.
* `OUT (11)` - `PIO` `Channel` `B` control.

`PIO` chip triggers interrupts signaling the terminal input and output data availability.

### CTC Z8430

A single `CTC Z8430` chip controls 4 timers/counters.

* `CLK/TRG` lines of channels `0` and `2` are connected to a (CPU clock independent) 100µs pulse (10 kHz) clock.
* the `ZT/CO` line of `Channel` `0` is connected to the `CLK/TRG` line of `Channel` `1`.
* the `ZT/CO` line of `Channel` `1` is currently not connected to anything.
* the `ZT/CO` line of `Channel` `2` is connected to the `CLK/TRG` line of `Channel` `3`.

* `IN (4)` - `CTC` `Channel` `0` current counter value.
* `OUT (4)` - `CTC` `Channel` `0` control.
* `IN (5)` - `CTC` `Channel` `1` current counter value.
* `OUT (5)` - `CTC` `Channel` `1` control.
* `IN (6)` - `CTC` `Channel` `2` current counter value.
* `OUT (6)` - `CTC` `Channel` `2` control.
* `IN (7)` - `CTC` `Channel` `3` current counter value.
* `OUT (7)` - `CTC` `Channel` `3` control.

The `CTC` chip may trigger interrupts on a countdown to `0`.


Interrupts
----------

`IM 2` interrupt mode must be always on.


System
------
The `RAM` memory area mapped between `0x2000` and `0x3FFF` is for exclusive use by the ROM kernel only.

The machine stack occupies the last bytes of `RAM` memory. `SP` is initiated to the last address of available `RAM` + 1 after boot. System refuse to start if no user `RAM` memory is detected.

* `RST 00h` - A soft system reset.
* `RST 08h` - Fetches the input character into Accumulator signaling a new character with `ZF=0`.
              The `A` register is being modified only on new input.
              The `HL'` registers are being modified.
              When called with `CF=1` waits until the next character is available, always succeeds.
              When called with `CF=0` returns immediately, signalling a possible failure with `ZF=1`.
* `RST 10h` - Outputs a single character given in Accumulator signalling the success with `ZF=1`.
              The `HL'` registers are being modified.
              When called with `CF=0` waits until the character could be buffered, always succeeds.
              When called with `CF=1` returns immediately, signalling a possible failure with `ZF=0`.
* `RST 18h` - Forwards a call to an address in `IX`.
* `RST 20h` - Forwards a call to an address in `IY`.
* `RST 28h` - Forwards to one of the syslib functions identified as an 8-bit function vector in the Accumulator.
              Modifies `A` and `HL'`. Functions may alter more registers.
              For a list of system library routines consult the `ROM` kernel source [rom/ral1243_rom.rb].
* `RST 30h` - Forwards a call to an address in `HL`.
* `RST 38h` - Back to system menu.
* `NMI` - Back to system menu if running the `EX-ROM` code.


Terminal
--------

The terminal forwards any user key input to the `RAL 1243` `PIO` input device.

Keys wired to control codes sent to `PIO` `A`:

| key name    | data |
|-------------|------|
|`PgUp`       |  `1` |
|`Home`       |  `2` |
|`End`        |  `3` |
|`PgDn`       |  `4` |
|`Ins`        |  `5` |
|`Backspace`  |  `8` |
|`Tab`        |  `9` |
|`Up`         | `17` |
|`Left`       | `18` |
|`Down`       | `19` |
|`Right`      | `20` |
|`Esc`        | `27` |
|`Del`        |`127` |

Any ASCII printable character is forwarded as such.

Data from a `PIO` output (`Channel` `B`) is being forwarded to the console.

Special output control codes interpreted by the terminal:

| code | action
|------|------------------------------------------
| `8`  | Moves the cursor back left.
| `10` | Moves the cursor to the next line.
| `12` | Clears the terminal.
| `13` | Moves the cursor to the first column.
| `16` | Moves the cursor to an absolute position; followed by a row index, followed by a column index.
| `17` | Moves cursor ↑ up
| `18` | Moves cursor ← left
| `19` | Moves cursor ↓ down
| `20` | Moves cursor → right
| `21` | Changes cursor appearanc; followed by a cursor shape number.

The cursor shapes:

* `0` - hidden
* `1` - underscore
* `2` - block

Depending on the terminal capability more shapes may be available.

[z80emu repo]: https://github.com/royaltm/rust-z80emu
[z80emu examples]: https://github.com/royaltm/rust-z80emu/tree/master/examples