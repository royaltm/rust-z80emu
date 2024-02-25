z80emu examples
===============

Shuffle
-------

`shuffle` is a benchmarking tool that runs `z80emu` performing various tasks like random shuffling and sorting with different algorithms an array of 256 bytes.

Usage:

```
cargo run -r --example shuffle -- ARGS MAX
```

where `ARGS` selects the `Z80` implementations:

*  `nmos`, `cmos`, `bm1`, `anynmos`, `anycmos`, `anybm1`

or mode of running:

* `debug` - execute next instruction in a loop with a debug closure,
* `steps` - execute next instruction in a loop without a debug closure,
* `limit` - run until limit is reached

and `MAX` limits the duration of each test in seconds.


Terminal
--------

Is a terminal application that runs the emulator of [`Ral1243`](ral1243/).

```
F1  - generates NMI signal
F4  - generates RESET signal

F5  - DEBUG next
F6  - DEBUG run to completion
F7  - DEBUG run to IRQ
F8  - RUN (exit DEBUG)

F10 - exit
```

Usage:

```
cargo run -r --example terminal -- --help
```

```
RAL1243 Terminal
A terminal for RAL1243, a Z80 CPU based demonstration computer.

USAGE:
    terminal [OPTIONS] [exromdir]

FLAGS:
    -h, --help       Prints help information
    -V, --version    Prints version information

OPTIONS:
    -c, --clock <kHz>    CPU clock frequency in kHz
    -m, --ram <kb>       How many kilobytes of RAM

ARGS:
    <exromdir>    A path to a directory containing EX-ROM files
```

To run the emulator with the built-in ex-ROMs, 16kb of user RAM and CPU clocked at 4MHz:

```
cargo run -r --example terminal
```

To run the emulator with 48kb RAM, the CPU at 8MHz and externally loaded ex-ROMs:

```
cargo run -r --example terminal -- examples/ral1243/exroms -m 48 -c 8000
```

... so you may enjoy "Sssshnake", the only game written for RAL 1243.
