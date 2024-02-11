Z80 Embedded
============

A small example of using z80emu with a `no_std` target.

The example can't possibly run as it lacks the linker script and hardware initialization.

The example serves as part of the testing process to detect if any of the dependencies require the std crate.

This example use [`cortex-m-rt`](https://crates.io/crates/cortex-m-rt) crate that targets a Cortex-M microcontroller.

Build it with one of the following targets:

* `thumbv6m-none-eabi`
* `thumbv7em-none-eabi`
* `thumbv7em-none-eabihf`
* `thumbv7m-none-eabi`
* `thumbv8m.base-none-eabi`
* `thumbv8m.main-none-eabi`
* `thumbv8m.main-none-eabihf`

For example:
```
cargo build -p z80emu-embedded-example --target thumbv7em-none-eabihf
```
