Z80 emu
=======

[![Crate][Crate img]][Crate Link]
[![Docs][Docs img]][Docs Link]
[![Build Status][Build img]][Build Link]
[![Minimum rustc version][rustc version img]][rustc version link]
[![License][License img]][License Link]

`z80emu` is a [Rust](https://www.rust-lang.org) library which provides building blocks for emulators based on
Zilog's [Z80 CPU](https://en.wikipedia.org/wiki/Zilog_Z80).


```text
  _______
=|       |=
=|       |=                               
=|       |= ---------------- =[   Clock   ]
=|       |=                         |
=|       |=                         |
=|       |=                         |
=|       |=                         |
=|       |=                         |
=|  Cpu  |=                    _____|_____
=|       |=                   |           |
=|  Z80  |= \                 |           |
=|       |= <--------------> =| Memory+Io |=:::::
=|       |= /                 |           |
=|       |=                   |___________|
=|       |=
=|       |=
=|       |=
=|       |=
=|       |=
=|_______|=
```

`z80emu` was developed as an attempt to create a minimalistic emulation library. It provides the necessary tools
for the retro emulators to be built upon, avoiding any assumptions about side effects of those emulators.

Please see the [documentation][Docs Link] for a full introduction.

This repository also contains an [example implementation](examples/ral1243) of a complete emulator program.

Rust Version Requirements
-------------------------

`z80emu` requires Rustc version 1.36 or greater due to the usage of some macro features and api that was introduced
or stabilized in this version.


[Crate Link]: https://crates.io/crates/z80emu
[Crate img]: https://img.shields.io/crates/v/z80emu.svg
[Docs Link]: https://docs.rs/z80emu
[Docs img]: https://docs.rs/z80emu/badge.svg
[Build img]: https://travis-ci.org/royaltm/rust-z80emu.svg?branch=master
[Build Link]: https://travis-ci.org/royaltm/rust-z80emu
[rustc version img]: https://img.shields.io/badge/rustc-1.36+-lightgray.svg
[rustc version link]: https://github.com/royaltm/rust-z80emu#rust-version-requirements
[License img]: https://img.shields.io/crates/l/z80emu
[License Link]: LICENSE.md
