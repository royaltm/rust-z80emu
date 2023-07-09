Z80 emu
=======

[![Crate][Crate img]][Crate Link]
[![Docs][Docs img]][Docs Link]
[![Build Status][Build img]][Build Link]
[![Coverage Status][Coverage img]][Coverage Link]
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

`z80emu` was developed as an attempt to create a minimalistic emulation library. It provides the necessary tools for the retro emulators to be built upon, avoiding any assumptions about the side effects of those emulators.

Please see the [documentation][Docs Link] for a full introduction.

The repository also contains an [example implementation](examples/ral1243) of a complete emulator program.

Another example built on top of `z80emu` is the [SPECTRUSTY](https://royaltm.github.io/spectrusty) library that focuses on "ZX Spectrum" emulators.


Rust Version Requirements
-------------------------

`z80emu` requires Rustc version 1.60 or greater due to the stabilization of const generics in this version.


Licensing
---------

Starting from version `0.6.0`, the `z80emu` library is released under the terms of the GNU Lesser General Public License (LGPL) version 3 or later.

Non-essential programs in this repository, i.e.: tests, examples, and benchmarks, are covered under different terms.

Please mind that each source file contains a copyright notice indicating the details.

[Crate Link]: https://crates.io/crates/z80emu
[Crate img]: https://img.shields.io/crates/v/z80emu.svg
[Docs Link]: https://docs.rs/z80emu
[Docs img]: https://docs.rs/z80emu/badge.svg
[Build Link]: https://github.com/royaltm/rust-z80emu/actions/workflows/ci.yml
[Build img]: https://github.com/royaltm/rust-z80emu/actions/workflows/ci.yml/badge.svg?branch=master
[rustc version link]: https://github.com/royaltm/rust-z80emu#rust-version-requirements
[rustc version img]: https://img.shields.io/badge/rustc-1.60+-lightgray.svg
[License Link]: https://www.gnu.org/licenses/#LGPL
[License img]: https://img.shields.io/crates/l/z80emu
[Coverage Link]: https://coveralls.io/github/royaltm/rust-z80emu?branch=master
[Coverage img]: https://coveralls.io/repos/github/royaltm/rust-z80emu/badge.svg?branch=master
