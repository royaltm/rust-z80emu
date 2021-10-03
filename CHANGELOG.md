v0.7.0
* added Justfile to demonstrate Profiled-Guided Optimizations for benchmarks
* CI: migration to Github Actions
* minimum supported rust version changed to Rust 1.51 (const generics)
* remove trailing semicolons in expression macro bodies and clippy suggested changes
* deps: arrayvec updated to 0.7 and bitflags to 1.3, dev-deps updated

v0.6.0
* z80emu is now released under the terms of the GNU LESSER GENERAL PUBLIC LICENSE 3.0 or later, starting from this version onward. The reason for this is the announcement of shutting down the License Zero project by Artless Devices.
* Only essential files are now being included in the packages published on crates.io.

v0.5.0
* Cpu methods for setting 16-bit registers from a pair of 8-bit values
* fixed deserialization of Z80 and Z80Any from streams

v0.4.0
* Z80Any enum
* fixed typo in Z80BM1 type definition
* flavour transitive (de)serialization

v0.3.0
* more compact serialization of RegisterPair
* relaxed deserialization of RegisterPair including signed int and hex strings
* transitional serialization and deserialization of Flavours
* conversion between Flavours (lossy)
* conversion between instances of Z80 with different flavours
* serde implementation for TsCounter
* serialize using camelCase field names, deserialize both camelCase and rust case field names
* reimplemented Debug trait for Z80 using formatter helpers

v0.2.0
* some functions have self passed by value instead of reference as suggested by clippy
* some other minor clippy suggested changes
* disasm module added
* deps: arrayvec updated to 0.5
