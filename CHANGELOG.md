v0.3.0
* more compact serialization of RegisterPair
* relaxed deserialization of RegisterPair including signed int and hex strings
* transitional serialization and deserialization of Flavours
* conversion between Flavours (lossy)
* conversion between instances of Z80 with different flavours
* serde implementation for TsCounter

v0.2.0
* some functions have self passed by value instead of reference as suggested by clippy
* some other minor clippy suggested changes
* disasm module added
* deps: arrayvec updated to 0.5
