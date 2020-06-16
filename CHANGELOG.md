v0.4.0
* Z80Any enum

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
