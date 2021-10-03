name := 'z80emu'
llvm_profdata_exe := '$USERPROFILE/.rustup/toolchains/nightly-x86_64-pc-windows-msvc/lib/rustlib/x86_64-pc-windows-msvc/bin/llvm-profdata'

# profile benchmarks
bench-profgen:
    set -euxo pipefail
    # rustup component add llvm-tools-preview
    rm -rf tmp/pgo-data
    RUSTFLAGS="-Cprofile-generate=tmp/pgo-data" cargo +nightly bench --bench shuffle -- --nocapture
    {{llvm_profdata_exe}} merge -o tmp/pgo-data/merged.profdata tmp/pgo-data

# run all benchmarks with profiled optimizations
bench-prof:
    RUSTFLAGS="-Cllvm-args=-pgo-warn-missing-function -Cprofile-use=$(pwd)/tmp/pgo-data/merged.profdata" cargo +nightly bench --bench shuffle -- --nocapture

# run all benchmarks
bench:
    cargo +nightly bench --bench shuffle -- --nocapture

# run example
example:
    cargo run --example terminal --release -- examples/ral1243/exroms -m 48 -c 8000

# build all docs
doc:
    cargo +nightly doc -p z80emu --all-features

# run all tests
test:
    cargo test --no-default-features
    cargo test

# run all tests with no capture
test-nocapt:
    cargo test --no-default-features -- --nocapture
    cargo test -- --nocapture

# run clippy tests
clippy:
    touch src/lib.rs
    cargo clippy -- -D warnings
    cargo clippy --no-default-features -- -D warnings
