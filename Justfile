name := 'z80emu'
shuffle_args := env_var_or_default('SHUFFLE_TESTS','nmos cmos bm1 anynmos anycmos anybm1 debug steps limit')
llvm_profdata_exe := replace(clean(`rustc --print target-libdir` / ".." / "bin" / "llvm-profdata"),'\','/')
target := replace_regex(trim_end_match(`rustup default`, ' (default)'), '^[^-]+-', '')
optimizations := '-Zno-parallel-llvm -Ccodegen-units=1'
mir_opts := '-Zmir-opt-level=4 -Zprint-fuel=z80emu'
mir_extra := '-Zinline-mir=yes -Zinline-mir-threshold=500 -Zinline-mir-hint-threshold=1000'

iter_default := '10'

# run RAL1243 example
example:
    cargo run --example terminal --release -- examples/ral1243/exroms -m 48 -c 8000

# run shuffle example
shuffle iters=iter_default:
    cargo run --example shuffle --release -- {{shuffle_args}} {{iters}}

# run shuffle example with full mir optimizations
shuffle-mir iters=iter_default:
    RUSTFLAGS="{{optimizations}} {{mir_opts}}" cargo +nightly-{{target}} run --example shuffle --release -- {{shuffle_args}} {{iters}}

# run shuffle example with full mir optimizations and mir inline
shuffle-mir-extra iters=iter_default:
    RUSTFLAGS="{{optimizations}} {{mir_opts}} {{mir_extra}}" cargo +nightly-{{target}} run --example shuffle --release -- {{shuffle_args}} {{iters}}

# profile run shuffle
shuffle-profgen iters=iter_default:
    @echo "TARGET: [{{target}}]"
    @echo "using {{llvm_profdata_exe}}"
    set -euxo pipefail
    rm -rf tmp/pgo-data
    RUSTFLAGS="-Cprofile-generate=tmp/pgo-data" cargo +nightly-{{target}} run --example shuffle --target="{{target}}" --release -- {{shuffle_args}} {{iters}}
    {{llvm_profdata_exe}} merge -o tmp/pgo-data/merged.profdata tmp/pgo-data

# run shuffle profiled
shuffle-prof iters=iter_default:
    RUSTFLAGS="-Cllvm-args=-pgo-warn-missing-function -Cprofile-use={{justfile_directory()}}/tmp/pgo-data/merged.profdata" cargo +nightly-{{target}} run --example shuffle --target="{{target}}" --release -- {{shuffle_args}} {{iters}}

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

clean:
    cargo clean
    rm -rf tmp/pgo-data
