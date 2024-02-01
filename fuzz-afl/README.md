# Fuzzing harnesses

This is intended for integration fuzzing and those decoders that do not yet
live in their own crate. `image-png` for example has their own fuzzing targets.

## Using the fuzzer

Install afl:

    $ cargo install afl

Build fuzz target:

    $ cargo afl build --bin fuzz_<format>

Run afl:

    $ mkdir out/<format>
    $ cargo afl fuzz -i ./in/<format> -o ./out/<format> ./target/debug/fuzz_<format>

To reproduce a crash:

    $ cargo run --bin reproduce_<format>


## Known issues

Since about Oct. 2021 the nightly Rust builds use an llvm version that no
longer accepts one of the sanitizer passes. As a temporary workaround you must
adjust the flags passed to `afl`:

    $ RUSTFLAGS="-Znew-llvm-pass-manager=no" cargo +nightly afl run …
