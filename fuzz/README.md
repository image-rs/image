# Fuzzing harnesses

This is intended for integration fuzzing and those decoders that do not yet
live in their own crate. `image-png` for example has their own fuzzing targets.

## Using the fuzzer

> $ cargo install afl
> $ cargo afl build
> $ cargo afl fuzz -i ./in/<format> -o ./out/<format> ./target/release/fuzzer_script_<format>

