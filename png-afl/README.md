## Build fuzzer binary

RUSTFLAGS='-C codegen-units=1' cargo afl build

NOTE: the RUSTFLAGS is only needed on Linux (and not if using gold linker), see https://github.com/rust-lang/rust/issues/53945


## Run fuzzer

    cargo afl fuzz -m 200 -i fuzzing_seeds -o out target/debug/png-afl

NOTE: -m 200 sets memory limit to 200 mb. afl defaults to 50 megabytes memory usage. If we would not increase it, many invocations will exit with SIGABRT and look like crashes.


### More info

https://rust-fuzz.github.io/book/afl/tutorial.html
