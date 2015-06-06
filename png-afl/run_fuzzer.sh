#!/bin/sh
DYLD_LIBRARY_PATH=$PWD/../../rust/x86_64-apple-darwin/stage2/lib cargo build --release
AFL_RS_CRASH_ON_PANIC=1 afl-fuzz -i samples -o fuzzer_out -- target/release/png-afl