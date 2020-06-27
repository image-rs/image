
# Fuzzing `i686` on native `x86_64` Linux

It's possible and quite straightforward to fuzz 32-bit architecture on 64-bit
Linux systems with the following two steps:

```bash
$ rustup target add i686-unknown-linux-gnu
$ cargo +nightly fuzz run --target i686-unknown-linux-gnu decode --release -s none
```
