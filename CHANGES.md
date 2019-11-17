
## 0.15.1

* Fix encoding writing invalid chunks. Images written can be corrected: see
  https://github.com/image-rs/image/issues/1074 for a recovery.
* Fix a panic in bit unpacking with checked arithmetic (e.g. in debug builds)
* Added better fuzzer integration
* Update `term`, `rand` dev-dependency
* Note: The `show` example program requires a newer compiler than 1.34.2 on
  some targets due to depending on `glium`. This is not considered a breaking
  bug.

## 0.15

Begin of changelog
