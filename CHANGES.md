## 0.16.7

* Added `Encoder::set_trns` to register a transparency table to be written.

## 0.16.6

* Fixed silent integer overflows in buffer size calculation, resulting in
  panics from assertions and out-of-bounds accesses when actually decoding.
  This improves the stability of 32-bit and 16-bit targets and make decoding
  run as stable as on 64-bit.
* Reject invalid color/depth combinations. Some would lead to mismatched output
  buffer size and panics during decoding.
* Add `Clone` impl for `Info` struct.

## 0.16.5

* Decoding of APNG subframes is now officially supported and specified. Note
  that dispose ops and positioning in the image need to be done by the caller.
* Added encoding of indexed data.
* Switched to `miniz_oxide` for decompressing image data, with 30%-50% speedup
  in common cases and up to 200% in special ones.
* Fix accepting images only with consecutive IDAT chunks, rules out data loss.

## 0.16.4

* The fdAT frames are no longer inspected when the main image is read. This
  would previously be the case for non-interlaced images. This would lead to
  incorrect failure and, e.g. an error of the form `"invalid filter method"`.
* Fix always validating the last IDAT-chunks checksum, was sometimes ignored.
* Prevent encoding color/bit-depth combinations forbidden by the specification.
* The fixes for APNG/fdAT enable further implementation. The _next_ release is
  expected to officially support APNG.

## 0.16.3

* Fix encoding with filtering methods Up, Avg, Paeth
* Optimize decoding throughput by up to +30%

## 0.16.2

* Added method constructing an owned stream encoder.

## 0.16.1

* Addressed files bloating the packed crate

## 0.16.0

* Fix a bug compressing images with deflate
* Address use of deprecated error interfaces

## 0.15.3

* Fix panic while trying to encode empty images. Such images are no longer
  accepted and error when calling `write_header` before any data has been
  written. The specification does not permit empty images.

## 0.15.2

* Fix `EXPAND` transformation to leave bit depths above 8 unchanged

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
