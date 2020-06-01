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
