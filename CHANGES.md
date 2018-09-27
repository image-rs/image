# Rust Image Release Notes

Rust image aims to be a pure-Rust implementation of various popular image formats. Accompanying reading/write support, rust image provides basic imaging processing function. See `README.md` for further details.

## Known issues
 - Interlaced (progressive) or animated images are not well supported.
 - Images with *n* bit/channel (*n ≠ 8*) are not well supported.

## Changes

### Version 0.20

- Clippy lint pass
- Updated num-rational dependency
- Added BGRA and BGR color types
- Improved performance of image resizing
- Improved PBM decoding
- PNM P4 decoding now returns bits instead of bytes
- Fixed move of overlapping buffers in BMP decoder
- Fixed some document comments
- `GenericImage` and `GenericImageView` is now object-safe
- Moved TIFF code to its own library
- Fixed README examples
- Fixed ordering of interpolated parameters in TIFF decode error string
- Thumbnail now handles upscaling
- GIF encoding for multiple frames
- Improved subimages API
- Cargo fmt fixes

### Version 0.19

- Fixed panic when blending with alpha zero.
- Made `save` consistent.
- Consistent size calculation.
- Fixed bug in `apply_with_alpha`.
- Implemented `TGADecoder::read_scanline`.
- Use deprecated attribute for `pixels_mut`.
- Fixed bug in JPEG grayscale encoding.
- Fixed multi image TIFF.
- PNM encoder.
- Added `#[derive(Hash)]` for `ColorType`.
- Use `num-derive` for `#[derive(FromPrimitive)]`.
- Added `into_frames` implementation for GIF.
- Made rayon an optional dependency.
- Fixed issue where resizing image did not give exact width/height.
- Improved downscale.
- Added a way to expose options when saving files.
- Fixed some compiler warnings.
- Switched to lzw crate instead of using built-in version.
- Added `ExactSizeIterator` implementations to buffer structs.
- Added `resize_to_fill` method.
- DXT encoding support.
- Applied clippy suggestions.

### Version 0.4
 - Various improvements.
 - Additional supported image formats (BMP and ICO).
 - GIF and PNG codec moved into separate crates.

### Version 0.3
 - Replace `std::old_io` with `std::io`.

### Version 0.2
 - Support for interlaced PNG images.
 - Writing support for GIF images (full color and paletted).
 - Color quantizer that converts 32bit images to paletted including the alpha channel.
 - Initial support for reading TGA images.
 - Reading support for TIFF images (packbits and FAX compression not supported).
 - Various bug fixes and improvements.

### Version 0.1
- Initial release
- Basic reading support for png, jpeg, gif, ppm and webp.
- Basic writing support for png and jpeg.
- A collection of basic imaging processing function like `blur` or `invert`
