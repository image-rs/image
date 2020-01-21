# Rust Image Release Notes

Rust image aims to be a pure-Rust implementation of various popular image formats. Accompanying reading/write support, rust image provides basic imaging processing function. See `README.md` for further details.

## Known issues
 - Interlaced (progressive) or animated images are not well supported.
 - Images with *n* bit/channel (*n ≠ 8*) are not well supported.

## Changes

### Version 0.23.0-preview.0

This major release intends to improve the interface with regards to handling of
color format data for both decoding and encoding. This necessitated many
breaking changes anyways so it was used to improve the compliance to the
interface guidelines such as outstanding renaming.

WIP: This preview version is intended to allow an evaluation and feedback on
the new interface. It is not yet perfect with regards to color spaces but it
was designed mainly as an improvement over the current interface with regards
to in-memory color formats, first. We'll get to color spaces in a later major
version.

- Heavily reworked `ColorType`:
  - This type is now used for denoting formats for which we support operations
      on buffers in these memory representations. Particularly, all channels in
      pixel types are assumed to be an integer number of bytes (In terms of the
      Rust type system, these are `Sized` and one can crate slices of channel
      values).
  - An `ExtendedColorType` (WIP: do You have better name?) is used to express
      more generic color formats for which the library has limited support but
      can be converted/scaled/mapped into a `ColorType` buffer. This operation
      might be fallible but, for example, includes sources with 1/2/4-bit
      components.
  - Both types are non-exhaustive to add more formats in a minor release.
  - A work-in-progress (#1085) will further separate the color model from the
      specific channel instantiation, e.g. both `8-bit RGB` and `16-bit BGR`
      are instantiations of `RGB` color model.
- Reworked the `ImageDecoder` trait:
  - `read_image` takes an output buffer argument instead of allocating all
      memory on its own.
  - The return type of `dimensions` now aligns with `GenericImage` sizes.
  - The `colortype` method was renamed to `color_type` for conformity.
- The result of `encode` operations is now uniformly an `ImageResult<()>`.
- Removed public converters from some `tiff` and `png` types. This allows
  upgrading the dependency across major versions without a major release in
  `image` itself.
- The enums `ColorType`, `DynamicImage`, `imageops::FilterType`, `ImageFormat`
  no longer re-export all of their variants in the top-level of the crate. This
  removes the growing pollution in the documentation and usage.
- The capitalization of types such as `HDRDecoder` etc. has been adjusted to
  adhere to the API guidelines. These are now spelled `HdrDecoder` etc. The
  same change has been made on `ImageFormat` and other enum variants.
- The `Progress` type has finally received public access functions. Strange
  that no one reported them missing.
- Introduced `PixelDensity` and `PixelDensityUnit` to store DPI information in
  formats that support encoding this form of metadata (e.g. in `jpeg`).

### Version 0.22.4

- Added in-place variants for flip and rotate operations.
- The bmp encoder now checks if dimensions are valid for the format. It would
  previously write a subset or panic.
- Removed deprecated implementations of `Error::description`
- Added `DynamicImage::into_*` which convert without an additional allocation.
- The PNG encoder errors on unsupported color types where it had previously
  silently swapped color channels.
- Enabled saving images as `gif` with `save_buffer`.

### Version 0.22.3

- Added a new module `io` containing a configurable `Reader`. It can replace
  the bunch of free functions: `image::{load_*, open, image_dimensions}` while
  enabling new combinations such as `open` but with format deduced from content
  instead of file path.
- Fixed `const_err` lint in the macro expanded implementations of `Pixel`. This
  can only affect your crate if `image` is used as a path dependency.

### Version 0.22.2

- Undeprecate `unsafe` trait accessors. Further evaluation showed that their
  deprecation should be delayed until trait `impl` specialization is available.
- Fixed magic bytes used to detect `tiff` images.
- Added `DynamicImage::from_decoder`.
- Fixed a bug in the `PNGReader` that caused an infinite loop.
- Added `ColorType::{bits_per_pixel, num_components}`.
- Added `ImageFormat::from_path`, same format deduction as the `open` method.
- Fixed a panic in the gif decoder.
- Aligned background color handling of `gif` to web browser implementations.
- Fixed handling of partial frames in animated `gif`.
- Removed unused direct `lzw` dependency, an indirect dependency in `tiff`.

### Version 0.22.1

- Fixed build without no features enabled

### Version 0.22

- The required Rust version is now `1.34.2`.
- Note the website and blog: [image-rs.org][1] and [blog.image-rs.org][2]
- `PixelMut` now only on `ImageBuffer` and removed from `GenericImage`
  interface. Prefer iterating manually in the generic case.
- Replaced an unsafe interface in the hdr decoder with a safe variant.
- Support loading 2-bit BMP images
- Add method to save an `ImageBuffer`/`DynamicImage` with specified format
- Update tiff to `0.3` with a writer
- Update png to `0.15`, fixes reading of interlaced sub-byte pixels
- Always use custom struct for `ImageDecoder::Reader`
- Added `apply_without_alpha` and `map_without_alpha` to `Pixel` trait
- Pixel information now with associated constants instead of static methods
- Changed color structs to tuple types with single component. Improves
  ergonomics of destructuring assignment and construction.
- Add lifetime parameter on `ImageDecoder` trait.
- Remove unecessary `'static` bounds on affine operations
- Add function to retrieve image dimensions without loading full image
- Allow different image types in overlay and replace
- Iterators over rows of `ImageBuffer`, mutable variants

[1]: https://www.image-rs.org
[2]: https://blog.image-rs.org

### Version 0.21.2

- Fixed a variety of crashes and opaque errors in webp
- Updated the png limits to be less restrictive
- Reworked even more `unsafe` operations into safe alternatives
- Derived Debug on FilterType and Deref on Pixel
- Removed a restriction on DXT to always require power of two dimensions
- Change the encoding of RGBA in bmp using bitfields
- Corrected various urls

### Version 0.21.1

- A fairly important bugfix backport
- Fixed a potentially memory safety issue in the hdr and tiff decoders, see #885
- See [the full advisory](docs/2019-04-23-memory-unsafety.md) for an analysis
- Fixes `ImageBuffer` index calculation for very, very large images
- Fix some crashes while parsing specific incomplete pnm images
- Added comprehensive fuzzing for the pam image types

### Version 0.21

- Updated README to use `GenericImageView`
- Removed outdated version number from CHANGES
- Compiles now with wasm-unknown-emscripten target
- Restructured `ImageDecoder` trait
- Updated README with a more colorful example for the Julia fractal
- Use Rust 1.24.1 as minimum supported version
- Support for loading GIF frames one at a time with `animation::Frames`
- The TGA decoder now recognizes 32 bpp as RGBA(8)
- Fixed `to_bgra` document comment
- Added release test script
- Removed unsafe code blocks several places
- Fixed overlay overflow bug issues with documented proofs

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
