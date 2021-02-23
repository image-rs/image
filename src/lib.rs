//! # Overview
//!
//! This crate provides native rust implementations of image encoding and decoding as well as some
//! basic image manipulation functions. Additional documentation can currently also be found in the
//! [README.md file which is most easily viewed on
//! github](https://github.com/image-rs/image/blob/master/README.md).
//!
//! There are two core problems for which this library provides solutions: a unified interface for image
//! encodings and simple generic buffers for their content. It's possible to use either feature
//! without the other. The focus is on a small and stable set of common operations that can be
//! supplemented by other specialized crates. The library also prefers safe solutions with few
//! dependencies.
//!
//! # High level API
//!
//! Load images using [`io::Reader`]:
//!
//! ```rust,no_run
//! # use std::io::Cursor;
//! use image::io::Reader as ImageReader;
//! # fn main() -> Result<(), image::ImageError> {
//! # let bytes = vec![0u8];
//!
//! let img = ImageReader::open("myimage.png")?.decode()?;
//! let img2 = ImageReader::new(Cursor::new(bytes)).decode()?;
//! # Ok(())
//! # }
//! ```
//!
//! And save them using [`save`] or [`write_to`] methods:
//!
//! ```rust,no_run
//! # use std::io::Write;
//! # use image::ImageOutputFormat;
//! # use image::DynamicImage;
//! # #[cfg(feature = "png")]
//! # fn main() -> Result<(), image::ImageError> {
//! # let img: DynamicImage = unimplemented!();
//! # let img2: DynamicImage = unimplemented!();
//! img.save("empty.jpg")?;
//!
//! let mut bytes: Vec<u8> = Vec::new();
//! img2.write_to(&mut bytes, image::ImageOutputFormat::Png)?;
//! # Ok(())
//! # }
//! # #[cfg(not(feature = "png"))] fn main() {}
//! ```
//!
//! With default features, the crate includes support for [many common image formats](codecs/index.html#supported-formats).
//!
//! [`save`]: enum.DynamicImage.html#method.save
//! [`write_to`]: enum.DynamicImage.html#method.write_to
//! [`io::Reader`]: io/struct.Reader.html
//!
//! # Image buffers
//!
//! The two main types for storing images:
//! * [`ImageBuffer`] which holds statically typed image contents.
//! * [`DynamicImage`] which is an enum over the supported ImageBuffer formats
//!     and supports conversions between them.
//!
//! As well as a few more specialized options:
//! * [`GenericImage`] trait for a mutable image buffer.
//! * [`GenericImageView`] trait for read only references to a GenericImage.
//! * [`flat`] module containing types for interoperability with generic channel
//!     matrices and foreign interfaces.
//!
//! [`GenericImageView`]: trait.GenericImageView.html
//! [`GenericImage`]: trait.GenericImage.html
//! [`ImageBuffer`]: struct.ImageBuffer.html
//! [`DynamicImage`]: enum.DynamicImage.html
//! [`flat`]: flat/index.html
//!
//! # Low level encoding/decoding API
//!
//! The [`ImageDecoder`] and [`ImageDecoderExt`] traits are implemented for many image file
//! formats. They decode image data by directly on raw byte slices. Given an ImageDecoder, you can
//! produce a DynamicImage via [`DynamicImage::from_decoder`].
//!
//! [`ImageEncoder`] provides the analogous functionality for encoding image data.
//!
//! [`DynamicImage::from_decoder`]: enum.DynamicImage.html#method.from_decoder
//! [`ImageDecoderExt`]: trait.ImageDecoderExt.html
//! [`ImageDecoder`]: trait.ImageDecoder.html
//! [`ImageEncoder`]: trait.ImageEncoder.html
#![warn(missing_docs)]
#![warn(unused_qualifications)]
#![deny(unreachable_pub)]
#![deny(deprecated)]
#![deny(missing_copy_implementations)]
#![cfg_attr(all(test, feature = "benchmarks"), feature(test))]
// it's a bit of a pain otherwise
#![allow(clippy::many_single_char_names)]
// it's a backwards compatibility break
#![allow(clippy::wrong_self_convention, clippy::enum_variant_names)]

#[cfg(all(test, feature = "benchmarks"))]
extern crate test;

#[cfg(test)]
#[macro_use]
extern crate quickcheck;

use std::io::Write;

pub use crate::color::{ColorType, ExtendedColorType};

pub use crate::color::{Luma, LumaA, Rgb, Rgba, Bgr, Bgra};

pub use crate::error::{ImageError, ImageResult};

pub use crate::image::{AnimationDecoder,
                GenericImage,
                GenericImageView,
                ImageDecoder,
                ImageDecoderExt,
                ImageEncoder,
                ImageFormat,
                ImageOutputFormat,
                Progress,
                // Iterators
                Pixels,
                SubImage};

pub use crate::buffer_::{
                 GrayAlphaImage,
                 GrayImage,
                 // Image types
                 ImageBuffer,
                 RgbImage,
                 RgbaImage};

pub use crate::flat::FlatSamples;

// Traits
pub use crate::traits::{EncodableLayout, Primitive, Pixel};

// Opening and loading images
pub use crate::io::free_functions::{guess_format, load};
pub use crate::dynimage::{load_from_memory, load_from_memory_with_format, open,
                   save_buffer, save_buffer_with_format, image_dimensions};

pub use crate::dynimage::DynamicImage;

pub use crate::animation::{Delay, Frame, Frames};

// More detailed error type
pub mod error;

/// Iterators and other auxiliary structure for the `ImageBuffer` type.
pub mod buffer {
    // Only those not exported at the top-level
    pub use crate::buffer_::{
        ConvertBuffer,
        EnumeratePixels,
        EnumeratePixelsMut,
        EnumerateRows,
        EnumerateRowsMut,
        Pixels,
        PixelsMut,
        Rows,
        RowsMut,
    };
}

// Math utils
pub mod math;

// Image processing functions
pub mod imageops;

// Io bindings
pub mod io;

// Buffer representations for ffi.
pub mod flat;

/// Encoding and decoding for various image file formats.
///
/// # Supported formats
///
/// | Format | Decoding | Encoding |
/// | ------ | -------- | -------- |
/// | PNG    | All supported color types | Same as decoding |
/// | JPEG   | Baseline and progressive | Baseline JPEG |
/// | GIF    | Yes | Yes |
/// | BMP    | Yes | RGB8, RGBA8, Gray8, GrayA8 |
/// | ICO    | Yes | Yes |
/// | TIFF   | Baseline(no fax support) + LZW + PackBits | RGB8, RGBA8, Gray8 |
/// | WebP   | Lossy(Luma channel only) | No |
/// | AVIF   | Only 8-bit | Lossy |
/// | PNM    | PBM, PGM, PPM, standard PAM | Yes |
/// | DDS    | DXT1, DXT3, DXT5 | No |
/// | TGA    | Yes | RGB8, RGBA8, BGR8, BGRA8, Gray8, GrayA8 |
/// | farbfeld | Yes | Yes |
///
/// ## A note on format specific features
///
/// One of the main goals of `image` is stability, in runtime but also for programmers. This
/// ensures that performance as well as safety fixes reach a majority of its user base with little
/// effort. Re-exporting all details of its dependencies would run counter to this goal as it
/// linked _all_ major version bumps between them and `image`. As such, we are wary of exposing too
/// many details, or configuration options, that are not shared between different image formats.
///
/// Nevertheless, the advantage of precise control is hard to ignore. We will thus consider
/// _wrappers_, not direct re-exports, in either of the following cases:
///
/// 1. A standard specifies that configuration _x_ is required for decoders/encoders and there
///    exists an essentially canonical way to control it.
/// 2. At least two different implementations agree on some (sub-)set of features in practice.
/// 3. A technical argument including measurements of the performance, space benefits, or otherwise
///    objectively quantified benefits can be made, and the added interface is unlikely to require
///    breaking changes.
///
/// Features that fulfill two or more criteria are preferred.
///
/// Re-exports of dependencies that reach version `1` will be discussed when it happens.
pub mod codecs {
    #[cfg(any(feature = "avif-encoder", feature = "avif-decoder"))]
    pub mod avif;
    #[cfg(feature = "bmp")]
    pub mod bmp;
    #[cfg(feature = "dds")]
    pub mod dds;
    #[cfg(feature = "dxt")]
    pub mod dxt;
    #[cfg(feature = "farbfeld")]
    pub mod farbfeld;
    #[cfg(feature = "gif")]
    pub mod gif;
    #[cfg(feature = "hdr")]
    pub mod hdr;
    #[cfg(feature = "ico")]
    pub mod ico;
    #[cfg(feature = "jpeg")]
    pub mod jpeg;
    #[cfg(feature = "png")]
    pub mod png;
    #[cfg(feature = "pnm")]
    pub mod pnm;
    #[cfg(feature = "tga")]
    pub mod tga;
    #[cfg(feature = "tiff")]
    pub mod tiff;
    #[cfg(feature = "webp")]
    pub mod webp;
}

#[cfg(feature = "avif-encoder")]
#[deprecated = "Use codecs::avif instead"]
pub mod avif {
    //! Encoding of AVIF images.
    pub use crate::codecs::avif::AvifEncoder;
}
#[cfg(feature = "bmp")]
#[deprecated = "Use codecs::bmp instead"]
pub mod bmp {
    //! Decoding and Encoding of BMP Images
    #[allow(deprecated)]
    pub use crate::codecs::bmp::{BMPEncoder, BmpDecoder, BmpEncoder};
}
#[cfg(feature = "dds")]
#[deprecated = "Use codecs::dds instead"]
pub mod dds {
    //! Decoding of DDS images
    pub use crate::codecs::dds::DdsDecoder;
}
#[cfg(feature = "dxt")]
#[deprecated = "Use codecs:: instead"]
pub mod dxt {
    //! Decoding of DXT (S3TC) compression
    #[allow(deprecated)]
    pub use crate::codecs::dxt::{
        DXTEncoder, DXTReader, DXTVariant, DxtDecoder, DxtEncoder, DxtReader, DxtVariant,
    };
}
#[cfg(feature = "farbfeld")]
#[deprecated = "Use codecs::farbfeld instead"]
pub mod farbfeld {
    //! Decoding of farbfeld images
    pub use crate::codecs::farbfeld::{FarbfeldDecoder, FarbfeldEncoder, FarbfeldReader};
}
#[cfg(feature = "gif")]
#[deprecated = "Use codecs::gif instead"]
pub mod gif {
    //! Decoding of GIF Images
    #[allow(deprecated)]
    pub use crate::codecs::gif::{Encoder, GifDecoder, GifEncoder, GifReader, Repeat};
}
#[cfg(feature = "hdr")]
#[deprecated = "Use codecs::hdr instead"]
pub mod hdr {
    //! Decoding of Radiance HDR Images
    #[allow(deprecated)]
    pub use crate::codecs::hdr::{
        read_raw_file, rgbe8, to_rgbe8, HDRAdapter, HDREncoder, HDRImageDecoderIterator,
        HDRMetadata, HdrAdapter, HdrDecoder, HdrEncoder, HdrImageDecoderIterator, HdrMetadata,
        HdrReader, RGBE8Pixel, Rgbe8Pixel, SIGNATURE,
    };
}
#[cfg(feature = "ico")]
#[deprecated = "Use codecs::ico instead"]
pub mod ico {
    //! Decoding and Encoding of ICO files
    #[allow(deprecated)]
    pub use crate::codecs::ico::{ICOEncoder, IcoDecoder, IcoEncoder};
}
#[cfg(feature = "jpeg")]
#[deprecated = "Use codecs::jpeg instead"]
pub mod jpeg {
    //! Decoding and Encoding of JPEG Images
    #[allow(deprecated)]
    pub use crate::codecs::jpeg::{
        JPEGEncoder, JpegDecoder, JpegEncoder, PixelDensity, PixelDensityUnit,
    };
}
#[cfg(feature = "png")]
#[deprecated = "Use codecs::png instead"]
pub mod png {
    //! Decoding and Encoding of PNG Images
    #[allow(deprecated)]
    pub use crate::codecs::png::{
        ApngDecoder, CompressionType, FilterType, PNGEncoder, PNGReader, PngDecoder, PngEncoder,
        PngReader,
    };
}
#[cfg(feature = "pnm")]
#[deprecated = "Use codecs::pnm instead"]
pub mod pnm {
    //! Decoding and Encoding of netpbm image formats (pbm, pgm, ppm and pam)
    #[allow(deprecated)]
    pub use crate::codecs::pnm::{
        ArbitraryHeader, ArbitraryTuplType, BitmapHeader, GraymapHeader, PNMEncoder, PNMHeader,
        PNMSubtype, PixmapHeader, PnmDecoder, PnmEncoder, PnmHeader, PnmSubtype, SampleEncoding,
    };
}
#[cfg(feature = "tga")]
#[deprecated = "Use codecs::tga instead"]
pub mod tga {
    //! Decoding and Encoding of TGA Images
    #[allow(deprecated)]
    pub use crate::codecs::tga::{TgaDecoder, TgaEncoder};
}
#[cfg(feature = "tiff")]
#[deprecated = "Use codecs::tiff instead"]
pub mod tiff {
    //! Decoding and Encoding of TIFF Images
    #[allow(deprecated)]
    pub use crate::codecs::tiff::{TiffDecoder, TiffEncoder, TiffReader};
}
#[cfg(feature = "webp")]
#[deprecated = "Use codecs::webp instead"]
pub mod webp {
    //! Decoding of WebP Images
    #[allow(deprecated)]
    pub use crate::codecs::webp::{vp8, WebPDecoder};
}


mod animation;
#[path = "buffer.rs"]
mod buffer_;
mod color;
mod dynimage;
mod image;
mod traits;
mod utils;

// Can't use the macro-call itself within the `doc` attribute. So force it to eval it as part of
// the macro invocation.
//
// The inspiration for the macro and implementation is from
// <https://github.com/GuillaumeGomez/doc-comment>
//
// MIT License
//
// Copyright (c) 2018 Guillaume Gomez
macro_rules! insert_as_doc {
    { $content:expr } => {
        #[doc = $content] extern { }
    }
}

// Provides the README.md as doc, to ensure the example works!
insert_as_doc!(include_str!("../README.md"));

// Copies data from `src` to `dst`
//
// Panics if the length of `dst` is less than the length of `src`.
#[inline]
fn copy_memory(src: &[u8], mut dst: &mut [u8]) {
    let len_src = src.len();
    assert!(dst.len() >= len_src);
    dst.write_all(src).unwrap();
}
