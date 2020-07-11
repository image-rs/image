//! This crate provides native rust implementations of
//! image encoders and decoders and basic image manipulation
//! functions.
//!
//! Additional documentation can currently also be found in the
//! [README.md file which is most easily viewed on github](https://github.com/image-rs/image/blob/master/README.md).
//!
//! [Jump forward to crate content](#reexports)
//!
//! # Overview
//!
//! There are two core problems for which this library provides solutions: a unified interface for image
//! encodings and simple generic buffers for their content. It's possible to use either feature
//! without the other. The focus is on a small and stable set of common operations that can be
//! supplemented by other specialized crates. The library also prefers safe solutions with few
//! dependencies.
//!
//! | Format | Decoding | Encoding |
//! | ------ | -------- | -------- |
//! | PNG    | All supported color types | Same as decoding |
//! | JPEG   | Baseline and progressive | Baseline JPEG |
//! | GIF    | Yes | Yes |
//! | BMP    | Yes | RGB(8), RGBA(8), Gray(8), GrayA(8) |
//! | ICO    | Yes | Yes |
//! | TIFF   | Baseline(no fax support) + LZW + PackBits | RGB(8), RGBA(8), Gray(8) |
//! | WebP   | Lossy(Luma channel only) | No |
//! | PNM    | PBM, PGM, PPM, standard PAM | Yes |
//! | DDS    | DXT1, DXT3, DXT5 | No |
//! | TGA    | Yes | RGB(8), RGBA(8), BGR(8), BGRA(8), Gray(8), GrayA(8) |
//! | farbfeld | Yes | Yes |
//!
//! ## Using images decoders
//!
//! There exists a huge variety of image formats that are concerned with efficiently encoding image
//! pixel data and auxiliary meta data for many different purposes. The `image` library provides
//! decoders for many common formats, depending on the active features. The best way to use them
//! depends on your use case.
//!
//! * [`open`] is a very simple way to load images from the file system, automatically deducing the
//!   format but offering little customization.
//! * [`load_from_memory`], [`load_from_memory_with_format`] present a similar interface for images
//!   whose encoded data is already present in memory.
//! * [`io::Reader`] is a builder providing a superset of the functions. It offers both
//!   customization and auto-deduction but is slightly more involved. The main benefit is that the
//!   interface is easier to evolve.
//! * [`ImageDecoder`] is a trait for querying meta data and reading image pixels into a generic
//!   byte buffer. It also contains a `Read` adaptor for stream reading the pixels.
//! * [`DynamicImage::from_decoder`] can be used for creating a buffer from a single specific or
//!   any custom decoder implementing the [`ImageDecoder`] trait.
//!
//! [`open`]: fn.open.html
//! [`load_from_memory`]: fn.load_from_memory.html
//! [`load_from_memory_with_format`]: fn.load_from_memory_with_format.html
//! [`io::Reader`]: io/struct.Reader.html
//! [`DynamicImage::from_decoder`]: enum.DynamicImage.html#method.from_decoder
//! [`ImageDecoder`]: trait.ImageDecoder.html
//!
//! ## Using image encoders
//!
//! Encoding pixel data is supported for the majority of formats but not quite as broadly.
//!
//! * [`DynamicImage::save`] is the converse of `open` and stores a `DynamicImage`.
//! * [`DynamicImage::write_to`] can be used to encode an image into any writer, for example into a
//!   vector of bytes in memory.
//! * [`save_buffer`], [`save_buffer_with_format`] provide a low-level interface for saving an image
//!   in the file system, where the library initializes the chosen encoder.
//! * [`ImageEncoder`] is a trait for encoding a byte buffer of image data and the inverse of the
//!   `ImageDecoder` interface.
//!
//! [`save_buffer`]: #fn.save_buffer.html
//! [`save_buffer_with_format`]: #fn.save_buffer_with_format.html
//! [`DynamicImage::save`]: enum.DynamicImage.html#method.save
//! [`DynamicImage::write_to`]: enum.DynamicImage.html#method.write_to
//! [`ImageEncoder`]: trait.ImageEncoder.html
//!
//! ## Image buffers
//!
//! The library adds containers for channel data which together form some representation of a 2D
//! matrix of pixels. These are all statically typed to avoid misinterpretation of byte data (and
//! since Rust has no standard safe encapsulation for reinterpreting byte slices as another type).
//! The main traits [`GenericImageView`] and [`GenericImage`] model a view on a 2D-matrix of
//! addressable pixels and a buffer of independently accessible pixels respectively.
//!
//! The two main types for owning pixel data are [`ImageBuffer`] and [`DynamicImage`]. Note that
//! the latter is an enum over well-supported pixel types that also offers conversion
//! functionality.
//!
//! Additionally, the [`flat`] module contains items for interoperability with generic channel
//! matrices and foreign interface. While still strictly typed, these dynamically validate length and
//! other layout assumptions required to provide the trait interface. While they are quite generic, you
//! should be prepared for a bit of boilerplate when using these types.
//!
//! [`GenericImageView`]: trait.GenericImageView.html
//! [`GenericImage`]: trait.GenericImage.html
//! [`ImageBuffer`]: struct.ImageBuffer.html
//! [`DynamicImage`]: enum.DynamicImage.html
//! [`flat`]: flat/index.html
//!
//! ## A note on format specific features
//!
//! One of the main goals of `image` is stability, in runtime but also for programmers. This
//! ensures that performance as well as safety fixes reach a majority of its user base with little
//! effort. Re-exporting all details of its dependencies would run counter to this goal as it
//! linked _all_ major version bumps between them and `image`. As such, we are wary of exposing too
//! many details, or configuration options, that are not shared between different image formats.
//!
//! Nevertheless, the advantage of precise control is hard to ignore. We will thus consider
//! _wrappers_, not direct re-exports, in either of the following cases:
//!
//! 1. A standard specifies that configuration _x_ is required for decoders/encoders and there
//!    exists an essentially canonical way to control it.
//! 2. At least two different implementations agree on some (sub-)set of features in practice.
//! 3. A technical argument including measurements of the performance, space benefits, or otherwise
//!    objectively quantified benefits can be made, and the added interface is unlikely to require
//!    breaking changes.
//!
//! Features that fulfill two or more criteria are preferred.
//!
//! Re-exports of dependencies that reach version `1` will be discussed when it happens.

#![warn(missing_docs)]
#![warn(unused_qualifications)]
#![deny(unreachable_pub)]
#![deny(deprecated)]
#![deny(missing_copy_implementations)]
#![cfg_attr(all(test, feature = "benchmarks"), feature(test))]
// it's a bit of a pain otherwise
#![allow(clippy::many_single_char_names)]

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

// Image codecs
#[cfg(feature = "bmp")]
pub mod bmp;
#[cfg(feature = "dds")]
pub mod dds;
#[cfg(feature = "dxt")]
pub mod dxt;
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
#[cfg(feature = "farbfeld")]
pub mod farbfeld;

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
