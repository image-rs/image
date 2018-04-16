//! This crate provides native rust implementations of
//! image encoders and decoders and basic image manipulation
//! functions.

#![warn(missing_docs)]
#![warn(unused_qualifications)]
#![deny(missing_copy_implementations)]
#![cfg_attr(all(test, feature = "benchmarks"), feature(test))]

extern crate byteorder;
extern crate num_iter;
extern crate num_rational;
extern crate num_traits;
extern crate lzw;
#[macro_use]
extern crate num_derive;
#[cfg(all(test, feature = "benchmarks"))]
extern crate test;

#[cfg(test)]
#[macro_use]
extern crate quickcheck;

use std::io::Write;

pub use color::ColorType::{
    self,
    Gray,
    RGB,
    Palette,
    GrayA,
    RGBA
};

pub use color::{
    Luma,
    LumaA,
    Rgb,
    Rgba
};

pub use image::{
    ImageDecoder,
    ImageError,
    ImageResult,
    DecodingResult,
    SubImage,
    GenericImage,
    // Iterators
    Pixels,
    MutPixels
};

pub use imageops::FilterType::{
    self,
    Triangle,
    Nearest,
    CatmullRom,
    Gaussian,
    Lanczos3
};

pub use image::ImageFormat::{
    self,
    PNG,
    JPEG,
    GIF,
    WEBP,
    BMP,
    ICO,
    PNM,
};

pub use image::ImageOutputFormat;

pub use buffer::{
    Pixel,
    ConvertBuffer,
    // Image types
    ImageBuffer,
    RgbImage,
    RgbaImage,
    GrayImage,
    GrayAlphaImage
};

// Traits
pub use traits::Primitive;

// Opening and loading images
pub use dynimage::{
    open,
    load,
    load_from_memory,
    load_from_memory_with_format,
    guess_format,
    save_buffer
};

pub use dynimage::DynamicImage::{
    self,
    ImageRgb8,
    ImageRgba8,
    ImageLuma8,
    ImageLumaA8
};

pub use animation::{
    Frame,
    Frames
};

// Math utils
pub mod math;

// Image processing functions
pub mod imageops;

// Image codecs
#[cfg(feature = "webp")]
pub mod webp;
#[cfg(feature = "pnm")]
pub mod pnm;
#[cfg(feature = "png_codec")]
pub mod png;
#[cfg(feature = "ico")]
pub mod ico;
#[cfg(feature = "jpeg")]
pub mod jpeg;
#[cfg(feature = "gif_codec")]
pub mod gif;
#[cfg(feature = "tiff")]
pub mod tiff;
#[cfg(feature = "tga")]
pub mod tga;
#[cfg(feature = "bmp")]
pub mod bmp;
#[cfg(feature = "hdr")]
pub mod hdr;
#[cfg(feature = "dxt")]
pub mod dxt;

mod image;
mod utils;
mod dynimage;
mod color;
mod buffer;
mod traits;
mod animation;

// Copies data from `src` to `dst`
//
// Panics if the length of `dst` is less than the length of `src`.
#[inline]
fn copy_memory(src: &[u8], mut dst: &mut [u8]) {
    let len_src = src.len();
    assert!(dst.len() >= len_src);
    dst.write_all(src).unwrap();
}
