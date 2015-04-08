//! This crate provides native rust implementations of
//! Image encoders and decoders and basic image manipulation
//! functions.

#![crate_name = "image"]
#![crate_type = "rlib"]
#![warn(missing_docs)]
#![warn(unused_qualifications)]
#![deny(missing_copy_implementations)]
#![cfg_attr(test, feature(test))]

extern crate byteorder;
extern crate num;
#[macro_use]
extern crate enum_primitive;
#[cfg(test)] extern crate test;

pub use color::ColorType as ColorType;
pub use traits::Primitive;

pub use color::ColorType:: {
    Gray,
    RGB,
    Palette,
    GrayA,
    RGBA,
};
pub use color:: {
    Luma,
    LumaA,
    Rgb,
    Rgba,
};

pub use buffer::Pixel;

pub use image::ImageDecoder as ImageDecoder;
pub use image::ImageError as ImageError;
pub use image::ImageResult as ImageResult;
pub use image::ImageFormat as ImageFormat;
pub use imageops::FilterType as FilterType;

pub use imageops:: {
    Triangle,
    Nearest,
    CatmullRom,
    Gaussian,
    Lanczos3
};

pub use image::ImageFormat:: {
    PNG,
    JPEG,
    GIF,
    WEBP,
    PPM
};

// Image Types
pub use image::SubImage as SubImage;
pub use dynimage::DynamicImage as DynamicImage;
pub use buffer::{
    ImageBuffer,
    RgbImage,
    RgbaImage,
    GrayImage,
    GrayAlphaImage
};

// Traits
pub use image::GenericImage as GenericImage;

// Iterators
pub use image::Pixels as Pixels;
pub use image::MutPixels as MutPixels;



/// Opening and loading images
pub use dynimage:: {
    open,
    load,
    load_from_memory,
    load_from_memory_with_format,
    save_buffer,
};
pub use dynimage::DynamicImage:: {
    ImageRgb8,
    ImageRgba8,
    ImageLuma8,
    ImageLumaA8,
};

pub use animation:: {
    Frame, Frames
};

// Math utils
pub mod math;

// Image Processing Functions
pub mod imageops;

// Image Codecs
#[cfg(feature = "webp")]
pub mod webp;
#[cfg(feature = "ppm")]
pub mod ppm;
#[cfg(feature = "png")]
pub mod png;
#[cfg(feature = "jpeg")]
pub mod jpeg;
#[cfg(feature = "gif")]
pub mod gif;
#[cfg(feature = "tiff")]
pub mod tiff;
#[cfg(feature = "tga")]
pub mod tga;


mod image;
mod utils;
mod dynimage;
mod color;
mod buffer;
mod traits;
mod animation;

/// Copies data from `src` to `dst`
///
/// Panics if the length of `dst` is less than the length of `src`.
// NOTE: this is a copy-paste of the unstable function `std::::copy_memory`.
#[inline]
pub fn copy_memory(src: &[u8], dst: &mut [u8]) {
    let len_src = src.len();
    assert!(dst.len() >= len_src);
    // `dst` is unaliasable, so we know statically it doesn't overlap
    // with `src`.
    unsafe {
        std::ptr::copy_nonoverlapping(src.as_ptr(),
                                      dst.as_mut_ptr(),
                                      len_src);
    }
}
