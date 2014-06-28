//! This crate provides native rust implementations of
//! Image encoders and decoders and basic image manipulation
//! functions.

#![crate_id = "image"]
#![crate_type = "rlib"]

#![deny(missing_doc)]
#![warn(unnecessary_qualification)]
#![warn(unnecessary_typecast)]
#![feature(macro_rules)]

extern crate flate;

pub use ColorType = imaging::colortype::ColorType;
pub use Grey      = imaging::colortype::Grey;
pub use RGB       = imaging::colortype::RGB;
pub use Palette   = imaging::colortype::Palette;
pub use GreyA     = imaging::colortype::GreyA;
pub use RGBA      = imaging::colortype::RGBA;

pub use ImageDecoder = image::ImageDecoder;
pub use ImageError   = image::ImageError;
pub use ImageResult  = image::ImageResult;
pub use ImageFormat  = image::ImageFormat;
pub use FilterType   = imaging::sample::FilterType;

pub use imaging::sample::{
    Triangle,
    Nearest,
    CatmullRom,
    Gaussian,
    Lanczos3
};

pub use image::{
    PNG,
    JPEG,
    GIF,
    WEBP,
    PPM
};

pub use Image = image::Image;
pub use SubImage = image::SubImage;
pub use Tiles = image::Tiles;

pub use JPEGDecoder = codecs::jpeg::JPEGDecoder;
pub use JPEGEncoder = codecs::jpeg::JPEGEncoder;
pub use PNGDecoder  = codecs::png::PNGDecoder;
pub use PNGEncoder  = codecs::png::PNGEncoder;
pub use GIFDecoder  = codecs::gif::GIFDecoder;
pub use PPMEncoder  = codecs::ppm::PPMEncoder;
pub use WebpDecoder = codecs::webp::WebpDecoder;

///Image Codecs
pub mod codecs {
    pub mod vp8;
    pub mod jpeg;
    pub mod png;
    pub mod gif;
    pub mod webp;
    pub mod ppm;
}

#[path = "codecs/hash.rs"]
mod hash;

#[path = "codecs/transform.rs"]
mod transform;

#[path = "codecs/deflate.rs"]
mod deflate;

#[path = "codecs/zlib.rs"]
mod zlib;

#[path = "codecs/lzw.rs"]
mod lzw;

///Image Processing Functions
pub mod imaging {
    pub mod colortype;
    pub mod pixel;
    pub mod sample;
    pub mod colorops;
    pub mod pixelbuf;
    pub mod affine;
}

mod image;