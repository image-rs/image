//! This crate provides native rust implementations of
//! Image encoders and decoders and basic image manipulation
//! functions.

#![crate_name = "image"]
#![crate_type = "rlib"]

#![warn(missing_doc)]
#![warn(unnecessary_qualification)]
#![warn(unnecessary_typecast)]
#![feature(macro_rules)]

extern crate flate;

pub use ColorType = color::ColorType;

pub use color:: {
    Grey,
    RGB,
    Palette,
    GreyA,
    RGBA,

    Pixel,

    Luma,
    LumaA,
    Rgb,
    Rgba,
};

pub use ImageDecoder = image::ImageDecoder;
pub use ImageError   = image::ImageError;
pub use ImageResult  = image::ImageResult;
pub use ImageFormat  = image::ImageFormat;
pub use FilterType   = imageops::FilterType;

pub use imageops:: {
    Triangle,
    Nearest,
    CatmullRom,
    Gaussian,
    Lanczos3
};

pub use image:: {
    PNG,
    JPEG,
    GIF,
    WEBP,
    PPM
};

//Image Types
pub use SubImage        = image::SubImage;
pub use ImageBuf        = image::ImageBuf;
pub use DynamicImage    = dynimage::DynamicImage;

//Traits
pub use GenericImage    = image::GenericImage;
pub use MutableRefImage = image::MutableRefImage;

//Iterators
pub use Pixels          = image::Pixels;
pub use MutPixels       = image::MutPixels;

///opening and loading images
pub use dynimage:: {
    open,
    load,
    load_from_memory,
};

//Image Processing Functions
pub mod imageops;

//Image Codecs
pub mod webp;
pub mod ppm;
pub mod png;
pub mod jpeg;
pub mod gif;

mod image;
mod dynimage;
mod color;