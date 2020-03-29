//! This crate provides native Rust implementations of
//! image encoders and decoders and basic image manipulation
//! functions.
//!
//! Additional documentation can currently be found in the
//! [README.md file which is most easily viewed on github](https://github.com/image-rs/image/blob/master/README.md).
//!
//! Maintainers: @HeroicKatora, @fintelia
//!
//! [How to contribute](https://github.com/image-rs/organization/blob/master/CONTRIBUTING.md).
//!
//! This crate provides basic imaging processing functions and methods for converting to and from image formats.
//! All image processing functions provided operate on types that implement the [`GenericImage`] trait and return an [`ImageBuffer`].
//!
//! ## Supported Image Formats
//!
//! `image` provides implementations of common image format encoders and decoders.
//!
//!| Format | Decoding | Encoding |
//!| ------ | -------- | -------- |
//!| PNG    | All supported color types | Same as decoding |
//!| JPEG   | Baseline and progressive | Baseline JPEG |
//!| GIF    | Yes | Yes |
//!| BMP    | Yes | RGB(8), RGBA(8), Gray(8), GrayA(8) |
//!| ICO    | Yes | Yes |
//!| TIFF   | Baseline(no fax support) + LZW + PackBits | RGB(8), RGBA(8), Gray(8) |
//!| WebP   | Lossy(Luma channel only) | No |
//!| PNM    | PBM, PGM, PPM, standard PAM | Yes |
//!| DDS    | DXT1, DXT3, DXT5 | No |
//!| TGA    | Yes | No |
//!| farbfeld | Yes | Yes |
//!
//! ### The [`ImageDecoder`] and [`ImageDecoderExt`] Traits
//!
//! All image format decoders implement the [`ImageDecoder`] trait which provide
//! basic methods for getting image metadata and decoding images. Some formats
//! additionally provide [`ImageDecoderExt`] implementations which allow for
//! decoding only part of an image at once.
//!
//! The most important methods for decoders are...
//! * [`dimensions`](ImageDecoder::dimensions): Return a tuple containing the width and height of the image.
//! * [`color_type`](ImageDecoder::color_type): Return the color type of the image data produced by this decoder.
//! * [`read_image`](ImageDecoder::read_image): Decode the entire image into a slice of bytes.
//!
//! ## Pixels
//!
//! `image` provides the following pixel types:
//! * [`Rgb`]: RGB pixel
//! * [`Rgba`]: RGBA pixel
//! * [`Luma`]: Grayscale pixel
//! * [`LumaA`]: Grayscale with alpha
//!
//! All pixels are parameterised by their component type.
//!
//! ## Images
//! ### The [`GenericImage`] Trait
//!
//! A trait that provides functions for manipulating images, parameterised over the image's pixel type.
//!
//! ```
//! # use image::{Pixel, Pixels};
//! pub trait GenericImage {
//!   /// The pixel type.
//!   type Pixel: Pixel;
//!
//!   /// The width and height of this image.
//!   fn dimensions(&self) -> (u32, u32);
//!
//!   /// The bounding rectangle of this image.
//!   fn bounds(&self) -> (u32, u32, u32, u32);
//!
//!   /// Return the pixel located at (x, y)
//!   fn get_pixel(&self, x: u32, y: u32) -> Self::Pixel;
//!
//!   /// Put a pixel at location (x, y)
//!   fn put_pixel(&mut self, x: u32, y: u32, pixel: Self::Pixel);
//!
//!   /// Return an Iterator over the pixels of this image.
//!   /// The iterator yields the coordinates of each pixel
//!   /// along with their value
//!   fn pixels(&self) -> Pixels<Self>;
//! }
//! ```
//!
//! ### Representation of Images
//! `image` provides two main ways of representing image data:
//!
//! #### [`ImageBuffer`]
//! An image parameterised by its Pixel types, represented by a width and height and a vector of pixels. It provides direct access to its pixels and implements the [`GenericImage`] trait.
//!
//! ```
//! extern crate image;
//!
//! use image::{GenericImage, GenericImageView, ImageBuffer, RgbImage};
//!
//! // Construct a new RGB ImageBuffer with the specified width and height.
//! let img: RgbImage = ImageBuffer::new(512, 512);
//!
//! // Construct a new by repeated calls to the supplied closure.
//! let mut img = ImageBuffer::from_fn(512, 512, |x, y| {
//!   if x % 2 == 0 {
//!     image::Luma([0u8])
//!   } else {
//!     image::Luma([255u8])
//!   }
//! });
//!
//! // Obtain the image's width and height.
//! let (width, height) = img.dimensions();
//!
//! // Access the pixel at coordinate (100, 100).
//! let pixel = img[(100, 100)];
//!
//! // Or use the `get_pixel` method from the `GenericImage` trait.
//! let pixel = *img.get_pixel(100, 100);
//!
//! // Put a pixel at coordinate (100, 100).
//! img.put_pixel(100, 100, pixel);
//!
//! // Iterate over all pixels in the image.
//! for pixel in img.pixels() {
//!   // Do something with pixel.
//! }
//! ```
//!
//! #### [`DynamicImage`]
//! A `DynamicImage` is an enumeration over all supported `ImageBuffer<P>` types.
//! Its exact image type is determined at runtime. It is the type returned when opening an image.
//! For convenience [`DynamicImage`]s reimplement all image processing functions.
//!
//! [`DynamicImage`] implement the [`GenericImage`] trait for RGBA pixels.
//!
//! #### [`SubImage`]
//! A view into another image, delimited by the coordinates of a rectangle.
//! This is used to perform image processing functions on a subregion of an image.
//!
//! ```
//! extern crate image;
//!
//! use image::{GenericImageView, ImageBuffer, RgbImage, imageops};
//!
//! let mut img: RgbImage = ImageBuffer::new(512, 512);
//! let subimg = imageops::crop(&mut img, 0, 0, 100, 100);
//!
//! assert!(subimg.dimensions() == (100, 100));
//! ```
//!
//! ## Image Processing Functions
//! These are the functions defined in the `imageops` module. All functions operate on types that implement the [`GenericImage`] trait.
//!
//! * [`blur`](imageops::blur`): Performs a Gaussian blur on the supplied image.
//! * [`brighten`](imageops::brighten`): Brighten the supplied image
//! * [`huerotate`](imageops::huerotate`): Hue rotate the supplied image by degrees
//! * [`contrast`](imageops::contrast`): Adjust the contrast of the supplied image
//! * [`crop`](imageops::crop`): Return a mutable view into an image
//! * [`filter3x3`](imageops::filter3x3`): Perform a 3x3 box filter on the supplied image.
//! * [`flip_horizontal`](imageops::flip_horizontal`): Flip an image horizontally
//! * [`flip_vertical`](imageops::flip_vertical`): Flip an image vertically
//! * [`grayscale`](imageops::grayscale`): Convert the supplied image to grayscale
//! * [`invert`](imageops::invert`): Invert each pixel within the supplied image This function operates in place.
//! * [`resize`](imageops::resize`): Resize the supplied image to the specified dimensions
//! * [`rotate180`](imageops::rotate180`): Rotate an image 180 degrees clockwise.
//! * [`rotate270`](imageops::rotate270`): Rotate an image 270 degrees clockwise.
//! * [`rotate90`](imageops::rotate90`): Rotate an image 90 degrees clockwise.
//! * [`unsharpen`](imageops::unsharpen`): Performs an unsharpen mask on the supplied image
//!
//! For more options, see the [`imageproc`](https://crates.io/crates/imageproc) crate.
//!
//! ## Examples
//! ### Opening And Saving Images
//!
//! `image` provides the [`open`] function for opening images from a path.  The image
//! format is determined from the path's file extension. An [`io`] module provides a
//! reader which offer some more control.
//!
//! ```rust,no_run
//! extern crate image;
//!
//! use image::GenericImageView;
//!
//! fn main() {
//!   // Use the open function to load an image from a Path.
//!   // `open` returns a `DynamicImage` on success.
//!   let img = image::open("tests/images/jpg/progressive/cat.jpg").unwrap();
//!
//!   // The dimensions method returns the images width and height.
//!   println!("dimensions {:?}", img.dimensions());
//!
//!   // The color method returns the image's `ColorType`.
//!   println!("{:?}", img.color());
//!
//!   // Write the contents of this image to the Writer in PNG format.
//!   img.save("test.png").unwrap();
//! }
//! ```
//!
//! ### Generating Fractals
//!
//! ```rust,no_run
//! //! An example of generating julia fractals.
//! extern crate image;
//! extern crate num_complex;
//!
//! fn main() {
//!   let imgx = 800;
//!   let imgy = 800;
//!
//!   let scalex = 3.0 / imgx as f32;
//!   let scaley = 3.0 / imgy as f32;
//!
//!   // Create a new ImgBuf with width: imgx and height: imgy
//!   let mut imgbuf = image::ImageBuffer::new(imgx, imgy);
//!
//!   // Iterate over the coordinates and pixels of the image
//!   for (x, y, pixel) in imgbuf.enumerate_pixels_mut() {
//!     let r = (0.3 * x as f32) as u8;
//!     let b = (0.3 * y as f32) as u8;
//!     *pixel = image::Rgb([r, 0, b]);
//!   }
//!
//!   // A redundant loop to demonstrate reading image data
//!   for x in 0..imgx {
//!     for y in 0..imgy {
//!       let cx = y as f32 * scalex - 1.5;
//!       let cy = x as f32 * scaley - 1.5;
//!
//!       let c = num_complex::Complex::new(-0.4, 0.6);
//!       let mut z = num_complex::Complex::new(cx, cy);
//!
//!       let mut i = 0;
//!       while i < 255 && z.norm() <= 2.0 {
//!         z = z * z + c;
//!         i += 1;
//!       }
//!
//!       let pixel = imgbuf.get_pixel_mut(x, y);
//!       let image::Rgb(data) = *pixel;
//!       *pixel = image::Rgb([data[0], i as u8, data[2]]);
//!     }
//!   }
//!
//!   // Save the image as “fractal.png”, the format is deduced from the path
//!   imgbuf.save("fractal.png").unwrap();
//! }
//! ```
//!
//! Example output:
//!
//! <img src="https://github.com/image-rs/image/raw/master/examples/fractal.png" alt="A Julia Fractal, c: -0.4 + 0.6i" width="500" />
//!
//! ### Writing raw buffers
//! If the high level interface is not needed because the image was obtained by other means, `image` provides the function [`save_buffer`] to save a buffer to a file.
//!
//! ```rust,no_run
//! extern crate image;
//!
//! fn main() {
//!
//!   let buffer: &[u8] = unimplemented!(); // Generate the image data
//!
//!   // Save the buffer as "image.png"
//!   image::save_buffer("image.png", buffer, 800, 600, image::ColorType::Rgb8).unwrap()
//! }
//!
//!```

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

pub use crate::buffer::{ConvertBuffer,
                 GrayAlphaImage,
                 GrayImage,
                 // Image types
                 ImageBuffer,
                 Pixel,
                 RgbImage,
                 RgbaImage,
                 };

pub use crate::flat::FlatSamples;

// Traits
pub use crate::traits::Primitive;

// Opening and loading images
pub use crate::io::free_functions::{guess_format, load};
pub use crate::dynimage::{load_from_memory, load_from_memory_with_format, open,
                   save_buffer, save_buffer_with_format, image_dimensions};

pub use crate::dynimage::DynamicImage;

pub use crate::animation::{Delay, Frame, Frames};

// More detailed error type
pub mod error;

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
mod buffer;
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
