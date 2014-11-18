//!  Image Processing Functions
use std::cmp;

use image:: {
    SubImage,
    GenericImage,
};

use color::{ Pixel, Primitive };

pub use self::sample::FilterType;

pub use self::sample::FilterType:: {
    Triangle,
    Nearest,
    CatmullRom,
    Gaussian,
    Lanczos3
};

///Affine transformations
pub use self::affine:: {
    rotate90,
    rotate180,
    rotate270,
    flip_horizontal,
    flip_vertical,
};

///Image sampling
pub use self::sample:: {
    filter3x3,
    resize,
    blur,
    unsharpen,
};

///Color operations
pub use self::colorops:: {
    grayscale,
    invert,
    contrast,
    brighten,
};

mod affine;
mod colorops;
mod sample;

/// Return a mutable view into an image
pub fn crop<P: Primitive, T: Pixel<P>, I: GenericImage<T>>(
    image:  &mut I,
    x: u32,
    y: u32,
    width: u32,
    height: u32) -> SubImage<I> {

    let (iwidth, iheight) = image.dimensions();

    let x = cmp::min(x, iwidth);
    let y = cmp::min(y, iheight);

    let height = cmp::min(height, iheight - y);
    let width  = cmp::min(width, iwidth - x);

    SubImage::new(image, x, y, width, height)
}
