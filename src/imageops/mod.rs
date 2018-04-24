//! Image Processing Functions
use std::cmp;

use image::{GenericImage, SubImage};

use buffer::Pixel;

pub use self::sample::FilterType;

pub use self::sample::FilterType::{CatmullRom, Gaussian, Lanczos3, Nearest, Triangle};

/// Affine transformations
pub use self::affine::{flip_horizontal, flip_vertical, rotate180, rotate270, rotate90};

/// Image sampling
pub use self::sample::{blur, filter3x3, resize, thumbnail, unsharpen};

/// Color operations
pub use self::colorops::{brighten, contrast, dither, grayscale, huerotate, index_colors, invert,
                         BiLevel, ColorMap};

mod affine;
// Public only because of Rust bug:
// https://github.com/rust-lang/rust/issues/18241
pub mod colorops;
mod sample;

/// Return a mutable view into an image
// TODO: Is a 'static bound on `I` really required? Acn we avoid it?
pub fn crop<I: GenericImage + 'static>(
    image: &mut I,
    x: u32,
    y: u32,
    width: u32,
    height: u32,
) -> SubImage<I>
where
    I::Pixel: 'static,
    <I::Pixel as Pixel>::Subpixel: 'static,
{
    let (iwidth, iheight) = image.dimensions();

    let x = cmp::min(x, iwidth);
    let y = cmp::min(y, iheight);

    let height = cmp::min(height, iheight - y);
    let width = cmp::min(width, iwidth - x);

    SubImage::new(image, x, y, width, height)
}

/// Overlay an image at a given coordinate (x, y)
pub fn overlay<I: GenericImage>(bottom: &mut I, top: &I, x: u32, y: u32) {
    let (top_width, top_height) = top.dimensions();
    let (bottom_width, bottom_height) = bottom.dimensions();

    // Crop our top image if we're going out of bounds
    let range_width = if x + top_width > bottom_width {
        bottom_width - x
    } else {
        top_width
    };

    let range_height = if y + top_height > bottom_height {
        bottom_height - y
    } else {
        top_height
    };

    for top_y in 0..range_height {
        for top_x in 0..range_width {
            let p = top.get_pixel(top_x, top_y);
            let mut bottom_pixel = bottom.get_pixel(x + top_x, y + top_y);
            bottom_pixel.blend(&p);

            bottom.put_pixel(x + top_x, y + top_y, bottom_pixel);
        }
    }
}

/// Replace the contents of an image at a given coordinate (x, y)
pub fn replace<I: GenericImage>(bottom: &mut I, top: &I, x: u32, y: u32) {
    let (top_width, top_height) = top.dimensions();
    let (bottom_width, bottom_height) = bottom.dimensions();

    // Crop our top image if we're going out of bounds
    let range_width = if x + top_width > bottom_width {
        bottom_width - x
    } else {
        top_width
    };

    let range_height = if y + top_height > bottom_height {
        bottom_height - y
    } else {
        top_height
    };

    for top_y in 0..range_height {
        for top_x in 0..range_width {
            let p = top.get_pixel(top_x, top_y);
            bottom.put_pixel(x + top_x, y + top_y, p);
        }
    }
}

#[cfg(test)]
mod tests {

    use super::overlay;
    use buffer::ImageBuffer;
    use color::Rgb;

    #[test]
    /// Test that images written into other images works
    fn test_image_in_image() {
        let mut target = ImageBuffer::new(32, 32);
        let source = ImageBuffer::from_pixel(16, 16, Rgb([255u8, 0, 0]));
        overlay(&mut target, &source, 0, 0);
        assert!(*target.get_pixel(0, 0) == Rgb([255u8, 0, 0]));
        assert!(*target.get_pixel(15, 0) == Rgb([255u8, 0, 0]));
        assert!(*target.get_pixel(16, 0) == Rgb([0u8, 0, 0]));
        assert!(*target.get_pixel(0, 15) == Rgb([255u8, 0, 0]));
        assert!(*target.get_pixel(0, 16) == Rgb([0u8, 0, 0]));
    }

    #[test]
    /// Test that images written outside of a frame doesn't blow up
    fn test_image_in_image_outside_of_bounds() {
        let mut target = ImageBuffer::new(32, 32);
        let source = ImageBuffer::from_pixel(32, 32, Rgb([255u8, 0, 0]));
        overlay(&mut target, &source, 1, 1);
        assert!(*target.get_pixel(0, 0) == Rgb([0, 0, 0]));
        assert!(*target.get_pixel(1, 1) == Rgb([255u8, 0, 0]));
        assert!(*target.get_pixel(31, 31) == Rgb([255u8, 0, 0]));
    }

}
