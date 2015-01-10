//! Image Processing Functions
use std::cmp;

use image:: {
    SubImage,
    GenericImage,
};

use buffer::Pixel;
use traits::Primitive;

pub use self::sample::FilterType;

pub use self::sample::FilterType:: {
    Triangle,
    Nearest,
    CatmullRom,
    Gaussian,
    Lanczos3
};

/// Affine transformations
pub use self::affine:: {
    rotate90,
    rotate180,
    rotate270,
    flip_horizontal,
    flip_vertical,
};

/// Image sampling
pub use self::sample:: {
    filter3x3,
    resize,
    blur,
    unsharpen,
};

/// Color operations
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
pub fn crop<P: Primitive + 'static, T: Pixel<P> + 'static, I: GenericImage<T>>(
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

/// Overlay an image at a given coordinate (x, y)
pub fn overlay<P: Primitive, T: Pixel<P>, I: GenericImage<T>>(
    bottom: &mut I,
    top: &I,
    x: u32,
    y: u32)  {
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

    for top_y in range(0, range_height) {
        for top_x in range(0, range_width) {
            let p = top.get_pixel(top_x, top_y);
            bottom.get_pixel_mut(x + top_x, y + top_y).blend(&p);
        }
    }
}

#[cfg(test)]
mod tests {

    use image::GenericImage;
    use buffer::ImageBuffer;
    use color::{Rgb};
    use super::overlay;

    #[test]
    /// Test that images written into other images works
    fn test_image_in_image() {
        let mut target = ImageBuffer::new(32, 32);
        let source = ImageBuffer::from_fn(16, 16, Box::new(|&: _, _| {
            Rgb([255u8, 0, 0])
        }));
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
        let source = ImageBuffer::from_fn(32, 32, Box::new(|&: _, _| {
            Rgb([255u8, 0, 0])
        }));
        overlay(&mut target, &source, 1, 1);
        assert!(*target.get_pixel(0, 0) == Rgb([0, 0, 0]));
        assert!(*target.get_pixel(1, 1) == Rgb([255u8, 0, 0]));
        assert!(*target.get_pixel(31, 31) == Rgb([255u8, 0, 0]));
    }

}
