//! Image Processing Functions
use std::cmp;

use image:: {
    SubImage,
    GenericImage,
};

use buffer::Pixel;

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
    ColorMap,
    BiLevel,
    dither,
    index_colors,
};

mod affine;
/// Public only because of Rust bug:
/// https://github.com/rust-lang/rust/issues/18241
pub mod colorops;
mod sample;

/// Return a mutable view into an image
// TODO: Is a 'static bound on `I` really required? Acn we avoid it?
pub fn crop<I: GenericImage + 'static>(image: &mut I, x: u32, y: u32,
                                       width: u32, height: u32)
                                       -> SubImage<I>
    where I::Pixel: 'static,
          <I::Pixel as Pixel>::Subpixel: 'static {

    let (iwidth, iheight) = image.dimensions();

    let x = cmp::min(x, iwidth);
    let y = cmp::min(y, iheight);

    let height = cmp::min(height, iheight - y);
    let width  = cmp::min(width, iwidth - x);

    SubImage::new(image, x, y, width, height)
}

/// Overlay an image at a given coordinate (x, y)
pub fn overlay<I: GenericImage>(bottom: &mut I, top: &I, x: u32, y:u32) {
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
            bottom.get_pixel_mut(x + top_x, y + top_y).blend(&p);
        }
    }
}

/// Replace the contents of an image at a given coordinate (x, y)
pub fn replace<I: GenericImage>(bottom: &mut I, top: &I, x: u32, y:u32) {
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
            *bottom.get_pixel_mut(x + top_x, y + top_y)= p;
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
