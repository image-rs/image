//! Functions for performing affine transformations.

use buffer::Pixel;
use traits::Primitive;
use image:: {
    GenericImage,
};
use buffer::ImageBuffer;

/// Rotate an image 90 degrees clockwise.
pub fn rotate90<P: Pixel + 'static, I: GenericImage<P>>(image:  &I)
    -> ImageBuffer<P, Vec<P::Subpixel>> where P::Subpixel: 'static {
    let (width, height) = image.dimensions();
    let mut out = ImageBuffer::new(height, width);

    for y in (0..height) {
        for x in (0..width) {
            let p = image.get_pixel(x, y);
            out.put_pixel(height - 1 - y, x, p);
        }
    }

    out
}

/// Rotate an image 180 degrees clockwise.
pub fn rotate180<P: Pixel + 'static, I: GenericImage<P>>(image:  &I)
    -> ImageBuffer<P, Vec<P::Subpixel>> where P::Subpixel: 'static {
    let (width, height) = image.dimensions();
    let (width, height) = image.dimensions();
    let mut out = ImageBuffer::new(width, height);

    for y in (0..height) {
        for x in (0..width) {
            let p = image.get_pixel(x, y);
            out.put_pixel(width - 1 - x, height - 1 - y, p);
        }
    }

    out
}

/// Rotate an image 270 degrees clockwise.
pub fn rotate270<P: Pixel + 'static, I: GenericImage<P>>(image:  &I)
    -> ImageBuffer<P, Vec<P::Subpixel>> where P::Subpixel: 'static {
    let (width, height) = image.dimensions();
    let mut out = ImageBuffer::new(height, width);

    for y in (0..height) {
        for x in (0..width) {
            let p = image.get_pixel(x, y);
            out.put_pixel(y, width - 1 - x, p);
        }
    }

    out
}

/// Flip an image horizontally
pub fn flip_horizontal<P: Pixel + 'static, I: GenericImage<P>>(image:  &I)
    -> ImageBuffer<P, Vec<P::Subpixel>> where P::Subpixel: 'static {
    let (width, height) = image.dimensions();
    let mut out = ImageBuffer::new(height, width);

    for y in (0..height) {
        for x in (0..width) {
            let p = image.get_pixel(x, y);
            out.put_pixel(width - 1 - x, y, p);
        }
    }

    out
}

/// Flip an image vertically
pub fn flip_vertical<P: Pixel + 'static, I: GenericImage<P>>(image:  &I)
    -> ImageBuffer<P, Vec<P::Subpixel>> where P::Subpixel: 'static {
    let (width, height) = image.dimensions();
    let mut out = ImageBuffer::new(width, height);

    for y in (0..height) {
        for x in (0..width) {
            let p = image.get_pixel(x, y);
            out.put_pixel(x, height - 1 - y, p);
        }
    }

    out
}
