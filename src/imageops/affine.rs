//! Functions for performing affine transformations.

use color::Pixel;
use traits::Primitive;
use image:: {
    GenericImage,
    ImageBuf,
};

///Rotate an image 90 degrees clockwise.
pub fn rotate90<P: Primitive, T: Pixel<P>, I: GenericImage<T>>(image:  &I) -> ImageBuf<T> {
    let (width, height) = image.dimensions();
    let mut out = ImageBuf::new(height, width);

    for y in range(0, height) {
        for x in range(0, width) {
            let p = image.get_pixel(x, y);
            out.put_pixel(height - 1 - y, x, p);
        }
    }

    out
}

///Rotate an image 180 degrees clockwise.
pub fn rotate180<P: Primitive, T: Pixel<P>, I: GenericImage<T>>(image:  &I) -> ImageBuf<T> {
    let (width, height) = image.dimensions();
    let mut out = ImageBuf::new(width, height);

    for y in range(0, height) {
        for x in range(0, width) {
            let p = image.get_pixel(x, y);
            out.put_pixel(width - 1 - x, height - 1 - y, p);
        }
    }

    out
}

///Rotate an image 270 degrees clockwise.
pub fn rotate270<P: Primitive, T: Pixel<P>, I: GenericImage<T>>(image:  &I) -> ImageBuf<T> {
    let (width, height) = image.dimensions();
    let mut out = ImageBuf::new(height, width);

    for y in range(0, height) {
        for x in range(0, width) {
            let p = image.get_pixel(x, y);
            out.put_pixel(y, width - 1 - x, p);
        }
    }

    out
}

///Flip an image horizontally
pub fn flip_horizontal<P: Primitive, T: Pixel<P>, I: GenericImage<T>>(image:  &I) -> ImageBuf<T> {
    let (width, height) = image.dimensions();
    let mut out = ImageBuf::new(height, width);

    for y in range(0, height) {
        for x in range(0, width) {
            let p = image.get_pixel(x, y);
            out.put_pixel(width - 1 - x, y, p);
        }
    }

    out
}

///Flip an image vertically
pub fn flip_vertical<P: Primitive, T: Pixel<P>, I: GenericImage<T>>(image:  &I) -> ImageBuf<T> {
    let (width, height) = image.dimensions();
    let mut out = ImageBuf::new(width, height);

    for y in range(0, height) {
        for x in range(0, width) {
            let p = image.get_pixel(x, y);
            out.put_pixel(x, height - 1 - y, p);
        }
    }

    out
}
