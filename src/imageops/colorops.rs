//! Functions for altering and converting the color of pixelbufs
use std::num:: {
    cast,
    Float,
};

use std::default::Default;

use color::Luma;
use buffer::Pixel;

use traits::Primitive;

use image:: {
    GenericImage,
};
use buffer::ImageBuffer;

fn clamp <N: PartialOrd> (a: N, min: N, max: N) -> N {
    if a > max { max }
    else if a < min { min }
    else { a }
}

/// Convert the supplied image to grayscale
pub fn grayscale<P: Pixel + 'static, I: GenericImage<P>> (image: &I)
    -> ImageBuffer<Vec<P::Subpixel>, P::Subpixel, Luma<P::Subpixel>>
    where P::Subpixel: Primitive + Default + 'static {
    let (width, height) = image.dimensions();
    let mut out = ImageBuffer::new(width, height);

    for y in (0..height) {
        for x in (0..width) {
            let p = image.get_pixel(x, y).to_luma();
            out.put_pixel(x, y, p);
        }
    }

    out
}

/// Invert each pixel within the supplied image
/// This function operates in place.
pub fn invert<P: Pixel, I: GenericImage<P>>(image: &mut I) where P::Subpixel : Primitive {
    let (width, height) = image.dimensions();

    for y in (0..height) {
        for x in (0..width) {
            let mut p = image.get_pixel(x, y);
            p.invert();

            image.put_pixel(x, y, p);
        }
    }
}

/// Adjust the contrast of the supplied image
/// ```contrast``` is the amount to adjust the contrast by.
/// Negative values decrease the contrast and positive values increase the contrast.
pub fn contrast<P: Pixel + 'static, I: GenericImage<P>>(
    image:    &I,
    contrast: f32) -> ImageBuffer<Vec<P::Subpixel>, P::Subpixel, P>
    where P::Subpixel: Primitive + 'static {

    let (width, height) = image.dimensions();
    let mut out = ImageBuffer::new(width, height);

    let max: P::Subpixel = Primitive::max_value();
    let max = cast::<P::Subpixel, f32>(max).unwrap();

    let percent = ((100.0 + contrast) / 100.0).powi(2);

    for y in (0..height) {
        for x in (0..width) {
            let f = image.get_pixel(x, y).map(|&: b| {
                let c = cast::<P::Subpixel, f32>(b).unwrap();

                let d = ((c / max - 0.5) * percent  + 0.5) * max;
                let e = clamp(d, 0.0, max);

                cast::<f32, P::Subpixel>(e).unwrap()
            });

            out.put_pixel(x, y, f);
        }
    }

    out
}

/// Brighten the supplied image
/// ```value``` is the amount to brighten each pixel by.
/// Negative values decrease the brightness and positive values increase it.
pub fn brighten<P: Pixel + 'static, I: GenericImage<P>>(
    image: &I,
    value: i32) -> ImageBuffer<Vec<P::Subpixel>, P::Subpixel, P>
    where P::Subpixel: Primitive + 'static {

    let (width, height) = image.dimensions();
    let mut out = ImageBuffer::new(width, height);

    let max: P::Subpixel = Primitive::max_value();
    let max = cast::<P::Subpixel, i32>(max).unwrap();

    for y in (0..height) {
        for x in (0..width) {
            let e = image.get_pixel(x, y).map_with_alpha(|&:b| {
                let c = cast::<P::Subpixel, i32>(b).unwrap();
                let d = clamp(c + value, 0, max);

                cast::<i32, P::Subpixel>(d).unwrap()
            }, |&:alpha| alpha);

            out.put_pixel(x, y, e);
        }
    }

    out
}
