//! Functions for altering and converting the color of pixelbufs

use color::{Luma, Rgba};
use buffer::{ImageBuffer, Pixel};
use traits::Primitive;
use image::GenericImage;
use math::utils::clamp;
use math::nq;
use num_traits::{Num, NumCast};

/// Convert the supplied image to grayscale
pub fn grayscale<'a, I: GenericImage>(image: &I)
    -> ImageBuffer<Luma<<I::Pixel as Pixel>::Subpixel>, Vec<<I::Pixel as Pixel>::Subpixel>>
    where <I::Pixel as Pixel>::Subpixel: 'static,
          <<I::Pixel as Pixel>::Subpixel as Num>::FromStrRadixErr: 'static {
    let (width, height) = image.dimensions();
    let mut out = ImageBuffer::new(width, height);

    for y in 0..height {
        for x in 0..width {
            let p = image.get_pixel(x, y).to_luma();
            out.put_pixel(x, y, p);
        }
    }

    out
}

/// Invert each pixel within the supplied image.
/// This function operates in place.
pub fn invert<I: GenericImage>(image: &mut I) {
    let (width, height) = image.dimensions();

    for y in 0..height {
        for x in 0..width {
            let mut p = image.get_pixel(x, y);
            p.invert();

            image.put_pixel(x, y, p);
        }
    }
}

/// Adjust the contrast of the supplied image.
/// ```contrast``` is the amount to adjust the contrast by.
/// Negative values decrease the contrast and positive values increase the contrast.
pub fn contrast<I, P, S>(image: &I, contrast: f32)
    -> ImageBuffer<P, Vec<S>>
    where I: GenericImage<Pixel=P>,
          P: Pixel<Subpixel=S> + 'static,
          S: Primitive + 'static {

    let (width, height) = image.dimensions();
    let mut out = ImageBuffer::new(width, height);

    let max = S::max_value();
    let max: f32 = NumCast::from(max).unwrap();

    let percent = ((100.0 + contrast) / 100.0).powi(2);

    for y in 0..height {
        for x in 0..width {
            let f = image.get_pixel(x, y).map(|b| {
                let c: f32 = NumCast::from(b).unwrap();

                let d = ((c / max - 0.5) * percent  + 0.5) * max;
                let e = clamp(d, 0.0, max);

                NumCast::from(e).unwrap()
            });

            out.put_pixel(x, y, f);
        }
    }

    out
}

/// The sepia filter effect is a brownish-grey to dark yellowish-brown tone
/// imparted to an image and is expressed in percentages from 0.0 to 1.0
/// `value` is the amount/percentage e.g. sepia(1.0) just like the css filter sepia(100%)
/// https://developer.mozilla.org/de/docs/Web/CSS/filter
pub fn sepia<I, P, S>(image: &I, value: f64)
    -> ImageBuffer<P, Vec<S>>
    where I: GenericImage<Pixel=P>,
          P: Pixel<Subpixel=S> + 'static,
          S: Primitive + 'static {

    let (width, height) = image.dimensions();
    let mut out = ImageBuffer::new(width, height);

    // TODO: clamp to 0.0/1.0
    let amount: f64 = 1.0f64 - value;

    for (x, y, pixel) in out.enumerate_pixels_mut() {
        let p = image.get_pixel(x, y);
        let (k1, k2, k3, k4) = p.channels4();
        let vec: (f64, f64, f64, f64) = (
            NumCast::from(k1).unwrap(),
            NumCast::from(k2).unwrap(),
            NumCast::from(k3).unwrap(),
            NumCast::from(k4).unwrap()
        );

        let r = vec.0;
        let g = vec.1;
        let b = vec.2;

        let new_r = (0.393 + 0.607 * amount) * r + (0.769 - 0.769 * amount) * g + (0.189 - 0.189 * amount) * b;
        let new_g = (0.349 - 0.349 * amount) * r + (0.686 + 0.314 * amount) * g + (0.168 - 0.168 * amount) * b;
        let new_b = (0.272 - 0.272 * amount) * r + (0.534 - 0.534 * amount) * g + (0.131 + 0.869 * amount) * b;
        let max = 255f64;
        let outpixel = Pixel::from_channels(
            NumCast::from(clamp(new_r, 0.0, max)).unwrap(),
            NumCast::from(clamp(new_g, 0.0, max)).unwrap(),
            NumCast::from(clamp(new_b, 0.0, max)).unwrap(),
            NumCast::from(clamp(vec.3, 0.0, max)).unwrap()
        );
        *pixel = outpixel;
    }
    out
}

/// Brighten the supplied image.
/// ```value``` is the amount to brighten each pixel by.
/// Negative values decrease the brightness and positive values increase it.
pub fn brighten<I, P, S>(image: &I, value: i32)
    -> ImageBuffer<P, Vec<S>>
    where I: GenericImage<Pixel=P>,
          P: Pixel<Subpixel=S> + 'static,
          S: Primitive + 'static {

    let (width, height) = image.dimensions();
    let mut out = ImageBuffer::new(width, height);

    let max = S::max_value();
    let max: i32 = NumCast::from(max).unwrap();

    for y in 0..height {
        for x in 0..width {
            let e = image.get_pixel(x, y).map_with_alpha(|b| {
                let c: i32 = NumCast::from(b).unwrap();
                let d = clamp(c + value, 0, max);

                NumCast::from(d).unwrap()
            }, |alpha| alpha);

            out.put_pixel(x, y, e);
        }
    }

    out
}

/// A color map
pub trait ColorMap {
    /// The color type on which the map operates on
    type Color;
    /// Returns the index of the closed match of `color`
    /// in the color map.
    fn index_of(&self, color: &Self::Color) -> usize;
    /// Maps `color` to the closest color in the color map.
    fn map_color(&self, color: &mut Self::Color);
}

/// A bi-level color map
#[derive(Clone, Copy)]
pub struct BiLevel;

impl ColorMap for BiLevel {
    type Color = Luma<u8>;

    #[inline(always)]
    fn index_of(&self, color: &Luma<u8>) -> usize {
        let luma = color.data;
        if luma[0] > 127 {
            1
        } else {
            0
        }
    }

    #[inline(always)]
    fn map_color(&self, color: &mut Luma<u8>) {
        let new_color = 0xFF * self.index_of(color) as u8;
        let luma = &mut color.data;
        luma[0] = new_color;
    }
}

impl ColorMap for nq::NeuQuant {
    type Color = Rgba<u8>;

    #[inline(always)]
    fn index_of(&self, color: &Rgba<u8>) -> usize {
        self.index_of(color.channels())
    }

    #[inline(always)]
    fn map_color(&self, color: &mut Rgba<u8>) {
        self.map_pixel(color.channels_mut())
    }
}

/// Floyd-Steinberg error diffusion
fn diffuse_err<P: Pixel<Subpixel=u8>>(pixel: &mut P, error: [i16; 3], factor: i16) {
    for (e, c) in error.iter().zip(pixel.channels_mut().iter_mut()) {
        *c = match *c as i16 + e * factor / 16 {
            val if val < 0 => {
                0
            },
            val if val > 0xFF => {
                0xFF
            },
            val => val as u8
        }
    }

}

macro_rules! do_dithering(
    ($map:expr, $image:expr, $err:expr, $x:expr, $y:expr) => (
        {
            let old_pixel = $image[($x, $y)];
            let new_pixel = $image.get_pixel_mut($x, $y);
            $map.map_color(new_pixel);
            for ((e, &old), &new) in $err.iter_mut()
                                        .zip(old_pixel.channels().iter())
                                        .zip(new_pixel.channels().iter())
            {
                *e = old as i16 - new as i16
            }
        }
    )
);

/// Reduces the colors of the image using the supplied `color_map` while applying
/// Floyd-Steinberg dithering to improve the visual conception
pub fn dither<Pix, Map>(image: &mut ImageBuffer<Pix, Vec<u8>>, color_map: &Map)
where Map: ColorMap<Color=Pix>,
      Pix: Pixel<Subpixel=u8> + 'static,
{
    let (width, height) = image.dimensions();
    let mut err: [i16; 3] = [0; 3];
    for y in 0..height-1 {
        let x = 0;
        do_dithering!(color_map, image, err, x, y);
        diffuse_err(image.get_pixel_mut(x+1, y+0), err, 7);
        diffuse_err(image.get_pixel_mut(x+0, y+1), err, 5);
        diffuse_err(image.get_pixel_mut(x+1, y+1), err, 1);
        for x in 1..width-1 {
            do_dithering!(color_map, image, err, x, y);
            diffuse_err(image.get_pixel_mut(x+1, y+0), err, 7);
            diffuse_err(image.get_pixel_mut(x-1, y+1), err, 3);
            diffuse_err(image.get_pixel_mut(x+0, y+1), err, 5);
            diffuse_err(image.get_pixel_mut(x+1, y+1), err, 1);
        }
        let x = width-1;
        do_dithering!(color_map, image, err, x, y);
        diffuse_err(image.get_pixel_mut(x-1, y+1), err, 3);
        diffuse_err(image.get_pixel_mut(x+0, y+1), err, 5);
    }
    let y = height-1;
    let x = 0;
    do_dithering!(color_map, image, err, x, y);
    diffuse_err(image.get_pixel_mut(x+1, y+0), err, 7);
    for x in 1..width-1 {
        do_dithering!(color_map, image, err, x, y);
        diffuse_err(image.get_pixel_mut(x+1, y+0), err, 7);
    }
    let x = width-1;
    do_dithering!(color_map, image, err, x, y);
}

/// Reduces the colors using the supplied `color_map` and returns an image of the indices
pub fn index_colors<Pix, Map>(image: &ImageBuffer<Pix, Vec<u8>>, color_map: &Map) ->
ImageBuffer<Luma<u8>, Vec<u8>>
where Map: ColorMap<Color=Pix>,
      Pix: Pixel<Subpixel=u8> + 'static,
{
    let mut indices = ImageBuffer::new(image.width(), image.height());
    for (pixel, idx) in image.pixels().zip(indices.pixels_mut()) {
        *idx = Luma([color_map.index_of(pixel) as u8])
    }
    indices
}

#[cfg(test)]
mod test {

    use ImageBuffer;
    use super::*;

    #[test]
    fn test_dither() {
        let mut image = ImageBuffer::from_raw(2, 2, vec![127, 127, 127, 127]).unwrap();
        let cmap = BiLevel;
        dither(&mut image, &cmap);
        assert_eq!(&*image, &[0, 0xFF, 0xFF, 0]);
        assert_eq!(index_colors(&image, &cmap).into_raw(), vec![0, 1, 1, 0])
    }
}
