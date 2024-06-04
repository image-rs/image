//! Functions for altering and converting the color of pixelbufs

use num_traits::NumCast;
use pixeli::{ContiguousPixel, FromPixelCommon, Gray, GrayAlpha, Pixel, PixelComponent, Rgba};
use std::f64::consts::PI;

use crate::color::Invert;
use crate::image::{GenericImage, GenericImageView};
use crate::utils::clamp;
use crate::ImageBuffer;

type Component<I> = <<I as GenericImageView>::Pixel as Pixel>::Component;

/// Convert the supplied image to grayscale. Alpha channel is discarded.
#[deprecated(note = "use image::imageops::convert_generic_image() instead")]
pub fn grayscale<I: GenericImageView>(
    image: &I,
) -> ImageBuffer<Gray<Component<I>>, Vec<Component<I>>>
where
    Gray<<<I as GenericImageView>::Pixel as Pixel>::Component>:
        FromPixelCommon<<I as GenericImageView>::Pixel>,
{
    convert_generic_image(image)
}

/// Convert the supplied image to grayscale. Alpha channel is preserved.
#[deprecated(note = "use image::imageops::convert_generic_image() instead")]
pub fn grayscale_alpha<I: GenericImageView>(
    image: &I,
) -> ImageBuffer<GrayAlpha<Component<I>>, Vec<Component<I>>>
where
    GrayAlpha<<<I as GenericImageView>::Pixel as Pixel>::Component>:
        FromPixelCommon<<I as GenericImageView>::Pixel>,
{
    convert_generic_image(image)
}

/// Convert the supplied image to a grayscale image with the specified pixel type. Alpha channel is discarded.
#[deprecated(note = "use image::imageops::convert_generic_image() instead")]
pub fn grayscale_with_type<NewPixel, I: GenericImageView>(
    image: &I,
) -> ImageBuffer<NewPixel, Vec<NewPixel::Component>>
where
    NewPixel: ContiguousPixel + FromPixelCommon<<I as GenericImageView>::Pixel>,
{
    convert_generic_image::<NewPixel, I>(image)
}

/// Convert the supplied image to a grayscale image with the specified pixel type. Alpha channel is preserved.
#[deprecated(note = "use image::imageops::convert_generic_image() instead")]
pub fn grayscale_with_type_alpha<NewPixel, I: GenericImageView>(
    image: &I,
) -> ImageBuffer<NewPixel, Vec<NewPixel::Component>>
where
    NewPixel: ContiguousPixel + FromPixelCommon<<I as GenericImageView>::Pixel>,
{
    convert_generic_image::<NewPixel, I>(image)
}

/// Convert the supplied image to an image with a different pixel type.
pub fn convert_generic_image<NewPixel, I>(
    image: &I,
) -> ImageBuffer<NewPixel, Vec<NewPixel::Component>>
where
    I: GenericImageView,
    NewPixel: ContiguousPixel + FromPixelCommon<I::Pixel>,
{
    let (width, height) = image.dimensions();
    let mut out = ImageBuffer::new(width, height);

    for (x, y, pixel) in image.pixels() {
        out.put_pixel(x, y, NewPixel::from_pixel_common(pixel));
    }

    out
}

/// Invert each pixel within the supplied image.
/// This function operates in place.
pub fn invert<I: GenericImage>(image: &mut I)
where
    I::Pixel: Invert,
{
    // TODO find a way to use pixels?
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
///
/// *[See also `contrast_in_place`.][contrast_in_place]*
pub fn contrast<I, P, S>(image: &I, contrast: f32) -> ImageBuffer<P, Vec<S>>
where
    I: GenericImageView<Pixel = P>,
    P: Pixel<Component = S, SelfType<S> = P> + ContiguousPixel + 'static,
    S: PixelComponent + 'static,
{
    let (width, height) = image.dimensions();
    let mut out = ImageBuffer::new(width, height);

    let max = S::COMPONENT_MAX;
    let max: f32 = NumCast::from(max).unwrap();

    let percent = ((100.0 + contrast) / 100.0).powi(2);

    for (x, y, pixel) in image.pixels() {
        let f = pixel.map_components(|b| -> S {
            let c: f32 = NumCast::from(b).unwrap();

            let d = ((c / max - 0.5) * percent + 0.5) * max;
            let e = clamp(d, 0.0, max);

            NumCast::from(e).unwrap()
        });
        out.put_pixel(x, y, f);
    }

    out
}

/// Adjust the contrast of the supplied image in place.
/// ```contrast``` is the amount to adjust the contrast by.
/// Negative values decrease the contrast and positive values increase the contrast.
///
/// *[See also `contrast`.][contrast]*
pub fn contrast_in_place<I>(image: &mut I, contrast: f32)
where
    I: GenericImage,
    <I as GenericImageView>::Pixel: Pixel<SelfType<<I::Pixel as Pixel>::Component> = I::Pixel>,
{
    let (width, height) = image.dimensions();

    let max = <<I::Pixel as Pixel>::Component as PixelComponent>::COMPONENT_MAX;
    let max: f32 = NumCast::from(max).unwrap();

    let percent = ((100.0 + contrast) / 100.0).powi(2);

    // TODO find a way to use pixels?
    for y in 0..height {
        for x in 0..width {
            let f = image
                .get_pixel(x, y)
                .map_components(|b| -> <I::Pixel as Pixel>::Component {
                    let c: f32 = NumCast::from(b).unwrap();

                    let d = ((c / max - 0.5) * percent + 0.5) * max;
                    let e = clamp(d, 0.0, max);

                    NumCast::from(e).unwrap()
                });

            image.put_pixel(x, y, f);
        }
    }
}

/// Brighten the supplied image.
/// ```value``` is the amount to brighten each pixel by.
/// Negative values decrease the brightness and positive values increase it.
///
/// *[See also `brighten_in_place`.][brighten_in_place]*
pub fn brighten<I, P, S>(image: &I, value: i32) -> ImageBuffer<P, Vec<S>>
where
    I: GenericImageView<Pixel = P>,
    P: Pixel<Component = S> + ContiguousPixel + 'static,
    S: PixelComponent + 'static,
{
    let (width, height) = image.dimensions();
    let mut out = ImageBuffer::new(width, height);

    let max = S::COMPONENT_MAX;
    let max: i32 = NumCast::from(max).unwrap();

    for (x, y, pixel) in image.pixels() {
        let e = pixel.map_colors(|b| {
            let c: i32 = NumCast::from(b).unwrap();
            let d = clamp(c + value, 0, max);

            NumCast::from(d).unwrap()
        });
        out.put_pixel(x, y, e);
    }

    out
}

/// Brighten the supplied image in place.
/// ```value``` is the amount to brighten each pixel by.
/// Negative values decrease the brightness and positive values increase it.
///
/// *[See also `brighten`.][brighten]*
pub fn brighten_in_place<I>(image: &mut I, value: i32)
where
    I: GenericImage,
{
    let (width, height) = image.dimensions();

    let max = <I::Pixel as Pixel>::Component::COMPONENT_MAX;
    let max: i32 = NumCast::from(max).unwrap(); // TODO what does this do for f32? clamp at 1??

    // TODO find a way to use pixels?
    for y in 0..height {
        for x in 0..width {
            let e = image.get_pixel(x, y).map_colors(|b| {
                let c: i32 = NumCast::from(b).unwrap();
                let d = clamp(c + value, 0, max);

                NumCast::from(d).unwrap()
            });

            image.put_pixel(x, y, e);
        }
    }
}

/// Hue rotate the supplied image.
/// `value` is the degrees to rotate each pixel by.
/// 0 and 360 do nothing, the rest rotates by the given degree value.
/// just like the css webkit filter hue-rotate(180)
///
/// *[See also `huerotate_in_place`.][huerotate_in_place]*
pub fn huerotate<I, P, S>(image: &I, value: i32) -> ImageBuffer<P, Vec<S>>
where
    I: GenericImageView<Pixel = P>,
    P: Pixel<Component = S> + ContiguousPixel + 'static,
    S: PixelComponent + 'static,
    Rgba<f64>: FromPixelCommon<P>,
    P: FromPixelCommon<Rgba<f64>>,
{
    let (width, height) = image.dimensions();
    let mut out = ImageBuffer::new(width, height);

    let angle: f64 = NumCast::from(value).unwrap();

    let cosv = (angle * PI / 180.0).cos();
    let sinv = (angle * PI / 180.0).sin();
    let matrix: [f64; 9] = [
        // Reds
        0.213 + cosv * 0.787 - sinv * 0.213,
        0.715 - cosv * 0.715 - sinv * 0.715,
        0.072 - cosv * 0.072 + sinv * 0.928,
        // Greens
        0.213 - cosv * 0.213 + sinv * 0.143,
        0.715 + cosv * 0.285 + sinv * 0.140,
        0.072 - cosv * 0.072 - sinv * 0.283,
        // Blues
        0.213 - cosv * 0.213 - sinv * 0.787,
        0.715 - cosv * 0.715 + sinv * 0.715,
        0.072 + cosv * 0.928 + sinv * 0.072,
    ];
    for (x, y, pixel) in out.enumerate_pixels_mut() {
        let p = image.get_pixel(x, y);

        let p = Rgba::<f64>::from_pixel_common(p);

        let new_r = matrix[0] * p.r + matrix[1] * p.g + matrix[2] * p.b;
        let new_g = matrix[3] * p.r + matrix[4] * p.g + matrix[5] * p.b;
        let new_b = matrix[6] * p.r + matrix[7] * p.g + matrix[8] * p.b;

        let outpixel = P::from_pixel_common(Rgba {
            r: new_r,
            g: new_g,
            b: new_b,
            a: p.a,
        });

        *pixel = outpixel;
    }
    out
}

/// Hue rotate the supplied image in place.
/// `value` is the degrees to rotate each pixel by.
/// 0 and 360 do nothing, the rest rotates by the given degree value.
/// just like the css webkit filter hue-rotate(180)
///
/// *[See also `huerotate`.][huerotate]*
pub fn huerotate_in_place<I>(image: &mut I, value: i32)
where
    I: GenericImage,
    Rgba<f64>: FromPixelCommon<<I as GenericImageView>::Pixel>,
    <I as GenericImageView>::Pixel: FromPixelCommon<Rgba<f64>>,
{
    let (width, height) = image.dimensions();

    let angle: f64 = NumCast::from(value).unwrap();

    let cosv = (angle * PI / 180.0).cos();
    let sinv = (angle * PI / 180.0).sin();
    let matrix: [f64; 9] = [
        // Reds
        0.213 + cosv * 0.787 - sinv * 0.213,
        0.715 - cosv * 0.715 - sinv * 0.715,
        0.072 - cosv * 0.072 + sinv * 0.928,
        // Greens
        0.213 - cosv * 0.213 + sinv * 0.143,
        0.715 + cosv * 0.285 + sinv * 0.140,
        0.072 - cosv * 0.072 - sinv * 0.283,
        // Blues
        0.213 - cosv * 0.213 - sinv * 0.787,
        0.715 - cosv * 0.715 + sinv * 0.715,
        0.072 + cosv * 0.928 + sinv * 0.072,
    ];

    // TODO find a way to use pixels?
    for y in 0..height {
        for x in 0..width {
            let p = image.get_pixel(x, y);

            let p = Rgba::<f64>::from_pixel_common(p);

            let new_r = matrix[0] * p.r + matrix[1] * p.g + matrix[2] * p.b;
            let new_g = matrix[3] * p.r + matrix[4] * p.g + matrix[5] * p.b;
            let new_b = matrix[6] * p.r + matrix[7] * p.g + matrix[8] * p.b;

            let outpixel = <I as GenericImageView>::Pixel::from_pixel_common(Rgba {
                r: new_r,
                g: new_g,
                b: new_b,
                a: p.a,
            });

            image.put_pixel(x, y, outpixel);
        }
    }
}

/// A color map
pub trait ColorMap {
    /// The color type on which the map operates on
    type Color;
    /// Returns the index of the closest match of `color`
    /// in the color map.
    fn index_of(&self, color: &Self::Color) -> usize;
    /// Looks up color by index in the color map.  If `idx` is out of range for the color map, or
    /// ColorMap doesn't implement `lookup` `None` is returned.
    fn lookup(&self, index: usize) -> Option<Self::Color> {
        let _ = index;
        None
    }
    /// Determine if this implementation of ColorMap overrides the default `lookup`.
    fn has_lookup(&self) -> bool {
        false
    }
    /// Maps `color` to the closest color in the color map.
    fn map_color(&self, color: &mut Self::Color);
}

/// A bi-level color map
///
/// # Examples
/// ```
/// use image::imageops::colorops::{index_colors, BiLevel, ColorMap};
/// use image::ImageBuffer;
/// use pixeli::Gray;
///
/// let (w, h) = (16, 16);
/// // Create an image with a smooth horizontal gradient from black (0) to white (255).
/// let gray = ImageBuffer::from_fn(w, h, |x, y| -> Gray<u8> { [(255 * x / w) as u8].into() });
/// // Mapping the gray image through the `BiLevel` filter should map gray pixels less than half
/// // intensity (127) to black (0), and anything greater to white (255).
/// let cmap = BiLevel;
/// let palletized = index_colors(&gray, &cmap);
/// let mapped = ImageBuffer::from_fn(w, h, |x, y| {
///     let p = palletized.get_pixel(x, y);
///     cmap.lookup(p.gray as usize)
///         .expect("indexed color out-of-range")
/// });
/// // Create an black and white image of expected output.
/// let bw = ImageBuffer::from_fn(w, h, |x, y| -> Gray<u8> {
///     if x <= (w / 2) {
///         [0].into()
///     } else {
///         [255].into()
///     }
/// });
/// assert_eq!(mapped, bw);
/// ```
#[derive(Clone, Copy)]
pub struct BiLevel;

impl ColorMap for BiLevel {
    type Color = Gray<u8>;

    #[inline(always)]
    fn index_of(&self, color: &Gray<u8>) -> usize {
        if color.gray > 127 {
            1
        } else {
            0
        }
    }

    #[inline(always)]
    fn lookup(&self, idx: usize) -> Option<Self::Color> {
        match idx {
            0 => Some(Gray { gray: u8::MIN }),
            1 => Some(Gray { gray: u8::MAX }),
            _ => None,
        }
    }

    /// Indicate NeuQuant implements `lookup`.
    fn has_lookup(&self) -> bool {
        true
    }

    #[inline(always)]
    fn map_color(&self, color: &mut Gray<u8>) {
        let new_color = 0xFF * self.index_of(color) as u8;
        color.gray = new_color;
    }
}

#[cfg(feature = "color_quant")]
impl ColorMap for color_quant::NeuQuant {
    type Color = Rgba<u8>;

    #[inline(always)]
    fn index_of(&self, color: &Self::Color) -> usize {
        self.index_of(color.component_array().as_slice())
    }

    #[inline(always)]
    fn lookup(&self, idx: usize) -> Option<Self::Color> {
        self.lookup(idx).map(|p| p.into())
    }

    /// Indicate NeuQuant implements `lookup`.
    fn has_lookup(&self) -> bool {
        true
    }

    #[inline(always)]
    fn map_color(&self, color: &mut Self::Color) {
        self.map_pixel(color.component_array().as_mut_slice())
    }
}

/// Floyd-Steinberg error diffusion
fn diffuse_err<P: Pixel<Component = u8>>(pixel: &mut P, error: [i16; 3], factor: i16) {
    *pixel = P::from_components(
        error
            .into_iter()
            .zip(pixel.component_array())
            .map(|(e, c)| match <i16 as From<_>>::from(c) + e * factor / 16 {
                val if val < 0 => 0,
                val if val > 0xFF => 0xFF,
                val => val as u8,
            }),
    );
}

macro_rules! do_dithering(
    ($map:expr, $image:expr, $err:expr, $x:expr, $y:expr) => (
        {
            let old_pixel = $image[($x, $y)];
            let new_pixel = $image.get_pixel_mut($x, $y);
            $map.map_color(new_pixel);
            for ((e, old), new) in $err.iter_mut()
                                        .zip(old_pixel.component_array().into_iter())
                                        .zip(new_pixel.component_array().into_iter())
            {
                *e = <i16 as From<_>>::from(old) - <i16 as From<_>>::from(new)
            }
        }
    )
);

/// Reduces the colors of the image using the supplied `color_map` while applying
/// Floyd-Steinberg dithering to improve the visual conception
pub fn dither<Pix, Map>(image: &mut ImageBuffer<Pix, Vec<u8>>, color_map: &Map)
where
    Map: ColorMap<Color = Pix> + ?Sized,
    Pix: Pixel<Component = u8> + ContiguousPixel + 'static,
{
    let (width, height) = image.dimensions();
    let mut err: [i16; 3] = [0; 3];
    for y in 0..height - 1 {
        let x = 0;
        do_dithering!(color_map, image, err, x, y);
        diffuse_err(image.get_pixel_mut(x + 1, y), err, 7);
        diffuse_err(image.get_pixel_mut(x, y + 1), err, 5);
        diffuse_err(image.get_pixel_mut(x + 1, y + 1), err, 1);
        for x in 1..width - 1 {
            do_dithering!(color_map, image, err, x, y);
            diffuse_err(image.get_pixel_mut(x + 1, y), err, 7);
            diffuse_err(image.get_pixel_mut(x - 1, y + 1), err, 3);
            diffuse_err(image.get_pixel_mut(x, y + 1), err, 5);
            diffuse_err(image.get_pixel_mut(x + 1, y + 1), err, 1);
        }
        let x = width - 1;
        do_dithering!(color_map, image, err, x, y);
        diffuse_err(image.get_pixel_mut(x - 1, y + 1), err, 3);
        diffuse_err(image.get_pixel_mut(x, y + 1), err, 5);
    }
    let y = height - 1;
    let x = 0;
    do_dithering!(color_map, image, err, x, y);
    diffuse_err(image.get_pixel_mut(x + 1, y), err, 7);
    for x in 1..width - 1 {
        do_dithering!(color_map, image, err, x, y);
        diffuse_err(image.get_pixel_mut(x + 1, y), err, 7);
    }
    let x = width - 1;
    do_dithering!(color_map, image, err, x, y);
}

/// Reduces the colors using the supplied `color_map` and returns an image of the indices
pub fn index_colors<Pix, Map>(
    image: &ImageBuffer<Pix, Vec<u8>>,
    color_map: &Map,
) -> ImageBuffer<Gray<u8>, Vec<u8>>
where
    Map: ColorMap<Color = Pix> + ?Sized,
    Pix: Pixel<Component = u8> + ContiguousPixel + 'static,
{
    let mut indices = ImageBuffer::new(image.width(), image.height());
    for (pixel, idx) in image.pixels().zip(indices.pixels_mut()) {
        *idx = Gray {
            gray: color_map.index_of(pixel) as u8,
        }
    }
    indices
}

#[cfg(test)]
mod test {

    use super::*;
    use crate::GrayImage;

    macro_rules! assert_pixels_eq {
        ($actual:expr, $expected:expr) => {{
            let actual_dim = $actual.dimensions();
            let expected_dim = $expected.dimensions();

            if actual_dim != expected_dim {
                panic!(
                    "dimensions do not match. \
                     actual: {:?}, expected: {:?}",
                    actual_dim, expected_dim
                )
            }

            let diffs = pixel_diffs($actual, $expected);

            if !diffs.is_empty() {
                let mut err = "".to_string();

                let diff_messages = diffs
                    .iter()
                    .take(5)
                    .map(|d| format!("\nactual: {:?}, expected {:?} ", d.0, d.1))
                    .collect::<Vec<_>>()
                    .join("");

                err.push_str(&diff_messages);
                panic!("pixels do not match. {:?}", err)
            }
        }};
    }

    #[test]
    fn test_dither() {
        let mut image = ImageBuffer::from_raw(2, 2, vec![127, 127, 127, 127]).unwrap();
        let cmap = BiLevel;
        dither(&mut image, &cmap);
        assert_eq!(&*image, &[0, 0xFF, 0xFF, 0]);
        assert_eq!(index_colors(&image, &cmap).into_raw(), vec![0, 1, 1, 0])
    }

    #[test]
    fn test_grayscale() {
        let image: GrayImage =
            ImageBuffer::from_raw(3, 2, vec![0u8, 1u8, 2u8, 10u8, 11u8, 12u8]).unwrap();

        let expected: GrayImage =
            ImageBuffer::from_raw(3, 2, vec![0u8, 1u8, 2u8, 10u8, 11u8, 12u8]).unwrap();

        assert_pixels_eq!(&convert_generic_image::<Gray<u8>, _>(&image), &expected);
    }

    #[test]
    fn test_invert() {
        let mut image: GrayImage =
            ImageBuffer::from_raw(3, 2, vec![0u8, 1u8, 2u8, 10u8, 11u8, 12u8]).unwrap();

        let expected: GrayImage =
            ImageBuffer::from_raw(3, 2, vec![255u8, 254u8, 253u8, 245u8, 244u8, 243u8]).unwrap();

        invert(&mut image);
        assert_pixels_eq!(&image, &expected);
    }
    #[test]
    fn test_brighten() {
        let image: GrayImage =
            ImageBuffer::from_raw(3, 2, vec![0u8, 1u8, 2u8, 10u8, 11u8, 12u8]).unwrap();

        let expected: GrayImage =
            ImageBuffer::from_raw(3, 2, vec![10u8, 11u8, 12u8, 20u8, 21u8, 22u8]).unwrap();

        assert_pixels_eq!(&brighten(&image, 10), &expected);
    }

    #[test]
    fn test_brighten_place() {
        let mut image: GrayImage =
            ImageBuffer::from_raw(3, 2, vec![0u8, 1u8, 2u8, 10u8, 11u8, 12u8]).unwrap();

        let expected: GrayImage =
            ImageBuffer::from_raw(3, 2, vec![10u8, 11u8, 12u8, 20u8, 21u8, 22u8]).unwrap();

        brighten_in_place(&mut image, 10);
        assert_pixels_eq!(&image, &expected);
    }

    #[allow(clippy::type_complexity)]
    fn pixel_diffs<I, J, P>(left: &I, right: &J) -> Vec<((u32, u32, P), (u32, u32, P))>
    where
        I: GenericImage<Pixel = P>,
        J: GenericImage<Pixel = P>,
        P: Pixel + Eq,
    {
        left.pixels()
            .zip(right.pixels())
            .filter(|&(p, q)| p != q)
            .collect::<Vec<_>>()
    }
}
