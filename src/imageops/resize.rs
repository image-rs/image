//! Implementation of resizing delegated to pic-scale-safe crate.
//! It is an all-safe-code implementation with competitive performance
//! and with optional parallelism to boost it even further.
//!
//! Everything that references pic-scale-safe crate is contained to this file
//! so that it could easily be made an optional dependency in the future.

use std::ops::{AddAssign, BitXor};

use crate::{imageops::FilterType, DynamicImage, ImageBuffer, Pixel};
use pic_scale_safe::ResamplingFunction;

pub(crate) fn resize_impl(
    image: &mut DynamicImage,
    dst_width: u32,
    dst_height: u32,
    algorithm: FilterType,
) -> Result<(), String> {
    if image.width() == dst_width && image.height() == dst_height {
        return Ok(());
    }
    let algorithm = convert_filter_type(algorithm);
    let alg = algorithm; // otherwise rustfmt breaks up too-long-lines and the formatting is a mess
    let src_size = ImageSize::new(image.width() as usize, image.height() as usize);
    let dst_size = ImageSize::new(dst_width as usize, dst_height as usize);

    // Premultiply the image by alpha channel to avoid color bleed from fully transparent pixels.
    let mut premultiplied_by_alpha = false;
    // There is no need to premultiply for nearest-neighbor resampling because it does not perform any blending.
    // This is actually a valuable optimization because thumbnailing uses nearest-neighbor for a part of the process.
    if algorithm != ResamplingFunction::Nearest && !has_constant_alpha(image) {
        premultiply_alpha(image);
        premultiplied_by_alpha = true;
    }

    use pic_scale_safe::*;
    use DynamicImage::*;
    let mut resized = match image {
        // pic-scale-safe's area filter gives whole input pixels full weight
        // when downscaling by a non-integer factor, so use our own.
        _ if alg == ResamplingFunction::Area => resize_area(image, dst_width, dst_height),
        ImageLuma8(src) => {
            let resized = resize_plane8(src.subpixels(), src_size, dst_size, alg)?;
            ImageLuma8(ImageBuffer::from_raw(dst_width, dst_height, resized).unwrap())
        }
        ImageLumaA8(src) => {
            let resized = resize_plane8_with_alpha(src.subpixels(), src_size, dst_size, alg)?;
            ImageLumaA8(ImageBuffer::from_raw(dst_width, dst_height, resized).unwrap())
        }
        ImageRgb8(src) => {
            let resized = resize_rgb8(src.subpixels(), src_size, dst_size, alg)?;
            ImageRgb8(ImageBuffer::from_raw(dst_width, dst_height, resized).unwrap())
        }
        ImageRgba8(src) => {
            let resized = resize_rgba8(src.subpixels(), src_size, dst_size, alg)?;
            ImageRgba8(ImageBuffer::from_raw(dst_width, dst_height, resized).unwrap())
        }
        ImageLuma16(src) => {
            let resized = resize_plane16(src.subpixels(), src_size, dst_size, 16, alg)?;
            ImageLuma16(ImageBuffer::from_raw(dst_width, dst_height, resized).unwrap())
        }
        ImageLumaA16(src) => {
            let resized = resize_plane16_with_alpha(src.subpixels(), src_size, dst_size, 16, alg)?;
            ImageLumaA16(ImageBuffer::from_raw(dst_width, dst_height, resized).unwrap())
        }
        ImageRgb16(src) => {
            let resized = resize_rgb16(src.subpixels(), src_size, dst_size, 16, alg)?;
            ImageRgb16(ImageBuffer::from_raw(dst_width, dst_height, resized).unwrap())
        }
        ImageRgba16(src) => {
            let resized = resize_rgba16(src.subpixels(), src_size, dst_size, 16, alg)?;
            ImageRgba16(ImageBuffer::from_raw(dst_width, dst_height, resized).unwrap())
        }
        ImageLuma32F(src) => {
            let resized = resize_plane_f32(src.as_raw(), src_size, dst_size, alg)?;
            ImageLuma32F(ImageBuffer::from_raw(dst_width, dst_height, resized).unwrap())
        }
        ImageLumaA32F(src) => {
            let resized = resize_luma_alpha_f32(src.as_raw(), src_size, dst_size, alg)?;
            ImageLumaA32F(ImageBuffer::from_raw(dst_width, dst_height, resized).unwrap())
        }
        ImageRgb32F(src) => {
            let resized = resize_rgb_f32(src.subpixels(), src_size, dst_size, alg)?;
            ImageRgb32F(ImageBuffer::from_raw(dst_width, dst_height, resized).unwrap())
        }
        ImageRgba32F(src) => {
            let resized = resize_rgba_f32(src.subpixels(), src_size, dst_size, alg)?;
            ImageRgba32F(ImageBuffer::from_raw(dst_width, dst_height, resized).unwrap())
        }
    };
    if premultiplied_by_alpha {
        unpremultiply_alpha(&mut resized);
    }
    *image = resized;
    Ok(())
}

fn resize_area(image: &DynamicImage, dst_width: u32, dst_height: u32) -> DynamicImage {
    use crate::imageops::resize;
    use DynamicImage::*;
    let (w, h, f) = (dst_width, dst_height, FilterType::Area);
    match image {
        ImageLuma8(src) => ImageLuma8(resize(src, w, h, f)),
        ImageLumaA8(src) => ImageLumaA8(resize(src, w, h, f)),
        ImageRgb8(src) => ImageRgb8(resize(src, w, h, f)),
        ImageRgba8(src) => ImageRgba8(resize(src, w, h, f)),
        ImageLuma16(src) => ImageLuma16(resize(src, w, h, f)),
        ImageLumaA16(src) => ImageLumaA16(resize(src, w, h, f)),
        ImageRgb16(src) => ImageRgb16(resize(src, w, h, f)),
        ImageRgba16(src) => ImageRgba16(resize(src, w, h, f)),
        ImageLuma32F(src) => ImageLuma32F(resize(src, w, h, f)),
        ImageLumaA32F(src) => ImageLumaA32F(resize(src, w, h, f)),
        ImageRgb32F(src) => ImageRgb32F(resize(src, w, h, f)),
        ImageRgba32F(src) => ImageRgba32F(resize(src, w, h, f)),
    }
}

fn convert_filter_type(filter: FilterType) -> ResamplingFunction {
    match filter {
        FilterType::Nearest => ResamplingFunction::Nearest,
        FilterType::Triangle => ResamplingFunction::Bilinear,
        FilterType::CatmullRom => ResamplingFunction::CatmullRom,
        FilterType::Gaussian => ResamplingFunction::Gaussian,
        FilterType::Lanczos3 => ResamplingFunction::Lanczos3,
        FilterType::Area => ResamplingFunction::Area,
    }
}

fn premultiply_alpha(image: &mut DynamicImage) {
    use pic_scale_safe::*;
    match image {
        DynamicImage::ImageLuma8(_) => (),
        DynamicImage::ImageLumaA8(buf) => {
            premultiply_la8(buf.subpixels_mut());
        }
        DynamicImage::ImageRgb8(_) => (),
        DynamicImage::ImageRgba8(buf) => {
            premultiply_rgba8(buf.subpixels_mut());
        }
        DynamicImage::ImageLuma16(_) => (),
        DynamicImage::ImageLumaA16(buf) => {
            premultiply_la16(buf.subpixels_mut(), 16);
        }
        DynamicImage::ImageRgb16(_) => (),
        DynamicImage::ImageRgba16(buf) => {
            premultiply_rgba16(buf.subpixels_mut(), 16);
        }
        DynamicImage::ImageLuma32F(_) => (),
        DynamicImage::ImageLumaA32F(buf) => {
            premultiply_luma_alpha_f32(buf.as_mut());
        }
        DynamicImage::ImageRgb32F(_) => (),
        DynamicImage::ImageRgba32F(buf) => {
            premultiply_rgba_f32(buf.subpixels_mut());
        }
    }
}

/// Reverses premultiplication by alpha
fn unpremultiply_alpha(image: &mut DynamicImage) {
    use pic_scale_safe::*;
    match image {
        DynamicImage::ImageLuma8(_) => (),
        DynamicImage::ImageLumaA8(buf) => unpremultiply_la8(buf.subpixels_mut()),
        DynamicImage::ImageRgb8(_) => (),
        DynamicImage::ImageRgba8(buf) => unpremultiply_rgba8(buf.subpixels_mut()),
        DynamicImage::ImageLuma16(_) => (),
        DynamicImage::ImageLumaA16(buf) => unpremultiply_la16(buf.subpixels_mut(), 16),
        DynamicImage::ImageRgb16(_) => (),
        DynamicImage::ImageRgba16(buf) => unpremultiply_rgba16(buf.subpixels_mut(), 16),
        DynamicImage::ImageLuma32F(_) => (),
        DynamicImage::ImageLumaA32F(buf) => unpremultiply_luma_alpha_f32(buf.subpixels_mut()),
        DynamicImage::ImageRgb32F(_) => (),
        DynamicImage::ImageRgba32F(buf) => unpremultiply_rgba_f32(buf.subpixels_mut()),
    }
}

#[must_use]
fn has_constant_alpha(image: &DynamicImage) -> bool {
    match image {
        DynamicImage::ImageLuma8(_) => true,
        DynamicImage::ImageLumaA8(buf) => has_constant_alpha_integer(buf),
        DynamicImage::ImageRgb8(_) => true,
        DynamicImage::ImageRgba8(buf) => has_constant_alpha_integer(buf),
        DynamicImage::ImageLuma16(_) => true,
        DynamicImage::ImageLumaA16(buf) => has_constant_alpha_integer(buf),
        DynamicImage::ImageRgb16(_) => true,
        DynamicImage::ImageRgba16(buf) => has_constant_alpha_integer(buf),
        DynamicImage::ImageLuma32F(_) => true,
        DynamicImage::ImageLumaA32F(buf) => has_constant_alpha_f32(buf),
        DynamicImage::ImageRgb32F(_) => true,
        DynamicImage::ImageRgba32F(buf) => has_constant_alpha_f32(buf),
    }
}

#[must_use]
fn has_constant_alpha_integer<P, Container>(img: &ImageBuffer<P, Container>) -> bool
where
    P: Pixel,
    Container: std::ops::Deref<Target = [P::Subpixel]>,
    P::Subpixel:
        Copy + PartialEq + BitXor<P::Subpixel, Output = P::Subpixel> + AddAssign + Into<u64>,
{
    let first_pixel_alpha = *match img.pixels().first() {
        Some(pixel) => pixel.channels().last().unwrap(), // there doesn't seem to be a better way to retrieve the alpha channel
        None => return true,                             // empty input image
    };
    // A naive, slower implementation that branches on every pixel:
    //
    // img.pixels().map(|pixel| pixel.channels().last().unwrap()).all(|alpha| alpha == first_pixel_alpha);
    //
    // Instead of doing that we scan every row first with cheap arithmetic instructions
    // and only compare the sum of divergences on every row, which should be 0
    let mut sum_of_diffs: u64 = 0;
    for row in img.rows() {
        row.iter().for_each(|pixel| {
            let alpha = pixel.alpha();
            sum_of_diffs += alpha.bitxor(first_pixel_alpha).into();
        });
        if sum_of_diffs != 0 {
            return false;
        }
    }
    true
}

#[must_use]
fn has_constant_alpha_f32<P, Container>(img: &ImageBuffer<P, Container>) -> bool
where
    P: Pixel<Subpixel = f32>,
    Container: std::ops::Deref<Target = [f32]>,
{
    // Optimizing correctly in presence of NaNs and infinities is tricky, so just do the naive thing for now
    let first_pixel_alpha = match img.pixels().first() {
        Some(pixel) => pixel.alpha(),
        None => return true, // empty input image
    };
    img.pixels()
        .iter()
        .map(|pixel| pixel.channels().last().unwrap())
        .all(|alpha| *alpha == first_pixel_alpha)
}

#[cfg(test)]
mod tests {
    use crate::{imageops::resize::resize_impl, DynamicImage};

    #[test]
    fn smoke_test() {
        let mut image = DynamicImage::new(10, 10, crate::ColorType::Rgba8);
        resize_impl(&mut image, 5, 5, crate::imageops::FilterType::Lanczos3).unwrap();
        assert_eq!(image.height(), 5);
        assert_eq!(image.width(), 5);
    }

    #[test]
    fn area() {
        // (input, output width, expected output)
        let cases: [(&[u8], u32, &[u8]); 3] = [
            // Integer factor: each output pixel is the mean of two input pixels.
            (&[0, 10, 20, 40], 2, &[5, 30]),
            // Each output pixel covers one and a half input pixels.
            (&[0, 90, 180], 2, &[30, 150]),
            // Upscaling: the middle output pixel covers a third of each input pixel.
            (&[0, 90], 3, &[0, 45, 90]),
        ];

        for (input, width, expected) in cases {
            let len = input.len() as u32;
            let row = crate::GrayImage::from_raw(len, 1, input.to_vec()).unwrap();
            let mut image = DynamicImage::ImageLuma8(row);
            resize_impl(&mut image, width, 1, crate::imageops::FilterType::Area).unwrap();
            assert_eq!(image.as_bytes(), expected, "row {input:?}");

            let col = crate::GrayImage::from_raw(1, len, input.to_vec()).unwrap();
            let mut image = DynamicImage::ImageLuma8(col);
            resize_impl(&mut image, 1, width, crate::imageops::FilterType::Area).unwrap();
            assert_eq!(image.as_bytes(), expected, "column {input:?}");
        }

        // Float values above 1.0 are not clamped.
        let pixels = [0.0, 3.0, 6.0].map(|v| [v; 3]).concat();
        let row = crate::Rgb32FImage::from_raw(3, 1, pixels).unwrap();
        let mut image = DynamicImage::ImageRgb32F(row);
        resize_impl(&mut image, 2, 1, crate::imageops::FilterType::Area).unwrap();
        let out = image.as_rgb32f().unwrap().as_raw();
        assert_eq!(out.as_slice(), [1.0, 1.0, 1.0, 5.0, 5.0, 5.0]);
    }
}
