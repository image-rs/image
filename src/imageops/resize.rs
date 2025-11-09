use std::ops::{AddAssign, BitXor};

use crate::{DynamicImage, ImageBuffer, Pixel};
use pic_scale_safe::ResamplingFunction;

pub(crate) fn resize_impl(
    image: &mut DynamicImage,
    dst_width: u32,
    dst_height: u32,
    algorithm: ResamplingFunction,
) -> Result<DynamicImage, String> {
    if image.width() == dst_width && image.height() == dst_height {
        return Ok(image.clone());
    }
    let alg = algorithm; // otherwise rustfmt breaks up too-long-lines and the formatting is a mess
    let src_size = ImageSize::new(image.width() as usize, image.height() as usize);
    let dst_size = ImageSize::new(dst_width as usize, dst_height as usize);

    // Premultiply the image by alpha channel to avoid color bleed from fully transparent pixels.
    let mut premultiplied_by_alpha = false;
    // There is no need to premultiply for nearest-neighbor resampling because it does not perform any blending.
    // This is actually a valuable optimization because -thumbnail uses nearest-neighbor for a part of the process.
    if algorithm != ResamplingFunction::Nearest {
        premultiplied_by_alpha = premultiply_alpha_if_needed(image);
    }

    use pic_scale_safe::*;
    use DynamicImage::*;
    let mut resized = match image {
        ImageLuma8(src) => {
            let resized = resize_plane8(src.as_raw(), src_size, dst_size, alg)?;
            ImageLuma8(ImageBuffer::from_raw(dst_width, dst_height, resized).unwrap())
        }
        ImageLumaA8(src) => {
            let resized = resize_plane8_with_alpha(src.as_raw(), src_size, dst_size, alg)?;
            ImageLumaA8(ImageBuffer::from_raw(dst_width, dst_height, resized).unwrap())
        }
        ImageRgb8(src) => {
            let resized = resize_rgb8(src.as_raw(), src_size, dst_size, alg)?;
            ImageRgb8(ImageBuffer::from_raw(dst_width, dst_height, resized).unwrap())
        }
        ImageRgba8(src) => {
            let resized = resize_rgba8(src.as_raw(), src_size, dst_size, alg)?;
            ImageRgba8(ImageBuffer::from_raw(dst_width, dst_height, resized).unwrap())
        }
        ImageLuma16(src) => {
            let resized = resize_plane16(src.as_raw(), src_size, dst_size, 16, alg)?;
            ImageLuma16(ImageBuffer::from_raw(dst_width, dst_height, resized).unwrap())
        }
        ImageLumaA16(src) => {
            let resized = resize_plane16_with_alpha(src.as_raw(), src_size, dst_size, 16, alg)?;
            ImageLumaA16(ImageBuffer::from_raw(dst_width, dst_height, resized).unwrap())
        }
        ImageRgb16(src) => {
            let resized = resize_rgb16(src.as_raw(), src_size, dst_size, 16, alg)?;
            ImageRgb16(ImageBuffer::from_raw(dst_width, dst_height, resized).unwrap())
        }
        ImageRgba16(src) => {
            let resized = resize_rgba16(src.as_raw(), src_size, dst_size, 16, alg)?;
            ImageRgba16(ImageBuffer::from_raw(dst_width, dst_height, resized).unwrap())
        }
        ImageRgb32F(src) => {
            let resized = resize_rgb_f32(src.as_raw(), src_size, dst_size, alg)?;
            ImageRgb32F(ImageBuffer::from_raw(dst_width, dst_height, resized).unwrap())
        }
        ImageRgba32F(src) => {
            let resized = resize_rgba_f32(src.as_raw(), src_size, dst_size, alg)?;
            ImageRgba32F(ImageBuffer::from_raw(dst_width, dst_height, resized).unwrap())
        }
    };
    if premultiplied_by_alpha {
        unpremultiply_alpha(&mut resized);
    }
    Ok(resized)
}

/// Return value indicates whether the image was in premultiplied by alpha
#[must_use]
fn premultiply_alpha_if_needed(image: &mut DynamicImage) -> bool {
    use pic_scale_safe::*;
    if !has_constant_alpha(image) {
        match image {
            DynamicImage::ImageLuma8(_) => false,
            DynamicImage::ImageLumaA8(buf) => {
                premultiply_la8(buf.as_mut());
                true
            }
            DynamicImage::ImageRgb8(_) => false,
            DynamicImage::ImageRgba8(buf) => {
                premultiply_rgba8(buf.as_mut());
                true
            }
            DynamicImage::ImageLuma16(_) => false,
            DynamicImage::ImageLumaA16(buf) => {
                premultiply_la16(buf.as_mut(), 16);
                true
            }
            DynamicImage::ImageRgb16(_) => false,
            DynamicImage::ImageRgba16(buf) => {
                premultiply_rgba16(buf.as_mut(), 16);
                true
            }
            DynamicImage::ImageRgb32F(_) => false,
            DynamicImage::ImageRgba32F(buf) => {
                premultiply_rgba_f32(buf.as_mut());
                true
            }
        }
    } else {
        false
    }
}

/// Reverses premultiplication by alpha
fn unpremultiply_alpha(image: &mut DynamicImage) {
    use pic_scale_safe::*;
    match image {
        DynamicImage::ImageLuma8(_) => (),
        DynamicImage::ImageLumaA8(buf) => unpremultiply_la8(buf.as_mut()),
        DynamicImage::ImageRgb8(_) => (),
        DynamicImage::ImageRgba8(buf) => unpremultiply_rgba8(buf.as_mut()),
        DynamicImage::ImageLuma16(_) => (),
        DynamicImage::ImageLumaA16(buf) => unpremultiply_la16(buf.as_mut(), 16),
        DynamicImage::ImageRgb16(_) => (),
        DynamicImage::ImageRgba16(buf) => unpremultiply_rgba16(buf.as_mut(), 16),
        DynamicImage::ImageRgb32F(_) => (),
        DynamicImage::ImageRgba32F(buf) => unpremultiply_rgba_f32(buf),
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
        DynamicImage::ImageRgb32F(_) => true,
        DynamicImage::ImageRgba32F(buf) => has_constant_alpha_f32(buf),
    }
}

#[must_use]
fn has_constant_alpha_integer<P, Container>(img: &ImageBuffer<P, Container>) -> bool
where
    P: Pixel + 'static,
    Container: std::ops::Deref<Target = [P::Subpixel]>,
    P::Subpixel:
        Copy + PartialEq + BitXor<P::Subpixel, Output = P::Subpixel> + AddAssign + Into<u64>,
{
    let first_pixel_alpha = *match img.pixels().next() {
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
        row.for_each(|pixel| {
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
fn has_constant_alpha_f32(img: &ImageBuffer<crate::Rgba<f32>, Vec<f32>>) -> bool {
    // Optimizing correctly in presence of NaNs and infinities is tricky, so just do the naive thing for now
    let first_pixel_alpha = match img.pixels().next() {
        Some(pixel) => pixel.alpha(),
        None => return true, // empty input image
    };
    img.pixels()
        .map(|pixel| pixel.channels().last().unwrap())
        .all(|alpha| *alpha == first_pixel_alpha)
}
