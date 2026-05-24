//! Module for crate-private traits implemented for all primitive types.

use crate::imageops::fast_blur::BlurAccumulator;

/// Crate-private trait to seal the [`Primitive`](crate::Primitive) trait.
///
/// This trait is `pub` but not exported, so it cannot be implemented outside
/// this crate.
#[allow(private_bounds)]
pub trait PrimitiveSealed:
    Sized + NearestFrom<f32> + WithBlurAcc + BgraSwizzle + RgbToLuma
{
}

impl PrimitiveSealed for usize {}
impl PrimitiveSealed for u8 {}
impl PrimitiveSealed for u16 {}
impl PrimitiveSealed for u32 {}
impl PrimitiveSealed for u64 {}
impl PrimitiveSealed for isize {}
impl PrimitiveSealed for i8 {}
impl PrimitiveSealed for i16 {}
impl PrimitiveSealed for i32 {}
impl PrimitiveSealed for i64 {}
impl PrimitiveSealed for f32 {}
impl PrimitiveSealed for f64 {}

/// Defines specialized methods for rgb<->bgr and rgba<->bgra swizzles
///
/// By default, uses as_chunks_mut and swaps the first and third elements in the pixel slice.
/// For u8 rgba however, benchmarks have shown that interpreting the 4 bytes as a u32 and swap+rotate
/// ends up autovectorizing better.
// Note: no attempts have been made to find if a similar optimization could apply to primitives beyond u8 or to bgr instead of bgra.
pub(crate) trait BgraSwizzle: Sized {
    fn swizzle_rgb_bgr(pixels: &mut [Self]) {
        for pixel in pixels.as_chunks_mut::<3>().0 {
            pixel.swap(0, 2);
        }
    }
    fn swizzle_rgba_bgra(pixels: &mut [Self]) {
        for pix in pixels.as_chunks_mut::<4>().0 {
            pix.swap(0, 2);
        }
    }
}

impl BgraSwizzle for usize {}
impl BgraSwizzle for u8 {
    fn swizzle_rgba_bgra(pixels: &mut [Self]) {
        for pix in pixels.as_chunks_mut::<4>().0 {
            let bgra = u32::from_be_bytes(*pix);
            let argb = bgra.swap_bytes(); // reverses order of channels (bytes)
            let rgba = argb.rotate_left(8); // rotate first byte to last place
            *pix = rgba.to_be_bytes();
        }
    }
}
impl BgraSwizzle for u16 {}
impl BgraSwizzle for u32 {}
impl BgraSwizzle for u64 {}
impl BgraSwizzle for isize {}
impl BgraSwizzle for i8 {}
impl BgraSwizzle for i16 {}
impl BgraSwizzle for i32 {}
impl BgraSwizzle for i64 {}
impl BgraSwizzle for f32 {}
impl BgraSwizzle for f64 {}

/// Returns the nearest value of `Self` to a given value.
///
/// Properties:
/// - For a float -> int conversion:
///     - The float is rounded to the nearest integer.
///       (An implementation may use a fast approximation instead of precise rounding.)
///     - NaN is mapped to 0.
///     - Values outside the range of the integer type are clamped to the min or max value.
/// - For a float -> float conversion:
///     - The float is clamped to the range `[0.0, 1.0]`.
///     - NaN is mapped to 0.0.
pub(crate) trait NearestFrom<T> {
    /// Returns the nearest value of `Self` to `value`.
    ///
    /// Properties:
    /// - For a float -> int conversion:
    ///     - The float is rounded to the nearest integer.
    ///       (An implementation may use a fast approximation instead of precise rounding.)
    ///     - NaN is mapped to 0.
    ///     - Values outside the range of the integer type are clamped to the min or max value.
    /// - For a float -> float conversion:
    ///     - Values outside are clamped to +-inf.
    ///     - NaN is kept as NaN.
    ///     - Precision may be lost.
    fn nearest_from(value: T) -> Self;

    /// Returns the nearest value of `Self` to `value` *within* the default
    /// range of `Self`. This range is 0..=1 for floats and the full range of
    /// the integer type for integers.
    ///
    /// If `Self` is an integer type, this is the same as `nearest_from`.
    ///
    /// Properties:
    /// - For a float -> int conversion: Same as `nearest_from`.
    /// - For a float -> float conversion:
    ///     - The float is clamped to the range `[0.0, 1.0]`.
    ///     - NaN is mapped to 0.0.
    ///     - Precision may be lost.
    fn clamp_nearest_from(value: T) -> Self;
}

impl NearestFrom<f32> for u8 {
    fn nearest_from(value: f32) -> Self {
        // Approximate rounding using the well-known + 0.5 trick.
        // This does not handle certain cases correctly. E.g. `0.5_f32.nextdown()`
        // is incorrectly rounded to 1 instead of 0. However, this isn't typically
        // an issue in practice.
        (value + 0.5) as u8
    }
    fn clamp_nearest_from(value: f32) -> Self {
        Self::nearest_from(value)
    }
}
impl NearestFrom<f32> for u16 {
    fn nearest_from(value: f32) -> Self {
        (value + 0.5) as u16
    }
    fn clamp_nearest_from(value: f32) -> Self {
        Self::nearest_from(value)
    }
}
impl NearestFrom<f32> for f32 {
    fn nearest_from(value: f32) -> Self {
        value
    }
    #[allow(clippy::manual_clamp)] // to map NaN to 0.0
    fn clamp_nearest_from(value: f32) -> Self {
        value.max(0.0).min(1.0)
    }
}
impl NearestFrom<f32> for f64 {
    fn nearest_from(value: f32) -> Self {
        value as f64
    }
    #[allow(clippy::manual_clamp)] // to map NaN to 0.0
    fn clamp_nearest_from(value: f32) -> Self {
        value.max(0.0).min(1.0) as f64
    }
}

macro_rules! impl_nearest_from_f32_for_ints {
    ($($t:ty),+) => { $(
        impl NearestFrom<f32> for $t {
            fn nearest_from(value: f32) -> Self {
                value.round() as $t
            }
            fn clamp_nearest_from(value: f32) -> Self {
                Self::nearest_from(value)
            }
        }
    )+ };
}
impl_nearest_from_f32_for_ints!(u32, u64, usize, i8, i16, i32, i64, isize);

/// Crate-private companion to [`Primitive`] that picks the box-blur
/// accumulator type for each primitive.
///
/// `u8` uses an integer (`u32`) accumulator for speed; everything else goes
/// through `f32`.
pub(crate) trait WithBlurAcc: Sized {
    type BlurAcc: BlurAccumulator<Self>;
}

impl WithBlurAcc for u8 {
    type BlurAcc = u32;
}

macro_rules! impl_with_blur_acc_f32 {
    ($($t:ty),+) => { $(
        impl WithBlurAcc for $t {
            type BlurAcc = f32;
        }
    )+ };
}

impl_with_blur_acc_f32!(u16, u32, u64, usize, i8, i16, i32, i64, isize, f32, f64);

/// Coefficients to transform from sRGB to a CIE Y (luminance) value.
///
/// The coefficients are 2126/10000, 7152/10000, and 722/10000 for R, G, and B respectively.
const SRGB_LUMA: [u32; 3] = [2126, 7152, 722];
const SRGB_LUMA_DIV: u32 = 10000;
pub(crate) trait RgbToLuma: Copy + Sized + crate::traits::Enlargeable {
    #[inline]
    fn rgb_to_luma(r: Self, g: Self, b: Self) -> Self {
        use num_traits::NumCast;

        let l = <Self::Larger as NumCast>::from(SRGB_LUMA[0]).unwrap() * r.to_larger()
            + <Self::Larger as NumCast>::from(SRGB_LUMA[1]).unwrap() * g.to_larger()
            + <Self::Larger as NumCast>::from(SRGB_LUMA[2]).unwrap() * b.to_larger();
        Self::clamp_from(l / <Self::Larger as NumCast>::from(SRGB_LUMA_DIV).unwrap())
    }
}
impl RgbToLuma for usize {}
impl RgbToLuma for u8 {
    fn rgb_to_luma(r: u8, g: u8, b: u8) -> u8 {
        // The following constants give the same results as:
        //   ((r as u32 * 2126 + g as u32 * 7152 + b as u32 * 722 + 5000) / 10000) as u8
        // Note that results are correctly rounded to the nearest integer.
        const W_R: u32 = ((1_u64 << 24) * 2126).div_ceil(10000) as u32;
        const W_G: u32 = ((1_u64 << 24) * 7152).div_ceil(10000) as u32;
        const W_B: u32 = ((1_u64 << 24) * 722).div_ceil(10000) as u32;
        ((r as u32 * W_R + g as u32 * W_G + b as u32 * W_B + 0x800000) >> 24) as u8
    }
}
impl RgbToLuma for u16 {}
impl RgbToLuma for u32 {}
impl RgbToLuma for u64 {}
impl RgbToLuma for isize {}
impl RgbToLuma for i8 {}
impl RgbToLuma for i16 {}
impl RgbToLuma for i32 {}
impl RgbToLuma for i64 {}
impl RgbToLuma for f32 {
    #[inline]
    fn rgb_to_luma(r: f32, g: f32, b: f32) -> f32 {
        const SCALE_R: f32 = SRGB_LUMA[0] as f32 / SRGB_LUMA_DIV as f32;
        const SCALE_G: f32 = SRGB_LUMA[1] as f32 / SRGB_LUMA_DIV as f32;
        const SCALE_B: f32 = SRGB_LUMA[2] as f32 / SRGB_LUMA_DIV as f32;
        SCALE_R * r + SCALE_G * g + SCALE_B * b
    }
}
impl RgbToLuma for f64 {
    #[inline]
    fn rgb_to_luma(r: f64, g: f64, b: f64) -> f64 {
        const SCALE_R: f64 = SRGB_LUMA[0] as f64 / SRGB_LUMA_DIV as f64;
        const SCALE_G: f64 = SRGB_LUMA[1] as f64 / SRGB_LUMA_DIV as f64;
        const SCALE_B: f64 = SRGB_LUMA[2] as f64 / SRGB_LUMA_DIV as f64;
        SCALE_R * r + SCALE_G * g + SCALE_B * b
    }
}
