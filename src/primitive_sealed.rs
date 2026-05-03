//! Module for crate-private traits implemented for all primitive types.

use std::ops::{Add, AddAssign, Sub, SubAssign};

use num_traits::ToPrimitive;

/// Crate-private trait to seal the [`Primitive`](crate::Primitive) trait.
///
/// This trait is `pub` but not exported, so it cannot be implemented outside
/// this crate.
#[allow(private_bounds)]
pub trait PrimitiveSealed: Sized + NearestFrom<f32> {}

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

/// Precomputed reciprocal for fast integer division of u32 accumulators.
///
/// Replaces `(acc + ks/2) / ks` with a multiply-shift using the identity:
///   `floor(n / d) == floor(n * ceil(2^32 / d) / 2^32)`
/// which is exact for all `n` where `(d-1)*n < d * 2^32` (Granlund-Montgomery '94).
/// For blur accumulators (`n <= kernel_size * 255`), this holds for all practical sizes.
#[derive(Clone, Copy)]
pub struct U8Weight {
    reciprocal: u32, // ceil(2^32 / kernel_size)
    rounding_bias: u32, // kernel_size / 2
}

impl U8Weight {
    #[inline]
    fn new(kernel_size: u32) -> Self {
        debug_assert!(kernel_size >= 2);
        Self {
            // ceil(2^32 / ks) = floor((2^32 - 1) / ks) + 1 = (u32::MAX / ks) + 1
            reciprocal: (u32::MAX / kernel_size) + 1,
            rounding_bias: kernel_size / 2,
        }
    }

    /// Compute `(acc + bias) / kernel_size` using reciprocal multiplication.
    #[inline(always)]
    fn apply(self, acc: u32) -> u8 {
        let n = acc + self.rounding_bias;
        ((n as u64 * self.reciprocal as u64) >> 32) as u8
    }
}

#[inline]
fn rounding_saturating_mul<T: crate::Primitive>(v: f32, w: f32) -> T {
    T::clamp_nearest_from(v * w)
}

/// Accumulator abstraction for box blur.
/// `u8` uses `u32` integer accumulators; other types use `f32`.
pub trait BlurAccum: Copy + Sized {
    type Acc: Copy + Add<Output = Self::Acc> + AddAssign + Sub<Output = Self::Acc> + SubAssign;
    type Weight: Copy;

    const EMPTY_ACCUMULATOR: Self::Acc;

    fn to_acc(self) -> Self::Acc;
    fn scale(acc: Self::Acc, count: usize) -> Self::Acc;
    fn make_weight(kernel_size: usize) -> Self::Weight;
    fn to_store(acc: Self::Acc, weight: Self::Weight) -> Self;
}

impl BlurAccum for u8 {
    type Acc = u32;
    type Weight = U8Weight;
    const EMPTY_ACCUMULATOR: u32 = 0;
    #[inline(always)]
    fn to_acc(self) -> u32 {
        self as u32
    }
    #[inline(always)]
    fn scale(acc: u32, count: usize) -> u32 {
        acc * count as u32
    }
    #[inline(always)]
    fn make_weight(kernel_size: usize) -> U8Weight {
        U8Weight::new(kernel_size as u32)
    }
    #[inline(always)]
    fn to_store(acc: u32, weight: U8Weight) -> u8 {
        weight.apply(acc)
    }
}

macro_rules! impl_blur_accum_f32 {
    ($($t:ty),+) => { $(
        impl BlurAccum for $t {
            type Acc = f32;
            type Weight = f32;
            const EMPTY_ACCUMULATOR: f32 = 0.0;
            #[inline(always)]
            fn to_acc(self) -> f32 {
                self.to_f32().unwrap()
            }
            #[inline(always)]
            fn scale(acc: f32, count: usize) -> f32 {
                acc * count as f32
            }
            #[inline(always)]
            fn make_weight(kernel_size: usize) -> f32 {
                1.0 / kernel_size as f32
            }
            #[inline(always)]
            fn to_store(acc: f32, weight: f32) -> Self {
                rounding_saturating_mul(acc, weight)
            }
        }
    )+ };
}

impl_blur_accum_f32!(u16, u32, u64, usize, i8, i16, i32, i64, isize, f32, f64);

#[cfg(test)]
mod tests {
    use super::U8Weight;

    #[test]
    fn u8_weight_exhaustive_small() {
        for ks in (3u32..=51).step_by(2) {
            let w = U8Weight::new(ks);
            let max_acc = ks * 255;
            for acc in 0..=max_acc {
                let expected = ((acc + ks / 2) / ks) as u8;
                let got = w.apply(acc);
                assert_eq!(got, expected, "ks={ks}, acc={acc}");
            }
        }
    }

    #[test]
    fn u8_weight_sampled_large() {
        for ks in (53u32..=1025).step_by(2) {
            let w = U8Weight::new(ks);
            let max_acc = ks * 255;
            let step = (max_acc / 10_000).max(1) as usize;
            for acc in (0..=max_acc).step_by(step) {
                let expected = ((acc + ks / 2) / ks) as u8;
                let got = w.apply(acc);
                assert_eq!(got, expected, "ks={ks}, acc={acc}");
            }
            assert_eq!(w.apply(0), ((0 + ks / 2) / ks) as u8);
            assert_eq!(w.apply(max_acc), ((max_acc + ks / 2) / ks) as u8);
        }
    }
}
