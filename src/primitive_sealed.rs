//! Module for crate-private traits implemented for all primitive types.

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
///       (An implementation may use a fast approximation instead of a precise rounding method.)
///     - NaN is mapped to 0.
///     - Values outside the range of the integer type are clamped to the min or max value.
/// - For a float -> float conversion:
///     - The float is clamped to the range `[0.0, 1.0]`.
///     - NaN is mapped to 0.0.
pub(crate) trait NearestFrom<T> {
    fn nearest_from(value: T) -> Self;
}

impl NearestFrom<f32> for u8 {
    fn nearest_from(value: f32) -> Self {
        // Approximate rounding using the well-known + 0.5 trick.
        // This does not handle certain cases correctly. E.g. `0.5_f32.nextdown()`
        // is incorrectly rounded to 1 instead of 0. However, this isn't typically
        // an issue in practice.
        (value + 0.5) as u8
    }
}
impl NearestFrom<f32> for u16 {
    fn nearest_from(value: f32) -> Self {
        (value + 0.5) as u16
    }
}
impl NearestFrom<f32> for f32 {
    #[allow(clippy::manual_clamp)] // to map NaN to 0.0
    fn nearest_from(value: f32) -> Self {
        value.max(0.0).min(1.0)
    }
}
impl NearestFrom<f32> for f64 {
    #[allow(clippy::manual_clamp)] // to map NaN to 0.0
    fn nearest_from(value: f32) -> Self {
        value.max(0.0).min(1.0) as f64
    }
}

macro_rules! impl_nearest_from_f32_for_ints {
        ($($t:ty),+) => { $(
            impl NearestFrom<f32> for $t {
                fn nearest_from(value: f32) -> Self {
                    value.round() as $t
                }
            }
        )+ };
    }
impl_nearest_from_f32_for_ints!(u32, u64, usize, i8, i16, i32, i64, isize);
