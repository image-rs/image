//! This module provides useful traits that were deprecated in rust

// Note copied from the stdlib under MIT license

use num_traits::{Bounded, Num, NumCast};
pub use pixeli::Pixel;
use std::ops::AddAssign;

use crate::{
    color::{Gray, GrayAlpha, Rgb, Rgba},
    ExtendedColorType,
};

/// Types which are safe to treat as an immutable byte slice in a pixel layout
/// for image encoding.
pub trait EncodableLayout: seals::EncodableLayout {
    /// Get the bytes of this value.
    fn as_bytes(&self) -> &[u8];
}

impl EncodableLayout for [u8] {
    fn as_bytes(&self) -> &[u8] {
        bytemuck::cast_slice(self)
    }
}

impl EncodableLayout for [u16] {
    fn as_bytes(&self) -> &[u8] {
        bytemuck::cast_slice(self)
    }
}

impl EncodableLayout for [f32] {
    fn as_bytes(&self) -> &[u8] {
        bytemuck::cast_slice(self)
    }
}

/// The type of each channel in a pixel. For example, this can be `u8`, `u16`, `f32`.
// TODO rename to `PixelComponent`? Split up into separate traits? Seal?
pub trait Primitive: Copy + NumCast + Num + PartialOrd<Self> + Clone + Bounded {
    /// The maximum value for this type of primitive within the context of color.
    /// For floats, the maximum is `1.0`, whereas the integer types inherit their usual maximum values.
    const DEFAULT_MAX_VALUE: Self;

    /// The minimum value for this type of primitive within the context of color.
    /// For floats, the minimum is `0.0`, whereas the integer types inherit their usual minimum values.
    const DEFAULT_MIN_VALUE: Self;
}

macro_rules! declare_primitive {
    ($base:ty: ($from:expr)..$to:expr) => {
        impl Primitive for $base {
            const DEFAULT_MAX_VALUE: Self = $to;
            const DEFAULT_MIN_VALUE: Self = $from;
        }
    };
}

declare_primitive!(usize: (0)..Self::MAX);
declare_primitive!(u8: (0)..Self::MAX);
declare_primitive!(u16: (0)..Self::MAX);
declare_primitive!(u32: (0)..Self::MAX);
declare_primitive!(u64: (0)..Self::MAX);

declare_primitive!(isize: (Self::MIN)..Self::MAX);
declare_primitive!(i8: (Self::MIN)..Self::MAX);
declare_primitive!(i16: (Self::MIN)..Self::MAX);
declare_primitive!(i32: (Self::MIN)..Self::MAX);
declare_primitive!(i64: (Self::MIN)..Self::MAX);
declare_primitive!(f32: (0.0)..1.0);
declare_primitive!(f64: (0.0)..1.0);

/// An Enlargable::Larger value should be enough to calculate
/// the sum (average) of a few hundred or thousand Enlargeable values.
pub trait Enlargeable: Sized + Bounded + NumCast {
    type Larger: Copy + NumCast + Num + PartialOrd<Self::Larger> + Clone + Bounded + AddAssign;

    fn clamp_from(n: Self::Larger) -> Self {
        if n > Self::max_value().to_larger() {
            Self::max_value()
        } else if n < Self::min_value().to_larger() {
            Self::min_value()
        } else {
            NumCast::from(n).unwrap()
        }
    }

    fn to_larger(self) -> Self::Larger {
        NumCast::from(self).unwrap()
    }
}

impl Enlargeable for u8 {
    type Larger = u32;
}
impl Enlargeable for u16 {
    type Larger = u32;
}
impl Enlargeable for u32 {
    type Larger = u64;
}
impl Enlargeable for u64 {
    type Larger = u128;
}
impl Enlargeable for usize {
    // Note: On 32-bit architectures, u64 should be enough here.
    type Larger = u128;
}
impl Enlargeable for i8 {
    type Larger = i32;
}
impl Enlargeable for i16 {
    type Larger = i32;
}
impl Enlargeable for i32 {
    type Larger = i64;
}
impl Enlargeable for i64 {
    type Larger = i128;
}
impl Enlargeable for isize {
    // Note: On 32-bit architectures, i64 should be enough here.
    type Larger = i128;
}
impl Enlargeable for f32 {
    type Larger = f64;
}
impl Enlargeable for f64 {
    type Larger = f64;
}

/// Linear interpolation without involving floating numbers.
pub trait Lerp: Bounded + NumCast {
    type Ratio: Primitive;

    fn lerp(a: Self, b: Self, ratio: Self::Ratio) -> Self {
        let a = <Self::Ratio as NumCast>::from(a).unwrap();
        let b = <Self::Ratio as NumCast>::from(b).unwrap();

        let res = a + (b - a) * ratio;

        if res > NumCast::from(Self::max_value()).unwrap() {
            Self::max_value()
        } else if res < NumCast::from(0).unwrap() {
            NumCast::from(0).unwrap()
        } else {
            NumCast::from(res).unwrap()
        }
    }
}

impl Lerp for u8 {
    type Ratio = f32;
}

impl Lerp for u16 {
    type Ratio = f32;
}

impl Lerp for u32 {
    type Ratio = f64;
}

impl Lerp for f32 {
    type Ratio = f32;

    fn lerp(a: Self, b: Self, ratio: Self::Ratio) -> Self {
        a + (b - a) * ratio
    }
}

/// The pixel with an associated `ColorType`.
/// Not all possible pixels represent one of the predefined `ColorType`s.
pub trait PixelWithColorType: Pixel + private::Sealed {
    /// This pixel has the format of one of the predefined `ColorType`s,
    /// such as `Rgb8`, `La16` or `Rgba32F`.
    /// This is needed for automatically detecting
    /// a color format when saving an image as a file.
    const COLOR_TYPE: ExtendedColorType;
}

impl PixelWithColorType for Rgb<u8> {
    const COLOR_TYPE: ExtendedColorType = ExtendedColorType::Rgb8;
}
impl PixelWithColorType for Rgb<u16> {
    const COLOR_TYPE: ExtendedColorType = ExtendedColorType::Rgb16;
}
impl PixelWithColorType for Rgb<f32> {
    const COLOR_TYPE: ExtendedColorType = ExtendedColorType::Rgb32F;
}

impl PixelWithColorType for Rgba<u8> {
    const COLOR_TYPE: ExtendedColorType = ExtendedColorType::Rgba8;
}
impl PixelWithColorType for Rgba<u16> {
    const COLOR_TYPE: ExtendedColorType = ExtendedColorType::Rgba16;
}
impl PixelWithColorType for Rgba<f32> {
    const COLOR_TYPE: ExtendedColorType = ExtendedColorType::Rgba32F;
}

impl PixelWithColorType for Gray<u8> {
    const COLOR_TYPE: ExtendedColorType = ExtendedColorType::L8;
}
impl PixelWithColorType for Gray<u16> {
    const COLOR_TYPE: ExtendedColorType = ExtendedColorType::L16;
}
impl PixelWithColorType for GrayAlpha<u8> {
    const COLOR_TYPE: ExtendedColorType = ExtendedColorType::La8;
}
impl PixelWithColorType for GrayAlpha<u16> {
    const COLOR_TYPE: ExtendedColorType = ExtendedColorType::La16;
}

/// Prevents down-stream users from implementing the `Primitive` trait
mod private {
    use crate::color::*;

    pub trait Sealed {}
    impl Sealed for Rgb<u8> {}
    impl Sealed for Rgb<u16> {}
    impl Sealed for Rgb<f32> {}

    impl Sealed for Rgba<u8> {}
    impl Sealed for Rgba<u16> {}
    impl Sealed for Rgba<f32> {}

    impl Sealed for Gray<u8> {}
    impl Sealed for GrayAlpha<u8> {}

    impl Sealed for Gray<u16> {}
    impl Sealed for GrayAlpha<u16> {}
}

/// Private module for supertraits of sealed traits.
mod seals {
    pub trait EncodableLayout {}

    impl EncodableLayout for [u8] {}
    impl EncodableLayout for [u16] {}
    impl EncodableLayout for [f32] {}
}
