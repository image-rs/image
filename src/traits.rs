//! This module provides useful traits that were deprecated in rust

// Note copied from the stdlib under MIT license

use num_traits::{Bounded, NumCast};
use pixeli::{Gray, GrayAlpha, Pixel, PixelComponent, Rgb, Rgba};

use crate::ExtendedColorType;

use self::sealed::Sealed;

/// Types which are safe to treat as an immutable byte slice in a pixel layout
/// for image encoding.
pub trait EncodableLayout: Sealed {
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

/// Linear interpolation without involving floating numbers.
pub trait Lerp: Bounded + NumCast {
    type Ratio: PixelComponent;

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
pub trait PixelWithColorType: Pixel + Sealed {
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
impl PixelWithColorType for Gray<f32> {
    const COLOR_TYPE: ExtendedColorType = ExtendedColorType::Gray32F;
}

impl PixelWithColorType for GrayAlpha<u8> {
    const COLOR_TYPE: ExtendedColorType = ExtendedColorType::La8;
}
impl PixelWithColorType for GrayAlpha<u16> {
    const COLOR_TYPE: ExtendedColorType = ExtendedColorType::La16;
}
impl PixelWithColorType for GrayAlpha<f32> {
    const COLOR_TYPE: ExtendedColorType = ExtendedColorType::GrayAlpha32F;
}

/// Private module for supertraits of sealed traits.
mod sealed {
    use super::*;

    pub trait Sealed {}

    impl Sealed for Rgb<u8> {}
    impl Sealed for Rgb<u16> {}
    impl Sealed for Rgb<f32> {}

    impl Sealed for Rgba<u8> {}
    impl Sealed for Rgba<u16> {}
    impl Sealed for Rgba<f32> {}

    impl Sealed for Gray<u8> {}
    impl Sealed for Gray<u16> {}
    impl Sealed for Gray<f32> {}

    impl Sealed for GrayAlpha<u8> {}
    impl Sealed for GrayAlpha<u16> {}
    impl Sealed for GrayAlpha<f32> {}

    impl Sealed for [u8] {}
    impl Sealed for [u16] {}
    impl Sealed for [f32] {}
}
