//! This module provides useful traits that were deprecated in rust

// Note copied from the stdlib under MIT license

use num_traits::{Bounded, Num, NumCast};
use std::ops::{AddAssign};

use crate::color::{ColorType, Luma, LumaA, Rgb, Rgba};
use crate::{ImageError};
use crate::error::{UnsupportedError, ImageFormatHint, UnsupportedErrorKind};

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

/// Primitive trait from old stdlib
pub trait Primitive: Copy + NumCast + Num + PartialOrd<Self> + Clone + Bounded {}

impl Primitive for usize {}
impl Primitive for u8 {}
impl Primitive for u16 {}
impl Primitive for u32 {}
impl Primitive for u64 {}
impl Primitive for isize {}
impl Primitive for i8 {}
impl Primitive for i16 {}
impl Primitive for i32 {}
impl Primitive for i64 {}
impl Primitive for f32 {}
impl Primitive for f64 {}

/// An Enlargable::Larger value should be enough to calculate
/// the sum (average) of a few hundred or thousand Enlargeable values.
pub trait Enlargeable: Sized + Bounded + NumCast {
    type Larger: Primitive + AddAssign + 'static;

    fn clamp_from(n: Self::Larger) -> Self {
        // Note: Only unsigned value types supported.
        if n > NumCast::from(Self::max_value()).unwrap() {
            Self::max_value()
        } else {
            NumCast::from(n).unwrap()
        }
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
impl Enlargeable for f32 {
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

/// The type of each channel in a pixel.
pub trait PixelComponent: Primitive + 'static + private::Sealed {
    const DEFAULT_MAX_COMPONENT_VALUE: Self;
    const RGB_COLOR_TYPE: ColorTypeOrErr;
    const RGBA_COLOR_TYPE: ColorTypeOrErr;
    const L_COLOR_TYPE: ColorTypeOrErr;
    const LA_COLOR_TYPE: ColorTypeOrErr;
}

/// Prevents down-stream users from implementing the `PixelComponent` trait
mod private {
    pub trait Sealed {}
    impl Sealed for u8 {}
    impl Sealed for u16 {}
    impl Sealed for i32 {}
    impl Sealed for usize {}
    impl Sealed for f32 {}
}

pub(crate) type ColorTypeOrErr = Result<ColorType, &'static str>;
pub(crate) fn color_type_unsupported(error: &'static str) -> ImageError {
    ImageError::Unsupported(UnsupportedError::from_format_and_kind(
        ImageFormatHint::Unknown,
        UnsupportedErrorKind::GenericFeature(error.to_string())
    ))
}

impl PixelComponent for u8 {
    const DEFAULT_MAX_COMPONENT_VALUE: Self = Self::MAX;
    const RGB_COLOR_TYPE: ColorTypeOrErr = Ok(ColorType::Rgb8);
    const RGBA_COLOR_TYPE: ColorTypeOrErr = Ok(ColorType::Rgba8);
    const L_COLOR_TYPE: ColorTypeOrErr = Ok(ColorType::L8);
    const LA_COLOR_TYPE: ColorTypeOrErr = Ok(ColorType::La8);
}

impl PixelComponent for u16 {
    const DEFAULT_MAX_COMPONENT_VALUE: Self = Self::MAX;
    const RGB_COLOR_TYPE: ColorTypeOrErr = Ok(ColorType::Rgb16);
    const RGBA_COLOR_TYPE: ColorTypeOrErr = Ok(ColorType::Rgba16);
    const L_COLOR_TYPE: ColorTypeOrErr = Ok(ColorType::L16);
    const LA_COLOR_TYPE: ColorTypeOrErr = Ok(ColorType::La16);
}

impl PixelComponent for f32 {
    const DEFAULT_MAX_COMPONENT_VALUE: Self = 1.0;
    const RGB_COLOR_TYPE: ColorTypeOrErr = Ok(ColorType::Rgb32F);
    const RGBA_COLOR_TYPE: ColorTypeOrErr = Ok(ColorType::Rgba32F);
    const L_COLOR_TYPE: ColorTypeOrErr = Err("`Luma<f32>` does not have an appropriate `ColorType`");
    const LA_COLOR_TYPE: ColorTypeOrErr = Err("`LumaA<f32>` does not have an appropriate `ColorType`");
}


#[cfg(test)] // apparently i32 is used for testing somewhere
impl PixelComponent for i32 {
    const DEFAULT_MAX_COMPONENT_VALUE: Self = i32::MAX;
    const RGB_COLOR_TYPE: ColorTypeOrErr = Err("`Rgb<i32>` does not have an appropriate `ColorType`");
    const RGBA_COLOR_TYPE: ColorTypeOrErr = Err("`Rgba<i32>` does not have an appropriate `ColorType`");
    const L_COLOR_TYPE: ColorTypeOrErr = Err("`Luma<i32>` does not have an appropriate `ColorType`");
    const LA_COLOR_TYPE: ColorTypeOrErr = Err("`LumaA<i32>` does not have an appropriate `ColorType`");
}

#[cfg(test)] // apparently usize is used for testing somewhere
impl PixelComponent for usize {
    const DEFAULT_MAX_COMPONENT_VALUE: Self = usize::MAX;
    const RGB_COLOR_TYPE: ColorTypeOrErr = Err("`Rgb<usize>` does not have an appropriate `ColorType`");
    const RGBA_COLOR_TYPE: ColorTypeOrErr = Err("`Rgba<usize>` does not have an appropriate `ColorType`");
    const L_COLOR_TYPE: ColorTypeOrErr = Err("`Luma<usize>` does not have an appropriate `ColorType`");
    const LA_COLOR_TYPE: ColorTypeOrErr = Err("`LumaA<usize>` does not have an appropriate `ColorType`");
}

/// A generalized pixel.
///
/// A pixel object is usually not used standalone but as a view into an image buffer.
pub trait Pixel: Copy + Clone {

    /// The scalar type that is used to store each channel in this pixel.
    type Subpixel: PixelComponent;

    /// The number of channels of this pixel type.
    const CHANNEL_COUNT: u8;

    /// Returns the components as a slice.
    fn channels(&self) -> &[Self::Subpixel];

    /// Returns the components as a mutable slice
    fn channels_mut(&mut self) -> &mut [Self::Subpixel];

    /// A string that can help to interpret the meaning each channel
    /// See [gimp babl](http://gegl.org/babl/).
    const COLOR_MODEL: &'static str;


    /// The `Ok(ColorType)` for this pixel format,
    /// or `Err(message)` if this special type of pixel is not supported.
    const COLOR_TYPE: ColorTypeOrErr;


    /// Returns the channels of this pixel as a 4 tuple. If the pixel
    /// has less than 4 channels the remainder is filled with the maximum value
    #[deprecated(since="0.24.0", note="Use `channels()` or `channels_mut()`")]
    fn channels4(
        &self,
    ) -> (
        Self::Subpixel,
        Self::Subpixel,
        Self::Subpixel,
        Self::Subpixel,
    );

    /// Construct a pixel from the 4 channels a, b, c and d.
    /// If the pixel does not contain 4 channels the extra are ignored.
    #[deprecated(since="0.24.0", note="Use the constructor of the pixel, for example `Rgba::new(r,g,b,a)` or `Pixel::from_slice`")]
    fn from_channels(
        a: Self::Subpixel,
        b: Self::Subpixel,
        c: Self::Subpixel,
        d: Self::Subpixel,
    ) -> Self;

    /// Returns a view into a slice.
    ///
    /// Note: The slice length is not checked on creation. Thus the caller has to ensure
    /// that the slice is long enough to prevent panics if the pixel is used later on.
    fn from_slice(slice: &[Self::Subpixel]) -> &Self;

    /// Returns mutable view into a mutable slice.
    ///
    /// Note: The slice length is not checked on creation. Thus the caller has to ensure
    /// that the slice is long enough to prevent panics if the pixel is used later on.
    fn from_slice_mut(slice: &mut [Self::Subpixel]) -> &mut Self;

    /// Convert this pixel to RGB
    fn to_rgb(&self) -> Rgb<Self::Subpixel>;

    /// Convert this pixel to RGB with an alpha channel
    fn to_rgba(&self) -> Rgba<Self::Subpixel>;

    /// Convert this pixel to luma
    fn to_luma(&self) -> Luma<Self::Subpixel>;

    /// Convert this pixel to luma with an alpha channel
    fn to_luma_alpha(&self) -> LumaA<Self::Subpixel>;

    /// Apply the function ```f``` to each channel of this pixel.
    fn map<F>(&self, f: F) -> Self
    where
        F: FnMut(Self::Subpixel) -> Self::Subpixel;

    /// Apply the function ```f``` to each channel of this pixel.
    fn apply<F>(&mut self, f: F)
    where
        F: FnMut(Self::Subpixel) -> Self::Subpixel;

    /// Apply the function ```f``` to each channel except the alpha channel.
    /// Apply the function ```g``` to the alpha channel.
    fn map_with_alpha<F, G>(&self, f: F, g: G) -> Self
    where
        F: FnMut(Self::Subpixel) -> Self::Subpixel,
        G: FnMut(Self::Subpixel) -> Self::Subpixel;

    /// Apply the function ```f``` to each channel except the alpha channel.
    /// Apply the function ```g``` to the alpha channel. Works in-place.
    fn apply_with_alpha<F, G>(&mut self, f: F, g: G)
    where
        F: FnMut(Self::Subpixel) -> Self::Subpixel,
        G: FnMut(Self::Subpixel) -> Self::Subpixel;

    /// Apply the function ```f``` to each channel except the alpha channel.
    fn map_without_alpha<F>(&self, f: F) -> Self
    where
        F: FnMut(Self::Subpixel) -> Self::Subpixel,
    {
        let mut this = *self;
        this.apply_with_alpha(f, |x| x);
        this
    }

    /// Apply the function ```f``` to each channel except the alpha channel.
    /// Works in place.
    fn apply_without_alpha<F>(&mut self, f: F)
    where
        F: FnMut(Self::Subpixel) -> Self::Subpixel,
    {
        self.apply_with_alpha(f, |x| x);
    }

    /// Apply the function ```f``` to each channel of this pixel and
    /// ```other``` pairwise.
    fn map2<F>(&self, other: &Self, f: F) -> Self
    where
        F: FnMut(Self::Subpixel, Self::Subpixel) -> Self::Subpixel;

    /// Apply the function ```f``` to each channel of this pixel and
    /// ```other``` pairwise. Works in-place.
    fn apply2<F>(&mut self, other: &Self, f: F)
    where
        F: FnMut(Self::Subpixel, Self::Subpixel) -> Self::Subpixel;

    /// Invert this pixel
    fn invert(&mut self);

    /// Blend the color of a given pixel into ourself, taking into account alpha channels
    fn blend(&mut self, other: &Self);
}

/// Private module for supertraits of sealed traits.
mod seals {
    pub trait EncodableLayout {}

    impl EncodableLayout for [u8] {}
    impl EncodableLayout for [u16] {}
    impl EncodableLayout for [f32] {}
}
