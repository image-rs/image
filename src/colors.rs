use std::num::{NumCast};
use std::mem;

use buffer::{Pixel, FromColor};
use traits::Primitive;

///An enumeration over supported color types and their bit depths
#[deriving(PartialEq, Eq, Show, Clone)]
pub enum ColorType {
    ///Pixel is greyscale
    Grey(u8),

    ///Pixel contains R, G and B channels
    RGB(u8),

    ///Pixel is an index into a color palette
    Palette(u8),

    ///Pixel is greyscale with an alpha channel
    GreyA(u8),

    ///Pixel is RGB with an alpha channel
    RGBA(u8)
}

/// Trait that promises that the type implementing it
/// does not have any memory allocated on the heap or
/// needs any destructor to be run.
pub trait SafeToTransmute {}


///Returns the number of bits contained in a pixel of ColorType c
pub fn bits_per_pixel(c: ColorType) -> uint {
    match c {
        ColorType::Grey(n)    => n as uint,
        ColorType::RGB(n)     => 3 * n as uint,
        ColorType::Palette(n) => 3 * n as uint,
        ColorType::GreyA(n)   => 2 * n as uint,
        ColorType::RGBA(n)    => 4 * n as uint,
    }
}

///Returns the number of color channels that make up this pixel
pub fn num_components(c: ColorType) -> uint {
    match c {
        ColorType::Grey(_)    => 1,
        ColorType::RGB(_)     => 3,
        ColorType::Palette(_) => 3,
        ColorType::GreyA(_)   => 2,
        ColorType::RGBA(_)    => 4,
    }
}

macro_rules! define_colors {
    {$(
        $ident:ident
        $channels: expr
        $interpretation: expr
        #[$doc:meta];
    )*} => {

$( // START Structure definitions

#[$doc]
#[deriving(PartialEq, Eq, Clone, Show, Copy)]
pub struct $ident<T: Primitive>(pub [T, ..$channels]);

impl<'a, T: Primitive> Pixel<T> for $ident<T> {

    fn channel_count<'a>(_: Option<&'a $ident<T>>) -> u8 {
        $channels
    }
    fn color_model<'a>(_: Option<&'a $ident<T>>) -> &'static str {
        $interpretation
    }
    #[inline(always)]
    fn channels(&self) -> &[T] {
        let &$ident(ref this) = self;
        this.as_slice()
    }
    #[inline(always)]
    fn channels_mut(&mut self) -> &mut [T] {
        let &$ident(ref mut this) = self;
        this.as_mut_slice()
    }
    fn from_slice<'a>(_: Option<&'a $ident<T>>, slice: &'a [T]) -> &'a $ident<T> {
        assert_eq!(slice.len(), $channels)
        unsafe { mem::transmute(slice.as_ptr()) }
    }
    fn from_slice_mut<'a>(_: Option<&'a $ident<T>>, slice: &'a mut [T]) -> &'a mut $ident<T> {
        assert_eq!(slice.len(), $channels)
        unsafe { mem::transmute(slice.as_ptr()) }
    }
}

)* // END Structure definitions

    }
}

define_colors! {
    Rgb 3 "RGB" #[doc = "RGB colors"];
    Luma 1 "Y" #[doc = "Grayscale colors"];
    Rgba 4 "RGBA" #[doc = "RGB colors + alpha channel"];
    LumaA 2 "YA" #[doc = "Grayscale colors + alpha channel"];
}

impl<'a, 'b, T: Primitive> FromColor<Rgb<T>> for Luma<T> {
    fn from_color(&mut self, rgb: &Rgb<T>) {
            let gray = self.channels_mut();
            let rgb = rgb.channels();
            let l = 0.2125f32 * rgb[0].to_f32().unwrap() +
                    0.7154f32 * rgb[1].to_f32().unwrap() +
                    0.0721f32 * rgb[2].to_f32().unwrap();
            gray[0] = NumCast::from(l).unwrap()
    }
}

impl<'a, 'b, T: Primitive> FromColor<Luma<T>> for Rgb<T> {
    fn from_color(&mut self, gray: &Luma<T>) {
            let rgb = self.channels_mut();
            let gray = gray.channels()[0];
            rgb[0] = gray;
            rgb[1] = gray;
            rgb[2] = gray;
    }
}