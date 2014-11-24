//! Types and methods for representing and manipulating pixels
use std::num;
use std::num::{ Float, Int, NumCast };
use std::default::Default;

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

/// Placeholder for primitives.
pub trait Primitive: Clone + Copy + Sub<Self, Self> + NumCast {
    /// The maximum value of primitive.
    fn max_value() -> Self;
}

impl Primitive for uint {
    fn max_value() -> uint { Int::max_value() }
}
impl Primitive for u8 {
    fn max_value() -> u8 { Int::max_value() }
}
impl Primitive for u16 {
    fn max_value() -> u16 { Int::max_value() }
}
impl Primitive for u32 {
    fn max_value() -> u32 { Int::max_value() }
}
impl Primitive for u64 {
    fn max_value() -> u64 { Int::max_value() }
}
impl Primitive for int {
    fn max_value() -> int { Int::max_value() }
}
impl Primitive for i8 {
    fn max_value() -> i8 { Int::max_value() }
}
impl Primitive for i16 {
    fn max_value() -> i16 { Int::max_value() }
}
impl Primitive for i32 {
    fn max_value() -> i32 { Int::max_value() }
}
impl Primitive for i64 {
    fn max_value() -> i64 { Int::max_value() }
}
impl Primitive for f32 {
    fn max_value() -> f32 { Float::max_value() }
}
impl Primitive for f64 {
    fn max_value() -> f64 { Float::max_value() }
}

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

///A type to hold a grayscale pixel
#[packed]
#[deriving(Default, PartialEq, Eq, Clone, Show, Copy)]
pub struct Luma<T>(pub T);

impl<T: Primitive> Luma<T> {
    /// Returns the channels of this pixel as a tuple
    pub fn channel(&self) -> T {
        match *self {
            Luma(l) => l
        }
    }
}

impl<T: Primitive> SafeToTransmute for Luma<T> {}

/// A type to hold a grayscale pixel with an alpha channel
#[packed]
#[deriving(Default, PartialEq, Eq, Clone, Show, Copy)]
pub struct LumaA<T>(pub T, pub T);

impl<T: Primitive> LumaA<T> {
    /// Returns the channels of this pixel as a tuple
    pub fn channels(&self) -> (T, T) {
        match *self {
            LumaA(l, a) => (l, a)
        }
    }

    /// Returns the alpha channel of this pixel
    pub fn alpha(&self) -> T {
        match *self {
            LumaA(_, a) => a
        }
    }
}

impl<T: Primitive> SafeToTransmute for LumaA<T> {}

/// A type to hold an RGB pixel
#[packed]
#[deriving(Default, PartialEq, Eq, Clone, Show, Copy)]
pub struct Rgb<T>(pub T, pub T, pub T);

impl<T: Primitive> Rgb<T> {
    /// Returns the channels of this pixel as a tuple
    pub fn channels(&self) -> (T, T, T) {
        match *self {
            Rgb(r, g, b) => (r, g, b)
        }
    }
}

impl<T: Primitive> SafeToTransmute for Rgb<T> {}

/// A type to hold an RGB pixel with an alpha channel
#[packed]
#[deriving(Default, PartialEq, Eq, Clone, Show, Copy)]
pub struct Rgba<T>(pub T, pub T, pub T, pub T);

impl<T: Primitive> Rgba<T> {
    /// Returns the channels of this pixel as a tuple
    pub fn channels(&self) -> (T, T, T, T) {
        match *self {
            Rgba(r, g, b, a) => (r, g, b, a)
        }
    }

    /// Returns the alpha channel of this pixel
    pub fn alpha(&self) -> T {
        match *self {
            Rgba(_, _, _, a) => a
        }
    }
}

impl<T: Primitive> SafeToTransmute for Rgba<T> {}

/// A trait that all pixels implement.
pub trait Pixel<T>: Copy + Clone + Default {
    /// Construct a pixel from the 4 channels a, b, c and d.
    /// If the pixel does not contain 4 channels the extra are ignored.
    fn from_channels(a: T, b: T, c: T, d: T) -> Self;

    /// Convert this pixel to RGB
    fn to_rgb(&self) -> Rgb<T>;

    /// Convert this pixel to RGB with an alpha channel
    fn to_rgba(&self) -> Rgba<T>;

    /// Convert this pixel to luma
    fn to_luma(&self) -> Luma<T>;

    /// Convert this pixel to luma with an alpha channel
    fn to_luma_alpha(&self) -> LumaA<T>;

    /// Invert this pixel
    fn invert(&mut self);

    /// Apply the function ```f``` to each channel of this pixel.
    fn map(&self, f: | T | -> T) -> Self;

    ///Apply the function f to each channel except the alpha channel.
    ///Apply the function g to the alpha channel.
    fn map_with_alpha(&self, f: |T| -> T, g: |T| -> T) -> Self;

    /// Apply the function ```f``` to each channel of this pixel and
    /// ```other``` pairwise.
    fn map2(&self, other: Self, f: | T, T | -> T) -> Self;

    /// Returns the channels of this pixel as a 4 tuple. If the pixel
    /// has less than 4 channels the remainder is filled with the maximum value
    fn channels4(&self) -> (T, T, T, T);

    /// Blend the color of a given pixel into ourself, taking into account alpha channels
    fn blend(&self, other: Self) -> Self;
}

impl<T: Primitive + Default> Pixel<T> for Rgb<T> {
    fn from_channels(a: T, b: T, c: T, _: T) -> Rgb<T> {
        Rgb(a, b, c)
    }

    fn to_luma(&self) -> Luma<T> {
        let (r, g, b) = self.channels();

        let l = 0.2125f32 * r.to_f32().unwrap() +
                0.7154f32 * g.to_f32().unwrap() +
                0.0721f32 * b.to_f32().unwrap();

        Luma(num::cast::<f32, T>(l).unwrap())
    }

    fn to_luma_alpha(&self) -> LumaA<T> {
        let l = self.to_luma().channel();

        LumaA(l, Primitive::max_value())
    }

    fn to_rgb(&self) -> Rgb<T> {
        self.clone()
    }

    fn to_rgba(&self) -> Rgba<T> {
        let (r, g, b) = self.channels();

        Rgba(r, g, b, Primitive::max_value())
    }

    fn invert(&mut self) {
        let (r, g, b) = self.channels();

        let max: T = Primitive::max_value();

        let r1 = max - r;
        let g1 = max - g;
        let b1 = max - b;

        *self = Rgb(r1, g1, b1)
    }

    fn map(&self, f: | a: T | -> T) -> Rgb<T> {
        let (r, g, b) = self.channels();

        let r1 = f(r);
        let g1 = f(g);
        let b1 = f(b);

        Rgb(r1, g1, b1)
    }

    fn map_with_alpha(&self, f: |c: T| -> T, _: |a: T| -> T) -> Rgb<T> {
        self.map(f)
    }


    fn map2(&self, other: Rgb<T>, f: | a: T, b: T | -> T) -> Rgb<T> {
        let (r1, g1, b1) = self.channels();
        let (r2, g2, b2) = other.channels();

        let r3 = f(r1, r2);
        let g3 = f(g1, g2);
        let b3 = f(b1, b2);

        Rgb(r3, g3, b3)
    }

    fn channels4(&self) -> (T, T, T, T) {
        let (r, g, b) = self.channels();

        (r, g, b, Primitive::max_value())
    }

    fn blend(&self, other: Rgb<T>) -> Rgb<T> {
        other
    }
}

impl<T: Primitive + Default> Pixel<T> for Rgba<T> {
    fn from_channels(a: T, b: T, c: T, d: T) -> Rgba<T> {
        Rgba(a, b, c, d)
    }

    fn to_luma(&self) -> Luma<T> {
        self.to_rgb().to_luma()
    }

    fn to_luma_alpha(&self) -> LumaA<T> {
        let l = self.to_luma().channel();
        let a = self.alpha();

        LumaA(l, a)
    }

    fn to_rgb(&self) -> Rgb<T> {
        let (r, g, b, _) = self.channels();

        Rgb(r, g, b)
    }

    fn to_rgba(&self) -> Rgba<T> {
        self.clone()
    }

    fn invert(&mut self) {
        let (r, g, b) = self.to_rgb().channels();
        let a = self.alpha();

        let max: T = Primitive::max_value();

        *self = Rgba(max - r, max - g, max - b, a)
    }

    fn map(&self, f: | a: T | -> T) -> Rgba<T> {
        let (r, g, b, a) = self.channels();

        let r1 = f(r);
        let g1 = f(g);
        let b1 = f(b);
        let a1 = f(a);

        Rgba(r1, g1, b1, a1)
    }

    fn map_with_alpha(&self, f: |c: T| -> T, h: |b: T| -> T) -> Rgba<T> {
        let (r, g, b, a) = self.channels();

        let r1 = f(r);
        let g1 = f(g);
        let b1 = f(b);
        let a1 = h(a);

        Rgba(r1, g1, b1, a1)
    }

    fn map2(&self, other: Rgba<T>, f: | a: T, b: T | -> T) -> Rgba<T> {
        let (r1, g1, b1, a1) = self.channels();
        let (r2, g2, b2, _) = other.channels();

        let r3 = f(r1, r2);
        let g3 = f(g1, g2);
        let b3 = f(b1, b2);

        Rgba(r3, g3, b3, a1)
    }

    fn channels4(&self) ->(T, T, T, T) {
        let (r, g, b, a) = self.channels();

        (r, g, b, a)
    }

    fn blend(&self, other: Rgba<T>) -> Rgba<T> {
        // http://stackoverflow.com/questions/7438263/alpha-compositing-algorithm-blend-modes#answer-11163848

        // First, as we don't know what type our pixel is, we have to convert to floats between 0.0 and 1.0
        let max_t: T = Primitive::max_value();
        let max_t = max_t.to_f32().unwrap();
        let (bg_r, bg_g, bg_b, bg_a) = self.channels();
        let (fg_r, fg_g, fg_b, fg_a) = other.channels();
        let (bg_r, bg_g, bg_b, bg_a) = (bg_r.to_f32().unwrap() / max_t, bg_g.to_f32().unwrap() / max_t, bg_b.to_f32().unwrap() / max_t, bg_a.to_f32().unwrap() / max_t);
        let (fg_r, fg_g, fg_b, fg_a) = (fg_r.to_f32().unwrap() / max_t, fg_g.to_f32().unwrap() / max_t, fg_b.to_f32().unwrap() / max_t, fg_a.to_f32().unwrap() / max_t);

        //Work out what the final alpha level will be
        let alpha_final = bg_a + fg_a - bg_a * fg_a;

        //We premultiply our channels bu their alpha, as this makes it easier to calculate
        let (bg_r_a, bg_g_a, bg_b_a) = (bg_r * bg_a, bg_g * bg_a, bg_b * bg_a);
        let (fg_r_a, fg_g_a, fg_b_a) = (fg_r * fg_a, fg_g * fg_a, fg_b * fg_a);

        //Standard formula for src-over alpha compositing
        let (out_r_a, out_g_a, out_b_a) = (fg_r_a + bg_r_a * (1.0 - fg_a), fg_g_a + bg_g_a * (1.0 - fg_a), fg_b_a + bg_b_a * (1.0 - fg_a));

        //Unmultiply the channels by our resultant alpha channel
        let (out_r, out_g, out_b) = (out_r_a / alpha_final, out_g_a / alpha_final, out_b_a / alpha_final);

        // Cast back to our initial type on return
        Rgba(
            num::cast::<f32, T>(max_t * out_r).unwrap(),
            num::cast::<f32, T>(max_t * out_g).unwrap(),
            num::cast::<f32, T>(max_t * out_b).unwrap(),
            num::cast::<f32, T>(max_t * alpha_final).unwrap()
        )
    }
}

impl<T: Primitive + Default> Pixel<T> for Luma<T> {
    fn from_channels(a: T, _: T, _: T, _: T) -> Luma<T> {
        Luma(a)
    }

    fn to_luma(&self) -> Luma<T> {
        self.clone()
    }

    fn to_luma_alpha(&self) -> LumaA<T> {
        let l = self.channel();

        LumaA(l, Primitive::max_value())
    }

    fn to_rgb(&self) -> Rgb<T> {
        let l1 = self.channel();
        let l2 = self.channel();
        let l3 = self.channel();

        Rgb(l1, l2, l3)
    }

    fn to_rgba(&self) -> Rgba<T> {
        let (r, g, b) = self.to_rgb().channels();

        Rgba(r, g, b, Primitive::max_value())
    }

    fn invert(&mut self) {
        let max: T = Primitive::max_value();
        let l1 = max - self.channel();

        *self = Luma(l1)
    }

    fn map(&self, f: | a: T | -> T) -> Luma<T> {
        let l  = self.channel();
        let l1 = f(l);

        Luma(l1)
    }

    fn map_with_alpha(&self, f: |c: T| -> T, _: |a: T| -> T) -> Luma<T> {
        self.map(f)
    }

    fn map2(&self, other: Luma<T>, f: | a: T, b: T | -> T) -> Luma<T> {
        let l1 = self.channel();
        let l2 = other.channel();

        let l3 = f(l1, l2);

        Luma(l3)
    }

    fn channels4(&self) ->(T, T, T, T) {
        let l = self.channel();
        let max: T = Primitive::max_value();

        (l, max.clone(), max.clone(), max.clone())
    }

    fn blend(&self, other: Luma<T>) -> Luma<T> {
        other
    }
}

impl<T: Primitive + Default> Pixel<T> for LumaA<T> {
    fn from_channels(a: T, b: T, _: T, _: T) -> LumaA<T> {
        LumaA(a, b)
    }

    fn to_luma(&self) -> Luma<T> {
        let (l, _) = self.channels();
        Luma(l)
    }

    fn to_luma_alpha(&self) -> LumaA<T> {
        self.clone()
    }

    fn to_rgb(&self) -> Rgb<T> {
        let (l1, _) = self.channels();
        let (l2, _) = self.channels();
        let (l3, _) = self.channels();

        Rgb(l1, l2, l3)
    }

    fn to_rgba(&self) -> Rgba<T> {
        let (r, g, b) = self.to_rgb().channels();
        let a = self.alpha();

        Rgba(r, g, b, a)
    }

    fn invert(&mut self) {
        let l = self.to_luma().channel();
        let a  = self.alpha();
        let max: T = Primitive::max_value();

        *self = LumaA(max - l, a)
    }

    fn map(&self, f: | a: T | -> T) -> LumaA<T> {
        let (l, a) = self.channels();

        let l1 = f(l);
        let a1 = f(a);

        LumaA(l1, a1)
    }

    fn map_with_alpha(&self, f: |c: T| -> T, g: |b: T| -> T) -> LumaA<T> {
        let (l, a) = self.channels();

        let l1 = f(l);
        let a1 = g(a);

        LumaA(l1, a1)
    }

    fn map2(&self, other: LumaA<T>, f: | a: T, b: T | -> T) -> LumaA<T> {
        let (l1, a1) = self.channels();
        let (l2, _)  = other.channels();

        let l3 = f(l1, l2);

        LumaA(l3, a1)
    }

    fn channels4(&self) ->(T, T, T, T) {
        let (l, a) = self.channels();
        let max: T = Primitive::max_value();

        (l, a, max.clone(), max.clone())
    }

    fn blend(&self, other: LumaA<T>) -> LumaA<T> {
        let max_t: T = Primitive::max_value();
        let max_t = max_t.to_f32().unwrap();
        let (bg_luma, bg_a) = self.channels();
        let (fg_luma, fg_a) = other.channels();
        let (bg_luma, bg_a) = (bg_luma.to_f32().unwrap() / max_t, bg_a.to_f32().unwrap() / max_t);
        let (fg_luma, fg_a) = (fg_luma.to_f32().unwrap() / max_t, fg_a.to_f32().unwrap() / max_t);

        let alpha_final = bg_a + fg_a - bg_a * fg_a; 
        let bg_luma_a = bg_luma * bg_a;
        let fg_luma_a = fg_luma * fg_a;

        let out_luma_a = fg_luma_a + bg_luma_a * (1.0 - fg_a);
        let out_luma = out_luma_a / alpha_final;

        LumaA(
            num::cast::<f32, T>(max_t * out_luma).unwrap(),
            num::cast::<f32, T>(max_t * alpha_final).unwrap()
        )
    }
}
