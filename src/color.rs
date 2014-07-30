//! Types and methods for representing and manipulating pixels
use std::num;
use std::num::Bounded;
use std::default::Default;

///An enumeration over supported color types and their bit depths
#[deriving(PartialEq, Show, Clone)]
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

///Returns the number of bits contained in a pixel of ColorType c
pub fn bits_per_pixel(c: ColorType) -> uint {
    match c {
        Grey(n)    => n as uint,
        RGB(n)     => 3 * n as uint,
        Palette(n) => 3 * n as uint,
        GreyA(n)   => 2 * n as uint,
        RGBA(n)    => 4 * n as uint,
    }
}

///Returns the number of color channels that make up this pixel
pub fn num_components(c: ColorType) -> uint {
    match c {
        Grey(_)    => 1,
        RGB(_)     => 3,
        Palette(_) => 3,
        GreyA(_)   => 2,
        RGBA(_)    => 4,
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
}

impl < T: Primitive + Default > Pixel<T> for Rgb<T> {
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

        LumaA(l, Bounded::max_value())
    }

    fn to_rgb(&self) -> Rgb<T> {
        self.clone()
    }

    fn to_rgba(&self) -> Rgba<T> {
        let (r, g, b) = self.channels();

        Rgba(r, g, b, Bounded::max_value())
    }

    fn invert(&mut self) {
        let (r, g, b) = self.channels();

        let max: T = Bounded::max_value();

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

    fn channels4(&self) ->(T, T, T, T) {
        let (r, g, b) = self.channels();

        (r, g, b, Bounded::max_value())
    }
}

impl < T: Primitive + Default > Pixel<T> for Rgba<T> {
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

        let max: T = Bounded::max_value();

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
}

impl < T: Primitive + Default > Pixel<T> for Luma<T> {
    fn from_channels(a: T, _: T, _: T, _: T) -> Luma<T> {
        Luma(a)
    }

    fn to_luma(&self) -> Luma<T> {
        self.clone()
    }

    fn to_luma_alpha(&self) -> LumaA<T> {
        let l = self.channel();

        LumaA(l, Bounded::max_value())
    }

    fn to_rgb(&self) -> Rgb<T> {
        let l1 = self.channel();
        let l2 = self.channel();
        let l3 = self.channel();

        Rgb(l1, l2, l3)
    }

    fn to_rgba(&self) -> Rgba<T> {
        let (r, g, b) = self.to_rgb().channels();

        Rgba(r, g, b, Bounded::max_value())
    }

    fn invert(&mut self) {
        let max: T = Bounded::max_value();
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
        let max: T = Bounded::max_value();

        (l, max.clone(), max.clone(), max.clone())
    }
}

impl < T: Primitive + Default > Pixel<T> for LumaA<T> {
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
        let max: T = Bounded::max_value();

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
        let max: T = Bounded::max_value();

        (l, a, max.clone(), max.clone())
    }
}