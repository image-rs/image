//! Types and functions for working with pixels, where the colortype is not known
//! at compile time.

use imaging::pixel::{
        Pixel,
        Luma,
        LumaA,
        Rgb,
        Rgba
};

use imaging::{
    colortype,
};

use imaging::colortype::ColorType;

/// An abstraction over a vector of pixel types
#[deriving(Clone, Show, PartialEq, Eq)]
pub enum PixelBuf {
        /// Each pixel in this buffer is 8-bit Luma
        Luma8(Vec<Luma<u8>>),
        //Luma16(Vec<Luma<u16>>),

        /// Each pixel in this buffer is 8-bit Luma with alpha
        LumaA8(Vec<LumaA<u8>>),
        //LumaA16(Vec<LumaA<u16>>),

        /// Each pixel in this buffer is 8-bit Rgb
        Rgb8(Vec<Rgb<u8>>),
        //Rgb16(Vec<Rgb<u16>>),

        /// Each pixel in this buffer is 8-bit Rgb with alpha
        Rgba8(Vec<Rgba<u8>>),
        //Rgba16(Vec<Rgba<u16>>),
}

impl PixelBuf {
        /// Convert from self to an array of 8-bit Luma pixels.
        pub fn as_luma8<'a>(&'a self) -> &'a [Luma<u8>] {
                match *self {
                        Luma8(ref p) => p.as_slice(),
                        _            => &[]
                }
        }

        /// Convert from self to an array of 8-bit Luma pixels with alpha.
        pub fn as_luma_alpha8<'a>(&'a self) -> &'a [LumaA<u8>] {
                match *self {
                        LumaA8(ref p) => p.as_slice(),
                        _             => &[]
                }
        }

        /// Convert from self to an array of 8-bit RGB pixels.
        pub fn as_rgb8<'a>(&'a self) -> &'a [Rgb<u8>] {
                match *self {
                        Rgb8(ref p) => p.as_slice(),
                        _           => &[]
                }
        }

        /// Convert from self to an array of 8-bit RGB pixels with alpha.
        pub fn as_rgba8<'a>(&'a self) -> &'a [Rgba<u8>] {
                match *self {
                        Rgba8(ref p) => p.as_slice(),
                        _            => &[]
                }
        }

        /// Convert from a vector of bytes to a ```PixelBuf```
        /// Returns the original buffer if the conversion cannot be done.
        pub fn from_bytes(buf: Vec<u8>, color: ColorType) -> Result<PixelBuf, Vec<u8>> {
                //TODO: consider transmuting
                match color {
                        colortype::RGB(8) => {
                                let p = buf.as_slice()
                                           .chunks(3)
                                           .map(|a| Rgb::<u8>(a[0], a[1], a[2]))
                                           .collect();

                                Ok(Rgb8(p))
                        }

                        colortype::RGBA(8) => {
                                let p = buf.as_slice()
                                           .chunks(4)
                                           .map(|a| Rgba::<u8>(a[0], a[1], a[2], a[3]))
                                           .collect();

                                Ok(Rgba8(p))
                        }

                        colortype::Grey(8) => {
                                let p = buf.as_slice()
                                           .iter()
                                           .map(|a| Luma::<u8>(*a))
                                           .collect();

                                Ok(Luma8(p))
                        }

                        colortype::GreyA(8) => {
                                let p = buf.as_slice()
                                           .chunks(2)
                                           .map(|a| LumaA::<u8>(a[0], a[1]))
                                           .collect();

                                Ok(LumaA8(p))
                        }

                        _ => Err(buf)
                }
        }

        /// Convert from a ```PixelBuf``` to a vector of bytes
        pub fn to_bytes(&self) -> Vec<u8> {
                let mut r = Vec::new();
                //TODO: consider transmuting

                match *self {
                        Luma8(ref a) => {
                                for &i in a.iter() {
                                        r.push(i.channel());
                                }
                        }

                        LumaA8(ref a) => {
                                for &i in a.iter() {
                                        let (l, a) = i.channels();
                                        r.push(l);
                                        r.push(a);
                                }
                        }

                        Rgb8(ref a)  => {
                                for &i in a.iter() {
                                        let (red, g, b) = i.channels();
                                        r.push(red);
                                        r.push(g);
                                        r.push(b);
                                }
                        }

                        Rgba8(ref a) => {
                                for &i in a.iter() {
                                        let (red, g, b, alpha) = i.channels();
                                        r.push(red);
                                        r.push(g);
                                        r.push(b);
                                        r.push(alpha);
                                }
                        }
                }

                r
        }
}

impl Collection for PixelBuf {
        fn len(&self) -> uint {
                match *self {
                        Luma8(ref p)  => p.len(),
                        LumaA8(ref p) => p.len(),
                        Rgb8(ref p)   => p.len(),
                        Rgba8(ref p)  => p.len(),
                }
        }
}