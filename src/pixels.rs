//! Types and functions for dealing with pixels.

use std::mem;
use std::num::cast;
use std::num::Bounded;

use sample;
use colortype;
use colortype::ColorType;

///A type to hold a grayscale pixel
#[packed]
#[deriving(Default, PartialEq, Clone, Show, Copy)]
pub struct Luma<T>(pub T);

impl<T: Primitive> Luma<T> {
	/// Returns the channels of this pixel as a tuple
	pub fn channel(&self) -> T {
		match *self {
			Luma(l) => l
		}
	}
}

/// A type to hold a grayscale pixel with an alha channel
#[packed]
#[deriving(Default, PartialEq, Clone, Show, Copy)]
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
#[deriving(Default, PartialEq, Clone, Show, Copy)]
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
#[deriving(Default, PartialEq, Clone, Show, Copy)]
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
pub trait Pixel<T> {
	fn from_channels(&self, a: T, b: T, c: T, d: T) -> Self;
	/// Convert this pixel to RGB
	fn to_rgb(&self) -> Rgb<T>;

	/// Convert this pixel to RGB with an alpha channel
	fn to_rgba(&self) -> Rgba<T>;

	/// Convert this pixel to luma
	fn to_luma(&self) -> Luma<T>;

	/// Convert this pixel to luma with an alpha channel
	fn to_luma_alpha(&self) -> LumaA<T>;

	/// Invert the color of this pixel
	fn invert(&mut self);

	/// Apply the function ```f``` to each channel of this pixel.
	/// If there is an alpha channel it is not changed.
	fn map(&self, f: |T| -> T) -> Self;

	/// Apply the function ```f``` to each channel of this pixel and
	/// ```other``` pairwise.
	fn map2(&self, other: Self, f: |T, T| -> T) -> Self;

	/// Returns the channels of this pixes as a 4 tuple. If the pixel
	/// has less than 4 channels the remainder is filled with the maximum value
	fn channels4(&self) -> (T, T, T, T);
}

impl<T: Primitive> Pixel<T> for Rgb<T> {
	fn from_channels(&self, a: T, b: T, c: T, _: T) -> Rgb<T> {
		Rgb(a, b, c)
	}

	fn to_luma(&self) -> Luma<T> {
		let (r, g, b) = self.channels();

		let l = 0.2125f32 * r.to_f32().unwrap() +
			0.7154f32 * g.to_f32().unwrap() +
			0.0721f32 * b.to_f32().unwrap();

		Luma(cast::<f32, T>(l).unwrap())
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

	fn map(&self, f: |a: T| -> T) -> Rgb<T> {
		let (r, g, b) = self.channels();

		let r1 = f(r);
		let g1 = f(g);
		let b1 = f(b);

		Rgb(r1, g1, b1)
	}

	fn map2(&self, other: Rgb<T>, f: |a: T, b: T| -> T) -> Rgb<T> {
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

impl<T: Primitive> Pixel<T> for Rgba<T> {
	fn from_channels(&self, a: T, b: T, c: T, d: T) -> Rgba<T> {
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

	fn map(&self, f: |a: T| -> T) -> Rgba<T> {
		let (r, g, b, a) = self.channels();

		let r1 = f(r);
		let g1 = f(g);
		let b1 = f(b);

		Rgba(r1, g1, b1, a)
	}

	fn map2(&self, other: Rgba<T>, f: |a: T, b: T| -> T) -> Rgba<T> {
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

impl<T: Primitive> Pixel<T> for Luma<T> {
	fn from_channels(&self, a: T, _: T, _: T, _: T) -> Luma<T> {
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

	fn map(&self, f: |a: T| -> T) -> Luma<T> {
		let l  = self.channel();
		let l1 = f(l);

		Luma(l1)
	}

	fn map2(&self, other: Luma<T>, f: |a: T, b: T| -> T) -> Luma<T> {
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

impl<T: Primitive> Pixel<T> for LumaA<T> {
	fn from_channels(&self, a: T, b: T, _: T, _: T) -> LumaA<T> {
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

	fn map(&self, f: |a: T| -> T) -> LumaA<T> {
		let (l, a) = self.channels();

		let l1 = f(l);

		LumaA(l1, a)
	}

	fn map2(&self, other: LumaA<T>, f: |a: T, b: T| -> T) -> LumaA<T> {
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

/// An abstraction over a vector of pixel types
#[deriving(Clone, Show, PartialEq)]
pub enum PixelBuf {
	Luma8(Vec<Luma<u8>>),
	//Luma16(Vec<Luma<u16>>),

	LumaA8(Vec<LumaA<u8>>),
	//LumaA16(Vec<LumaA<u16>>),

	Rgb8(Vec<Rgb<u8>>),
	//Rgb16(Vec<Rgb<u16>>),

	Rgba8(Vec<Rgba<u8>>),
	//Rgba16(Vec<Rgba<u16>>),
}

impl PixelBuf {
	/// Convert from a vector of bytes to a ```PixelBuf```
	/// Returns ```None``` if the conversion cannot be done.
	pub fn from_bytes(buf: Vec<u8>, color: ColorType) -> Option<PixelBuf> {
		// This can be done safely by using iterators:
		//
		// buf.as_slice()
		//    .chunks(3)
		//    .map(|a| Rgb::<u8>(a[0], a[1], a[2]))
		//    .collect();
		//
		// but requires a copy of ```buf``` to be made,
		// whereas mem::transmute() operates inplace and
		// elides traversing the vector.

		match color {
			colortype::RGB(8) => {
				let p = unsafe {
					let size = mem::size_of::<Rgb<u8>>();

					assert!(size == 3);
					assert!(buf.len() % size == 0);

					let len = buf.len() / size;
					let mut tmp: Vec<Rgb<u8>> = mem::transmute(buf);
					tmp.set_len(len);
					tmp
				};

				Some(Rgb8(p))
			}

			colortype::RGBA(8) => {
				let p = unsafe {
					let size = mem::size_of::<Rgba<u8>>();

					assert!(size == 4);
					assert!(buf.len() % size == 0);

					let len = buf.len() / size;
					let mut tmp: Vec<Rgba<u8>> = mem::transmute(buf);
					tmp.set_len(len);
					tmp
				};

				Some(Rgba8(p))
			}

			colortype::Grey(8) => {
				let p = unsafe {
					let size = mem::size_of::<Luma<u8>>();

					assert!(size == 1);

					let tmp: Vec<Luma<u8>> = mem::transmute(buf);
					tmp
				};

				Some(Luma8(p))
			}

			colortype::GreyA(8) => {
				let p = unsafe {
					let size = mem::size_of::<LumaA<u8>>();

					assert!(size == 2);
					assert!(buf.len() % size == 0);

					let len = buf.len() / size;
					let mut tmp: Vec<LumaA<u8>> = mem::transmute(buf);
					tmp.set_len(len);
					tmp
				};

				Some(LumaA8(p))
			}

			_ => None
		}
	}

	/// Convert from a ```PixelBuf``` to a vector of bytes
	pub fn to_bytes(&self) -> Vec<u8> {
		let mut r = Vec::new();

		//TODO: Consider using mem::transmute
		// and returning a reference
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

/// Convert the ```PixelBuf``` pixels to graysacle
pub fn grayscale(pixels: &PixelBuf) -> PixelBuf {
	match *pixels {
		Luma8(_)      => pixels.clone(),

		LumaA8(ref p) => {
			let n = p.iter().map(|i| i.to_luma()).collect();
			Luma8(n)
		}

		Rgb8(ref p)   => {
			let n = p.iter().map(|i| i.to_luma()).collect();
			Luma8(n)
		}

		Rgba8(ref p)  => {
			let n = p.iter().map(|i| i.to_luma()).collect();
			Luma8(n)
		}
	}
}

fn invert_pixels<A: Primitive, T: Pixel<A>>(pixels: &mut [T]) {
	for i in pixels.mut_iter() {
		i.invert();
	}
}

//TODO: consider implementing a generic map function
//that operates over T: Pixel trait

/// Invert the pixels in ```PixelBuf``` pixels
pub fn invert(pixels: &mut PixelBuf) {
	match *pixels {
		Luma8(ref mut p)  => invert_pixels(p.as_mut_slice()),
		LumaA8(ref mut p) => invert_pixels(p.as_mut_slice()),
		Rgb8(ref mut p)   => invert_pixels(p.as_mut_slice()),
		Rgba8(ref mut p)  => invert_pixels(p.as_mut_slice()),
	}
}

/// Resize this ```PixelBuf``` pixels.
/// ```width``` and ```height``` are the original dimensions.
/// ```nwidth``` and ```nheight``` are the new dimensions.
pub fn resize(pixels:  &PixelBuf,
	      width:   u32,
	      height:  u32,
	      nwidth:  u32,
	      nheight: u32,
	      filter:  sample::FilterType) -> PixelBuf {

	let tmp = match *pixels {
		Luma8(ref p)  => Luma8(sample::vertical_sample(p.as_slice(), height, width, nheight, filter)),
		LumaA8(ref p) => LumaA8(sample::vertical_sample(p.as_slice(), height, width, nheight, filter)),
		Rgb8(ref p)   => Rgb8(sample::vertical_sample(p.as_slice(), height, width, nheight, filter)),
		Rgba8(ref p)  => Rgba8(sample::vertical_sample(p.as_slice(), height, width, nheight, filter)),
	};

	match tmp {
		Luma8(ref p)  => Luma8(sample::horizontal_sample(p.as_slice(), width, nheight, nwidth, filter)),
		LumaA8(ref p) => LumaA8(sample::horizontal_sample(p.as_slice(), width, nheight, nwidth, filter)),
		Rgb8(ref p)   => Rgb8(sample::horizontal_sample(p.as_slice(), width, nheight, nwidth, filter)),
		Rgba8(ref p)  => Rgba8(sample::horizontal_sample(p.as_slice(), width, nheight, nwidth, filter)),
	}
}