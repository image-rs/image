use std::mem;
use std::num::Bounded;

use colortype;
use colortype::ColorType;

#[packed]
#[deriving(PartialEq, Clone, Show)]
pub struct Luma<T>(pub T);

impl<T: Primitive + NumCast + Clone + Bounded> Luma<T> {
	pub fn channel(&self) -> T {
		match *self {
			Luma(l) => l
		}
	}
}

#[packed]
#[deriving(PartialEq, Clone, Show)]
pub struct LumaA<T>(pub T, pub T);

impl<T: Primitive + NumCast + Clone + Bounded> LumaA<T> {
	pub fn channels(&self) -> (T, T) {
		match *self {
			LumaA(l, a) => (l, a)
		}
	}

	pub fn alpha(&self) -> T {
		match *self {
			LumaA(_, a) => a
		}
	}
}

#[packed]
#[deriving(PartialEq, Clone, Show)]
pub struct Rgb<T>(pub T, pub T, pub T);

impl<T: Primitive + NumCast + Clone + Bounded> Rgb<T> {
	pub fn channels(&self) -> (T, T, T) {
		match *self {
			Rgb(r, g, b) => (r, g, b)
		}
	}
}

#[packed]
#[deriving(PartialEq, Clone, Show)]
pub struct Rgba<T>(pub T, pub T, pub T, pub T);

impl<T: Primitive + NumCast + Clone + Bounded> Rgba<T> {
	pub fn channels(&self) -> (T, T, T, T) {
		match *self {
			Rgba(r, g, b, a) => (r, g, b, a)
		}
	}

	pub fn alpha(&self) -> T {
		match *self {
			Rgba(_, _, _, a) => a
		}
	}
}

pub trait ConvertColor<T> {
	fn to_rgb(&self) -> Rgb<T>;
	fn to_rgba(&self) -> Rgba<T>;
	fn to_luma(&self) -> Luma<T>;
	fn to_luma_alpha(&self) -> LumaA<T>;
	fn invert(&mut self);
}

impl<T: Primitive + NumCast + Clone + Bounded> ConvertColor<T> for Rgb<T> {
	fn to_luma(&self) -> Luma<T> {
		let (r, g, b) = self.channels();

		let l = 0.2125f32 * r.to_f32().unwrap() +
			0.7154f32 * g.to_f32().unwrap() +
			0.0721f32 * b.to_f32().unwrap();

		Luma(NumCast::from(l).unwrap())
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
}

impl<T: Primitive + NumCast + Clone + Bounded> ConvertColor<T> for Rgba<T> {
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
}

impl<T: Primitive + NumCast + Clone + Bounded> ConvertColor<T> for Luma<T> {
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
}

impl<T: Primitive + NumCast + Clone + Bounded> ConvertColor<T> for LumaA<T> {
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
}

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
	pub fn from_bytes(buf: Vec<u8>, color: ColorType) -> Option<PixelBuf> {
		// This can be done safely by using iterators,
		// but requires a copy of ```buf``` to be made,
		// whereas mem::transmute() operates inplace and
		// elides traversing the vector.
		//
		// buf.as_slice()
		//    .chunks(3)
		//    .map(|a| Rgb::<u8>(a[0], a[1], a[2]))
		//    .collect();

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

fn invert_pixels<A: Primitive + NumCast + Clone + Bounded, T: ConvertColor<A>>(pixels: &mut [T]) {
	for i in pixels.mut_iter() {
		i.invert();
	}
}

//Todo:: consider implementing a generic map function
//that operates over T: ConvertColor or a future T: Pixel trait
pub fn invert(pixels: &mut PixelBuf) {
	match *pixels {
		Luma8(ref mut p)  => invert_pixels(p.as_mut_slice()),
		LumaA8(ref mut p) => invert_pixels(p.as_mut_slice()),
		Rgb8(ref mut p)   => invert_pixels(p.as_mut_slice()),
		Rgba8(ref mut p)  => invert_pixels(p.as_mut_slice()),
	}
}