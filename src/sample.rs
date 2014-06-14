// See http://cs.brown.edu/courses/cs123/lectures/08_Image_Processing_IV.pdf
// for some of the theory behind image scaling and convolution

use std::f32;
use std::num::cast;
use std::num::Bounded;
use std::default::Default;

use pixels::Pixel;

/// Available Sampling Filters
pub enum FilterType {
	/// Nearest Neighbor
	Nearest,

	/// Linear Filter
	Triangle,

	/// Cubic Filter
	CatmullRom,

	/// Gaussian Filter
	Gaussian,

	/// Lanczos with window 3
	Lanczos3
}

pub struct Filter<'a> {
	pub kernel:  |f32|: 'a -> f32,
	pub support: f32
}

fn sinc(t: f32) -> f32 {
	let a = t * f32::consts::PI;

	if t == 0.0 {
		1.0
	} else {
		a.sin() / a
	}
}

fn lanczos(x: f32, t: f32) -> f32 {
	if x.abs() < t {
		sinc(x) * sinc(x / t)
	} else {
		0.0
	}
}

fn bc_cubic_spline(x: f32, b: f32, c: f32) -> f32 {
	let a = x.abs();

	let k = if a < 1.0 {
		  (12.0 - 9.0 * b - 6.0 * c) * a.powi(3) +
		(-18.0 + 12.0 * b + 6.0 * c) * a.powi(2) +
		             (6.0 - 2.0 * b)
	} else if a < 2.0 {
		       (-b -  6.0 * c) * a.powi(3) +
		  (6.0 * b + 30.0 * c) * a.powi(2) +
		(-12.0 * b - 48.0 * c) * a +
		  (8.0 * b + 24.0 * c)
	} else {
		0.0
	};

	k / 6.0
}

pub fn gaussian(x: f32, r: f32) -> f32 {
	((2.0 * f32::consts::PI).sqrt() * r).recip() *
	(-x.powi(2) / (2.0 * r.powi(2))).exp()
}

pub fn lanczos3_kernel(x: f32) -> f32 {
	lanczos(x, 3.0)
}

pub fn gaussian_kernel(x: f32) -> f32 {
	gaussian(x, 1.0)
}

pub fn catmullrom_kernel(x: f32) -> f32 {
	bc_cubic_spline(x, 0.0, 0.5)
}

pub fn triangle_kernel(x: f32) -> f32 {
	if x.abs() < 1.0 {
		1.0 - x
	} else {
		0.0
	}
}

pub fn box_kernel(x: f32) -> f32 {
	if x.abs() <= 0.5 {
		1.0
	} else {
		0.0
	}
}

fn clamp<N: Num + PartialOrd>(a: N, min: N, max: N) -> N {
	if a > max { max }
	else if a < min { min }
	else { a }
}

pub fn horizontal_sample<P: Primitive, T: Pixel<P> + Default + Clone>(
	pixels: &[T],
	width:  u32,
	height: u32,
	nwidth: u32,
	mut method: Filter) -> Vec<T> {

	let method = &mut method;
	let d: T = Default::default();

	let mut vec = Vec::from_elem(nwidth as uint * height as uint, d.clone());

	for y in range(0, height as uint) {
		let inr = pixels.slice_from(y * width as uint)
				.slice_to(width as uint);

		let outr = vec.mut_slice_from(y * nwidth as uint)
			      .mut_slice_to(nwidth as uint);

		convolve1d(inr, outr, width, nwidth, method, 1);
	}

	vec
}

pub fn vertical_sample<P: Primitive, T: Pixel<P> + Default + Clone>(
	pixels:  &[T],
	height:  u32,
	width:   u32,
	nheight: u32,
	mut method:  Filter) -> Vec<T> {

	let method = &mut method;
	let d: T = Default::default();

	let mut vec = Vec::from_elem(width as uint * nheight as uint, d.clone());

	for x in range(0, width as uint) {
		let inc  = pixels.slice_from(x);

		let outc = vec.as_mut_slice()
			      .mut_slice_from(x);

		convolve1d(inc, outc, height, nheight, method, width as uint);
	}

	vec
}

fn convolve1d<P: Primitive, T: Pixel<P> + Default>(
	inrow:  &[T],
	outrow: &mut [T],
	inlen:  u32,
	outlen: u32,
	filter: &mut Filter,
	stride: uint) {

	let max: P = Bounded::max_value();
	let max = cast::<P, f32>(max).unwrap();

	let ratio = inlen as f32 / outlen as f32;

	let filter_scale = if ratio > 1.0 {
		ratio
	} else {
		1.0
	};

	let filter_radius = (filter.support * filter_scale).ceil();

	for outx in range(0, outlen as uint) {
		let inputx = (outx as f32 + 0.5) * ratio;

		let left  = (inputx - filter_radius).ceil() as int;
		let left  = clamp(left, 0, inrow.len() as int - 1) as uint;

		let right = (inputx + filter_radius).floor() as int;
		let right = clamp(right, 0, inrow.len() as int - 1) as uint;

		let tmp: T = Default::default();

		let mut sum = 0.0;

		let mut t1 = 0.0;
		let mut t2 = 0.0;
		let mut t3 = 0.0;
		let mut t4 = 0.0;

		for i in range(left, right + 1) {
			let w = (filter.kernel)((i as f32 - inputx) / filter_scale);
			sum += w;

			let (k1, k2, k3, k4) = inrow[clamp(i * stride, 0, inrow.len() - 1)].channels4();
			let (a, b, c, d) = (cast::<P, f32>(k1).unwrap(),
					    cast::<P, f32>(k2).unwrap(),
					    cast::<P, f32>(k3).unwrap(),
					    cast::<P, f32>(k4).unwrap());

			let (a1, b1, c1, d1) = ( a  * w,  b * w,   c * w,   d * w);
			let (a2, b2, c2, d2) = (a1 + t1, b1 + t2, c1 + t3, d1 + t4);

			t1 = a2;
			t2 = b2;
			t3 = c2;
			t4 = d2;
		}

		t1 /= sum;
		t2 /= sum;
		t3 /= sum;
		t4 /= sum;

		outrow[outx * stride] = tmp.from_channels(cast::<f32, P>(clamp(t1, 0.0, max)).unwrap(),
						 	  cast::<f32, P>(clamp(t2, 0.0, max)).unwrap(),
							  cast::<f32, P>(clamp(t3, 0.0, max)).unwrap(),
							  cast::<f32, P>(clamp(t4, 0.0, max)).unwrap());
	}
}
