//! Functions and filters for the sampling of pixels.

// See http://cs.brown.edu/courses/cs123/lectures/08_Image_Processing_IV.pdf
// for some of the theory behind image scaling and convolution

use std::f32;
use std::num::{
        cast,
        Bounded
};
use std::default::Default;

use imaging::pixel::Pixel;

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

/// A Representation of a seperable filter.
pub struct Filter<'a> {
	/// The filter's filter function.
	pub kernel:  |f32|: 'a -> f32,

	/// The window on which this filter operates.
	pub support: f32
}

// sinc function: the ideal sampling filter.
fn sinc(t: f32) -> f32 {
	let a = t * f32::consts::PI;

	if t == 0.0 {
		1.0
	} else {
		a.sin() / a
	}
}

// lanczos kernel function. A windowed sinc function.
fn lanczos(x: f32, t: f32) -> f32 {
	if x.abs() < t {
		sinc(x) * sinc(x / t)
	} else {
		0.0
	}
}

// Calculate a splice based on the b and c parameters.
// from authors Mitchell and Netravali.
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

/// The Gaussian Function.
/// ```r``` is the standard deviation.
pub fn gaussian(x: f32, r: f32) -> f32 {
	((2.0 * f32::consts::PI).sqrt() * r).recip() *
	(-x.powi(2) / (2.0 * r.powi(2))).exp()
}

/// Calculate the lanczos kernel with a window of 3
pub fn lanczos3_kernel(x: f32) -> f32 {
	lanczos(x, 3.0)
}

/// Calculate the gaussian function with a
/// standard deviation of 1.0
pub fn gaussian_kernel(x: f32) -> f32 {
	gaussian(x, 1.0)
}

/// Calculate the Catmull-Rom cubic spline.
/// Also known as a form of BiCubic sampling in two dimensions.
pub fn catmullrom_kernel(x: f32) -> f32 {
	bc_cubic_spline(x, 0.0, 0.5)
}

/// Calculate the triangle function.
/// Also known as BiLinear sampling in two dimensions.
pub fn triangle_kernel(x: f32) -> f32 {
	if x.abs() < 1.0 {
		1.0 - x
	} else {
		0.0
	}
}

/// Calculate the box kernel.
/// When applied in two dimensions with a support of 0.5
/// it is equivalent to nearest neighbor sampling.
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

// Sample the rows of ```pixels`` using the provided filter.
// The height of ```pixels``` remains unchanged.
// ```width``` is the current width of ```pixels```.
// ```height``` is the current height of ```pixels```.
// ```nwidth``` is the desired width of ```pixels```
// ```method``` is the filter to use for sampling.
fn horizontal_sample<P: Primitive, T: Pixel<P> + Default + Clone>(
	pixels: &[T],
	width:  u32,
	height: u32,
	nwidth: u32,
	method: &mut Filter) -> Vec<T> {

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

// Sample the columns of ```pixels`` using the provided filter.
// The width of ```pixels``` remains unchanged.
// ```width``` is the current width of ```pixels```.
// ```height``` is the current height of ```pixels```.
// ```nheight``` is the desired height of ```pixels```
// ```method``` is the filter to use for sampling.
fn vertical_sample<P: Primitive, T: Pixel<P> + Default + Clone>(
	pixels:  &[T],
	height:  u32,
	width:   u32,
	nheight: u32,
	method:  &mut Filter) -> Vec<T> {

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

//Performs a 1 - dimensional convultion of ```inrow`` using the Filter
//``filter`` and store the result in ```outrow```
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

	//Scale the filter when downsampling.
	let filter_scale = if ratio > 1.0 {
		ratio
	} else {
		1.0
	};

	let filter_radius = (filter.support * filter_scale).ceil();

	//TODO: Precalculate the filter weights once per row and column
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

/// Perform a 3x3 box filter on ```pixels```.
/// ```width``` is the width of pixels.
/// ```height``` is the height of pixels.
/// ```kernel``` is an array of the filter weights of length 9.
pub fn filter3x3<P: Primitive, T: Pixel<P> + Default + Clone>(
	pixels: &[T],
	width:  u32,
	height: u32,
	kernel: &[f32]) -> Vec<T> {

	// The kernel's input positions relative to the current pixel.
	let taps = [
		-(width as i64 - 1),
		-(width as i64),
		-(width as i64 + 1),
        	-1,
        	0,
        	1,
        	width as i64 - 1,
        	width as i64,
        	width as i64 + 1
        ];

        let d: T = Default::default();
	let mut out = Vec::from_elem(width as uint * height as uint, d.clone());

        let max: P = Bounded::max_value();
	let max = cast::<P, f32>(max).unwrap();

	let sum = kernel.iter().fold(0.0, |a, f| a + *f);
	let sum = if sum == 0.0 { 1.0 }
		  else { sum };

	for y in range(1, height as uint - 1) {
		for x in range(1, width as uint - 1) {
			let index = (y * width as uint) + x;

			let mut t1 = 0.0;
			let mut t2 = 0.0;
			let mut t3 = 0.0;
			let mut t4 = 0.0;

			//TODO: There is no need to recalculate the kernel for each pixel.
			//Only a subtract and addition is needed for pixels after the first
			//in each row.
			for (&k, &tap) in kernel.iter().zip(taps.iter()) {
				let p = pixels[(index as i64 + tap) as uint].clone();

				let (k1, k2, k3, k4) = p.channels4();

				let (a, b, c, d) = (cast::<P, f32>(k1).unwrap(),
					    	    cast::<P, f32>(k2).unwrap(),
					    	    cast::<P, f32>(k3).unwrap(),
					    	    cast::<P, f32>(k4).unwrap());

				let (a1, b1, c1, d1) = (a * k, b * k, c * k, d * k);

				t1 += a1;
				t2 += b1;
				t3 += c1;
				t4 += d1;
			}

			t1 /= sum;
			t2 /= sum;
			t3 /= sum;
			t4 /= sum;

			let tmp: T = Default::default();

			out.as_mut_slice()[index] = tmp.from_channels(cast::<f32, P>(clamp(t1, 0.0, max)).unwrap(),
							              cast::<f32, P>(clamp(t2, 0.0, max)).unwrap(),
							              cast::<f32, P>(clamp(t3, 0.0, max)).unwrap(),
							              cast::<f32, P>(clamp(t4, 0.0, max)).unwrap());
		}
	}

	out
}

/// Resize ```pixels```.
/// ```width``` and ```height``` are the original dimensions.
/// ```nwidth``` and ```nheight``` are the new dimensions.
/// ```filter``` is the sampling filter to use.
pub fn resize<A: Primitive, T: Pixel<A> + Default + Clone>(
	pixels:  &[T],
	width:   u32,
	height:  u32,
	nwidth:  u32,
	nheight: u32,
	filter:  FilterType) -> Vec<T> {

        let mut method = match filter {
                Nearest    =>   Filter {
	                kernel:  |x| box_kernel(x),
	                support: 0.5
                },
                Triangle   => Filter {
                        kernel:  |x| triangle_kernel(x),
                        support: 1.0
                },
                CatmullRom => Filter {
                        kernel:  |x| catmullrom_kernel(x),
                        support: 2.0
                },
                Gaussian   => Filter {
                        kernel:  |x| gaussian_kernel(x),
                        support: 3.0
                },
                Lanczos3   => Filter {
                        kernel:  |x| lanczos3_kernel(x),
                        support: 3.0
                },
        };

        let tmp = vertical_sample(pixels, height, width, nheight, &mut method);

	horizontal_sample(tmp.as_slice(), width, nheight, nwidth, &mut method)
}

/// Perfomrs a Gausian blur on ```pixels```
/// ```width``` and ```height``` are the dimensions of the buffer.
/// ```sigma``` is a meausure of how much to blur by.
pub fn blur<A: Primitive, T: Pixel<A> + Default + Clone>(
	pixels:  &[T],
	width:   u32,
        height:  u32,
        sigma:   f32) -> Vec<T> {

        let sigma = if sigma < 0.0 {
                1.0
        } else {
                sigma
        };

        let mut method = Filter {
                kernel:  |x| gaussian(x, sigma),
                support: 2.0 * sigma
        };

        // Keep width and height the same for horizontal and
        // vertical sampling.
        let tmp = vertical_sample(pixels, height, width, height, &mut method);

        horizontal_sample(tmp.as_slice(), width, height, width, &mut method)
}

/// Performs an unsharpen mask on ```pixels```
/// ```sigma``` is the amount to blur the image by.
/// ```threshold``` is the threshold for the difference between
/// see https://en.wikipedia.org/wiki/Unsharp_masking#Digital_unsharp_masking
pub fn unsharpen<A: Primitive, T: Pixel<A> + Clone + Default>(
	pixels:    &[T],
	width:     u32,
	height:    u32,
	sigma:     f32,
	threshold: i32) -> Vec<T> {

	let mut tmp = blur(pixels, width, height, sigma);

        let max: A = Bounded::max_value();

        for (p, b) in pixels.iter().zip(tmp.mut_iter()) {
                let a = p.map2(b.clone(), |c, d| {
                        let ic = cast::<A, i32>(c).unwrap();
                        let id = cast::<A, i32>(d).unwrap();

                        let diff = (ic - id).abs();

                        if diff > threshold {
                                let e = clamp(ic + diff, 0, cast::<A, i32>(max).unwrap());

                                cast::<i32, A>(e).unwrap()
                        } else {
                                c
                        }
                });

                *b = a;
        }

        tmp
}