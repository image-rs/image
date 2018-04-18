//! Functions and filters for the sampling of pixels.

// See http://cs.brown.edu/courses/cs123/lectures/08_Image_Processing_IV.pdf
// for some of the theory behind image scaling and convolution

use std::f32;

use num_traits::{NumCast, Zero};

use buffer::{ImageBuffer, Pixel};
use traits::{Primitive, Enlargeable};
use image::GenericImage;
use math::utils::clamp;

/// Available Sampling Filters
#[derive(Clone, Copy)]
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

/// A Representation of a separable filter.
pub struct Filter <'a> {
    /// The filter's filter function.
    pub kernel: Box<Fn(f32) -> f32 + 'a>,

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
/// standard deviation of 0.5
pub fn gaussian_kernel(x: f32) -> f32 {
    gaussian(x, 0.5)
}

/// Calculate the Catmull-Rom cubic spline.
/// Also known as a form of `BiCubic` sampling in two dimensions.
pub fn catmullrom_kernel(x: f32) -> f32 {
    bc_cubic_spline(x, 0.0, 0.5)
}

/// Calculate the triangle function.
/// Also known as `BiLinear` sampling in two dimensions.
pub fn triangle_kernel(x: f32) -> f32 {
    if x.abs() < 1.0 {
        1.0 - x.abs()
    } else {
        0.0
    }
}

/// Calculate the box kernel.
/// Only pixels inside the box should be considered, and those
/// contribute equally.  So this method simply returns 1.
pub fn box_kernel(_x: f32) -> f32 {
    1.0
}

// Sample the rows of the supplied image using the provided filter.
// The height of the image remains unchanged.
// ```new_width``` is the desired width of the new image
// ```filter``` is the filter to use for sampling.
// TODO: Do we really need the 'static bound on `I`? Can we avoid it?
fn horizontal_sample<I, P, S>(image: &I, new_width: u32,
                              filter: &mut Filter)
    -> ImageBuffer<P, Vec<S>>
    where I: GenericImage<Pixel=P> + 'static,
          P: Pixel<Subpixel=S> + 'static,
          S: Primitive + 'static {

    let (width, height) = image.dimensions();
    let mut out = ImageBuffer::new(new_width, height);

    let max: f32 = NumCast::from(S::max_value()).unwrap();
    let ratio = width as f32 / new_width as f32;
    let sratio = if ratio < 1.0 { 1.0 } else { ratio };
    let src_support = filter.support * sratio;

    for y in 0..height {

        for outx in 0..new_width {

            // Find the point in the input image corresponding to the centre
            // of the current pixel in the output image.
            let inputx = (outx as f32 + 0.5) * ratio;

            // Left and right are slice bounds for the input pixels relevant
            // to the output pixel we are calculating.  Pixel x is relevant
            // if and only if (x >= left) && (x < right).

            // Invariant: 0 <= left < right <= width

            let left  = (inputx - src_support).floor() as i64;
            let left  = clamp(left, 0, width as i64 - 1) as u32;

            let right = (inputx + src_support).ceil() as i64;
            let right = clamp(right, left as i64 + 1, width as i64) as u32;

            // Go back to left boundary of pixel, to properly compare with i
            // below, as the kernel treats the centre of a pixel as 0.
            let inputx = inputx - 0.5;

            let mut sum = 0.;

            let mut t = (0., 0., 0., 0.);

            for i in left..right {
                let w = (filter.kernel)((i as f32 - inputx) / sratio);
                sum += w;

                let p = image.get_pixel(i, y);

                let (k1, k2, k3, k4) = p.channels4();
                let vec: (f32, f32, f32, f32) = (
                    NumCast::from(k1).unwrap(),
                    NumCast::from(k2).unwrap(),
                    NumCast::from(k3).unwrap(),
                    NumCast::from(k4).unwrap()
                );

                t.0 += vec.0 * w; t.1 += vec.1 * w;
                t.2 += vec.2 * w; t.3 += vec.3 * w;
            }

            let (t1, t2, t3, t4) = (t.0 / sum, t.1 / sum, t.2 / sum, t.3 / sum);
            let t = Pixel::from_channels(
                NumCast::from(clamp(t1, 0.0, max)).unwrap(),
                NumCast::from(clamp(t2, 0.0, max)).unwrap(),
                NumCast::from(clamp(t3, 0.0, max)).unwrap(),
                NumCast::from(clamp(t4, 0.0, max)).unwrap()
            );

            out.put_pixel(outx, y, t);
        }
    }

    out
}

// Sample the columns of the supplied image using the provided filter.
// The width of the image remains unchanged.
// ```new_height``` is the desired height of the new image
// ```filter``` is the filter to use for sampling.
// TODO: Do we really need the 'static bound on `I`? Can we avoid it?
fn vertical_sample<I, P, S>(image: &I, new_height: u32,
                            filter: &mut Filter)
    -> ImageBuffer<P, Vec<S>>
    where I: GenericImage<Pixel=P> + 'static,
          P: Pixel<Subpixel=S> + 'static,
          S: Primitive + 'static {

    let (width, height) = image.dimensions();
    let mut out = ImageBuffer::new(width, new_height);

    let max: f32 = NumCast::from(S::max_value()).unwrap();
    let ratio = height as f32 / new_height as f32;
    let sratio = if ratio < 1.0 { 1.0 } else { ratio };
    let src_support = filter.support * sratio;

    for x in 0..width {

        for outy in 0..new_height {

            // For an explanation of this algorithm, see the comments
            // in horizontal_sample.
            let inputy = (outy as f32 + 0.5) * ratio;

            let left  = (inputy - src_support).floor() as i64;
            let left  = clamp(left, 0, height as i64 - 1) as u32;

            let right = (inputy + src_support).ceil() as i64;
            let right = clamp(right, left as i64 + 1, height as i64) as u32;

            let inputy = inputy - 0.5;

            let mut sum = 0.;

            let mut t = (0., 0., 0., 0.);

            for i in left..right {
                let w = (filter.kernel)((i as f32 - inputy) / sratio);
                sum += w;

                let p = image.get_pixel(x, i);

                let (k1, k2, k3, k4) = p.channels4();
                let vec: (f32, f32, f32, f32) = (
                    NumCast::from(k1).unwrap(),
                    NumCast::from(k2).unwrap(),
                    NumCast::from(k3).unwrap(),
                    NumCast::from(k4).unwrap()
                );

                t.0 += vec.0 * w; t.1 += vec.1 * w;
                t.2 += vec.2 * w; t.3 += vec.3 * w;
            }

            let (t1, t2, t3, t4) = (t.0 / sum, t.1 / sum, t.2 / sum, t.3 / sum);
            let t = Pixel::from_channels(
                NumCast::from(clamp(t1, 0.0, max)).unwrap(),
                NumCast::from(clamp(t2, 0.0, max)).unwrap(),
                NumCast::from(clamp(t3, 0.0, max)).unwrap(),
                NumCast::from(clamp(t4, 0.0, max)).unwrap()
            );

            out.put_pixel(x, outy, t);
        }
    }

    out
}

/// Resize the supplied image down to the specific dimensions.
pub fn thumbnail<I, P, S>(
    image: &I,
    new_width: u32,
    new_height: u32,
) -> ImageBuffer<P, Vec<S>>
where
    I: GenericImage<Pixel=P>,
    P: Pixel<Subpixel=S> + 'static,
    S: Primitive + Enlargeable + 'static,
{
    let (width, height) = image.dimensions();
    let mut out = ImageBuffer::new(new_width, new_height);

    let x_ratio = width as f32 / new_width as f32;
    let y_ratio = height as f32 / new_height as f32;

    let mut top = 0;
    for outy in 0..new_height {
        let bottom = top;
        top = clamp(((outy + 1) as f32 * y_ratio).round() as u32,
                    bottom + 1,
                    height);

        let mut right = 0;
        for outx in 0..new_width {
            let left = right;
            right = clamp(((outx + 1) as f32 * x_ratio).round() as u32,
                          left + 1,
                          width);

            let mut sum = (S::Larger::zero(), S::Larger::zero(),
                           S::Larger::zero(), S::Larger::zero());
            for y in bottom..top {
                for x in left..right {
                    let k = image.get_pixel(x, y).channels4();
                    sum.0 += NumCast::from(k.0).unwrap();
                    sum.1 += NumCast::from(k.1).unwrap();
                    sum.2 += NumCast::from(k.2).unwrap();
                    sum.3 += NumCast::from(k.3).unwrap();
                }
            }
            let n = NumCast::from((right - left) * (top - bottom)).unwrap();
            let round = NumCast::from(n / NumCast::from(2).unwrap()).unwrap();
            out.put_pixel(outx, outy, Pixel::from_channels(
                S::clamp_from((sum.0 + round) / n),
                S::clamp_from((sum.1 + round) / n),
                S::clamp_from((sum.2 + round) / n),
                S::clamp_from((sum.3 + round) / n),
            ));
        }
    }

    out
}

/// Perform a 3x3 box filter on the supplied image.
/// ```kernel``` is an array of the filter weights of length 9.
// TODO: Do we really need the 'static bound on `I`? Can we avoid it?
pub fn filter3x3<I, P, S>(image: &I, kernel: &[f32])
    -> ImageBuffer<P, Vec<S>>
    where I: GenericImage<Pixel=P> + 'static,
          P: Pixel<Subpixel=S> + 'static,
          S: Primitive + 'static {

    // The kernel's input positions relative to the current pixel.
    let taps: &[(isize, isize)] = &[
        (-1, -1), ( 0, -1), ( 1, -1),
        (-1,  0), ( 0,  0), ( 1,  0),
        (-1,  1), ( 0,  1), ( 1,  1),
      ];

    let (width, height) = image.dimensions();

    let mut out = ImageBuffer::new(width, height);

    let max = S::max_value();
    let max: f32 = NumCast::from(max).unwrap();

    let sum = match kernel.iter().fold(0.0, |s, &item| s + item) {
        x if x == 0.0 => 1.0,
        sum => sum
    };
    let sum = (sum, sum, sum, sum);

    for y in 1..height - 1 {
        for x in 1..width - 1 {
            let mut t = (0., 0., 0., 0.);


            // TODO: There is no need to recalculate the kernel for each pixel.
            // Only a subtract and addition is needed for pixels after the first
            // in each row.
            for (&k, &(a, b)) in kernel.iter().zip(taps.iter()) {
                let k = (k, k, k, k);
                let x0 = x as isize + a;
                let y0 = y as isize + b;

                let p = image.get_pixel(x0 as u32, y0 as u32);

                let (k1, k2, k3, k4) = p.channels4();

                let vec: (f32, f32, f32, f32) = (
                    NumCast::from(k1).unwrap(),
                    NumCast::from(k2).unwrap(),
                    NumCast::from(k3).unwrap(),
                    NumCast::from(k4).unwrap()
                );

                t.0 += vec.0 * k.0; t.1 += vec.1 * k.1;
                t.2 += vec.2 * k.2; t.3 += vec.3 * k.3;
            }

            let (t1, t2, t3, t4) = (t.0 / sum.0, t.1 / sum.1, t.2 / sum.2, t.3 / sum.3);

            let t = Pixel::from_channels(
                NumCast::from(clamp(t1, 0.0, max)).unwrap(),
                NumCast::from(clamp(t2, 0.0, max)).unwrap(),
                NumCast::from(clamp(t3, 0.0, max)).unwrap(),
                NumCast::from(clamp(t4, 0.0, max)).unwrap()
            );

            out.put_pixel(x, y, t);
        }
    }

    out
}

/// Resize the supplied image to the specified dimensions.
/// ```nwidth``` and ```nheight``` are the new dimensions.
/// ```filter``` is the sampling filter to use.
// TODO: Do we really need the 'static bound on `I`? Can we avoid it?
pub fn resize<I: GenericImage + 'static>(image: &I, nwidth: u32, nheight: u32,
                                         filter: FilterType)
    -> ImageBuffer<I::Pixel, Vec<<I::Pixel as Pixel>::Subpixel>>
    where I::Pixel: 'static,
          <I::Pixel as Pixel>::Subpixel: 'static {

    let mut method = match filter {
        FilterType::Nearest    =>   Filter {
            kernel: Box::new(box_kernel),
            support: 0.0
        },
        FilterType::Triangle   => Filter {
            kernel: Box::new(triangle_kernel),
            support: 1.0
        },
        FilterType::CatmullRom => Filter {
            kernel: Box::new(catmullrom_kernel),
            support: 2.0
        },
        FilterType::Gaussian   => Filter {
            kernel: Box::new(gaussian_kernel),
            support: 3.0
        },
        FilterType::Lanczos3   => Filter {
            kernel: Box::new(lanczos3_kernel),
            support: 3.0
        },
};

    let tmp = vertical_sample(image, nheight, &mut method);
    horizontal_sample(&tmp, nwidth, &mut method)
}

/// Performs a Gaussian blur on the supplied image.
/// ```sigma``` is a measure of how much to blur by.
// TODO: Do we really need the 'static bound on `I`? Can we avoid it?
pub fn blur<I: GenericImage + 'static>(image: &I, sigma: f32)
    -> ImageBuffer<I::Pixel, Vec<<I::Pixel as Pixel>::Subpixel>>
    where I::Pixel: 'static,
          <I::Pixel as Pixel>::Subpixel: 'static {

    let sigma = if sigma < 0.0 {
        1.0
    } else {
        sigma
    };

    let mut method = Filter {
        kernel: Box::new(|x| gaussian(x, sigma)),
        support: 2.0 * sigma
    };

    let (width, height) = image.dimensions();

    // Keep width and height the same for horizontal and
    // vertical sampling.
    let tmp = vertical_sample(image, height, &mut method);
    horizontal_sample(&tmp, width, &mut method)
}

/// Performs an unsharpen mask on the supplied image.
/// ```sigma``` is the amount to blur the image by.
/// ```threshold``` is the threshold for the difference between
///
/// See <https://en.wikipedia.org/wiki/Unsharp_masking#Digital_unsharp_masking>
// TODO: Do we really need the 'static bound on `I`? Can we avoid it?
pub fn unsharpen<I, P, S>(image: &I, sigma: f32, threshold: i32)
    -> ImageBuffer<P, Vec<S>>
    where I: GenericImage<Pixel=P> + 'static,
          P: Pixel<Subpixel=S> + 'static,
          S: Primitive + 'static {

    let mut tmp = blur(image, sigma);

    let max = S::max_value();
    let max: i32 = NumCast::from(max).unwrap();
    let (width, height) = image.dimensions();

    for y in 0..height {
        for x in 0..width {
            let a = image.get_pixel(x, y);
            let b = tmp.get_pixel_mut(x, y);

            let p = a.map2(b, |c, d| {
                let ic: i32 = NumCast::from(c).unwrap();
                let id: i32 = NumCast::from(d).unwrap();

                let diff = (ic - id).abs();

                if diff > threshold {
                let e = clamp(ic + diff, 0, max);

                    NumCast::from(e).unwrap()
                } else {
                    c
                }
            });

            *b = p;
        }
    }

    tmp
}

#[cfg(test)]
mod tests {
    #[cfg(feature = "benchmarks")]
    use test;
    use buffer::{ImageBuffer, RgbImage};
    use super::{resize, FilterType};

    #[bench]
    #[cfg(all(feature = "benchmarks", feature = "png_codec"))]
    fn bench_resize(b: &mut test::Bencher) {
        use std::path::Path;
        let img = ::open(&Path::new("./examples/fractal.png")).unwrap();
        b.iter(|| {
            test::black_box(resize(&img, 200, 200, ::Nearest ));
        });
        b.bytes = 800*800*3 + 200*200*3;
    }

    #[test]
    fn test_issue_186() {
        let img: RgbImage = ImageBuffer::new(100, 100);
        let _ = resize(&img, 50, 50, FilterType::Lanczos3);
    }

}
