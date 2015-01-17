//! Functions and filters for the sampling of pixels.

// See http://cs.brown.edu/courses/cs123/lectures/08_Image_Processing_IV.pdf
// for some of the theory behind image scaling and convolution

use std::f32;

use std::num:: {
    cast,
    Float,
    SignedInt,
};

use buffer::{ImageBuffer, Pixel};
use traits::Primitive;
use image::GenericImage;

/// Available Sampling Filters
#[derive(Copy)]
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

fn clamp<N: PartialOrd>(a: N, min: N, max: N) -> N {
    if a > max { max }
    else if a < min { min }
    else { a }
}

// Sample the rows of the supplied image using the provided filter.
// The height of the image remains unchanged.
// ```new_width``` is the desired width of the new image
// ```filter``` is the filter to use for sampling.
fn horizontal_sample<I: GenericImage>(image: &I, new_width: u32,
                                      filter: &mut Filter)
    -> ImageBuffer<I::Pixel, Vec<<I::Pixel as Pixel>::Subpixel>>
    where I::Pixel: 'static,
          <I::Pixel as Pixel>::Subpixel: 'static {

    let (width, height) = image.dimensions();
    let mut out = ImageBuffer::new(new_width, height);

    for y in (0..height) {
        let max = Primitive::max_value();
        let max: f32 = cast(max).unwrap();

        let ratio = width as f32 / new_width as f32;

        // Scale the filter when downsampling.
        let filter_scale = if ratio > 1.0 {
            ratio
        } else {
            1.0
        };

        let filter_radius = (filter.support * filter_scale).ceil();

        for outx in (0..new_width) {

            let inputx = (outx as f32 + 0.5) * ratio;

            let left  = (inputx - filter_radius).ceil() as i64;
            let left  = clamp(left, 0, width as i64 - 1) as u32;

            let right = (inputx + filter_radius).floor() as i64;
            let right = clamp(right, 0, width as i64 - 1) as u32;

            let mut sum = 0.0;

            let mut t1 = 0.0;
            let mut t2 = 0.0;
            let mut t3 = 0.0;
            let mut t4 = 0.0;

            for i in (left..right + 1) {
                let w = (filter.kernel)((i as f32 - inputx) / filter_scale);
                sum += w;

                let x0  = clamp(i, 0, width - 1);
                let p = image.get_pixel(x0, y);

                let (k1, k2, k3, k4) = p.channels4();
                let (a, b, c, d): (f32, f32, f32, f32) = (
                    cast(k1).unwrap(),
                    cast(k2).unwrap(),
                    cast(k3).unwrap(),
                    cast(k4).unwrap()
                );

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

            let t = Pixel::from_channels(
                cast(clamp(t1, 0.0, max)).unwrap(),
                cast(clamp(t2, 0.0, max)).unwrap(),
                cast(clamp(t3, 0.0, max)).unwrap(),
                cast(clamp(t4, 0.0, max)).unwrap()
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
fn vertical_sample<I: GenericImage>(image: &I, new_height: u32,
                                    filter: &mut Filter)
    -> ImageBuffer<I::Pixel, Vec<<I::Pixel as Pixel>::Subpixel>>
    where I::Pixel: 'static,
          <I::Pixel as Pixel>::Subpixel: 'static {

    let (width, height) = image.dimensions();
    let mut out = ImageBuffer::new(width, new_height);


    for x in (0..width) {
        let max = Primitive::max_value();
        let max: f32 = cast(max).unwrap();

        let ratio = height as f32 / new_height as f32;

        // Scale the filter when downsampling.
        let filter_scale = if ratio > 1.0 {
            ratio
        } else {
            1.0
        };

        let filter_radius = (filter.support * filter_scale).ceil();

        for outy in (0..new_height) {
            let inputy = (outy as f32 + 0.5) * ratio;

            let left  = (inputy - filter_radius).ceil() as i64;
            let left  = clamp(left, 0, height as i64 - 1) as u32;

            let right = (inputy + filter_radius).floor() as i64;
            let right = clamp(right, 0, height as i64 - 1) as u32;

            let mut sum = 0.0;

            let mut t1 = 0.0;
            let mut t2 = 0.0;
            let mut t3 = 0.0;
            let mut t4 = 0.0;

            for i in (left..right + 1) {
                let w = (filter.kernel)((i as f32 - inputy) / filter_scale);
                sum += w;

                let y0  = clamp(i, 0, height - 1);
                let p = image.get_pixel(x, y0);

                let (k1, k2, k3, k4) = p.channels4();
                let (a, b, c, d): (f32, f32, f32, f32) = (
                    cast(k1).unwrap(),
                    cast(k2).unwrap(),
                    cast(k3).unwrap(),
                    cast(k4).unwrap()
                );

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

            let t = Pixel::from_channels(
                cast(clamp(t1, 0.0, max)).unwrap(),
                cast(clamp(t2, 0.0, max)).unwrap(),
                cast(clamp(t3, 0.0, max)).unwrap(),
                cast(clamp(t4, 0.0, max)).unwrap()
            );

            out.put_pixel(x, outy, t);
        }
    }

    out
}

/// Perform a 3x3 box filter on the supplied image.
/// ```kernel``` is an array of the filter weights of length 9.
pub fn filter3x3<I: GenericImage>(image: &I, kernel: &[f32])
    -> ImageBuffer<I::Pixel, Vec<<I::Pixel as Pixel>::Subpixel>>
    where I::Pixel: 'static,
          <I::Pixel as Pixel>::Subpixel: 'static {

    // The kernel's input positions relative to the current pixel.
    let taps: &[(isize, isize)] = &[
        (-1, -1), ( 0, -1), ( 1, -1),
        (-1,  0), ( 0,  0), ( 1,  0),
        (-1,  1), ( 0,  1), ( 1,  1),
      ];

    let (width, height) = image.dimensions();

    let mut out = ImageBuffer::new(width, height);


    let max = Primitive::max_value();
    let max: f32 = cast(max).unwrap();

    let sum = kernel.iter().fold(0.0, |&: a, f| a + *f);

    let sum = if sum == 0.0 {
        1.0
    } else {
        sum
    };

    for y in (1..height - 1) {
        for x in (1..width - 1) {
            let mut t1 = 0.0;
            let mut t2 = 0.0;
            let mut t3 = 0.0;
            let mut t4 = 0.0;

            // TODO: There is no need to recalculate the kernel for each pixel.
            // Only a subtract and addition is needed for pixels after the first
            // in each row.
            for (&k, &(a, b)) in kernel.iter().zip(taps.iter()) {
                let x0 = x as isize + a;
                let y0 = y as isize + b;

                let p = image.get_pixel(x0 as u32, y0 as u32);

                let (k1, k2, k3, k4) = p.channels4();

                let (a, b, c, d): (f32, f32, f32, f32) = (
                    cast(k1).unwrap(),
                    cast(k2).unwrap(),
                    cast(k3).unwrap(),
                    cast(k4).unwrap()
                );

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

            let t = Pixel::from_channels(
                cast(clamp(t1, 0.0, max)).unwrap(),
                cast(clamp(t2, 0.0, max)).unwrap(),
                cast(clamp(t3, 0.0, max)).unwrap(),
                cast(clamp(t4, 0.0, max)).unwrap()
            );

            out.put_pixel(x, y, t);
        }
    }

    out
}

/// Resize the supplied image to the specified dimensions
/// ```nwidth``` and ```nheight``` are the new dimensions.
/// ```filter``` is the sampling filter to use.
pub fn resize<I: GenericImage>(image: &I, nwidth: u32, nheight: u32,
                               filter: FilterType)
    -> ImageBuffer<I::Pixel, Vec<<I::Pixel as Pixel>::Subpixel>>
    where I::Pixel: 'static,
          <I::Pixel as Pixel>::Subpixel: 'static {

    let mut method = match filter {
        FilterType::Nearest    =>   Filter {
            kernel: Box::new(|&: x| box_kernel(x)),
            support: 0.5
        },
        FilterType::Triangle   => Filter {
            kernel: Box::new(|&: x| triangle_kernel(x)),
            support: 1.0
        },
        FilterType::CatmullRom => Filter {
            kernel: Box::new(|&: x| catmullrom_kernel(x)),
            support: 2.0
        },
        FilterType::Gaussian   => Filter {
            kernel: Box::new(|&: x| gaussian_kernel(x)),
            support: 3.0
        },
        FilterType::Lanczos3   => Filter {
            kernel: Box::new(|&: x| lanczos3_kernel(x)),
            support: 3.0
        },
};

    let tmp = vertical_sample(image, nheight, &mut method);
    horizontal_sample(&tmp, nwidth, &mut method)
}

/// Performs a Gaussian blur on the supplied image.
/// ```sigma``` is a measure of how much to blur by.
pub fn blur<I: GenericImage>(image: &I, sigma: f32)
    -> ImageBuffer<I::Pixel, Vec<<I::Pixel as Pixel>::Subpixel>>
    where I::Pixel: 'static,
          <I::Pixel as Pixel>::Subpixel: 'static {

    let sigma = if sigma < 0.0 {
        1.0
    } else {
        sigma
    };

    let mut method = Filter {
        kernel: Box::new(|&: x| gaussian(x, sigma)),
        support: 2.0 * sigma
    };

    let (width, height) = image.dimensions();

    // Keep width and height the same for horizontal and
    // vertical sampling.
    let tmp = vertical_sample(image, height, &mut method);
    horizontal_sample(&tmp, width, &mut method)
}

/// Performs an unsharpen mask on the supplied image
/// ```sigma``` is the amount to blur the image by.
/// ```threshold``` is the threshold for the difference between
/// see https://en.wikipedia.org/wiki/Unsharp_masking#Digital_unsharp_masking
pub fn unsharpen<I: GenericImage>(image: &I, sigma: f32, threshold: i32)
    -> ImageBuffer<I::Pixel, Vec<<I::Pixel as Pixel>::Subpixel>>
    where I::Pixel: 'static,
          <I::Pixel as Pixel>::Subpixel: 'static {

    let mut tmp = blur(image, sigma);

    let max = Primitive::max_value();
    let max: i32 = cast(max).unwrap();
    let (width, height) = image.dimensions();

    for y in (0..height) {
        for x in (0..width) {
            let a = image.get_pixel(x, y);
            let b = tmp.get_pixel_mut(x, y);

            let p = a.map2(b, |&: c, d| {
                let ic: i32 = cast(c).unwrap();
                let id: i32 = cast(d).unwrap();

                let diff = (ic - id).abs();

                if diff > threshold {
                let e = clamp(ic + diff, 0, max);

                    cast(e).unwrap()
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
    extern crate test;

    use buffer::{ImageBuffer, RgbImage};
    use super::{resize, FilterType};


    #[bench]
    fn bench_resize(b: &mut test::Bencher) {
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
