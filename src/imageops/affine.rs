//! Functions for performing affine transformations.

use buffer::{ImageBuffer, Pixel};
use image::GenericImage;

/// Rotate an image 90 degrees clockwise.
// TODO: Is the 'static bound on `I` really required? Can we avoid it?
pub fn rotate90<I: GenericImage + 'static>(image:  &I)
    -> ImageBuffer<I::Pixel, Vec<<I::Pixel as Pixel>::Subpixel>>
    where I::Pixel: 'static,
          <I::Pixel as Pixel>::Subpixel: 'static {
    let (width, height) = image.dimensions();
    let mut out = ImageBuffer::new(height, width);

    for y in 0..height {
        for x in 0..width {
            let p = image.get_pixel(x, y);
            out.put_pixel(height - 1 - y, x, p);
        }
    }

    out
}

/// Rotate an image 180 degrees clockwise.
// TODO: Is the 'static bound on `I` really required? Can we avoid it?
pub fn rotate180<I: GenericImage + 'static>(image:  &I)
    -> ImageBuffer<I::Pixel, Vec<<I::Pixel as Pixel>::Subpixel>>
    where I::Pixel: 'static,
          <I::Pixel as Pixel>::Subpixel: 'static {
    let (width, height) = image.dimensions();
    let mut out = ImageBuffer::new(width, height);

    for y in 0..height {
        for x in 0..width {
            let p = image.get_pixel(x, y);
            out.put_pixel(width - 1 - x, height - 1 - y, p);
        }
    }

    out
}

/// Rotate an image 270 degrees clockwise.
// TODO: Is the 'static bound on `I` really required? Can we avoid it?
pub fn rotate270<I: GenericImage + 'static>(image:  &I)
    -> ImageBuffer<I::Pixel, Vec<<I::Pixel as Pixel>::Subpixel>>
    where I::Pixel: 'static,
          <I::Pixel as Pixel>::Subpixel: 'static {
    let (width, height) = image.dimensions();
    let mut out = ImageBuffer::new(height, width);

    for y in 0..height {
        for x in 0..width {
            let p = image.get_pixel(x, y);
            out.put_pixel(y, width - 1 - x, p);
        }
    }

    out
}

/// Flip an image horizontally
// TODO: Is the 'static bound on `I` really required? Can we avoid it?
pub fn flip_horizontal<I: GenericImage + 'static>(image:  &I)
    -> ImageBuffer<I::Pixel, Vec<<I::Pixel as Pixel>::Subpixel>>
    where I::Pixel: 'static,
          <I::Pixel as Pixel>::Subpixel: 'static {
    let (width, height) = image.dimensions();
    let mut out = ImageBuffer::new(width, height);

    for y in 0..height {
        for x in 0..width {
            let p = image.get_pixel(x, y);
            out.put_pixel(width - 1 - x, y, p);
        }
    }

    out
}

/// Flip an image vertically
// TODO: Is the 'static bound on `I` really required? Can we avoid it?
pub fn flip_vertical<I: GenericImage + 'static>(image:  &I)
    -> ImageBuffer<I::Pixel, Vec<<I::Pixel as Pixel>::Subpixel>>
    where I::Pixel: 'static,
          <I::Pixel as Pixel>::Subpixel: 'static {
    let (width, height) = image.dimensions();
    let mut out = ImageBuffer::new(width, height);

    for y in 0..height {
        for x in 0..width {
            let p = image.get_pixel(x, y);
            out.put_pixel(x, height - 1 - y, p);
        }
    }

    out
}

/// Rotate an image clockwise about center by theta radians by choosing
/// the nearest source pixel to the pre-image of a given output pixel.
/// The output image has the same dimensions as the input. Output pixels
/// whose pre-image lies outside the input image are set to default.
// TODO: Is the 'static bound on `I` really required? Can we avoid it?
pub fn rotate_nearest<I: GenericImage + 'static>(
    image: &I,
    center: (f32, f32),
    theta: f32,
    default: I::Pixel)
    -> ImageBuffer<I::Pixel, Vec<<I::Pixel as Pixel>::Subpixel>>
    where I::Pixel: 'static,
          <I::Pixel as Pixel>::Subpixel: 'static {

    let (width, height) = image.dimensions();
    let mut out = ImageBuffer::new(width, height);

    let cos_theta = theta.cos();
    let sin_theta = theta.sin();
    let center_x  = center.0;
    let center_y  = center.1;

    for y in (0..height) {
        let dy = y as f32 - center_y;
        let mut px = center_x + sin_theta * dy - cos_theta * center_x;
        let mut py = center_y + cos_theta * dy + sin_theta * center_x;

        for x in (0..width) {

            let rx = px.round();
            let ry = py.round();

            let x_out_of_bounds = rx < 0f32 || rx >= width as f32;
            let y_out_of_bounds = ry < 0f32 || ry >= height as f32;

            if x_out_of_bounds || y_out_of_bounds {
                out.put_pixel(x, y, default);
            }
            else {
                let source = image.get_pixel(rx as u32, ry as u32);
                out.put_pixel(x, y, source);
            }

            px += cos_theta;
            py -= sin_theta;
        }
    }

    out
}

#[cfg(test)]
mod test {

    use super::{
        rotate90,
        rotate180,
        rotate270,
        rotate_nearest,
        flip_horizontal,
        flip_vertical};
    use buffer::{ImageBuffer,Pixel,GrayImage};
    use color::{Luma};
    use image::{GenericImage};
    use test;

    macro_rules! assert_pixels_eq {
        ($actual:expr, $expected:expr) => ({
            let actual_dim = $actual.dimensions();
            let expected_dim = $expected.dimensions();

            if actual_dim != expected_dim {
                panic!("dimensions do not match. \
                    actual: {:?}, expected: {:?}", actual_dim, expected_dim)
            }

            let diffs = pixel_diffs($actual, $expected);

            if !diffs.is_empty() {
                let mut err = "pixels do not match. ".to_string();

                let diff_messages = diffs
                    .iter()
                    .take(5)
                    .map(|d| format!("\nactual: {:?}, expected {:?} ", d.0, d.1))
                    .collect::<Vec<_>>()
                    .join("");

                err.push_str(&diff_messages);
                panic!(err)
            }
         })
     }

    #[test]
    fn test_rotate90() {
        let image: GrayImage = ImageBuffer::from_raw(3, 2, vec![
            00u8, 01u8, 02u8,
            10u8, 11u8, 12u8]).unwrap();

        let expected: GrayImage = ImageBuffer::from_raw(2, 3, vec![
            10u8, 00u8,
            11u8, 01u8,
            12u8, 02u8]).unwrap();

        assert_pixels_eq!(&rotate90(&image), &expected);
    }

    #[test]
    fn test_rotate180() {
        let image: GrayImage = ImageBuffer::from_raw(3, 2, vec![
            00u8, 01u8, 02u8,
            10u8, 11u8, 12u8]).unwrap();

        let expected: GrayImage = ImageBuffer::from_raw(3, 2, vec![
            12u8, 11u8, 10u8,
            02u8, 01u8, 00u8]).unwrap();

        assert_pixels_eq!(&rotate180(&image), &expected);
    }

    #[test]
    fn test_rotate270() {
        let image: GrayImage = ImageBuffer::from_raw(3, 2, vec![
            00u8, 01u8, 02u8,
            10u8, 11u8, 12u8]).unwrap();

        let expected: GrayImage = ImageBuffer::from_raw(2, 3, vec![
            02u8, 12u8,
            01u8, 11u8,
            00u8, 10u8]).unwrap();

        assert_pixels_eq!(&rotate270(&image), &expected);
    }

    #[test]
    fn test_flip_horizontal() {
        let image: GrayImage = ImageBuffer::from_raw(3, 2, vec![
            00u8, 01u8, 02u8,
            10u8, 11u8, 12u8]).unwrap();

        let expected: GrayImage = ImageBuffer::from_raw(3, 2, vec![
            02u8, 01u8, 00u8,
            12u8, 11u8, 10u8]).unwrap();

        assert_pixels_eq!(&flip_horizontal(&image), &expected);
    }

    #[test]
    fn test_flip_vertical() {
        let image: GrayImage = ImageBuffer::from_raw(3, 2, vec![
            00u8, 01u8, 02u8,
            10u8, 11u8, 12u8]).unwrap();

        let expected: GrayImage = ImageBuffer::from_raw(3, 2, vec![
            10u8, 11u8, 12u8,
            00u8, 01u8, 02u8]).unwrap();

        assert_pixels_eq!(&flip_vertical(&image), &expected);
    }

    #[test]
    fn test_rotate_nearest_zero_radians() {
        let image: GrayImage = ImageBuffer::from_raw(3, 2, vec![
            00u8, 01u8, 02u8,
            10u8, 11u8, 12u8]).unwrap();

        let rotated = rotate_nearest(&image, (1f32, 0f32), 0f32, Luma([99u8]));

        assert_pixels_eq!(&rotated, &image);
    }

    #[test]
    fn text_rotate_nearest_quarter_turn_clockwise() {
        use std::f32;

        let image: GrayImage = ImageBuffer::from_raw(3, 2, vec![
            00u8, 01u8, 02u8,
            10u8, 11u8, 12u8]).unwrap();

        let expected: GrayImage = ImageBuffer::from_raw(3, 2, vec![
            11u8, 01u8, 99u8,
            12u8, 02u8, 99u8]).unwrap();

        let rotated
            = rotate_nearest(&image, (1f32, 0f32), f32::consts::PI / 2f32, Luma([99u8]));

        assert_pixels_eq!(&rotated, &expected);
    }

    #[test]
    fn text_rotate_nearest_half_turn_anticlockwise() {
        use std::f32;

        let image: GrayImage = ImageBuffer::from_raw(3, 2, vec![
            00u8, 01u8, 02u8,
            10u8, 11u8, 12u8]).unwrap();

        let expected: GrayImage = ImageBuffer::from_raw(3, 2, vec![
            12u8, 11u8, 10u8,
            02u8, 01u8, 00u8]).unwrap();

        let rotated
            = rotate_nearest(&image, (1f32, 0.5f32), -f32::consts::PI, Luma([99u8]));

        assert_pixels_eq!(&rotated, &expected);
    }

    #[bench]
    fn bench_rotate_nearest(b: &mut test::Bencher) {
        let mut image: GrayImage = ImageBuffer::new(200, 200);
        for pix in image.pixels_mut() {
            *pix = Luma([15u8]);
        }

        b.iter(|| {
            let rotated = rotate_nearest(&image, (3f32, 3f32), 1f32, Luma([0u8]));
            test::black_box(rotated);
            });
    }

    fn pixel_diffs<I, J, P>(left: &I, right: &J) -> Vec<((u32, u32, P), (u32, u32, P))>
        where I: GenericImage<Pixel=P>,
              J: GenericImage<Pixel=P>,
              P: Pixel + Eq {
        left.pixels()
            .zip(right.pixels())
            .filter(|&(p, q)| p != q)
            .collect::<Vec<_>>()
    }
}
