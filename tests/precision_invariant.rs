//! Tests that operation results are always roughly the same regardless of the
//! precision of the subpixels.

use image::{
    buffer::ConvertBuffer, imageops::GaussianBlurParameters, DynamicImage, ImageBuffer, Rgb,
};

#[test]
fn blur_precision_invariant() {
    for sigma in [0.5, 1.0, 2.0, 7.0] {
        assert_precision_invariant(|img| {
            img.blur_advanced(GaussianBlurParameters::new_from_sigma(sigma))
        });
    }
}

#[test]
fn fast_blur_precision_invariant() {
    for sigma in [0.5, 1.0, 2.0, 7.0, 20.0] {
        assert_precision_invariant(|img| img.fast_blur(sigma));
    }
}

#[test]
fn grayscale_precision_invariant() {
    assert_precision_invariant(|img| img.grayscale());
}

#[test]
#[ignore = "see PR #2860"]
fn huerotate_precision_invariant() {
    assert_precision_invariant(|img| img.huerotate(123));
}

#[test]
#[ignore = "see issue #2850"]
fn brighten_precision_invariant() {
    assert_precision_invariant(|img| img.brighten(123));
}

#[test]
#[ignore = "see issue #2850"]
fn unsharpenprecision_invariant() {
    assert_precision_invariant(|img| img.unsharpen(2.0, 0));
    // with threshold
    assert_precision_invariant(|img| img.unsharpen(2.0, 10));
}

#[test]
fn filter3x3_precision_invariant() {
    assert_precision_invariant(|img| {
        img.filter3x3(&[1.0, 1.0, 1.0, 1.0, -8.0, 1.0, 1.0, 1.0, 1.0])
    });
}

#[test]
fn adjust_contrast_precision_invariant() {
    assert_precision_invariant(|img| img.adjust_contrast(-0.5));
    assert_precision_invariant(|img| img.adjust_contrast(3.0));
}

fn assert_precision_invariant(test: impl Fn(DynamicImage) -> DynamicImage) {
    let img_u8 = ImageBuffer::from_par_fn(256, 256, |x, y| -> Rgb<u8> {
        Rgb([x as u8, (y / 8) as u8, (x * 13 + y) as u8])
    });
    let img_u16: ImageBuffer<Rgb<u16>, Vec<u16>> = img_u8.convert();
    let img_f32: ImageBuffer<Rgb<f32>, Vec<f32>> = img_u8.convert();

    let result_u8 = test(DynamicImage::ImageRgb8(img_u8)).to_rgb8();
    let result_u16_as_u8 = test(DynamicImage::ImageRgb16(img_u16)).to_rgb8();
    let result_f32_as_u8 = test(DynamicImage::ImageRgb32F(img_f32)).to_rgb8();

    assert!(
        approx_eq(result_u8.as_raw(), result_u16_as_u8.as_raw(), 1),
        "Results differ between u8 and u16"
    );
    assert!(
        approx_eq(result_u8.as_raw(), result_f32_as_u8.as_raw(), 1),
        "Results differ between u8 and f32"
    );

    fn approx_eq(a: &[u8], b: &[u8], tolerance: u8) -> bool {
        a.iter()
            .zip(b.iter())
            .all(|(a, b)| a.abs_diff(*b) <= tolerance)
    }
}
