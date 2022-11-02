use image::{buffer::Canvas, ColorType, DynamicImage};
#[cfg(feature = "png")]
use std::{fs, io};

#[test]
#[cfg(feature = "png")]
fn read_canvas() {
    use image::codecs::png::PngDecoder;

    let img_path = format!(
        "{}/tests/images/png/interlaced/basi2c08.png",
        env!("CARGO_MANIFEST_DIR")
    );
    let stream = io::BufReader::new(fs::File::open(&img_path).unwrap());
    let decoder = PngDecoder::new(stream).expect("valid png");

    let mut canvas = Canvas::from_decoder(decoder).expect("valid png");
    assert_eq!(canvas.width(), 32);
    assert_eq!(canvas.height(), 32);

    // No extra allocation is happening here for the canvas.
    let stream = io::BufReader::new(fs::File::open(&img_path).unwrap());
    let decoder = PngDecoder::new(stream).expect("valid png");
    canvas.decode(decoder).expect("again, valid png");

    assert_eq!(canvas.width(), 32);
    assert_eq!(canvas.height(), 32);
}

#[test]
fn conversion_to_buffer() {
    let canvas = Canvas::new(ColorType::Rgb8, 32, 32);

    assert!(canvas.as_flat_samples_u8().is_some());
    assert!(canvas.as_flat_samples_u16().is_none());
    assert!(canvas.as_flat_samples_f32().is_none());

    assert!(matches!(canvas.to_dynamic(), DynamicImage::ImageRgb8(_)));

    let buffer_rgb8 = canvas.to_buffer::<image::Rgb<u8>>();
    let buffer_rgb16 = canvas.to_buffer::<image::Rgb<u16>>();
    let compare_rgb16 = DynamicImage::ImageRgb8(buffer_rgb8).into_rgb16();
    assert_eq!(buffer_rgb16, compare_rgb16);

    let canvas_rgb16 = Canvas::from(&buffer_rgb16);
    assert!(canvas_rgb16.as_flat_samples_u16().is_some());
}

#[test]
fn test_dynamic_image() {
    for ct in [
        ColorType::L8,
        ColorType::La8,
        ColorType::Rgb8,
        ColorType::Rgba8,
        ColorType::L16,
        ColorType::La16,
        ColorType::Rgb16,
        ColorType::Rgba16,
        ColorType::Rgb32F,
        ColorType::Rgba32F,
    ] {
        let dynamic = dynamic_image_with_color(ct, 32, 32);

        let canvas = Canvas::new(ct, 32, 32);
        assert_eq!(
            core::mem::discriminant(&canvas.to_dynamic()),
            core::mem::discriminant(&dynamic),
            "{:?}",
            ct
        );

        let canvas = Canvas::from(&dynamic);
        assert_eq!(
            core::mem::discriminant(&canvas.to_dynamic()),
            core::mem::discriminant(&dynamic),
            "{:?}",
            ct
        );
    }
}

/// Creates a dynamic image based off a color type for the buffer.
pub(crate) fn dynamic_image_with_color(color: ColorType, w: u32, h: u32) -> DynamicImage {
    (match color {
        ColorType::L8 => DynamicImage::new_luma8,
        ColorType::La8 => DynamicImage::new_luma_a8,
        ColorType::Rgb8 => DynamicImage::new_rgb8,
        ColorType::Rgba8 => DynamicImage::new_rgba8,
        ColorType::L16 => DynamicImage::new_luma16,
        ColorType::La16 => DynamicImage::new_luma_a16,
        ColorType::Rgb16 => DynamicImage::new_rgb16,
        ColorType::Rgba16 => DynamicImage::new_rgba16,
        ColorType::Rgb32F => DynamicImage::new_rgb32f,
        ColorType::Rgba32F => DynamicImage::new_rgba32f,
        _ => unreachable!(),
    })(w, h)
}
