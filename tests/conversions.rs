use image::buffer::ConvertBuffer;
use image::{ImageBuffer, Rgb, Rgba};

#[test]
fn test_rgbu8_to_rgbu16() {
    // Create an all white image using Rgb<u16>s for pixel values
    let image_u16 =
        ImageBuffer::from_pixel(2, 2, image::Rgb::<u16>([u16::MAX, u16::MAX, u16::MAX]));

    // Create an all white image using Rgb<u8>s for pixel values and convert it
    // to Rgb<u16>s.
    let image_u8 = ImageBuffer::from_pixel(2, 2, image::Rgb::<u8>([u8::MAX, u8::MAX, u8::MAX]));
    let image_converted: ImageBuffer<Rgb<u16>, _> = image_u8.convert();

    assert_eq!(image_u16, image_converted);
}

#[test]
fn test_rgbau8_to_rgbau16() {
    let image_u16 = ImageBuffer::from_pixel(
        2,
        2,
        image::Rgba::<u16>([u16::MAX, u16::MAX, u16::MAX, u16::MAX]),
    );

    let image_u8 = ImageBuffer::from_pixel(
        2,
        2,
        image::Rgba::<u8>([u8::MAX, u8::MAX, u8::MAX, u8::MAX]),
    );
    let image_converted: ImageBuffer<Rgba<u16>, _> = image_u8.convert();

    assert_eq!(image_u16, image_converted);
}
