use image::buffer::ConvertBuffer;
use image::{ImageBuffer, Rgb, Rgba};

use std::fs;
use std::path::PathBuf;
use std::str::FromStr;

#[cfg(feature = "tiff")]
use image::{codecs::tiff::TiffDecoder, ImageReader};

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

#[cfg(feature = "tiff")]
#[test]
fn test_decode_8bit_jpeg_ycbcr() -> Result<(), image::ImageError> {
    const PATH: &str = "tests/images/tiff/testsuite/ycbcr_jpeg_8bit.tif";
    let img_path = PathBuf::from_str(PATH).unwrap();

    let data = fs::read(img_path)?;
    let tiff_decoder = TiffDecoder::new(std::io::Cursor::new(data))?;
    let mut reader = ImageReader::from_decoder(Box::new(tiff_decoder));

    let layout = reader.peek_layout()?;
    assert_eq!(layout.color, image::ColorType::Rgb8);

    let (image, meta) = reader.decode()?;
    let original_type = meta.attributes().original_color_type;

    assert_eq!(original_type, Some(image::ExtendedColorType::YCbCr8));
    assert!(image.as_bytes().iter().any(|&x| x != 0));

    Ok(())
}

#[cfg(feature = "tiff")]
#[test]
fn test_decode_8bit_ycbcr_lzw_bt709() -> Result<(), image::ImageError> {
    const PATH: &str = "tests/images/tiff/testsuite/ycbcr_lzw_bt709.tif";
    let img_path = PathBuf::from_str(PATH).unwrap();

    let data = fs::read(img_path)?;
    let tiff_decoder = TiffDecoder::new(std::io::Cursor::new(data))?;
    let mut reader = ImageReader::from_decoder(Box::new(tiff_decoder));

    let layout = reader.peek_layout()?;
    assert_eq!(layout.color, image::ColorType::Rgb8);

    let (image, meta) = reader.decode()?;
    let original_type = meta.attributes().original_color_type;

    assert_eq!(original_type, Some(image::ExtendedColorType::YCbCr8));
    assert!(image.as_bytes().iter().any(|&x| x != 0));

    Ok(())
}

#[cfg(feature = "tiff")]
#[test]
fn test_decode_8bit_ycbcr_lzw_invalid_coefficients() {
    let img_path = PathBuf::from("tests/images/tiff/testsuite/ycbcr_lzw_broken.tif");
    let data = fs::read(img_path).expect("Test image missing");

    let result = TiffDecoder::new(std::io::Cursor::new(data)).and_then(|decoder| {
        let mut reader = ImageReader::from_decoder(Box::new(decoder));
        reader.peek_layout()
    });

    assert!(result.is_err());
}
