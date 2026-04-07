use image::buffer::ConvertBuffer;
use image::{ImageBuffer, Rgb, Rgba};

use std::fs;
use std::path::PathBuf;
use std::str::FromStr;

use image::ImageDecoder;

#[cfg(feature = "tiff")]
use image::codecs::tiff::TiffDecoder;

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

    let (w, h) = tiff_decoder.dimensions();
    let original_type = tiff_decoder.original_color_type();
    let target_type = tiff_decoder.color_type();
    let total_bytes = tiff_decoder.total_bytes() as usize;

    assert_eq!(original_type, image::ExtendedColorType::YCbCr8);
    assert_eq!(target_type, image::ColorType::Rgb8);

    let mut buffer = vec![0u8; total_bytes];
    tiff_decoder.read_image(&mut buffer)?;

    assert_eq!(buffer.len(), (w * h * 3) as usize);
    assert!(buffer.iter().any(|&x| x != 0));

    Ok(())
}

#[cfg(feature = "tiff")]
#[test]
fn test_decode_8bit_ycbcr_lzw_bt709() -> Result<(), image::ImageError> {
    const PATH: &str = "tests/images/tiff/testsuite/ycbcr_lzw_bt709.tif";
    let img_path = PathBuf::from_str(PATH).unwrap();

    let data = fs::read(img_path)?;
    let tiff_decoder = TiffDecoder::new(std::io::Cursor::new(data))?;

    let (w, h) = tiff_decoder.dimensions();

    assert_eq!(
        tiff_decoder.original_color_type(),
        image::ExtendedColorType::YCbCr8
    );
    assert_eq!(tiff_decoder.color_type(), image::ColorType::Rgb8);

    let mut buffer = vec![0u8; tiff_decoder.total_bytes() as usize];
    tiff_decoder.read_image(&mut buffer)?;

    assert_eq!(buffer.len(), (w * h * 3) as usize);
    assert!(buffer.iter().any(|&x| x != 0));

    Ok(())
}

#[cfg(feature = "tiff")]
#[test]
fn test_decode_8bit_ycbcr_lzw_invalid_coefficients() {
    let img_path = PathBuf::from("tests/images/tiff/testsuite/ycbcr_lzw_broken.tif");
    let data = fs::read(img_path).expect("Test image missing");

    let result = TiffDecoder::new(std::io::Cursor::new(data));
    assert!(result.is_err());
}

#[cfg(feature = "tiff")]
#[test]
fn test_decode_8bit_cmyk() -> Result<(), image::ImageError> {
    let img_path = PathBuf::from("tests/images/tiff/testsuite/cmyk_u8_edge_case.tif");
    let data = fs::read(img_path).expect("Test image missing");

    let tiff_decoder = TiffDecoder::new(std::io::Cursor::new(data))?;

    assert_eq!(tiff_decoder.color_type(), image::ColorType::Rgb8);

    let mut buffer = vec![0u8; tiff_decoder.total_bytes() as usize];
    tiff_decoder.read_image(&mut buffer)?;
    assert_eq!(buffer, vec![190, 190, 190]);

    Ok(())
}

#[cfg(feature = "tiff")]
#[test]
fn test_decode_8bit_cmyk_truncation() -> Result<(), image::ImageError> {
    let img_path = PathBuf::from("tests/images/tiff/testsuite/cmyk_u8_trunc_case.tif");
    let data = fs::read(img_path).expect("Test image missing");

    let tiff_decoder = TiffDecoder::new(std::io::Cursor::new(data))?;

    assert_eq!(tiff_decoder.color_type(), image::ColorType::Rgb8);

    let mut buffer = vec![0u8; tiff_decoder.total_bytes() as usize];
    tiff_decoder.read_image(&mut buffer)?;
    assert_eq!(buffer, vec![126, 126, 126]);

    Ok(())
}
