use std::fs;
use std::path::PathBuf;
use std::str::FromStr;

use image::ImageDecoder;

#[cfg(all(feature = "avif", feature = "avif-native"))]
use image::codecs::avif::AvifDecoder;
#[cfg(feature = "jpeg")]
use image::codecs::jpeg::JpegDecoder;
#[cfg(feature = "png")]
use image::codecs::png::PngDecoder;
#[cfg(feature = "tiff")]
use image::codecs::tiff::TiffDecoder;
#[cfg(feature = "webp")]
use image::codecs::webp::WebPDecoder;

extern crate glob;
extern crate image;

#[test]
#[cfg(feature = "png")]
fn test_read_xmp_png() -> Result<(), image::ImageError> {
    const XMP_PNG_PATH: &str = "tests/images/png/transparency/tp1n3p08_xmp.png";
    const EXPECTED_PNG_METADATA: &str = "<?xpacket begin='\u{feff}' id='W5M0MpCehiHzreSzNTczkc9d'?>\n<x:xmpmeta xmlns:x='adobe:ns:meta/' x:xmptk='Image::ExifTool 13.25'>\n<rdf:RDF xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns#'>\n\n <rdf:Description rdf:about=''\n  xmlns:dc='http://purl.org/dc/elements/1.1/'>\n  <dc:subject>\n   <rdf:Bag>\n    <rdf:li>sunset, mountains, nature</rdf:li>\n   </rdf:Bag>\n  </dc:subject>\n </rdf:Description>\n</rdf:RDF>\n</x:xmpmeta>\n<?xpacket end='r'?>";

    let img_path = PathBuf::from_str(XMP_PNG_PATH).unwrap();

    let data = fs::read(img_path)?;
    let mut png_decoder = PngDecoder::new(std::io::Cursor::new(data))?;
    let metadata = png_decoder.xmp_metadata()?;
    assert!(metadata.is_some());
    assert_eq!(EXPECTED_PNG_METADATA.as_bytes(), metadata.unwrap());

    Ok(())
}

#[test]
#[cfg(feature = "webp")]
fn test_read_xmp_webp() -> Result<(), image::ImageError> {
    const XMP_WEBP_PATH: &str = "tests/images/webp/lossless_images/simple_xmp.webp";
    const EXPECTED_METADATA: &str = "<?xpacket begin='\u{feff}' id='W5M0MpCehiHzreSzNTczkc9d'?>\n<x:xmpmeta xmlns:x='adobe:ns:meta/' x:xmptk='Image::ExifTool 13.25'>\n<rdf:RDF xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns#'>\n\n <rdf:Description rdf:about=''\n  xmlns:dc='http://purl.org/dc/elements/1.1/'>\n  <dc:subject>\n   <rdf:Bag>\n    <rdf:li>sunset, mountains, nature</rdf:li>\n   </rdf:Bag>\n  </dc:subject>\n </rdf:Description>\n</rdf:RDF>\n</x:xmpmeta>\n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n<?xpacket end='w'?>";
    let img_path = PathBuf::from_str(XMP_WEBP_PATH).unwrap();

    let data = fs::read(img_path)?;
    let mut webp_decoder = WebPDecoder::new(std::io::Cursor::new(data))?;
    let metadata = webp_decoder.xmp_metadata()?;

    assert!(metadata.is_some());
    assert_eq!(EXPECTED_METADATA.as_bytes(), metadata.unwrap());

    Ok(())
}

#[test]
#[cfg(feature = "tiff")]
fn test_read_xmp_tiff() -> Result<(), image::ImageError> {
    const XMP_TIFF_PATH: &str = "tests/images/tiff/testsuite/l1_xmp.tiff";
    const EXPECTED_METADATA: &str = "<?xpacket begin='\u{feff}' id='W5M0MpCehiHzreSzNTczkc9d'?>\n<x:xmpmeta xmlns:x='adobe:ns:meta/' x:xmptk='Image::ExifTool 13.25'>\n<rdf:RDF xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns#'>\n\n <rdf:Description rdf:about=''\n  xmlns:dc='http://purl.org/dc/elements/1.1/'>\n  <dc:subject>\n   <rdf:Bag>\n    <rdf:li>sunset, mountains, nature</rdf:li>\n   </rdf:Bag>\n  </dc:subject>\n </rdf:Description>\n</rdf:RDF>\n</x:xmpmeta>\n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n<?xpacket end='w'?>";
    let img_path = PathBuf::from_str(XMP_TIFF_PATH).unwrap();

    let data = fs::read(img_path)?;
    let mut tiff_decoder = TiffDecoder::new(std::io::Cursor::new(data))?;
    let metadata = tiff_decoder.xmp_metadata()?;
    assert!(metadata.is_some());
    assert_eq!(EXPECTED_METADATA.as_bytes(), metadata.unwrap());

    Ok(())
}

#[test]
#[cfg(feature = "jpeg")]
fn test_read_iptc_jpeg() -> Result<(), image::ImageError> {
    const IPTC_JPG_PATH: &str = "tests/images/jpg/iptc.jpg";
    const EXPECTED_METADATA: &[u8] = &[
        56, 66, 73, 77, 4, 4, 0, 0, 0, 0, 0, 99, 28, 2, 90, 0, 8, 66, 117, 100, 97, 112, 101, 115,
        116, 28, 2, 101, 0, 7, 72, 117, 110, 103, 97, 114, 121, 28, 2, 25, 0, 3, 72, 118, 75, 28,
        2, 25, 0, 4, 50, 48, 48, 54, 28, 2, 25, 0, 6, 115, 117, 109, 109, 101, 114, 28, 2, 25, 0,
        4, 74, 117, 108, 121, 28, 2, 25, 0, 7, 104, 111, 108, 105, 100, 97, 121, 28, 2, 25, 0, 7,
        72, 117, 110, 103, 97, 114, 121, 28, 2, 25, 0, 8, 66, 117, 100, 97, 112, 101, 115, 116, 0,
    ];
    let img_path = PathBuf::from_str(IPTC_JPG_PATH).unwrap();

    let data = fs::read(img_path)?;
    let mut jpeg_decoder = JpegDecoder::new(std::io::Cursor::new(data))?;
    let metadata = jpeg_decoder.iptc_metadata()?;
    assert!(metadata.is_some());
    assert_eq!(EXPECTED_METADATA, metadata.unwrap());

    Ok(())
}

#[test]
#[cfg(feature = "png")]
fn test_read_iptc_png() -> Result<(), image::ImageError> {
    const IPTC_PNG_PATH: &str = "tests/images/png/iptc.png";
    const EXPECTED_METADATA: &[u8] = &[
        10, 73, 80, 84, 67, 32, 112, 114, 111, 102, 105, 108, 101, 10, 32, 32, 32, 32, 32, 32, 57,
        48, 10, 51, 56, 52, 50, 52, 57, 52, 100, 48, 52, 48, 52, 48, 48, 48, 48, 48, 48, 48, 48,
        48, 48, 51, 49, 49, 99, 48, 50, 54, 101, 48, 48, 49, 56, 52, 49, 52, 57, 50, 100, 52, 55,
        54, 53, 54, 101, 54, 53, 55, 50, 54, 49, 55, 52, 54, 53, 54, 52, 50, 48, 55, 55, 54, 57,
        55, 52, 54, 56, 50, 48, 52, 55, 10, 54, 102, 54, 102, 54, 55, 54, 99, 54, 53, 49, 99, 48,
        50, 53, 97, 48, 48, 48, 56, 52, 98, 54, 57, 54, 101, 54, 55, 55, 51, 55, 52, 54, 102, 54,
        101, 49, 99, 48, 50, 48, 48, 48, 48, 48, 50, 48, 48, 48, 52, 48, 48, 51, 56, 52, 50, 52,
        57, 52, 100, 48, 52, 50, 53, 48, 48, 48, 48, 48, 48, 48, 48, 10, 48, 48, 49, 48, 52, 51,
        53, 57, 99, 52, 52, 54, 99, 101, 101, 97, 49, 48, 48, 52, 51, 50, 53, 57, 101, 54, 55, 100,
        57, 51, 98, 102, 101, 54, 53, 49, 10,
    ];

    let img_path = PathBuf::from_str(IPTC_PNG_PATH).unwrap();

    let data = fs::read(img_path)?;
    let mut png_decoder = PngDecoder::new(std::io::Cursor::new(data))?;
    let metadata = png_decoder.iptc_metadata()?;
    assert!(metadata.is_some());
    assert_eq!(EXPECTED_METADATA, metadata.unwrap());

    Ok(())
}

#[test]
#[cfg(feature = "jpeg")]
fn test_read_xmp_jpeg() -> Result<(), image::ImageError> {
    const IMG_PATH: &str = "tests/images/jpg/exif-xmp-metadata.jpg";
    const EXPECTED_METADATA: &[u8] = include_bytes!("assets/jpg/expected_xmp.bin");
    let img_path = PathBuf::from_str(IMG_PATH).unwrap();

    let data = fs::read(img_path)?;
    let mut tiff_decoder = JpegDecoder::new(std::io::Cursor::new(data))?;
    let metadata = tiff_decoder.xmp_metadata()?;
    assert!(metadata.is_some());
    assert_eq!(EXPECTED_METADATA, &metadata.unwrap());

    Ok(())
}

#[test]
#[cfg(all(feature = "avif", feature = "avif-native"))]
fn test_read_avif_compatible_brands() -> Result<(), image::ImageError> {
    use image::{DynamicImage, RgbImage};
    use std::io::Cursor;
    // See details at:
    // https://source.chromium.org/chromium/chromium/src/+/823926119c0be100cc814c5e04bc4aa672785265:third_party/blink/renderer/platform/image-decoders/image_decoder_test.cc;l=349-366
    // This test verifies the edge case of that the decoder can handle AVIF files with major_brand "mif1" or the other

    // Generate a minimal 1x1 pixel AVIF image
    let image = DynamicImage::ImageRgb8(RgbImage::from_pixel(1, 1, image::Rgb([255, 0, 0])));
    let mut data = Vec::new();
    image.write_to(&mut Cursor::new(&mut data), image::ImageFormat::Avif)?;

    assert_eq!(&data[4..8], b"ftyp");

    // Modify the major_brand to "mif1" at offset 8-11 in the ftyp box
    data[8..12].copy_from_slice(b"mif1");

    // Verify decoder can successfully parse the file even though major_brand is "mif1"
    let decoder = AvifDecoder::new(Cursor::new(&data));
    assert!(decoder.is_ok());

    Ok(())
}
