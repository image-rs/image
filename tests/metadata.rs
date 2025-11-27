use std::fs;
use std::path::PathBuf;
use std::str::FromStr;

use image::ImageDecoder;

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
#[cfg(feature = "png")]
fn test_read_xmp_image_magick_png() -> Result<(), image::ImageError> {
    const XMP_PNG_PATH: &str = "tests/images/png/xmp_image_magick.png";
    const EXPECTED_PNG_METADATA: &str = r#"<?xpacket begin="﻿" id="W5M0MpCehiHzreSzNTczkc9d"?> <x:xmpmeta xmlns:x="adobe:ns:meta/" x:xmptk="XMP Core 5.1.2"> <rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"> <rdf:Description rdf:about=""/> </rdf:RDF> </x:xmpmeta>   <?xpacket end="w"?>"#;

    let img_path = PathBuf::from_str(XMP_PNG_PATH).unwrap();

    let data = fs::read(img_path)?;
    let mut png_decoder = PngDecoder::new(std::io::Cursor::new(data))?;
    let metadata = png_decoder.xmp_metadata()?;
    assert!(metadata.is_some());
    assert_eq!(EXPECTED_PNG_METADATA.as_bytes(), metadata.unwrap());

    Ok(())
}

#[test]
#[cfg(feature = "png")]
fn test_read_iptc_image_magick_png() -> Result<(), image::ImageError> {
    const XMP_PNG_PATH: &str = "tests/images/png/iptc_image_magick.png";
    const EXPECTED_PNG_METADATA: &[u8] = include_bytes!("images/png/iptc_image_magick.bin");

    let img_path = PathBuf::from_str(XMP_PNG_PATH).unwrap();

    let data = fs::read(img_path)?;
    let mut png_decoder = PngDecoder::new(std::io::Cursor::new(data))?;
    let metadata = png_decoder.iptc_metadata()?;
    assert!(metadata.is_some());
    assert_eq!(EXPECTED_PNG_METADATA, metadata.unwrap());

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
#[cfg(feature = "png")]
fn test_read_iptc_png() -> Result<(), image::ImageError> {
    const IPTC_PNG_PATH: &str = "tests/images/png/iptc.png";
    const EXPECTED_METADATA: &[u8] = &[
        56, 66, 73, 77, 4, 4, 0, 0, 0, 0, 0, 49, 28, 2, 110, 0, 24, 65, 73, 45, 71, 101, 110, 101,
        114, 97, 116, 101, 100, 32, 119, 105, 116, 104, 32, 71, 111, 111, 103, 108, 101, 28, 2, 90,
        0, 8, 75, 105, 110, 103, 115, 116, 111, 110, 28, 2, 0, 0, 2, 0, 4, 0, 56, 66, 73, 77, 4,
        37, 0, 0, 0, 0, 0, 16, 67, 89, 196, 70, 206, 234, 16, 4, 50, 89, 230, 125, 147, 191, 230,
        81,
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
    const EXPECTED_METADATA: &[u8] = include_bytes!("images/jpg/expected_xmp.bin");
    let img_path = PathBuf::from_str(IMG_PATH).unwrap();

    let data = fs::read(img_path)?;
    let mut tiff_decoder = JpegDecoder::new(std::io::Cursor::new(data))?;
    let metadata = tiff_decoder.xmp_metadata()?;
    assert!(metadata.is_some());
    assert_eq!(EXPECTED_METADATA, &metadata.unwrap());

    Ok(())
}
