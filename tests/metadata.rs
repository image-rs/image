use std::fs;
use std::path::PathBuf;
use std::str::FromStr;

use image::ImageDecoder;

#[cfg(feature = "png")]
use image::codecs::png::PngDecoder;
#[cfg(feature = "tiff")]
use image::codecs::tiff::TiffDecoder;
#[cfg(feature = "webp")]
use image::codecs::webp::WebPDecoder;

extern crate glob;
extern crate image;

const XMP_PNG_PATH: &str = "tests/images/png/transparency/tp1n3p08_xmp.png";
const EXPECTED_PNG_METADATA: &str = "<?xpacket begin='\u{feff}' id='W5M0MpCehiHzreSzNTczkc9d'?>\n<x:xmpmeta xmlns:x='adobe:ns:meta/' x:xmptk='Image::ExifTool 13.25'>\n<rdf:RDF xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns#'>\n\n <rdf:Description rdf:about=''\n  xmlns:dc='http://purl.org/dc/elements/1.1/'>\n  <dc:subject>\n   <rdf:Bag>\n    <rdf:li>sunset, mountains, nature</rdf:li>\n   </rdf:Bag>\n  </dc:subject>\n </rdf:Description>\n</rdf:RDF>\n</x:xmpmeta>\n<?xpacket end='r'?>";

const XMP_WEBP_PATH: &str = "tests/images/webp/lossless_images/simple_xmp.webp";
const EXPECTED_WEBP_TIFF_METADATA: &str = "<?xpacket begin='\u{feff}' id='W5M0MpCehiHzreSzNTczkc9d'?>\n<x:xmpmeta xmlns:x='adobe:ns:meta/' x:xmptk='Image::ExifTool 13.25'>\n<rdf:RDF xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns#'>\n\n <rdf:Description rdf:about=''\n  xmlns:dc='http://purl.org/dc/elements/1.1/'>\n  <dc:subject>\n   <rdf:Bag>\n    <rdf:li>sunset, mountains, nature</rdf:li>\n   </rdf:Bag>\n  </dc:subject>\n </rdf:Description>\n</rdf:RDF>\n</x:xmpmeta>\n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n                                                                                                    \n<?xpacket end='w'?>";

const XMP_TIFF_PATH: &str = "tests/images/tiff/testsuite/l1_xmp.tiff";

#[test]
#[cfg(feature = "png")]
fn test_read_xmp_png() -> Result<(), image::ImageError> {
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
    let img_path = PathBuf::from_str(XMP_WEBP_PATH).unwrap();

    let data = fs::read(img_path)?;
    let mut webp_decoder = WebPDecoder::new(std::io::Cursor::new(data))?;
    let metadata = webp_decoder.xmp_metadata()?;

    assert!(metadata.is_some());
    assert_eq!(EXPECTED_WEBP_TIFF_METADATA.as_bytes(), metadata.unwrap());

    Ok(())
}

#[test]
#[cfg(feature = "tiff")]
fn test_read_xmp_tiff() -> Result<(), image::ImageError> {
    let img_path = PathBuf::from_str(XMP_TIFF_PATH).unwrap();

    let data = fs::read(img_path)?;
    let mut tiff_decoder = TiffDecoder::new(std::io::Cursor::new(data))?;
    let metadata = tiff_decoder.xmp_metadata()?;
    assert!(metadata.is_some());
    assert_eq!(EXPECTED_WEBP_TIFF_METADATA.as_bytes(), metadata.unwrap());

    Ok(())
}
