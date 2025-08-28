use std::fs;
use std::path::PathBuf;
use std::str::FromStr;

use image::{codecs::png::PngDecoder, ImageDecoder};

extern crate glob;
extern crate image;

const XMP_PNG_PATH: &str = "tests/images/png/transparency/tp1n3p08_xmp.png";
const EXPECTED_METADATA: &str = "<?xpacket begin='\u{feff}' id='W5M0MpCehiHzreSzNTczkc9d'?>\n<x:xmpmeta xmlns:x='adobe:ns:meta/' x:xmptk='Image::ExifTool 13.25'>\n<rdf:RDF xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns#'>\n\n <rdf:Description rdf:about=''\n  xmlns:dc='http://purl.org/dc/elements/1.1/'>\n  <dc:subject>\n   <rdf:Bag>\n    <rdf:li>sunset, mountains, nature</rdf:li>\n   </rdf:Bag>\n  </dc:subject>\n </rdf:Description>\n</rdf:RDF>\n</x:xmpmeta>\n<?xpacket end='r'?>";

#[test]
fn test_read_xmp_png() -> Result<(), image::ImageError> {
    let img_path = PathBuf::from_str(XMP_PNG_PATH).unwrap();

    let data = fs::read(img_path)?;
    let mut png_decoder = PngDecoder::new(std::io::Cursor::new(data))?;
    let metadata = png_decoder.xmp_metadata()?;
    assert!(metadata.is_some());
    assert_eq!(EXPECTED_METADATA.as_bytes(), metadata.unwrap());

    Ok(())
}
