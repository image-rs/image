extern crate crc32fast;
extern crate glob;
extern crate image;

use std::fs;
use std::io;
use std::path::PathBuf;


#[cfg(feature = "png")]
use image::png::PNGDecoder;

use image::{ImageBuffer, Gray16Image, ImageResult};

const BASE_PATH: [&str; 2] = [".", "tests"];
const IMAGE_DIR: &str = "images";
const OUTPUT_DIR: &str = "output";

/// Read a 16 bits grayscale image and save it. It should be
/// a Gray16Image (ImageBuf<Luma<16>, Vec<16>>)
#[cfg(feature = "png")]
#[test]
fn decode_encode_16bits_png() {
    
    // First, read the image.
    let base: PathBuf = BASE_PATH.iter().collect();
    let mut read_path = base.clone();
    read_path.push(IMAGE_DIR);
    read_path.push("16bits");
    read_path.push("gray.png");
    let rdr = fs::File::open(read_path).unwrap();
    let rdr = io::BufReader::new(rdr);

    // need to use the codec directly as DynamicImage does not 
    // support 16-bits yet.
    let codec = PNGDecoder::new(rdr).unwrap();
    let im: ImageResult<Gray16Image> = Gray16Image::from_decoder(codec);
    assert!(im.is_ok());
    let im = im.unwrap();

    // Save it to output folder.
    let mut output_path = base.clone();
    output_path.push(OUTPUT_DIR);
    output_path.push("16bits");

    fs::create_dir_all(&output_path).unwrap();

    output_path.push("gray.png");
    let res = im.save(output_path);
    assert!(res.is_ok());
}

/// Try to load 8bits as 16-bits. 8-bits will be converted to 16-bits on load
#[cfg(feature = "png")]
#[test]
fn decode_16bits_when_8bits_source() {
     // First, read the image.
    let base: PathBuf = BASE_PATH.iter().collect();
    let mut read_path = base.clone();
    read_path.push(IMAGE_DIR);
    read_path.push("16bits");
    read_path.push("8bits.png");
    let rdr = fs::File::open(read_path).unwrap();
    let rdr = io::BufReader::new(rdr);

    // need to use the codec directly as DynamicImage does not 
    // support 16-bits yet.
    let codec = PNGDecoder::new(rdr).unwrap();
    let im: ImageResult<Gray16Image> = ImageBuffer::from_decoder(codec);
    assert!(im.is_err());
}
