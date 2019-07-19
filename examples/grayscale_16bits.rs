//! An example of opening an image 16-bits.
extern crate image;
extern crate byteorder;

use std::env;
use std::fs::File;
use std::io::BufReader;

use image::png::PNGDecoder;
use image::ImageBuffer;
use image::Gray16Image;
use image::imageops::thumbnail;

fn main() {
    let file = if env::args().count() == 2 {
        env::args().nth(1).unwrap()
    } else {
        panic!("Please enter a file")
    };

    // DynamicImage does not support 16bits image yet so we need
    // to load directly from the codec
    let r = File::open(file).unwrap();
    let r = BufReader::new(r);
    
    let codec = PNGDecoder::new(r).unwrap();
    let im: Gray16Image = ImageBuffer::from_decoder(codec).unwrap();

    // The dimensions method returns the images width and height
    println!("dimensions {:?}", im.dimensions());

    let thumbnail = thumbnail(&im, 200, 200);
    println!("SAVING TIFF");
    thumbnail.save("somewhereelse.tiff").unwrap();
}
