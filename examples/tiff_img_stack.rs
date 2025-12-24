//! An example of opening an image.
extern crate image;

use std::env;
use std::fs::File;
use std::path::Path;

use image::ImageStackDecoder;
use image::{codecs::tiff::TiffDecoder, GenericImageView};
use std::io::BufReader;

fn main() {
    let file = if env::args().count() == 2 {
        env::args().nth(1).unwrap()
    } else {
        panic!("Please enter a file")
    };

    // create a buf reader
    let buf_reader = BufReader::new(File::open(Path::new(&file)).unwrap());

    let decoder = TiffDecoder::new(buf_reader).unwrap();

    let img_stack_it = decoder.into_frames();

    for (i, frame) in img_stack_it.enumerate() {
        dbg!(i);
        let frame = frame.unwrap();
        let buffer = frame.buffer();
        let dimensions = buffer.dimensions();
        let color = buffer.color();
        println!("Frame {i}: dimensions: {dimensions:?}, color: {color:?}");
    }
}
