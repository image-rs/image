//! An example of opening an image 16-bits.
extern crate image;
extern crate byteorder;

use std::env;
use std::fs::File;
use std::io::{Cursor, BufReader};
use std::path::Path;

use image::png::PNGDecoder;
use image::ImageBuffer;
use image::ImageDecoder;
use byteorder::{ReadBytesExt, BigEndian};

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
    
    let mut codec = PNGDecoder::new(r).unwrap();
    let color = codec.colortype();
    let (w, h) = codec.dimensions();
    let size = (w as usize) * (h as usize);
    let buf = codec.read_image().unwrap();
    let (w, h) = (w as u32, h as u32);

    let mut buf16 = Vec::with_capacity(size);
    let mut rdr = Cursor::new(buf);
    for _ in 0..size {
        buf16.push(rdr.read_u16::<BigEndian>().unwrap());
    }
    
    let im = ImageBuffer::from_raw(w, h, buf16).unwrap();
    // The dimensions method returns the images width and height
    println!("dimensions {:?}", im.dimensions());

    // The color method returns the image's ColorType
    //println!("{:?}", im.color());

    //let fout = &mut File::create(&Path::new(&format!("{}.png", file))).unwrap();

    // Write the contents of this image to the Writer in PNG format.
    //im.write_to(fout, image::PNG).unwrap();
}
