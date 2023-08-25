//! An example of opening an image.
extern crate image;

use std::env;
use std::fs::File;
use std::path::Path;

use image::{GenericImageView, ImageFormat};

fn main() {
    let file = if env::args().count() == 2 {
        env::args().nth(1).unwrap()
    } else {
        panic!("Please enter a file")
    };

    // Use the open function to load an image from a Path.
    // ```open``` returns a dynamic image.
    let im = image::open(Path::new(&file)).unwrap();

    // The dimensions method returns the images width and height
    println!("dimensions {:?}", im.dimensions());

    // The color method returns the image's ColorType
    println!("{:?}", im.color());

    let fout = &mut File::create(Path::new(&format!("{}.png", file))).unwrap();

    // Write the contents of this image to the Writer in PNG format.
    im.write_to(fout, ImageFormat::Png).unwrap();
}
