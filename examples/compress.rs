//! Example showing JPEG compression.
extern crate image;

use std::env;
use std::fs::File;
use std::path::Path;

use image::GenericImage;

fn main() {
    let file = env::args().nth(1).expect("Please enter a file");
    let qual = env::args().nth(2).expect("Please enter a quality");
    let qual: u8 = qual.parse().expect(&format!("Invalid quality: {}", qual));

    // Use the open function to load an image from a Path.
    // ```open``` returns a dynamic image.
    let im = image::open(&Path::new(&file)).unwrap();

    // Create a new JPEG file with the same file name.
    let ref mut fout = File::create(format!("{}.jpg", file)).unwrap();

    // Write the image contents to the Writer in a compressed JPEG format.
    let _ = im.save_jpeg_with_quality(fout, qual).unwrap();
}
