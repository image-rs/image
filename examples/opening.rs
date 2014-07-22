//!An example of opening an image.

extern crate image;

use std::os;
use std::io::File;

use image::GenericImage;

fn main() {
    let file = if os::args().len() == 2 {
        os::args().as_slice()[1].clone()
    } else {
        fail!("Please enter a file")
    };

    //Use the open function to load an image from a PAth.
    //```open``` returns a dynamic image.
    let im = image::open(&Path::new(file.clone())).unwrap();

    //The dimensions method returns the images width and height
    println!("dimensions {}", im.dimensions());

    //The color method returns the image's ColorType
    println!("{}", im.color());

    let fout = File::create(&Path::new(format!("{}.png", os::args().as_slice()[1]))).unwrap();

    //Write the contents of this image to the Writer in PNG format.
    let _ = im.save(fout, image::PNG);
}