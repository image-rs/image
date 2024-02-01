//! An example of opening an image.
extern crate image;

use std::env;
use std::path::Path;

fn main() {
    let (from, into) = if env::args_os().count() == 3 {
        (
            env::args_os().nth(1).unwrap(),
            env::args_os().nth(2).unwrap(),
        )
    } else {
        println!("Please enter a from and into path.");
        std::process::exit(1);
    };

    // Use the open function to load an image from a Path.
    // ```open``` returns a dynamic image.
    let im = image::open(Path::new(&from)).unwrap();
    // Write the contents of this image using extension guessing.
    im.save(Path::new(&into)).unwrap();
}
