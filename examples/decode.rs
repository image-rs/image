//! An example of opening an image.
use std::env;
use std::path::Path;

fn main() {
    let from = if env::args_os().count() == 2 {
        env::args_os().nth(1).unwrap()
    } else {
        println!("Please enter the path to an image file.");
        std::process::exit(1);
    };

    // Use the open function to load an image from a Path.
    // ```open``` returns a dynamic image.
    let im = image::open(Path::new(&from)).unwrap();
    println!(
        "size: {} x {} ({} bytes), type {:?}",
        im.width(),
        im.height(),
        im.as_bytes().len(),
        im.color(),
    );
    println!("color space: {:?}", im.color_space());
}
