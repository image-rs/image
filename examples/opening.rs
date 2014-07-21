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

    let im = image::open(&Path::new(file.clone())).unwrap();

    println!("dimensions {}", im.dimensions());

    println!("{}", im.color());

    spawn(proc() {
        let mut t = im;
        let fout = File::create(&Path::new(format!("{}.png", os::args().as_slice()[1]))).unwrap();

        t.save(fout, image::PNG);
    });
}