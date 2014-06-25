extern crate image;

use std::os;
use std::io::File;

use image::Image;
use image::{PNG, JPEG};

fn main() {
	let file = if os::args().len() == 2 {
		os::args().as_slice()[1].clone()
	} else {
		fail!("Please enter a file")
	};

	let im = Image::open(&Path::new(file.clone())).unwrap();

	println!("dimensions {}", im.dimensions());
	println!("{}", im.colortype());
	println!("{} bytes", im.raw_pixels().len());

	let t = im.clone();
	spawn(proc() {
		let fout = File::create(&Path::new(format!("{}.jpg", os::args().as_slice()[1]))).unwrap();
		let _    = t.save(fout, JPEG);
	});

	let t = im.clone();
	spawn(proc() {
		for (i, g) in t.tiles(100, 100).enumerate() {
			let fout = File::create(&Path::new(format!("{0}_{1}.png", os::args().as_slice()[1], i))).unwrap();

			let h = g.to_image();
			let _ = h.save(fout, PNG);
		}
	});
}