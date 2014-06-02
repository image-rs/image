extern crate time;
extern crate image;

use std::os;
use std::io::File;
use std::io::MemReader;

use image::JPEGDecoder;
use image::JPEGEncoder;
use image::PNGDecoder;
use image::PNGEncoder;
use image::GIFDecoder;
use image::PPMEncoder;
use image::WebpDecoder;

fn main() {
	let file = if os::args().len() == 2 {
		os::args().as_slice()[1].clone()
	} else {
		fail!("provide a file")
	};

	let mut fin = File::open(&Path::new(file.clone())).unwrap();
	let buf = fin.read_to_end().unwrap();

	let m = MemReader::new(buf);

	let now = time::precise_time_ns();
	let (out, w, h, c) = match file.as_slice().split('.').last() {
		Some("jpg") | Some("jpeg") => {
			let mut j = JPEGDecoder::new(m);

			let a = j.read_image().unwrap();
			let (b, c) = j.dimensions().unwrap();
			let d = j.colortype().unwrap();

			(a, b, c, d)
		}
		Some("png") => {
			let mut p = PNGDecoder::new(m);

			let a = p.decode_image().unwrap();
			let (b, c) = p.dimensions();
			let d = p.color_type();
			let _ = p.palette();

			(a, b, c, d)
		}
		Some("gif") => {
			let mut g = GIFDecoder::new(m);

			//Decode first image only
			//Call again to decode successive images
			//Returns empty array when done
			let a = g.decode_image().unwrap();
			let (b, c) = g.dimensions();
			let d = g.color_type();
			let _ = g.delay();

			(a, b, c, d)
		}
		Some("webp") => {
			let mut w = WebpDecoder::new(m);
			let a = w.decode_image().unwrap();
			let (b, c) = w.dimensions();
			let d = w.color_type();

			(a, b, c, d)

		}
		_ => fail!("unimplemented image extension")
	};
	let after = time::precise_time_ns();

	println!("{0} x {1} pixels", w, h);
	println!("{}", c);
	println!("{} bytes", out.len());
	println!("decoded in {} ms", (after - now) / (1000 * 1000));

	let t = out.clone();
	spawn(proc() {
		let fout = File::create(&Path::new(format!("{}.jpg", os::args().as_slice()[1]))).unwrap();

		let now = time::precise_time_ns();
		let _ = JPEGEncoder::new(fout).encode(t.as_slice(), w, h, c);
		let after = time::precise_time_ns();

		println!("encoded jpeg in {} ms", (after - now) / (1000 * 1000));
	});

	let t = out.clone();
	spawn(proc() {
		let fout = File::create(&Path::new(format!("{}.ppm", os::args().as_slice()[1]))).unwrap();

		let now = time::precise_time_ns();
		let _ = PPMEncoder::new(fout).encode(t.as_slice(), w, h, c);
		let after = time::precise_time_ns();

		println!("encoded ppm in {} ms", (after - now) / (1000 * 1000));
	});

	let t = out.clone();
	spawn(proc() {
		let fout = File::create(&Path::new(format!("{}.png", os::args().as_slice()[1]))).unwrap();

		let now = time::precise_time_ns();
		let _ = PNGEncoder::new(fout).encode(t.as_slice(), w, h, c);
		let after = time::precise_time_ns();

		println!("encoded png in {} ms", (after - now) / (1000 * 1000));
	});
}