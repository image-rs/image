//! Encoding of PPM Images

use std::io::IoResult;
use std::num;
use std::num::ToStrRadix;

use colortype;
use colortype::{Grey, Palette, GreyA, RGB, RGBA};

pub struct PPMEncoder<W> {
	w: W
}

impl<W: Writer> PPMEncoder<W> {
	pub fn new(w: W) -> PPMEncoder<W> {
		PPMEncoder {w: w}
	}

	pub fn encode(&mut self, im: &[u8], w: u32, h: u32, c: colortype::ColorType) -> IoResult<()> {
		let _ = try!(self.write_magic_number());
		let _ = try!(self.write_metadata(w, h, c));

		self.write_image(im, c, w, h)
	}

	fn write_magic_number(&mut self) -> IoResult<()> {
		self.w.write_str("P6\n")
	}

	fn write_metadata(&mut self, width: u32, height: u32, pixel_type: colortype::ColorType) -> IoResult<()> {
		let w = width.to_str_radix(10);
		let h = height.to_str_radix(10);
		let m = max_pixel_value(pixel_type);

		self.w.write_str(format!("{0} {1}\n{2}\n", w, h, m).as_slice())
	}

	fn write_image(&mut self, buf: &[u8], pixel_type: colortype::ColorType, width: u32, height: u32) -> IoResult<()> {
		assert!(buf.len() > 0);
		match pixel_type {
			Grey(8) => {
				for i in range(0, (width * height) as uint) {
					let _ = try!(self.w.write_u8(buf[i]));
					let _ = try!(self.w.write_u8(buf[i]));
					let _ = try!(self.w.write_u8(buf[i]));
				}
			}
			RGB(8)  => try!(self.w.write(buf)),
			RGB(16) => try!(self.w.write(buf)),
			RGBA(8) => {
				for x in buf.chunks(4) {
					let _ = try!(self.w.write_u8(x[0]));
					let _ = try!(self.w.write_u8(x[1]));
					let _ = try!(self.w.write_u8(x[2]));
				}
			}

			a => fail!(format!("not implemented: {}", a))
		}

		Ok(())
	}
}

fn max_pixel_value(pixel_type: colortype::ColorType) -> u16 {
	let max = match pixel_type {
		Grey(n)    => num::pow(2, n as uint) - 1,
		RGB(n)     => num::pow(2, n as uint) - 1,
		Palette(n) => num::pow(2, n as uint) - 1,
		GreyA(n)   => num::pow(2, n as uint) - 1,
		RGBA(n)    => num::pow(2, n as uint) - 1
	};

	max as u16
}