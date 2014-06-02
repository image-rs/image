use std::slice;
use std::io::IoResult;
use std::io::MemWriter;

use colortype;
use hash::Crc32;

use super::filter::filter;
use super::PNGSIGNATURE;

pub struct PNGEncoder<W> {
	w: W,
	crc: Crc32
}

impl<W: Writer> PNGEncoder<W> {
	pub fn new(w: W) -> PNGEncoder<W> {
		PNGEncoder {
			w: w,
			crc: Crc32::new()
		}
	}

	pub fn encode(&mut self,
		      image: &[u8],
		      width: u32,
		      height: u32,
		      c: colortype::ColorType) -> IoResult<()> {

		let _ = try!(self.write_signature());

		let (bytes, bpp) = build_ihdr(width, height, c);
		let _ = try!(self.write_chunk("IHDR", bytes.as_slice()));

		let compressed_bytes = build_idat(image, bpp, width, height);

		for chunk in compressed_bytes.as_slice().chunks(1024 * 256) {
			let _ = try!(self.write_chunk("IDAT", chunk));
		}

		self.write_chunk("IEND", [])
	}

	fn write_signature(&mut self) -> IoResult<()> {
		self.w.write(PNGSIGNATURE)
	}

	fn write_chunk(&mut self, name: &str, buf: &[u8]) -> IoResult<()> {
		self.crc.reset();
		self.crc.update(name);
		self.crc.update(buf.as_slice());

		let crc = self.crc.checksum();

		let _ = try!(self.w.write_be_u32(buf.len() as u32));
		let _ = try!(self.w.write_str(name));

		let _ = try!(self.w.write(buf));
		let _ = try!(self.w.write_be_u32(crc));

		Ok(())
	}
}

fn build_ihdr(width: u32, height: u32, c: colortype::ColorType) -> (Vec<u8>, uint) {
	let mut m = MemWriter::with_capacity(13);

	let _ = m.write_be_u32(width);
	let _ = m.write_be_u32(height);

	let (colortype, bit_depth) = match c {
		colortype::Grey(1)    => (0, 1),
		colortype::Grey(2)    => (0, 2),
		colortype::Grey(4)    => (0, 4),
		colortype::Grey(8)    => (0, 8),
		colortype::Grey(16)   => (0, 16),
		colortype::RGB(8)     => (2, 8),
		colortype::RGB(16)    => (2, 16),
		colortype::Palette(1) => (3, 1),
		colortype::Palette(2) => (3, 2),
		colortype::Palette(4) => (3, 4),
		colortype::Palette(8) => (3, 8),
		colortype::GreyA(8)   => (4, 8),
		colortype::GreyA(16)  => (4, 16),
		colortype::RGBA(8)    => (6, 8),
		colortype::RGBA(16)   => (6, 16),
		_ => fail!("unsupported color type and bitdepth")
	};

	let _ = m.write_u8(bit_depth);
	let _ = m.write_u8(colortype);

	//compression method, filter method and interlace
	let _ = m.write_u8(0);
	let _ = m.write_u8(0);
	let _ = m.write_u8(0);

	let channels = match colortype {
		0 => 1,
		2 => 3,
		3 => 3,
		4 => 2,
		6 => 4,
		_ => fail!("unknown colour type")
	};

	let bpp = ((channels * bit_depth + 7) / 8) as uint;

	(m.unwrap(), bpp)
}

fn sum_abs_difference(buf: &[u8]) -> i32 {
	buf.iter().fold(0i32, |sum, &b| sum + if b < 128 {b as i32} else {256 - b as i32})
}

fn select_filter(rowlength: uint, bpp: uint, previous: &[u8], current_s: &mut [u8]) -> u8 {
	let mut sum    = sum_abs_difference(current_s.slice_to(rowlength));
	let mut method = 0;

	for (i, current) in current_s.mut_chunks(rowlength).enumerate() {
		filter(i as u8 + 1, bpp, previous, current);

		let this_sum = sum_abs_difference(current);
		if this_sum < sum {
			sum = this_sum;
			method = i as u8 + 1;
		}
	}

	method
}

fn build_idat(image: &[u8], bpp: uint, width: u32, height: u32) -> Vec<u8> {
	use flate::deflate_bytes_zlib;

	let rowlen = bpp * width as uint;

	let mut p = Vec::from_elem(rowlen, 0u8);
	let mut c = Vec::from_elem(4 * rowlen, 0u8);
	let mut b = Vec::from_elem(height as uint + rowlen * height as uint, 0u8);

	for (row, outrow) in image.as_slice().chunks(rowlen).zip(b.as_mut_slice().mut_chunks(1 + rowlen)) {
		for s in c.as_mut_slice().mut_chunks(rowlen) {
			slice::bytes::copy_memory(s, row);
		}

		let filter = select_filter(rowlen, bpp, p.as_slice(), c.as_mut_slice());

		outrow[0]  = filter;
		let out    = outrow.mut_slice_from(1);
		let stride = (filter as uint - 1) * rowlen;

		match filter {
			0 => slice::bytes::copy_memory(out, row),
			_ => slice::bytes::copy_memory(out, c.slice(stride, stride + rowlen)),
		}

		slice::bytes::copy_memory(p.as_mut_slice(), row);
	}

	Vec::from_slice(deflate_bytes_zlib(b.as_slice()).unwrap().as_slice())
}