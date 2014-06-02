use std::io;
use std::cmp;
use std::str;
use std::slice;
use std::io::IoResult;
use std::io::MemReader;

use colortype;
use hash::Crc32;
use zlib::ZlibDecoder;

use super::filter::unfilter;
use super::PNGSIGNATURE;

#[deriving(PartialEq)]
enum PNGState {
	Start,
	HaveSignature,
	HaveIHDR,
	HavePLTE,
	HaveFirstIDat,
	HaveLastIDat,
	HaveIEND
}

enum PNGError {
	UnknownCompressionMethod,
	UnknownFilterMethod,
	InvalidDimensions,
	InvalidPixelValue,
	InvalidPLTE
}

pub struct PNGDecoder<R> {
	z: ZlibDecoder<IDATReader<R>>,
	crc: Crc32,
	previous: Vec<u8>,
	state: PNGState,

	width: u32,
	height: u32,
	bit_depth: u8,
	colour_type: u8,
	pixel_type: colortype::ColorType,
	palette: Option<Vec<(u8, u8, u8)>>,

	interlace_method: u8,

	chunk_length: u32,
	chunk_type: Vec<u8>,
	bpp: uint,
	rlength: uint
}

impl<R: Reader> PNGDecoder<R> {
	pub fn new(r: R) -> PNGDecoder<R> {
		let idat_reader = IDATReader::new(r);
		PNGDecoder {
			pixel_type: colortype::Grey(1),
			palette: None,

			previous: Vec::new(),
			state: Start,
			z: ZlibDecoder::new(idat_reader),
			crc: Crc32::new(),

			width: 0,
			height: 0,
			bit_depth: 0,
			colour_type: 0,
			interlace_method: 0,

			chunk_length: 0,
			chunk_type: Vec::new(),
			bpp: 0,
			rlength: 0,
		}
	}

	pub fn dimensions(&self) -> (u32, u32) {
		(self.width, self.height)
	}

	pub fn color_type(&self) -> colortype::ColorType {
		self.pixel_type
	}

	pub fn rowlen(&self) -> uint {
		let bits = colortype::bits_per_pixel(self.pixel_type);
		(bits * self.width as uint + 7) / 8
	}

	pub fn read_scanline(&mut self, buf: &mut [u8]) -> IoResult<uint> {
		assert!(buf.len() >= self.previous.len());

		if self.state == Start {
			let _ = try!(self.read_metadata());
		}

		let filter  = try!(self.z.read_byte());

		let mut read = 0;
		while read < self.rlength {
			let r = try!(self.z.read(buf.mut_slice_from(read)));
			read += r;
		}

		unfilter(filter, self.bpp, self.previous.as_slice(), buf.mut_slice_to(self.rlength));
		slice::bytes::copy_memory(self.previous.as_mut_slice(), buf.slice_to(self.rlength));

		if self.palette.is_some() {
			let s = (*self.palette.get_ref()).as_slice();
			expand_palette(buf, s, self.rlength);
		}

		Ok(buf.len())
	}

	pub fn decode_image(&mut self) -> IoResult<Vec<u8>> {
		if self.state == Start {
			let _ = try!(self.read_metadata());
		}

		let row = self.rowlen();
		let mut buf = Vec::from_elem(row * self.height as uint, 0u8);

		for chunk in buf.as_mut_slice().mut_chunks(row) {
			let _len = try!(self.read_scanline(chunk));
		}

		Ok(buf)
	}

	pub fn palette<'a>(&'a self) -> &'a [(u8, u8, u8)] {
		match self.palette {
			Some(ref p) => p.as_slice(),
			None        => &[]
		}
	}

	fn read_signature(&mut self) -> IoResult<bool> {
		let png = try!(self.z.inner().r.read_exact(8));

		Ok(png.as_slice() == PNGSIGNATURE)
	}

	fn parse_ihdr(&mut self, buf: Vec<u8>) -> Result<(), PNGError> {
		self.crc.update(buf.as_slice());
		let mut m = MemReader::new(buf);

		self.width = m.read_be_u32().unwrap();
		self.height = m.read_be_u32().unwrap();

		self.bit_depth = m.read_byte().unwrap();
		self.colour_type = m.read_byte().unwrap();

		self.pixel_type = match (self.colour_type, self.bit_depth) {
			(0, 1)  => colortype::Grey(1),
			(0, 2)  => colortype::Grey(2),
			(0, 4)  => colortype::Grey(4),
			(0, 8)  => colortype::Grey(8),
			(0, 16) => colortype::Grey(16),
			(2, 8)  => colortype::RGB(8),
			(2, 16) => colortype::RGB(16),
			(3, 1)  => colortype::RGB(8),
			(3, 2)  => colortype::RGB(8),
			(3, 4)  => colortype::RGB(8),
			(3, 8)  => colortype::RGB(8),
			(4, 8)  => colortype::GreyA(8),
			(4, 16) => colortype::GreyA(16),
			(6, 8)  => colortype::RGBA(8),
			(6, 16) => colortype::RGBA(16),
			(_, _)  => return Err(InvalidPixelValue)
		};

		let compression_method = m.read_byte().unwrap();
		if compression_method != 0 {
			return Err(UnknownCompressionMethod)
		}

		let filter_method = m.read_byte().unwrap();
		if filter_method != 0 {
			return Err(UnknownFilterMethod)
		}

		self.interlace_method = m.read_byte().unwrap();
		if self.interlace_method != 0 {
			fail!("Interlace not implemented")
		}

		let channels = match self.colour_type {
			0 => 1,
			2 => 3,
			3 => 1,
			4 => 2,
			6 => 4,
			_ => fail!("unknown colour type")
		};

		let bits_per_pixel = channels * self.bit_depth as uint;

		self.rlength = (bits_per_pixel * self.width as uint + 7) / 8;
		self.bpp = (bits_per_pixel + 7) / 8;
		self.previous = Vec::from_elem(self.rlength as uint, 0u8);

		Ok(())
	}

	fn parse_plte(&mut self, buf: Vec<u8>) -> Result<(), PNGError> {
		self.crc.update(buf.as_slice());

		let len = buf.len() / 3;

		if len > 256 || len > (1 << self.bit_depth) || buf.len() % 3 != 0{
			return Err(InvalidPLTE)
		}

		let p = Vec::from_fn(256, |i| {
			if i < len {
				let r = buf.as_slice()[3 * i];
				let g = buf.as_slice()[3 * i + 1];
				let b = buf.as_slice()[3 * i + 2];

				(r, g, b)
			}
			else {
				(0, 0, 0)
			}
		});

		self.palette = Some(p);

		Ok(())
	}

	fn read_metadata(&mut self) -> IoResult<()> {
		assert!(self.state == Start);

		if !try!(self.read_signature()) {
			fail!("Wrong signature")
		}

		self.state = HaveSignature;

		loop {
			let length = try!(self.z.inner().r.read_be_u32());
			let chunk = try!(self.z.inner().r.read_exact(4));

			self.chunk_length = length;
			self.chunk_type   = chunk.clone();

			self.crc.update(chunk);

			let s = {
				let a = str::from_utf8_owned(self.chunk_type.clone());
				if a.is_err() {
					fail!("FIXME")
				}
				a.unwrap()
			};

			match (s.as_slice(), self.state) {
				("IHDR", HaveSignature) => {
					assert!(length == 13);
					let d = try!(self.z.inner().r.read_exact(length as uint));

					let _ = self.parse_ihdr(d);
					self.state = HaveIHDR;
				}

				("PLTE", HaveIHDR) => {
					let d = try!(self.z.inner().r.read_exact(length as uint));
					let _ = self.parse_plte(d);
					self.state = HavePLTE;
				}

				("tRNS", HavePLTE) => {
					assert!(self.palette.is_some());
					fail!("trns unimplemented")
				}

				("IDAT", HaveIHDR) if self.colour_type != 3 => {
					self.state = HaveFirstIDat;
					self.z.inner().set_inital_length(self.chunk_length);
					self.z.inner().crc.update(self.chunk_type.as_slice());

					break;
				}

				("IDAT", HavePLTE) if self.colour_type == 3 => {
					self.state = HaveFirstIDat;
					self.z.inner().set_inital_length(self.chunk_length);
					self.z.inner().crc.update(self.chunk_type.as_slice());

					break;
				}

				_ => {
					let b = try!(self.z.inner().r.read_exact(length as uint));
					self.crc.update(b);
				}
			}

			let chunk_crc = try!(self.z.inner().r.read_be_u32());
			let crc = self.crc.checksum();

			assert!(crc == chunk_crc);

			self.crc.reset();
		}

		Ok(())
	}
}

fn expand_palette(buf: &mut[u8], palette: &[(u8, u8, u8)], entries: uint) {
	assert!(buf.len() == entries * 3);
	let tmp = Vec::from_fn(entries, |i| buf[i]);

	for (chunk, &i) in buf.mut_chunks(3).zip(tmp.iter()) {
		let (r, g, b) = palette[i as uint];
		chunk[0] = r;
		chunk[1] = g;
		chunk[2] = b;
	}
}

pub struct IDATReader<R> {
	pub r: R,
	pub crc: Crc32,

	eof: bool,
	chunk_length: u32,
}

impl<R:Reader> IDATReader<R> {
	pub fn new(r: R) -> IDATReader<R> {
		IDATReader {
			r: r,
			crc: Crc32::new(),
			eof: false,
			chunk_length: 0,
		}
	}

	pub fn set_inital_length(&mut self, len: u32) {
		self.chunk_length = len;
	}
}

impl<R: Reader> Reader for IDATReader<R> {
	fn read(&mut self, buf: &mut [u8]) -> IoResult<uint> {
		if self.eof {
			return Err(io::standard_error(io::EndOfFile))
		}

		let len = buf.len();
		let mut start = 0;

		while start < len {
			while self.chunk_length == 0 {
				let chunk_crc = try!(self.r.read_be_u32());
				let crc = self.crc.checksum();

				assert!(crc == chunk_crc);
				self.crc.reset();

				self.chunk_length = try!(self.r.read_be_u32());

				let v = try!(self.r.read_exact(4));
				self.crc.update(v.as_slice());

				match str::from_utf8(v.as_slice()) {
					Some("IDAT") => (),
					_ 	     => {
						self.eof = true;
						break
					}
				}
			}

			let m = cmp::min(len - start, self.chunk_length as uint);

			let slice = buf.mut_slice(start, start + m);
			let r = try!(self.r.read(slice));

			start += r;

			self.chunk_length -= r as u32;
			self.crc.update(slice.as_slice());
		}

		Ok(start)
	}
}