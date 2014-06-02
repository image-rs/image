use std::io::IoResult;
use std::io::MemReader;

use colortype;
use vp8::VP8Decoder;

pub struct WebpDecoder<R> {
	r: R,
	width: u16,
	height: u16,
}

impl<R: Reader> WebpDecoder<R> {
	pub fn new(r: R) -> WebpDecoder<R> {
		WebpDecoder {
			r: r,
			width: 0,
			height: 0,
		}
	}

	fn read_riff_header(&mut self) -> IoResult<u32> {
		let riff = try!(self.r.read_exact(4));
		let size = try!(self.r.read_le_u32());
		let webp = try!(self.r.read_exact(4));

		assert!(riff.as_slice() == "RIFF".as_bytes());
		assert!(webp.as_slice() == "WEBP".as_bytes());

		Ok(size)
	}

	fn read_vp8_header(&mut self) -> IoResult<()> {
		let vp8 = try!(self.r.read_exact(4));
		assert!(vp8.as_slice() == "VP8 ".as_bytes());

		let _len = try!(self.r.read_le_u32());

		Ok(())
	}

	pub fn dimensions(&self) -> (u32, u32) {
		(self.width as u32, self.height as u32)
	}

	pub fn color_type(&self) -> colortype::ColorType {
		colortype::Grey(8)
	}

	pub fn decode_image(&mut self) -> IoResult<Vec<u8>> {
		let _size = try!(self.read_riff_header());
		let _ = try!(self.read_vp8_header());

		let framedata = try!(self.r.read_to_end());
		let m = MemReader::new(framedata);

		let mut v = VP8Decoder::new(m);
		let frame = try!(v.decode_frame());

		self.width = frame.width;
		self.height = frame.height;

		Ok(frame.ybuf.clone())
	}
}