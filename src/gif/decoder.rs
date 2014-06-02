use std::io::MemReader;
use std::io::IoResult;

use colortype;
use lzw::LZWReader;

static IMAGEDESCRIPTOR: u8 = 0x2C;
static EXTENSION: u8 = 0x21;
static APPLICATION: u8 = 0xFF;
static GRAPHICCONTROL: u8 = 0xF9;
static COMMENT: u8 = 0xFE;
static TRAILER: u8 = 0x3B;

pub struct GIFDecoder <R> {
	r: R,

	width: u16,
	height: u16,

	global_table: [(u8, u8, u8), ..256],
	local_table: Option<Vec<(u8, u8, u8)>>,

	delay: u16,
	image: Vec<u8>,

	global_backgroud_index: Option<u8>,
	local_transparent_index: Option<u8>,

	have_header: bool
}

impl<R: Reader> GIFDecoder<R> {
	pub fn new(r: R) -> GIFDecoder<R> {
		GIFDecoder {
			r: r,

			width: 0,
			height: 0,

			global_table: [(0u8, 0u8, 0u8), ..256],
			local_table: None,

			delay: 0,
			image: Vec::new(),

			global_backgroud_index: None,
			local_transparent_index: None,

			have_header: false
		}
	}

	fn read_header(&mut self) -> IoResult<()> {
		let signature = try!(self.r.read_exact(3));
		let version   = try!(self.r.read_exact(3));

		assert!(signature.as_slice() == "GIF".as_bytes());
		assert!(version.as_slice() == "87a".as_bytes() ||
			version.as_slice() == "89a".as_bytes());

		Ok(())
	}

	fn read_block(&mut self) -> IoResult<Vec<u8>> {
		let size = try!(self.r.read_u8());
		self.r.read_exact(size as uint)
	}

	fn read_image_data(&mut self) -> IoResult<Vec<u8>> {
		let minimum_code_size = try!(self.r.read_u8());
		assert!(minimum_code_size <= 8);
		let mut data = Vec::new();

		loop {
			let b = try!(self.read_block());
			if b.len() == 0 {
				break
			}

			data = data + b;
		}

		let m = MemReader::new(data);
		let mut lzw = LZWReader::new(m, minimum_code_size);
		let b = lzw.read_to_end().unwrap();

		Ok(b)
    	}

	fn read_image_descriptor(&mut self) -> IoResult<()> {
		let image_left   = try!(self.r.read_le_u16());
		let image_top    = try!(self.r.read_le_u16());
		let image_width  = try!(self.r.read_le_u16());
		let image_height = try!(self.r.read_le_u16());

		let fields = try!(self.r.read_u8());

		let local_table = fields & 80 != 0;
		let interlace   = fields & 40 != 0;
		let table_size  = fields & 7;

		if interlace {
			fail!("interlace not implemented")
		}

		if local_table {
			let n   = 1 << (table_size + 1);
			let buf = try!(self.r.read_exact(3 * n));
			let mut b = Vec::from_elem(n, (0u8, 0u8, 0u8));

			for (i, rgb) in buf.as_slice().chunks(3).enumerate() {
				b.as_mut_slice()[i] = (rgb[0], rgb[1], rgb[2]);
			}

			self.local_table = Some(b);
		}

		let indices = try!(self.read_image_data());

		{
			let trans_index = if self.local_transparent_index.is_some() {
				self.local_transparent_index
			} else {
				self.global_backgroud_index
			};

			let table = if self.local_table.is_some() {
				self.local_table.get_ref().as_slice()
			} else {
				self.global_table.as_slice()
			};

			expand_image(table,
				     indices.as_slice(),
				     image_top as uint,
				     image_left as uint,
				     image_width as uint,
				     image_height as uint,
				     self.width as uint * 3,
				     trans_index,
				     self.image.as_mut_slice());
		}

		self.local_table = None;
		self.local_transparent_index = None;

		Ok(())
	}

	fn read_extension(&mut self) -> IoResult<()> {
		let identifier = try!(self.r.read_u8());

		match identifier {
		    APPLICATION    => try!(self.read_application_extension()),
		    GRAPHICCONTROL => try!(self.read_graphic_control_extension()),
		    COMMENT 	   => try!(self.read_comment_extension()),
		    _ => fail!("unsupported extension")
		}

		Ok(())
	}

	fn read_comment_extension(&mut self) -> IoResult<()> {
		loop {
			let b = try!(self.read_block());
			if b.len() == 0 {
				break
			}
		}

		Ok(())
	}

	fn read_graphic_control_extension(&mut self) -> IoResult<()> {
		let size   = try!(self.r.read_u8());
		assert!(size == 4);

		let fields = try!(self.r.read_u8());
		self.delay = try!(self.r.read_le_u16());
		let trans  = try!(self.r.read_u8());

		if fields & 1 != 0 {
			self.local_transparent_index = Some(trans);
		}

		let _disposal = (fields & 0x1C) >> 2;
		let _term = try!(self.r.read_u8());

		Ok(())
	}

	fn read_application_extension(&mut self) -> IoResult<()> {
		let size = try!(self.r.read_u8());
		let _ = try!(self.r.read_exact(size as uint));

		loop {
			let b = try!(self.read_block());
			if b.len() == 0 {
				break
		    	}
		}

		Ok(())
	}

	fn read_logical_screen_descriptor(&mut self) -> IoResult<()> {
		self.width  = try!(self.r.read_le_u16());
		self.height = try!(self.r.read_le_u16());
		self.image  = Vec::from_elem(self.width as uint * self.height as uint * 3, 0u8);

		let fields = try!(self.r.read_u8());

		let global_table = fields & 0x80 != 0;
		let entries = if global_table { 1 << ((fields & 7) + 1)}
			      else {0};

		if global_table {
			let b = try!(self.r.read_u8());
			self.global_backgroud_index = Some(b);
		}

		let _aspect_ratio = try!(self.r.read_u8());

		let buf = try!(self.r.read_exact(3 * entries));

		for (i, rgb) in buf.as_slice().chunks(3).enumerate() {
			self.global_table.as_mut_slice()[i] = (rgb[0], rgb[1], rgb[2]);
		}

		Ok(())
	}

	pub fn dimensions(&self) -> (u32, u32) {
		(self.width as u32, self.height as u32)
	}

	pub fn color_type(&self) -> colortype::ColorType {
		colortype::RGB(8)
	}

	pub fn delay(&self) -> u16 {
		self.delay
	}

	pub fn decode_image(&mut self) -> IoResult<Vec<u8>> {
		if !self.have_header {
			let _ = try!(self.read_header());
			let _ = try!(self.read_logical_screen_descriptor());

			self.have_header = true;
		}

		loop {
			let block = try!(self.r.read_u8());

			match block {
				EXTENSION => try!(self.read_extension()),
				IMAGEDESCRIPTOR => {
					let _ = try!(self.read_image_descriptor());
					return Ok(self.image.clone())
				}
				TRAILER => break,
				_ => fail!("unknown block {:X}", block)
			}
		}

		Ok(Vec::new())
	}
}

fn expand_image(palete: &[(u8, u8, u8)],
		indices: &[u8],
		y0: uint,
		x0: uint,
		width: uint,
		height: uint,
		stride: uint,
		trans_index: Option<u8>,
		image: &mut [u8]) {

	for y in range(0, height) {
		for x in range(0, width) {
			let index = indices[y * width + x];
			if trans_index == Some(index as u8) {
				continue
			}

			let (r, g, b) = palete[index as uint];

			image[(y0 + y) * stride + x0 * 3 + x * 3 + 0] = r;
			image[(y0 + y) * stride + x0 * 3 + x * 3 + 1] = g;
			image[(y0 + y) * stride + x0 * 3 + x * 3 + 2] = b;
		}
	}
}