use std::slice;
use std::io::MemWriter;
use std::io::IoResult;
use std::cmp;
use std::iter::range_step;
use std::default::Default;

use collections::smallintmap::SmallIntMap;

use colortype;
use transform;

//Markers
//Baseline DCT
static SOF0: u8 = 0xC0;
//Progressive DCT
static SOF2: u8 = 0xC2;
//Huffman Tables
static DHT: u8 = 0xC4;
//Restart Interval start and End (standalone)
static RST0: u8 = 0xD0;
static RST7: u8 = 0xD7;
//Start of Image (standalone)
static SOI: u8 = 0xD8;
//End of image (standalone)
static EOI: u8 = 0xD9;
//Start of Scan
static SOS: u8 = 0xDA;
//Quantization Tables
static DQT: u8 = 0xDB;
//Number of lines
static DNL: u8 = 0xDC;
//Restart Interval
static DRI: u8 = 0xDD;
//Application segments start and end
static APP0: u8 = 0xE0;
static APPF: u8 = 0xEF;
//Comment
static COM: u8 = 0xFE;
//Reserved
static TEM: u8 = 0x01;

static UNZIGZAG: [u8, ..64] = [
	 0,  1,  8, 16,  9,  2,  3, 10,
	17, 24, 32, 25, 18, 11,  4,  5,
	12, 19, 26, 33, 40, 48, 41, 34,
	27, 20, 13,  6,  7, 14, 21, 28,
	35, 42, 49, 56, 57, 50, 43, 36,
	29, 22, 15, 23, 30, 37, 44, 51,
	58, 59, 52, 45, 38, 31, 39, 46,
	53, 60, 61, 54, 47, 55, 62, 63,
];

#[deriving(Default, Clone)]
struct HuffTable {
	lut:     Vec<(u8, u8)>,
	valptr:  Vec<int>,
	huffval: Vec<u8>,
	maxcode: Vec<int>,
	mincode: Vec<int>,
}

#[deriving(Clone)]
struct Component {
	id: u8,
	h: u8,
	v: u8,
	tq: u8,
	dc_table: u8,
	ac_table: u8,
	dc_pred: i32
}

#[deriving(PartialEq)]
enum JPEGState {
	Start,
	HaveSOI,
	HaveFirstFrame,
	HaveFirstScan,
	End
}

pub struct JPEGDecoder<R> {
	r: R,

	qtables: [u8, ..64 * 4],
	dctables: [HuffTable, ..2],
	actables: [HuffTable, ..2],

	h: HuffDecoder,

	height: u16,
	width: u16,

	num_components: u8,
	scan_components: Vec<u8>,
	components: SmallIntMap<Component>,

	mcu_row: Vec<u8>,
	mcu: Vec<u8>,
	hmax: u8,
	vmax: u8,

	interval: u16,
	mcucount: u16,
	expected_rst: u8,

	row_count: u8,
	state: JPEGState,
}

impl<R: Reader>JPEGDecoder<R> {
	pub fn new(r: R) -> JPEGDecoder<R> {
		let h: HuffTable  = Default::default();

		JPEGDecoder {
			r: r,

			qtables: [0u8, ..64 * 4],
			dctables: [h.clone(), h.clone()],
			actables: [h.clone(), h.clone()],

			h: HuffDecoder::new(),

			height: 0,
			width: 0,

			num_components: 0,
			scan_components: Vec::new(),
			components: SmallIntMap::new(),

			mcu_row: Vec::new(),
			mcu: Vec::new(),
			hmax: 0,
			vmax: 0,

			interval: 0,
			mcucount: 0,
			expected_rst: RST0,

			row_count: 0,
			state: Start,
		}
	}

	pub fn dimensions(&self) -> (u32, u32) {
		(self.width as u32, self.height as u32)
	}

	pub fn color_type(&self) -> colortype::ColorType {
		if self.num_components == 1 {colortype::Grey(8)}
		else {colortype::RGB(8)}
	}

	pub fn rowlen(&self) -> uint {
		self.width as uint * self.num_components as uint
	}

	pub fn read_scanline(&mut self, buf: &mut [u8]) -> IoResult<uint> {
		if self.state == Start {
			let _ = try!(self.read_metadata());
		}

		if self.row_count == 0 {
			let _ = try!(self.decode_mcu_row());
		}

		let w = 8 * ((self.width as uint + 7) / 8);
		let len = w * self.num_components as uint;

		let slice = self.mcu_row.slice(self.row_count as uint * len,
					       self.row_count as uint * len + buf.len());

		slice::bytes::copy_memory(buf, slice);
		self.row_count = (self.row_count + 1) % (self.vmax * 8);

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

	fn decode_mcu_row(&mut self) -> IoResult<()> {
		let w 	  = 8 * ((self.width as uint + 7) / 8);
		let bpp   = self.num_components as uint;

		for x0 in range_step(0, w * bpp, bpp * 8 * self.hmax as uint) {
			let _ = try!(self.decode_mcu());

			upsample_mcu(
				self.mcu_row.as_mut_slice(),
				x0,
				w,
				bpp,
				self.mcu.as_slice(),
				self.hmax,
				self.vmax
			);
		}

		Ok(())
	}

	fn decode_mcu(&mut self) -> IoResult<()> {
		let mut i = 0;

		let tmp = self.scan_components.clone();
		for id in tmp.iter() {
			let mut c = self.components.find(&(*id as uint)).unwrap().clone();

			for _ in range(0, c.h * c.v) {
				let pred  = try!(self.decode_block(i, c.dc_table, c.dc_pred, c.ac_table, c.tq));
				c.dc_pred = pred;

				i += 1;
			}

			self.components.insert(*id as uint, c);
		}

		self.mcucount += 1;
		self.read_restart()
	}

	fn decode_block(&mut self, i: uint, dc: u8, pred: i32, ac: u8, q: u8) -> IoResult<i32> {
		let zz   = self.mcu.mut_slice(i * 64, i * 64 + 64);
		let mut tmp = [0i32, ..64];

		let dctable = &self.dctables[dc as uint];
		let actable = &self.actables[ac as uint];
		let qtable  = self.qtables.slice(64 * q as uint, 64 * q as uint + 64);

		let t     = try!(self.h.decode_symbol(&mut self.r, dctable));
		let diff  = if t > 0 {
			try!(self.h.receive(&mut self.r, t))
		} else {
			0
		};

		//Section F.2.1.3.1
		let diff = extend(diff, t);
		let dc = diff + pred;

		tmp[0] = dc * qtable[0] as i32;

		let mut k = 0u;

		while k < 63 {
			let rs = try!(self.h.decode_symbol(&mut self.r, actable));

			let ssss = rs & 0x0F;
			let rrrr = rs >> 4;

			if ssss == 0 {
				if rrrr != 15 {
					break
				}
				k += 16;
			}
			else {
				k += rrrr as uint;

				//Figure F.14
				let t = try!(self.h.receive(&mut self.r, ssss));
				tmp[UNZIGZAG[k + 1] as uint] = extend(t, ssss) * qtable[k + 1] as i32;
				k += 1;
			}
		}

		transform::idct(tmp, zz);

		Ok(dc)
	}

	fn read_metadata(&mut self) -> IoResult<()> {
		while self.state != HaveFirstScan {
			let byte = try!(self.r.read_u8());

			if byte != 0xFF {
				continue;
			}

			let marker = try!(self.r.read_u8());

			match marker {
				SOI => self.state = HaveSOI,

				DHT => try!(self.read_huffman_tables()),

				DQT => try!(self.read_quantization_tables()),

				SOF0 => {
					let _ = try!(self.read_frame_header());
					self.state = HaveFirstFrame;
				}

				SOS => {
					let _ = try!(self.read_scan_header());
					self.state = HaveFirstScan;
				}

				DRI => try!(self.read_restart_interval()),

				APP0 .. APPF | COM => {
					let length = try!(self.r.read_be_u16());
					let _ = try!(self.r.read_exact((length -2) as uint));
				}

				TEM  => continue,

				SOF2 => fail!("Progressive DCT unimplemented"),

				DNL  => fail!("DNL not supported"),

				a    => fail!(format!("unexpected marker {:X}\n", a))
			}
		}

		Ok(())
	}

	fn read_frame_header(&mut self) -> IoResult<()> {
		let _frame_length = try!(self.r.read_be_u16());

		let sample_precision = try!(self.r.read_u8());
		assert!(sample_precision == 8);

		self.height 	    = try!(self.r.read_be_u16());
		self.width  	    = try!(self.r.read_be_u16());
		self.num_components = try!(self.r.read_u8());

		if self.height == 0 {
			fail!("DNL not supported")
		}

		if self.num_components != 1 && self.num_components != 3 {
			fail!(format!("unsupported number of components: {}", self.num_components))
		}

		self.read_frame_components(self.num_components)
	}

	fn read_frame_components(&mut self, n: u8) -> IoResult<()> {
		let mut blocks_per_mcu = 0;
		for _ in range(0, n) {
			let id = try!(self.r.read_u8());
			let hv = try!(self.r.read_u8());
			let tq = try!(self.r.read_u8());

			let c = Component {
				id: id,
				h:  hv >> 4,
				v:  hv & 0x0F,
				tq: tq,
				dc_table: 0,
				ac_table: 0,
				dc_pred: 0
			};

			blocks_per_mcu += (hv >> 4) * (hv & 0x0F);
			self.components.insert(id as uint, c);
		}

		let (hmax, vmax) = self.components.iter().fold((0, 0), |(h, v), (_, c)| {
			(cmp::max(h, c.h), cmp::max(v, c.v))
		});

		self.hmax = hmax;
		self.vmax = vmax;

		//only 1 component no interleaving
		if n == 1 {
			for (_, c) in self.components.mut_iter() {
				c.h = 1;
				c.v = 1;
			}

			blocks_per_mcu = 1;
			self.hmax = 1;
			self.vmax = 1;
		}

		self.mcu =  Vec::from_elem(blocks_per_mcu as uint * 64, 0u8);

		let mcus_per_row = (self.width as f32 / (8 * hmax) as f32).ceil() as uint;
		let mcu_row_len = (hmax as uint * vmax as uint) * self.mcu.len() * mcus_per_row;

		self.mcu_row = Vec::from_elem(mcu_row_len, 0u8);

		Ok(())
	}

	fn read_scan_header(&mut self) -> IoResult<()> {
		let _scan_length = try!(self.r.read_be_u16());

		let num_scan_components = try!(self.r.read_u8());

		self.scan_components = Vec::new();
		for _ in range(0, num_scan_components as uint) {
			let id = try!(self.r.read_u8());
			let tables = try!(self.r.read_u8());

			let c = self.components.find_mut(&(id as uint)).unwrap();

			c.dc_table = tables >> 4;
			c.ac_table = tables & 0x0F;

			self.scan_components.push(id);
		}

		let _spectral_end   = try!(self.r.read_u8());
		let _spectral_start = try!(self.r.read_u8());

		let approx = try!(self.r.read_u8());

		let _approx_high = approx >> 4;
		let _approx_low  = approx & 0x0F;

		Ok(())
	}

	fn read_quantization_tables(&mut self) -> IoResult<()> {
		let mut table_length = try!(self.r.read_be_u16()) as i32;
		table_length -= 2;

		while table_length > 0 {
			let pqtq = try!(self.r.read_u8());
			let pq = pqtq >> 4;
			let tq = pqtq & 0x0F;

			assert!(pq == 0);
			assert!(tq <= 3);

			let slice = self.qtables.mut_slice(64 * tq as uint, 64 * tq as uint + 64);

			for i in range(0u, 64) {
				slice[i] = try!(self.r.read_u8());
			}

			table_length -= 1 + 64;
		}

		Ok(())
	}

	fn read_huffman_tables(&mut self) -> IoResult<()> {
		let mut table_length = try!(self.r.read_be_u16());
		table_length -= 2;

		while table_length > 0 {
			let tcth = try!(self.r.read_u8());
			let tc = tcth >> 4;
			let th = tcth & 0x0F;

			assert!(tc == 0 || tc == 1);

			let bits = try!(self.r.read_exact(16));
			let len = bits.len();

			let mt = bits.iter().fold(0, |a, b| a + *b);
			let huffval = try!(self.r.read_exact(mt as uint));

			if tc == 0 {
				self.dctables[th as uint] = derive_tables(bits, huffval);
			}
			else {
				self.actables[th as uint] = derive_tables(bits, huffval);
			}

			table_length -= 1 + len as u16 + mt as u16;
		}

		Ok(())
	}


	fn read_restart_interval(&mut self) -> IoResult<()> {
		let _length = try!(self.r.read_be_u16());
		self.interval = try!(self.r.read_be_u16());

		Ok(())
	}

	fn read_restart(&mut self) -> IoResult<()> {
		let w = (self.width + 7) / (self.hmax * 8) as u16;
		let h = (self.height + 7) / (self.vmax * 8) as u16;

		if self.interval != 0  &&
		   self.mcucount % self.interval == 0 &&
		   self.mcucount < w * h {

			let rst = try!(self.find_restart_marker());

			if rst == self.expected_rst {
				self.reset();
				self.expected_rst += 1;

				if self.expected_rst > RST7 {
					self.expected_rst = RST0;
				}
			}
			else {
				fail!(format!("expected marker {0:X} but got {1:X}", self.expected_rst, rst));
			}
		}

		Ok(())
	}

	fn find_restart_marker(&mut self) -> IoResult<u8> {
		if self.h.marker != 0 {
			let m = self.h.marker;
			self.h.marker = 0;

			return Ok(m);
		}

		let mut b;
		loop {
			b = try!(self.r.read_u8());

			if b == 0xFF {
				b = try!(self.r.read_u8());
				match b {
					RST0 .. RST7 => break,
					EOI => fail!("unexpected end of image"),
					_   => continue
				}
			}
		}

		Ok(b)
	}

	fn reset(&mut self) {
		self.h.bits = 0;
		self.h.num_bits = 0;
		self.h.end = false;
		self.h.marker = 0;

		for (_, c) in self.components.mut_iter() {
			c.dc_pred = 0;
		}
	}
}

fn upsample_mcu(out: &mut [u8], xoffset: uint, width: uint, bpp: uint, mcu: &[u8], h: u8, v: u8) {
	if mcu.len() == 64 {
		for y in range(0u, 8) {
			for x in range(0u, 8) {
				out[xoffset + x + (y * width)] = mcu[x + y * 8] as u8
			}
		}
	}
	else {
		let y_blocks = h * v;

		let y_blocks = mcu.slice_to(y_blocks as uint * 64);
		let cb = mcu.slice(y_blocks.len(), y_blocks.len() + 64);
		let cr = mcu.slice_from(y_blocks.len() + cb.len());

		let mut k = 0;
		for by in range(0, v as uint) {
			let y0 = by * 8;

			for bx in range(0, h as uint) {
				let x0 = xoffset + bx * 8 * bpp;

				for y in range(0u, 8) {
					for x in range(0u, 8) {
						let (a, b, c) = (y_blocks[k * 64 + x + y * 8], cb[x + y * 8], cr[x + y * 8]);
						let (r, g, b) = ycbcr_to_rgb(a , b , c );

						let offset = (y0 + y) * (width * bpp) + x0 + x * bpp;
						out[offset + 0] = r;
						out[offset + 1] = g;
						out[offset + 2] = b;
					}
				}

				k += 1;
			}
		}
	}
}

fn ycbcr_to_rgb(y: u8, cb: u8, cr: u8) -> (u8, u8, u8) {
	let y = y as f32;
	let cr = cr as f32;
	let cb = cb as f32;

	let r1 = y + 1.402f32 * (cr - 128f32) ;
	let g1 = y - 0.34414f32 * (cb - 128f32) - 0.71414f32 * (cr - 128f32);
	let b1 = y + 1.772f32 * (cb - 128f32);

	let r = clamp(r1 as i32);
	let g = clamp(g1 as i32);
	let b = clamp(b1 as i32);

	(r, g, b)
}

fn clamp(a: i32) -> u8 {
	if a < 0 {0}
	else if a > 255 {255}
	else {a as u8}
}

//Section F.2.2.1
//Figure F.12
fn extend(v: i32, t: u8) -> i32 {
	let vt = 1 << t as uint - 1;
	let vt = vt as i32;

	if v < vt {
		v + ((-1) << t as uint) + 1
	}
	else {
		v
	}
}

struct HuffDecoder {
	bits: u32,
	num_bits: u8,
	end: bool,
	marker: u8,
}

impl HuffDecoder {
	pub fn new() -> HuffDecoder {
		HuffDecoder {bits: 0, num_bits: 0, end: false, marker: 0}
	}

	fn guarantee<R: Reader>(&mut self, r: &mut R, n: u8) -> IoResult<()> {
		while self.num_bits < n && !self.end {
			let byte = try!(r.read_u8());

			if byte == 0xFF {
				let byte2 = try!(r.read_u8());
				if byte2 != 0 {
					self.marker = byte2;
					self.end = true;
				}
			}

			self.bits |= (byte as u32 << (32 - 8)) >> self.num_bits as u32;
			self.num_bits += 8;
		}

		Ok(())
	}

	pub fn read_bit<R: Reader>(&mut self, r: &mut R) -> IoResult<u8> {
		let _ = try!(self.guarantee(r, 1));

		let bit = (self.bits & (1 << 31)) >> 31;
		self.consume(1);

		Ok(bit as u8)
	}

	//Section F.2.2.4
	//Figure F.17
	pub fn receive<R: Reader>(&mut self, r: &mut R, ssss: u8) -> IoResult<i32> {
		let _ = try!(self.guarantee(r, ssss));

		let bits = (self.bits & (0xFFFFFFFFu32 << (32 - ssss as u32))) >> (32 - ssss);
		self.consume(ssss);

		Ok(bits as i32)
	}

	fn consume(&mut self, n: u8) {
		self.bits <<= n as u32;
		self.num_bits -= n;
	}

	pub fn decode_symbol<R: Reader>(&mut self, r: &mut R, table: &HuffTable) -> IoResult<u8> {
		let _ = try!(self.guarantee(r, 8));
		let index = (self.bits & 0xFF000000) >> (32 - 8);
		let (val, size) = table.lut.as_slice()[index as uint];

		if index < 256 && size < 9 {
			self.consume(size);

			return Ok(val)
		}
		else {
			let mut code = 0u;

			for i in range(0u, 16) {
				let b = try!(self.read_bit(r));
				code |= b as uint;

				if (code as int) <= table.maxcode.as_slice()[i] {
					let index = table.valptr.as_slice()[i] +
						    code as int -
						    table.mincode.as_slice()[i];

					return Ok(table.huffval.as_slice()[index as uint])
				}
				code <<= 1;
			}

			fail!(format!("bad huffman code: {:t}", code));
		}
	}
}

fn derive_codes_and_sizes(bits: &[u8]) -> (Vec<u8>, Vec<u16>) {
	let mut huffsize = Vec::from_elem(256, 0u8);
	let mut huffcode = Vec::from_elem(256, 0u16);

	let mut k = 0;
	let mut j;

	//Annex C.2
	//Figure C.1
	//Generate table of individual code lengths
	for i in range(0u, 16) {
		j = 0;
		while j < bits.as_slice()[i] {
			huffsize.as_mut_slice()[k] = i as u8 + 1;
			k += 1;
			j += 1;
		}
	}

	huffsize.as_mut_slice()[k] = 0;

	//Annex C.2
	//Figure C.2
	//Generate table of huffman codes
	k = 0;
	let mut code = 0u16;
	let mut size = huffsize.as_slice()[0];

	while huffsize.as_slice()[k] != 0 {
		huffcode.as_mut_slice()[k] = code;
		code += 1;
		k += 1;

		if huffsize.as_slice()[k] == size {
			continue
		}

		let diff = huffsize.as_slice()[k] - size;
		code <<= diff as u16;

		size += diff
	}

	(huffsize, huffcode)
}

fn derive_tables(bits: Vec<u8>, huffval: Vec<u8>) -> HuffTable {
	let mut mincode = Vec::from_elem(16, -1i);
	let mut maxcode = Vec::from_elem(16, -1i);
	let mut valptr  = Vec::from_elem(16, -1i);
	let mut lut     = Vec::from_elem(256, (0u8, 17u8));

	let (huffsize, huffcode) = derive_codes_and_sizes(bits.as_slice());

	//Annex F.2.2.3
	//Figure F.15
	let mut j = 0;

	for i in range(0u, 16) {
		if bits.as_slice()[i] != 0 {
			valptr.as_mut_slice()[i] = j;
			mincode.as_mut_slice()[i] = huffcode.as_slice()[j as uint] as int;
			j += bits.as_slice()[i] as int - 1;
			maxcode.as_mut_slice()[i] = huffcode.as_slice()[j as uint] as int;

			j += 1;
		}
	}

	for (i, v) in huffval.iter().enumerate() {
		if huffsize.as_slice()[i] > 8 {
			break
		}

		let r = 8 - huffsize.as_slice()[i] as uint;

		for j in range(0, 1 << r) {
			let index = (huffcode.as_slice()[i] << r) + j as u16;
			lut.as_mut_slice()[index as uint] = (*v, huffsize.as_slice()[i]);
		}
	}

	HuffTable {
		lut: lut,
		huffval: huffval,
		maxcode: maxcode,
		mincode: mincode,
		valptr: valptr
	}
}

//section K.1
//table K.1
static STD_LUMA_QTABLE: [u8, ..64] = [
	16, 11, 10, 16, 124, 140, 151, 161,
	12, 12, 14, 19, 126, 158, 160, 155,
	14, 13, 16, 24, 140, 157, 169, 156,
	14, 17, 22, 29, 151, 187, 180, 162,
	18, 22, 37, 56, 168, 109, 103, 177,
	24, 35, 55, 64, 181, 104, 113, 192,
	49, 64, 78, 87, 103, 121, 120, 101,
	72, 92, 95, 98, 112, 100, 103, 199
];

//table K.2
static STD_CHROMA_QTABLE: [u8, ..64] = [
	17, 18, 24, 47, 99, 99, 99, 99,
	18, 21, 26, 66, 99, 99, 99, 99,
	24, 26, 56, 99, 99, 99, 99, 99,
	47, 66, 99, 99, 99, 99, 99, 99,
	99, 99, 99, 99, 99, 99, 99, 99,
	99, 99, 99, 99, 99, 99, 99, 99,
	99, 99, 99, 99, 99, 99, 99, 99,
	99, 99, 99, 99, 99, 99, 99, 99
];

//section K.3
//Code lengths and values for table K.3
static STD_LUMA_DC_CODE_LENGTHS: [u8, ..16] = [
	0x00, 0x01, 0x05, 0x01, 0x01, 0x01, 0x01, 0x01,
	0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
];

static STD_LUMA_DC_VALUES: [u8, ..12] = [
	0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
	0x08, 0x09, 0x0A, 0x0B
];

//Code lengths and values for table K.4
static STD_CHROMA_DC_CODE_LENGTHS: [u8, ..16] = [
	0x00, 0x03, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01,
	0x01, 0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00
];

static STD_CHROMA_DC_VALUES: [u8, ..12] = [
	0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
	0x08, 0x09, 0x0A, 0x0B
];

//Code lengths and values for table k.5
static STD_LUMA_AC_CODE_LENGTHS: [u8, ..16] = [
	0x00, 0x02, 0x01, 0x03, 0x03, 0x02, 0x04, 0x03,
	0x05, 0x05, 0x04, 0x04, 0x00, 0x00, 0x01, 0x7D
];

static STD_LUMA_AC_VALUES: [u8, ..162] = [
	0x01, 0x02, 0x03, 0x00, 0x04, 0x11, 0x05, 0x12, 0x21, 0x31, 0x41, 0x06, 0x13, 0x51, 0x61, 0x07,
	0x22, 0x71, 0x14, 0x32, 0x81, 0x91, 0xA1, 0x08, 0x23, 0x42, 0xB1, 0xC1, 0x15, 0x52, 0xD1, 0xF0,
	0x24, 0x33, 0x62, 0x72, 0x82, 0x09, 0x0A, 0x16, 0x17, 0x18, 0x19, 0x1A, 0x25, 0x26, 0x27, 0x28,
	0x29, 0x2A, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3A, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49,
	0x4A, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59, 0x5A, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69,
	0x6A, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79, 0x7A, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89,
	0x8A, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9A, 0xA2, 0xA3, 0xA4, 0xA5, 0xA6, 0xA7,
	0xA8, 0xA9, 0xAA, 0xB2, 0xB3, 0xB4, 0xB5, 0xB6, 0xB7, 0xB8, 0xB9, 0xBA, 0xC2, 0xC3, 0xC4, 0xC5,
	0xC6, 0xC7, 0xC8, 0xC9, 0xCA, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6, 0xD7, 0xD8, 0xD9, 0xDA, 0xE1, 0xE2,
	0xE3, 0xE4, 0xE5, 0xE6, 0xE7, 0xE8, 0xE9, 0xEA, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8,
	0xF9, 0xFA,
];

//Code lengths and values for table k.6
static STD_CHROMA_AC_CODE_LENGTHS: [u8, ..16] = [
	0x00, 0x02, 0x01, 0x02, 0x04, 0x04, 0x03, 0x04,
	0x07, 0x05, 0x04, 0x04, 0x00, 0x01, 0x02, 0x77,
];
static STD_CHROMA_AC_VALUES: [u8, ..162] = [
	0x00, 0x01, 0x02, 0x03, 0x11, 0x04, 0x05, 0x21, 0x31, 0x06, 0x12, 0x41, 0x51, 0x07, 0x61, 0x71,
	0x13, 0x22, 0x32, 0x81, 0x08, 0x14, 0x42, 0x91, 0xA1, 0xB1, 0xC1, 0x09, 0x23, 0x33, 0x52, 0xF0,
	0x15, 0x62, 0x72, 0xD1, 0x0A, 0x16, 0x24, 0x34, 0xE1, 0x25, 0xF1, 0x17, 0x18, 0x19, 0x1A, 0x26,
	0x27, 0x28, 0x29, 0x2A, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3A, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48,
	0x49, 0x4A, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59, 0x5A, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68,
	0x69, 0x6A, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79, 0x7A, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87,
	0x88, 0x89, 0x8A, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9A, 0xA2, 0xA3, 0xA4, 0xA5,
	0xA6, 0xA7, 0xA8, 0xA9, 0xAA, 0xB2, 0xB3, 0xB4, 0xB5, 0xB6, 0xB7, 0xB8, 0xB9, 0xBA, 0xC2, 0xC3,
	0xC4, 0xC5, 0xC6, 0xC7, 0xC8, 0xC9, 0xCA, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6, 0xD7, 0xD8, 0xD9, 0xDA,
	0xE2, 0xE3, 0xE4, 0xE5, 0xE6, 0xE7, 0xE8, 0xE9, 0xEA, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8,
	0xF9, 0xFA,
];

static DCCLASS: u8 = 0;
static ACCLASS: u8 = 1;

static LUMADESTINATION: u8 = 0;
static CHROMADESTINATION: u8 = 1;

static LUMAID: u8 = 1;
static CHROMABLUEID: u8 = 2;
static CHROMAREDID: u8 = 3;

pub struct JPEGEncoder<W> {
	w: W,

	components: Vec<Component>,
	tables: Vec<u8>,

	accumulator: u32,
	nbits: u8,

	luma_dctable: Vec<(u8, u16)>,
	luma_actable: Vec<(u8, u16)>,
	chroma_dctable: Vec<(u8, u16)>,
	chroma_actable: Vec<(u8, u16)>,
}

impl<W: Writer> JPEGEncoder<W> {
	pub fn new(w: W) -> JPEGEncoder<W> {
		let ld = build_huff_lut(STD_LUMA_DC_CODE_LENGTHS, STD_LUMA_DC_VALUES);
		let la = build_huff_lut(STD_LUMA_AC_CODE_LENGTHS, STD_LUMA_AC_VALUES);

		let cd = build_huff_lut(STD_CHROMA_DC_CODE_LENGTHS, STD_CHROMA_DC_VALUES);
		let ca = build_huff_lut(STD_CHROMA_AC_CODE_LENGTHS, STD_CHROMA_AC_VALUES);

		let components = vec![
			Component {id: LUMAID, h: 1, v: 1, tq: LUMADESTINATION, dc_table: LUMADESTINATION, ac_table: LUMADESTINATION, dc_pred: 0},
			Component {id: CHROMABLUEID, h: 1, v: 1, tq: CHROMADESTINATION, dc_table: CHROMADESTINATION, ac_table: CHROMADESTINATION, dc_pred: 0},
			Component {id: CHROMAREDID, h: 1, v: 1, tq: CHROMADESTINATION, dc_table: CHROMADESTINATION, ac_table: CHROMADESTINATION, dc_pred: 0}
		];

		let tables = Vec::new().append(STD_LUMA_QTABLE);
		let tables = tables.append(STD_CHROMA_QTABLE);

		JPEGEncoder {
			w: w,

			components: components,
			tables: tables,

			luma_dctable: ld,
			luma_actable: la,
			chroma_dctable: cd,
			chroma_actable: ca,

			accumulator: 0,
			nbits: 0,
		}
	}

	fn write_segment(&mut self, marker: u8, data: Option<Vec<u8>>) -> IoResult<()> {
		let _ = try!(self.w.write_u8(0xFF));
		let _ = try!(self.w.write_u8(marker));

		if data.is_some() {
			let b = data.unwrap();
			let _ = try!(self.w.write_be_u16(b.len() as u16 + 2));
			let _ = try!(self.w.write(b.as_slice()));
		}

		Ok(())
	}

	fn write_bits(&mut self, bits: u16, size: u8) -> IoResult<()> {
		self.accumulator |= bits as u32 << (32 - (self.nbits + size));
		self.nbits += size;

		while self.nbits >= 8 {
			let byte = (self.accumulator & (0xFFFFFFFFu32 << 24)) >> 24;

			let _ = try!(self.w.write_u8(byte as u8));
			if byte == 0xFF {
				let _ = try!(self.w.write_u8(0x00));
			}

			self.nbits -= 8;
			self.accumulator <<= 8;
		}

		Ok(())
	}

	fn pad_byte(&mut self) -> IoResult<()> {
		self.write_bits(0x7F, 7)
	}

	fn huffman_encode(&mut self, val: u8, table: &[(u8, u16)]) -> IoResult<()> {
		let (size, code) = table[val as uint];

		if size > 16 {
			fail!("bad huffman value");
		}

		self.write_bits(code, size)
	}

	fn write_block(&mut self,
		       block: &[i32],
		       prevdc: i32,
		       dctable: &[(u8, u16)],
		       actable: &[(u8, u16)]) -> IoResult<i32> {

		//Differential DC encoding
		let dcval = block[0];
		let diff  = dcval - prevdc;
		let (size, value) = encode_coefficient(diff);

		let _ = try!(self.huffman_encode(size, dctable));
		let _ = try!(self.write_bits(value, size));

		//Figure F.2
		let mut zero_run = 0;
		let mut k = 0u;

		loop {
			k += 1;

			if block[UNZIGZAG[k] as uint] == 0 {
				if k == 63 {
					let _ = try!(self.huffman_encode(0x00, actable));
					break
				}

				zero_run += 1;
			}
			else {
				while zero_run > 15 {
					let _ = try!(self.huffman_encode(0xF0, actable));
					zero_run -= 16;
				}

				let (size, value) = encode_coefficient(block[UNZIGZAG[k] as uint]);
				let symbol = (zero_run << 4) | size;

				let _ = try!(self.huffman_encode(symbol, actable));
				let _ = try!(self.write_bits(value, size));

				zero_run = 0;

				if k == 63 {
					break
				}
			}
		}

		Ok(dcval)
	}

	fn encode_grey(&mut self, image: &[u8], width: uint, height: uint, bpp: uint) -> IoResult<()> {
		let mut yblock     = [0u8, ..64];
		let mut y_dcprev   = 0;
		let mut dct_yblock = [0i32, ..64];

		for y in range_step(0, height as uint, 8) {
			for x in range_step(0, width as uint, 8) {
				//RGB -> YCbCr
				copy_blocks_grey(image, x, y, width as uint, bpp, &mut yblock);

				//Level shift and fdct
				//Coeffs are scaled by 8
				transform::fdct(yblock.as_slice(), dct_yblock);

				//Quantization
				for i in range(0u, 64) {
					dct_yblock[i]   = ((dct_yblock[i] / 8)   as f32 / self.tables.slice_to(64)[i] as f32).round() as i32;
				}

				let la = self.luma_actable.clone();
				let ld = self.luma_dctable.clone();

				y_dcprev  = try!(self.write_block(dct_yblock, y_dcprev, ld.as_slice(), la.as_slice()));
			}
		}

		Ok(())
	}

	fn encode_rgb(&mut self, image: &[u8], width: uint, height: uint, bpp: uint) -> IoResult<()> {
		let mut y_dcprev = 0;
		let mut cb_dcprev = 0;
		let mut cr_dcprev = 0;

		let mut dct_yblock   = [0i32, ..64];
		let mut dct_cb_block = [0i32, ..64];
		let mut dct_cr_block = [0i32, ..64];

		let mut yblock = [0u8, ..64];
		let mut cb_block = [0u8, ..64];
		let mut cr_block = [0u8, ..64];

		for y in range_step(0, height as uint, 8) {
			for x in range_step(0, width as uint, 8) {
				//RGB -> YCbCr
				copy_blocks_ycbcr(image, x, y, width as uint, bpp, &mut yblock, &mut cb_block, &mut cr_block);

				//Level shift and fdct
				//Coeffs are scaled by 8
				transform::fdct(yblock.as_slice(), dct_yblock);
				transform::fdct(cb_block.as_slice(), dct_cb_block);
				transform::fdct(cr_block.as_slice(), dct_cr_block);

				//Quantization
				for i in range(0u, 64) {
					dct_yblock[i]   = ((dct_yblock[i] / 8)   as f32 / self.tables.slice_to(64)[i] as f32).round() as i32;
					dct_cb_block[i] = ((dct_cb_block[i] / 8) as f32 / self.tables.slice_from(64)[i] as f32).round() as i32;
					dct_cr_block[i] = ((dct_cr_block[i] / 8) as f32 / self.tables.slice_from(64)[i] as f32).round() as i32;
				}

				let la = self.luma_actable.clone();
				let ld = self.luma_dctable.clone();
				let cd = self.chroma_dctable.clone();
				let ca = self.chroma_actable.clone();

				y_dcprev  = try!(self.write_block(dct_yblock, y_dcprev, ld.as_slice(), la.as_slice()));
				cb_dcprev = try!(self.write_block(dct_cb_block, cb_dcprev, cd.as_slice(), ca.as_slice()));
				cr_dcprev = try!(self.write_block(dct_cr_block, cr_dcprev, cd.as_slice(), ca.as_slice()));
			}
		}

		Ok(())
	}

	pub fn encode(&mut self,
		      image: &[u8],
		      width: u32,
		      height: u32,
		      c: colortype::ColorType) -> IoResult<()> {

		let n = colortype::num_components(c);
		let num_components = if n == 1 || n == 2 {1}
							 else {3};

		let _ = try!(self.write_segment(SOI, None));

		let buf = build_jfif_header();
		let _   = try!(self.write_segment(APP0, Some(buf)));

		let buf = build_frame_header(8, width as u16, height as u16, self.components.slice_to(num_components));
		let _   = try!(self.write_segment(SOF0, Some(buf)));

		assert!(self.tables.len() / 64 == 2);
		let numtables = if num_components == 1 {1}
				else {2};

		let t = self.tables.clone();
		for (i, table) in t.as_slice().chunks(64).enumerate().take(numtables) {
			let buf = build_quantization_segment(8, i as u8, table);
			let _   = try!(self.write_segment(DQT, Some(buf)));
		}

		let numcodes = STD_LUMA_DC_CODE_LENGTHS;
		let values   = STD_LUMA_DC_VALUES;
		let buf = build_huffman_segment(DCCLASS, LUMADESTINATION, numcodes, values);
		let _   = try!(self.write_segment(DHT, Some(buf)));

		let numcodes = STD_LUMA_AC_CODE_LENGTHS;
		let values   = STD_LUMA_AC_VALUES;
		let buf = build_huffman_segment(ACCLASS, LUMADESTINATION, numcodes, values);
		let _   = try!(self.write_segment(DHT, Some(buf)));

		if num_components == 3 {
			let numcodes = STD_CHROMA_DC_CODE_LENGTHS;
			let values   = STD_CHROMA_DC_VALUES;
			let buf = build_huffman_segment(DCCLASS, CHROMADESTINATION, numcodes, values);
			let _   = try!(self.write_segment(DHT, Some(buf)));

			let numcodes = STD_CHROMA_AC_CODE_LENGTHS;
			let values   = STD_CHROMA_AC_VALUES;
			let buf = build_huffman_segment(ACCLASS, CHROMADESTINATION, numcodes, values);
			let _   = try!(self.write_segment(DHT, Some(buf)));
		}

		let buf = build_scan_header(self.components.slice_to(num_components));
		let _   = try!(self.write_segment(SOS, Some(buf)));

		match c {
			colortype::RGB(8)   => try!(self.encode_rgb(image, width as uint, height as uint, 3)),
			colortype::RGBA(8)  => try!(self.encode_rgb(image, width as uint, height as uint, 4)),
			colortype::Grey(8)  => try!(self.encode_grey(image, width as uint, height as uint, 1)),
			colortype::GreyA(8) => try!(self.encode_grey(image, width as uint, height as uint, 2)),
			_  => fail!("unimplemented!")
		};

		let _ = try!(self.pad_byte());
		self.write_segment(EOI, None)
	}
}

fn build_jfif_header() -> Vec<u8> {
	let mut m = MemWriter::new();

	let _ = m.write_str("JFIF");
	let _ = m.write_u8(0);
	let _ = m.write_u8(0x01);
	let _ = m.write_u8(0x02);
	let _ = m.write_u8(0);
	let _ = m.write_be_u16(1);
	let _ = m.write_be_u16(1);
	let _ = m.write_u8(0);
	let _ = m.write_u8(0);

	m.unwrap()
}

fn build_frame_header(precision: u8,
		      width: u16,
		      height: u16,
		      components: &[Component]) -> Vec<u8> {

	let mut m = MemWriter::new();

	let _ = m.write_u8(precision);
	let _ = m.write_be_u16(height);
	let _ = m.write_be_u16(width);
	let _ = m.write_u8(components.len() as u8);

	for &comp in components.iter() {
		let _  = m.write_u8(comp.id);
		let hv = (comp.h << 4) | comp.v;
		let _  = m.write_u8(hv);
		let _  = m.write_u8(comp.tq);
	}

	m.unwrap()
}

fn build_scan_header(components: &[Component]) -> Vec<u8> {
	let mut m = MemWriter::new();

	let _ = m.write_u8(components.len() as u8);

	for &comp in components.iter() {
		let _ 	   = m.write_u8(comp.id);
		let tables = (comp.dc_table << 4) | comp.ac_table;
		let _ 	   = m.write_u8(tables);
	}

	//spectral start and end, approx. high and low
	let _ = m.write_u8(0);
	let _ = m.write_u8(63);
	let _ = m.write_u8(0);

	m.unwrap()
}

fn build_huffman_segment(class: u8,
			 destination: u8,
			 numcodes: &[u8],
			 values: &[u8]) -> Vec<u8> {
	let mut m = MemWriter::new();

	let tcth = (class << 4) | destination;
	let _    = m.write_u8(tcth);

	assert!(numcodes.len() == 16);

	let mut sum = 0u;
	for &i in numcodes.iter() {
		let _ = m.write_u8(i);
		sum += i as uint;
	}

	assert!(sum == values.len());
	for &i in values.iter() {
		let _ = m.write_u8(i);
	}

	m.unwrap()
}

fn build_quantization_segment(precision: u8,
			      identifier: u8,
			      qtable: &[u8]) -> Vec<u8> {

	assert!(qtable.len() % 64 == 0);
	let mut m = MemWriter::new();

	let p = if precision == 8 {0}
			else {1};

	let pqtq = (p << 4) | identifier as u8;
	let _    = m.write_u8(pqtq);

	for i in range(0u, 64) {
		let _ = m.write_u8(qtable[UNZIGZAG[i] as uint]);
	}

	m.unwrap()
}

fn encode_coefficient(coefficient: i32) -> (u8, u16) {
	let mut magnitude = coefficient.abs() as u16;
	let mut num_bits  = 0u8;

	while magnitude > 0 {
		magnitude >>= 1;
		num_bits += 1;
	}

	let mask = (1 << num_bits as u16) - 1;
	let val  = if coefficient < 0 { (coefficient - 1) as u16 & mask }
			   else {coefficient as u16 & mask};

	(num_bits, val)
}

fn rgb_to_ycbcr(r: u8, g: u8, b: u8) -> (u8, u8, u8) {
	let r = r as f32;
	let g = g as f32;
	let b = b as f32;

	let y  =  0.299f32  * r + 0.587f32  * g + 0.114f32  * b;
	let cb = -0.1687f32 * r - 0.3313f32 * g + 0.5f32    * b + 128f32;
	let cr =  0.5f32    * r - 0.4187f32 * g - 0.0813f32 * b + 128f32;

	(y as u8, cb as u8, cr as u8)
}

fn value_at(s: &[u8], index: uint) -> u8 {
	if index < s.len() {
		s[index]
	} else {
		s[s.len() - 1]
	}
}

fn copy_blocks_ycbcr(source: &[u8],
		     x0: uint,
		     y0: uint,
		     width: uint,
		     bpp: uint,
		     yb: &mut [u8, ..64],
		     cbb: &mut [u8, ..64],
		     crb: &mut [u8, ..64]) {
	for y in range(0u, 8) {
		let ystride = (y0 + y) * bpp * width;
		for x in range(0u, 8) {
			let xstride = x0 * bpp + x * bpp;

			let r = value_at(source, ystride + xstride + 0);
			let g = value_at(source, ystride + xstride + 1);
			let b = value_at(source, ystride + xstride + 2);

			let (yc, cb, cr) = rgb_to_ycbcr(r, g, b);

			yb[y * 8 + x]  = yc;
			cbb[y * 8 + x] = cb;
			crb[y * 8 + x] = cr;
		}
	}
}

fn copy_blocks_grey(source: &[u8],
		    x0: uint,
		    y0: uint,
		    width: uint,
		    bpp: uint,
		    gb: &mut [u8, ..64]) {
	for y in range(0u, 8) {
		let ystride = (y0 + y) * bpp * width;
		for x in range(0u, 8) {
			let xstride = x0 * bpp + x * bpp;
			gb[y * 8 + x] = value_at(source, ystride + xstride + 1);
		}
	}
}

fn build_huff_lut(bits: &[u8], huffval: &[u8]) -> Vec<(u8, u16)> {
	let mut lut = Vec::from_elem(256, (17u8, 0u16));
	let (huffsize, huffcode) = derive_codes_and_sizes(bits);

	for (i, &v) in huffval.iter().enumerate() {
		lut.as_mut_slice()[v as uint] = (huffsize.as_slice()[i as uint], huffcode.as_slice()[i as uint]);
	}

	lut
}