use std::cmp;
use std::slice;
use std::iter::range_step;
use std::default::Default;
use std::collections::smallintmap::SmallIntMap;

use color;
use super::transform;

use super::entropy:: {
    HuffTable,
    HuffDecoder,
    derive_tables,
};

use image;
use image::ImageResult;
use image::ImageDecoder;

macro_rules! io_try(
    ($e: expr) => (
        match $e {
            Ok(e) => e,
            Err(err) => return Err(image::IoError(err))
        }
    )
)

/// The permutation of dct coefficients.
pub static UNZIGZAG: [u8, ..64] = [
    0,  1,  8, 16,  9,  2,  3, 10,
    17, 24, 32, 25, 18, 11,  4,  5,
    12, 19, 26, 33, 40, 48, 41, 34,
    27, 20, 13,  6,  7, 14, 21, 28,
    35, 42, 49, 56, 57, 50, 43, 36,
    29, 22, 15, 23, 30, 37, 44, 51,
    58, 59, 52, 45, 38, 31, 39, 46,
    53, 60, 61, 54, 47, 55, 62, 63,
];

/// A representation of a JPEG component
#[deriving(Clone)]
pub struct Component {
    /// The Component's identifier
    pub id: u8,

    /// Horizontal sampling factor
    pub h: u8,

    /// Vertical sampling factor
    pub v: u8,

    /// The quantization table selector
    pub tq: u8,

    /// Index to the Huffman DC Table
    pub dc_table: u8,

    /// Index to the AC Huffman Table
    pub ac_table: u8,

    /// The dc prediction of the component
    pub dc_pred: i32
}

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

#[deriving(PartialEq)]
enum JPEGState {
    Start,
    HaveSOI,
    HaveFirstFrame,
    HaveFirstScan,
    End
}

/// The representation of a JPEG decoder
///
/// Does not support decoding progressive JPEG images
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
    decoded_rows: u32,
    padded_width: uint,
    state: JPEGState,
}

impl<R: Reader>JPEGDecoder<R> {
    /// Create a new decoder that decodes from the stream ```r```
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
            decoded_rows: 0,
            state: Start,
            padded_width: 0
        }
    }

    fn decode_mcu_row(&mut self) -> ImageResult<()> {
        let bytesperpixel = self.num_components as uint;

        for x0 in range_step(0, self.padded_width * bytesperpixel, bytesperpixel * 8 * self.hmax as uint) {

            let _ = try!(self.decode_mcu());

            upsample_mcu (
                self.mcu_row.as_mut_slice(),
                x0,
                self.padded_width,
                bytesperpixel,
                self.mcu.as_slice(),
                self.hmax,
                self.vmax
            );
        }

        Ok(())
    }

    fn decode_mcu(&mut self) -> ImageResult<()> {
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

    fn decode_block(&mut self, i: uint, dc: u8, pred: i32, ac: u8, q: u8) -> ImageResult<i32> {
        let zz   = self.mcu.slice_mut(i * 64, i * 64 + 64);
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
            } else {
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

    fn read_metadata(&mut self) -> ImageResult<()> {
        while self.state != HaveFirstScan {
            let byte = io_try!(self.r.read_u8());

            if byte != 0xFF {
                continue;
            }

            let marker = io_try!(self.r.read_u8());

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
                APP0 ... APPF | COM => {
                    let length = io_try!(self.r.read_be_u16());
                    let _ = io_try!(self.r.read_exact((length - 2) as uint));
                }
                TEM  => continue,
                SOF2 => return Err(image::UnsupportedError("Marker SOF2 ist not supported.".to_string())),
                DNL  => return Err(image::UnsupportedError("Marker DNL ist not supported.".to_string())),
                marker => return Err(image::FormatError(format!("Unkown marker {} encountered.", marker))),
            }
        }

        Ok(())
    }

    fn read_frame_header(&mut self) -> ImageResult<()> {
        let _frame_length = io_try!(self.r.read_be_u16());
        let sample_precision = io_try!(self.r.read_u8());

        if sample_precision != 8 {
            return Err(image::UnsupportedError(format!(
                "A sample precision of {} is not supported",
                sample_precision
            )))
        }

        self.height 	    = io_try!(self.r.read_be_u16());
        self.width  	    = io_try!(self.r.read_be_u16());
        self.num_components = io_try!(self.r.read_u8());

        if self.height == 0 || self.width == 0 {
            return Err(image::DimensionError)
        }

        if self.num_components != 1 && self.num_components != 3 {
            return Err(image::UnsupportedError(format!(
                "Frames with {} components are not supported",
                self.num_components
            )))
        }

        self.padded_width = 8 * ((self.width as uint + 7) / 8);

        let num_components = self.num_components;
        self.read_frame_components(num_components)
    }

    fn read_frame_components(&mut self, n: u8) -> ImageResult<()> {
        let mut blocks_per_mcu = 0;

        for _ in range(0, n) {
            let id = io_try!(self.r.read_u8());
            let hv = io_try!(self.r.read_u8());
            let tq = io_try!(self.r.read_u8());

            let c = Component {
                id: id,
                h: hv >> 4,
                v: hv & 0x0F,
                tq: tq,
                dc_table: 0,
                ac_table: 0,
                dc_pred: 0
            };

            blocks_per_mcu += (hv >> 4) * (hv & 0x0F);
            self.components.insert(id as uint, c);
        }

        let (hmax, vmax) = self.components.iter().fold((0, 0), | (h, v), (_, c) | {
            (cmp::max(h, c.h), cmp::max(v, c.v))
        });

        self.hmax = hmax;
        self.vmax = vmax;

        //only 1 component no interleaving
        if n == 1 {
        for (_, c) in self.components.iter_mut() {
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

    fn read_scan_header(&mut self) -> ImageResult<()> {
        let _scan_length = io_try!(self.r.read_be_u16());

        let num_scan_components = io_try!(self.r.read_u8());

        self.scan_components = Vec::new();

        for _ in range(0, num_scan_components as uint) {
            let id = io_try!(self.r.read_u8());
            let tables = io_try!(self.r.read_u8());

            let c = self.components.find_mut(&(id as uint)).unwrap();

            c.dc_table = tables >> 4;
            c.ac_table = tables & 0x0F;

            self.scan_components.push(id);
        }

        let _spectral_end   = io_try!(self.r.read_u8());
        let _spectral_start = io_try!(self.r.read_u8());

        let approx = io_try!(self.r.read_u8());

        let _approx_high = approx >> 4;
        let _approx_low  = approx & 0x0F;

        Ok(())
    }

    fn read_quantization_tables(&mut self) -> ImageResult<()> {
        let mut table_length = io_try!(self.r.read_be_u16()) as i32;
        table_length -= 2;

        while table_length > 0 {
            let pqtq = io_try!(self.r.read_u8());
            let pq = pqtq >> 4;
            let tq = pqtq & 0x0F;

            if pq != 0 || tq > 3 {
                return Err(image::FormatError("Quantization table malformed.".to_string()))
            }

            let slice = self.qtables.slice_mut(64 * tq as uint, 64 * tq as uint + 64);

            for i in range(0u, 64) {
                slice[i] = io_try!(self.r.read_u8());
            }

            table_length -= 1 + 64;
        }

        Ok(())
    }

    fn read_huffman_tables(&mut self) -> ImageResult<()> {
        let mut table_length = io_try!(self.r.read_be_u16());
        table_length -= 2;

        while table_length > 0 {
            let tcth = io_try!(self.r.read_u8());
            let tc = tcth >> 4;
            let th = tcth & 0x0F;

            if tc != 0 && tc != 1 {
                return Err(image::UnsupportedError(format!(
                    "Huffman table class {} is not supported", tc
                )))
            }

            let bits = io_try!(self.r.read_exact(16));
            let len = bits.len();

            let mt = bits.iter().fold(0, | a, b | a + *b);
            let huffval = io_try!(self.r.read_exact(mt as uint));

            if tc == 0 {
                self.dctables[th as uint] = derive_tables(bits, huffval);
            } else {
                self.actables[th as uint] = derive_tables(bits, huffval);
            }

            table_length -= 1 + len as u16 + mt as u16;
        }

        Ok(())
    }


    fn read_restart_interval(&mut self) -> ImageResult<()> {
        let _length = io_try!(self.r.read_be_u16());
        self.interval = io_try!(self.r.read_be_u16());

        Ok(())
    }

    fn read_restart(&mut self) -> ImageResult<()> {
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
            } else {
                return Err(image::FormatError(format!(
                    "Unexpected restart maker {} found", rst
                )))
            }
        }

        Ok(())
    }

    fn find_restart_marker(&mut self) -> ImageResult<u8> {
        if self.h.marker != 0 {
            let m = self.h.marker;
            self.h.marker = 0;

            return Ok(m);
        }

        let mut b;
        loop {
            b = io_try!(self.r.read_u8());

            if b == 0xFF {
            b = io_try!(self.r.read_u8());
                match b {
                    RST0 ... RST7 => break,
                    EOI => return Err(image::FormatError("Restart marker not found.".to_string())),
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

        for (_, c) in self.components.iter_mut() {
            c.dc_pred = 0;
        }
    }
}

impl<R: Reader> ImageDecoder for JPEGDecoder<R> {
    fn dimensions(&mut self) -> ImageResult<(u32, u32)> {
        if self.state == Start {
            let _ = try!(self.read_metadata());
        }

        Ok((self.width as u32, self.height as u32))
    }

    fn colortype(&mut self) -> ImageResult<color::ColorType> {
        if self.state == Start {
            let _ = try!(self.read_metadata());
        }

        let ctype = if self.num_components == 1 {
            color::Grey(8)
        } else {
            color::RGB(8)
        };

        Ok(ctype)
    }

    fn row_len(&mut self) -> ImageResult<uint> {
        if self.state == Start {
            let _ = try!(self.read_metadata());
        }

        let len = self.width as uint * self.num_components as uint;

        Ok(len)
    }

    fn read_scanline(&mut self, buf: &mut [u8]) -> ImageResult<u32> {
        if self.state == Start {
            let _ = try!(self.read_metadata());
        }

        if self.row_count == 0 {
            let _ = try!(self.decode_mcu_row());
        }

        let len   = self.padded_width * self.num_components as uint;
        let slice = self.mcu_row.slice(self.row_count as uint * len,
        self.row_count as uint * len + buf.len());

        slice::bytes::copy_memory(buf, slice);

        self.row_count = (self.row_count + 1) % (self.vmax * 8);
        self.decoded_rows += 1;

        Ok(self.decoded_rows)
    }

    fn read_image(&mut self) -> ImageResult<Vec<u8>> {
        if self.state == Start {
            let _ = try!(self.read_metadata());
        }

        let row = try!(self.row_len());
        let mut buf = Vec::from_elem(row * self.height as uint, 0u8);

        for chunk in buf.as_mut_slice().chunks_mut(row) {
            let _len = try!(self.read_scanline(chunk));
        }

        Ok(buf)
    }
}

fn upsample_mcu(out: &mut [u8], xoffset: uint, width: uint, bpp: uint, mcu: &[u8], h: u8, v: u8) {
    if mcu.len() == 64 {
        for y in range(0u, 8) {
            for x in range(0u, 8) {
                out[xoffset + x + (y * width)] = mcu[x + y * 8]
            }
        }
    } else {
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
let vt:
    i32 = 1 << t as uint - 1;

    if v < vt {
    v + ((-1) << t as uint) + 1
    }
    else {
        v
    }
}
