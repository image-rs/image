use std::io::{ self, Read };
use std::{ cmp, mem, iter, str, slice };
use std::iter::repeat;
use std::num::{ FromPrimitive, Float };
use byteorder::{ ReadBytesExt, BigEndian };

use image::{
    DecodingResult,
    ImageResult,
    ImageDecoder,
    ImageError
};
use color::{ self, ColorType };

use super::filter::unfilter;
use super::hash::Crc32;
use super::zlib::ZlibDecoder;


pub static PNGSIGNATURE: [u8; 8] = [137, 80, 78, 71, 13, 10, 26, 10];


#[derive(Copy, PartialEq)]
enum PNGState {
    Start,
    HaveSignature,
    HaveIHDR,
    HavePLTE,
    HaveFirstIDat,
    #[allow(dead_code)]
    HaveLastIDat,
    #[allow(dead_code)]
    HaveIEND
}

#[derive(Copy, FromPrimitive, Debug, PartialEq)]
enum InterlaceMethod {
    None = 0,
    Adam7 = 1
}

/// This iterator iterates over the different passes of an image Adam7 encoded
/// PNG image
/// The glorious pattern is:
///     16462646
///     77777777
///     56565656
///     77777777
///     36463646
///     77777777
///     56565656
///     77777777
///
#[derive(Copy)]
struct Adam7Iterator {
    line: u32,
    lines: u32,
    line_width: u32,
    current_pass: u8,
    width: u32,
    height: u32,
}

impl Adam7Iterator {
    pub fn new(width: u32, height: u32) -> Adam7Iterator {
        let mut this = Adam7Iterator {
            line: 0,
            lines: 0,
            line_width: 0,
            current_pass: 1,
            width: width,
            height: height
        };
        this.init_pass();
        this
    }

    /// Calculates the bounds of the current pass
    fn init_pass(&mut self) {
        let w = self.width as f64;
        let h = self.height as f64;
        let (line_width, lines) = match self.current_pass {
            1 => (w/8.0, h/8.0),
            2 => ((w-4.0)/8.0, h/8.0),
            3 => (w/4.0, (h-4.0)/8.0),
            4 => ((w-2.0)/4.0, h/4.0),
            5 => (w/2.0, (h-2.0)/4.0),
            6 => ((w-1.0)/2.0, h/2.0),
            7 => (w, (h-1.0)/2.0),
            _ => unreachable!()
        };
        self.line_width = line_width.ceil() as u32;
        self.lines = lines.ceil() as u32;
        self.line = 0;
    }
}

/// Iterates over the (passes, lines, widths)
impl Iterator for Adam7Iterator {
    type Item = (u8, u32, u32);
    fn next(&mut self) -> Option<(u8, u32, u32)> {
        if self.line < self.lines {
            let this_line = self.line;
            self.line += 1;
            Some((self.current_pass, this_line, self.line_width))
        } else if self.current_pass < 7 {
            self.current_pass += 1;
            self.init_pass();
            self.next()
        } else {
            None
        }
    }
}


/// The representation of a PNG decoder
///
/// Currently does not support decoding of interlaced images
pub struct PNGDecoder<R> {
    z: ZlibDecoder<IDATReader<R>>,
    crc: Crc32,
    previous: Vec<u8>,
    state: PNGState,

    width: u32,
    height: u32,

    bit_depth: u8,
    data_color_type: u8,
    data_pixel_type: ColorType,

    palette: Option<Vec<(u8, u8, u8)>>,
    trns: Option<Vec<u8>>,

    interlace_method: InterlaceMethod,
    pass_iterator: Option<Adam7Iterator>,

    chunk_length: u32,
    chunk_type: Vec<u8>,

    bpp: u8,
    bits_per_pixel: u8,
    decoded_rows: u32,
}

impl<R: Read> PNGDecoder<R> {
    /// Create a new decoder that decodes from the stream ```r```
    pub fn new(r: R) -> PNGDecoder<R> {
        let idat_reader = IDATReader::new(r);

        PNGDecoder {
            previous: Vec::new(),
            state: PNGState::Start,
            z: ZlibDecoder::new(idat_reader),
            crc: Crc32::new(),

            width: 0,
            height: 0,

            bit_depth: 0,
            data_color_type: 0,
            data_pixel_type: ColorType::Gray(1),

            palette: None,
            trns: None,

            interlace_method: InterlaceMethod::None,
            pass_iterator: None,

            chunk_length: 0,
            chunk_type: Vec::new(),

            bpp: 0,
            bits_per_pixel: 0,
            decoded_rows: 0,
        }
    }

    /// Returns a reference to the color palette used for indexed
    /// color images.
    /// Each array element is a tuple of RGB values.
    pub fn palette <'a>(&'a self) -> &'a [(u8, u8, u8)] {
        match self.palette {
            Some(ref p) => &p,
            None        => &[]
        }
    }

    fn read_signature(&mut self) -> ImageResult<bool> {
        let mut png = Vec::with_capacity(8);
        try!(self.z.inner().r.by_ref().take(8).read_to_end(&mut png));

        Ok(&png == &PNGSIGNATURE)
    }

    fn parse_ihdr(&mut self, buf: Vec<u8>) -> ImageResult<()> {
        self.crc.update(&*buf);
        let mut m = io::Cursor::new(buf);

        self.width = try!(m.read_u32::<BigEndian>());
        self.height = try!(m.read_u32::<BigEndian>());

        self.bit_depth = try!(try!(m.by_ref().bytes().next().ok_or(ImageError::ImageEnd)));
        self.data_color_type = try!(try!(m.by_ref().bytes().next().ok_or(ImageError::ImageEnd)));

        self.data_pixel_type = match (self.data_color_type, self.bit_depth) {
            (0, 1)  => ColorType::Gray(1),
            (0, 2)  => ColorType::Gray(2),
            (0, 4)  => ColorType::Gray(4),
            (0, 8)  => ColorType::Gray(8),
            (0, 16) => ColorType::Gray(16),
            (2, 8)  => ColorType::RGB(8),
            (2, 16) => ColorType::RGB(16),
            (3, 1)  => ColorType::RGB(8),
            (3, 2)  => ColorType::RGB(8),
            (3, 4)  => ColorType::RGB(8),
            (3, 8)  => ColorType::RGB(8),
            (4, 8)  => ColorType::GrayA(8),
            (4, 16) => ColorType::GrayA(16),
            (6, 8)  => ColorType::RGBA(8),
            (6, 16) => ColorType::RGBA(16),
            (_, _)  => return Err(ImageError::FormatError(
                "Invalid color/bit depth combination.".to_string()
            ))
        };

        let compression_method = try!(try!(m.by_ref().bytes().next().ok_or(ImageError::ImageEnd)));
        if compression_method != 0 {
            return Err(ImageError::UnsupportedError(format!(
                "The compression method {} is not supported.",
                compression_method
            )))
        }

        let filter_method = try!(try!(m.by_ref().bytes().next().ok_or(ImageError::ImageEnd)));
        if filter_method != 0 {
            return Err(ImageError::UnsupportedError(format!(
                "The filter method {} is not supported.",
                filter_method
            )))
        }

        self.interlace_method = match FromPrimitive::from_u8(try!(try!(m.by_ref().bytes().next().ok_or(ImageError::ImageEnd)))) {
            Some(method) => method,
            None => return Err(ImageError::UnsupportedError(
                "Unsupported interlace method.".to_string()
            ))
        };
        if self.interlace_method == InterlaceMethod::Adam7 {
            self.pass_iterator = Some(Adam7Iterator::new(self.width, self.height))
        }

        let channels = match self.data_color_type {
            0 => 1,
            2 => 3,
            3 => 1,
            4 => 2,
            6 => 4,
            _ => return Err(ImageError::FormatError("Unknown color type.".to_string()))
        };

        self.bits_per_pixel = channels * self.bit_depth;
        self.bpp = (self.bits_per_pixel + 7) / 8;
        self.previous = repeat(0u8).take(self.raw_row_length(self.width) as usize).collect();

        Ok(())
    }

    fn raw_row_length(&self, width: u32) -> u32 {
        (self.bits_per_pixel as u32 * width + 7) / 8
    }

    fn parse_trns(&mut self, len: usize) -> ImageResult<()> {
        let length =  match self.data_color_type {
            0 => 2,
            2 => 6,
            3 => len,
            _ => {
                return Err(ImageError::FormatError(format!(
                    "tRNS chunk may not appear for color type {}", self.data_color_type
                )))
            }
        };
        if length != len {
            return Err(ImageError::FormatError("Invalid tRNS chunk signature.".to_string()))
        }
        let mut buf = Vec::with_capacity(length as usize);
        try!(self.z.inner().r.by_ref().take(length as u64).read_to_end(&mut buf));
        self.crc.update(&*buf);
        self.trns = Some(buf);
        return Ok(())
    }

    fn parse_plte(&mut self, buf: Vec<u8>) -> ImageResult<()> {
        self.crc.update(&*buf);

        let len = buf.len() / 3;

        if len > 256 || len > (1 << self.bit_depth as usize) || buf.len() % 3 != 0{
            return Err(ImageError::FormatError("Color palette malformed.".to_string()))
        }

        let p: Vec<(u8, u8, u8)> = (0usize..256).map(|i| {
            if i < len {
                let r = buf[3 * i];
                let g = buf[3 * i + 1];
                let b = buf[3 * i + 2];

                (r, g, b)
            } else {
                (0, 0, 0)
            }
        }).collect();

        self.palette = Some(p);

        Ok(())
    }

    fn read_metadata(&mut self) -> ImageResult<()> {
        if !try!(self.read_signature()) {
            return Err(ImageError::FormatError("Could not read PNG signature.".to_string()))
        }

        self.state = PNGState::HaveSignature;

        loop {
            let length = try!(self.z.inner().r.read_u32::<BigEndian>());
            let mut chunk = Vec::with_capacity(4);
            try!(self.z.inner().r.by_ref().take(4).read_to_end(&mut chunk));

            self.chunk_length = length;
            self.chunk_type   = chunk.clone();

            self.crc.update(&*chunk);

            match (&*self.chunk_type, self.state) {
                (b"IHDR", PNGState::HaveSignature) => {
                    if length != 13 {
                        return Err(ImageError::FormatError("Invalid PNG signature.".to_string()))
                    }

                    let mut d = Vec::with_capacity(length as usize);
                    try!(self.z.inner().r.by_ref().take(length as u64).read_to_end(&mut d));
                    try!(self.parse_ihdr(d));

                    self.state = PNGState::HaveIHDR;
                }

                (b"PLTE", PNGState::HaveIHDR) => {
                    let mut d = Vec::with_capacity(length as usize);
                    try!(self.z.inner().r.by_ref().take(length as u64).read_to_end(&mut d));
                    try!(self.parse_plte(d));
                    self.state = PNGState::HavePLTE;
                }

                (b"tRNS", _) => {
                    try!(self.parse_trns(length as usize));
                }

                (b"IDAT", PNGState::HaveIHDR) if self.data_color_type != 3 => {
                    self.state = PNGState::HaveFirstIDat;
                    self.z.inner().set_inital_length(self.chunk_length);
                    self.z.inner().crc.update(&self.chunk_type);

                    break;
                }

                (b"IDAT", PNGState::HavePLTE) if self.data_color_type == 3 => {
                    self.state = PNGState::HaveFirstIDat;
                    self.z.inner().set_inital_length(self.chunk_length);
                    self.z.inner().crc.update(&self.chunk_type);

                    break;
                }

                _ => {
                    let mut b = Vec::with_capacity(length as usize);
                    try!(self.z.inner().r.by_ref().take(length as u64).read_to_end(&mut b));
                    self.crc.update(&*b);
                }
            }

            let chunk_crc = try!(self.z.inner().r.read_u32::<BigEndian>());
            let crc = self.crc.checksum();

            if crc != chunk_crc {
                return Err(ImageError::FormatError("CRC checksum invalid.".to_string()))
            }

            self.crc.reset();
        }

        Ok(())
    }

    fn extract_scanline(&mut self, buf: &mut [u8], rlength: u32) -> ImageResult<u32> {
        let filter_type = match FromPrimitive::from_u8(try!(try!(self.z.by_ref().bytes().next().ok_or(ImageError::ImageEnd)))) {
            Some(v) => v,
            _ => return Err(ImageError::FormatError("Unknown filter type.".to_string()))
        };

        {
            let mut read = 0usize;
            let read_buffer = &mut buf[..rlength as usize];
            while read < rlength as usize {
                let r = try!(self.z.read(&mut read_buffer[read..]));
                read += r;
            }
        }

        unfilter(filter_type, self.bpp as usize, &self.previous, &mut buf[..rlength as usize]);
        slice::bytes::copy_memory(&buf[..rlength as usize], &mut self.previous);


        if let Some(ref palette) = self.palette {
            expand_palette(buf, &palette, rlength as usize, self.bit_depth, self.trns.as_ref().map(|v| &**v));
        } else if let Some(ref trns) = self.trns {
            let mut _trns = [0u8; 4];
            let trns = match self.data_color_type {
                0 => {
                    _trns[0] = trns[1];
                    &_trns[..1]
                },
                2 => {
                    _trns[0] = trns[1];
                    _trns[2] = trns[5];
                    _trns[1] = trns[3];
                    &_trns[..3]
                },
                // panic is ok this should have been catched earlier
                _ => panic!("invalid color type for transparency")
            };
            if self.bit_depth < 8 {
                expand_trns_line_nbits(
                    buf,
                    trns[0],
                    rlength as usize,
                    self.bit_depth
                );
            } else {
                expand_trns_line(
                    buf,
                    trns,
                    rlength as usize,
                    color::num_components(self.data_pixel_type)
                );
            }
        }

        self.decoded_rows += 1;

        Ok(self.decoded_rows)
    }
}

impl<R: Read> ImageDecoder for PNGDecoder<R> {
    fn dimensions(&mut self) -> ImageResult<(u32, u32)> {
        if self.state == PNGState::Start {
            let _ = try!(self.read_metadata());
        }

        Ok((self.width, self.height))
    }

    fn colortype(&mut self) -> ImageResult<ColorType> {
        if self.state == PNGState::Start {
            let _ = try!(self.read_metadata());
        }
        let bits = self.bit_depth;
        if self.trns.is_some() {
            Ok(match self.data_pixel_type {
                ColorType::RGB(n) => ColorType::RGBA(n),
                ColorType::Gray(_) if bits == 1 || bits == 2 || bits == 4 => ColorType::GrayA(8),
                _ => return Err(ImageError::FormatError(
                    "Invalid transparency data".to_string()
                ))
            })
        } else {
            Ok(self.data_pixel_type)
        }
    }

    fn row_len(&mut self) -> ImageResult<usize> {
        if self.state == PNGState::Start {
            let _ = try!(self.read_metadata());
        }

        let bits = color::bits_per_pixel(try!(self.colortype()));

        Ok((bits * self.width as usize + 7) / 8)
    }

    fn read_scanline(&mut self, buf: &mut [u8]) -> ImageResult<u32> {
        if self.state == PNGState::Start {
            let _ = try!(self.read_metadata());
        }
        if self.interlace_method != InterlaceMethod::None {
            return Err(ImageError::UnsupportedError("Image is interlaced, extraction of single scanlines is unsupported".to_string()))
        }
        let rlength = self.raw_row_length(self.width);
        self.extract_scanline(buf, rlength)
    }

    fn read_image(&mut self) -> ImageResult<DecodingResult> {
        if self.state == PNGState::Start {
            let _ = try!(self.read_metadata());
        }
        let max_rowlen = try!(self.row_len());
        let mut buf: Vec<u8> = repeat(0u8).take(max_rowlen * self.height as usize).collect();
        if let Some(pass_iterator) = self.pass_iterator { // Method == Adam7
            let mut pass_buf: Vec<u8> = repeat(0u8).take(max_rowlen).collect();
            let mut old_pass = 1;
            let bytes = color::bits_per_pixel(try!(self.colortype()))/8;
            for (pass, line, width) in pass_iterator {
                let rlength = self.raw_row_length(width);
                if old_pass != pass {
                    // new subimage, reset previous
                    for v in self.previous.iter_mut() {
                        *v = 0;
                    }
                }
                let bits = color::bits_per_pixel(try!(self.colortype()));
                let _ = try!(
                    self.extract_scanline(&mut pass_buf[..
                        ((bits * width as usize + 7) / 8)
                    ], rlength)
                );
                expand_pass(
                    &mut buf, self.width * bytes as u32,
                    &mut pass_buf[..width as usize * bytes], pass, line, bytes as u8
                );
                old_pass = pass;
            }
            Ok(DecodingResult::U8(buf))
        } else {
            for chunk in buf.chunks_mut(max_rowlen) {
                let _ = try!(self.read_scanline(chunk));
            }
            Ok(DecodingResult::U8(buf))
        }
    }
}

macro_rules! expand_pass(
    ($img:expr, $scanline:expr, $j:ident, $pos:expr, $bytes_pp:expr) => {
        for ($j, pixel) in $scanline.chunks($bytes_pp).enumerate() {
            for (offset, val) in pixel.iter().enumerate() {
                $img[$pos + offset] = *val
            }
        }
    }
);

fn expand_pass(
    img: &mut[u8], width: u32, scanline: &mut[u8],
    pass: u8, line_no: u32, bytes_pp: u8) {
    let line_no = line_no as usize;
    let width = width as usize;
    let bytes_pp = bytes_pp as usize;
    match pass {
        1 => expand_pass!(img, scanline, j,  8*line_no    * width + bytes_pp * j*8     , bytes_pp),
        2 => expand_pass!(img, scanline, j,  8*line_no    * width + bytes_pp *(j*8 + 4), bytes_pp),
        3 => expand_pass!(img, scanline, j, (8*line_no+4) * width + bytes_pp * j*4     , bytes_pp),
        4 => expand_pass!(img, scanline, j,  4*line_no    * width + bytes_pp *(j*4 + 2), bytes_pp),
        5 => expand_pass!(img, scanline, j, (4*line_no+2) * width + bytes_pp * j*2     , bytes_pp),
        6 => expand_pass!(img, scanline, j,  2*line_no    * width + bytes_pp *(j*2+1)  , bytes_pp),
        7 => expand_pass!(img, scanline, j, (2*line_no+1) * width + bytes_pp * j       , bytes_pp),
        _ => {}
    }
}

#[inline(always)]
fn expand_packed<F>(buf: &mut [u8], channels: usize, bit_depth: u8, func: F)
where F: Fn(u8, &mut[u8]) {
    let entries = buf.len()/channels/(8/bit_depth as usize);
    let mask = ((1u16 << bit_depth) - 1) as u8;
    let i =
        (0..entries)
        .rev() // Reverse iterator
        .flat_map(|idx|
            // This has to be reversed to
            (0 .. 8).step_by(bit_depth)
            .zip(iter::iterate(
                idx, |idx| idx
            )
        ));
    let channels = channels as isize;
    let j = (buf.len() as isize - channels..-(channels)).step_by(-channels);
    //let j = (0..buf.len()).step_by(channels).rev() // ideal solution;
    for ((shift, i), j) in i.zip(j) {
        let pixel = (buf[i] & (mask << shift)) >> shift;
        func(pixel, &mut buf[j as usize..(j + channels) as usize])
    }
}

fn expand_trns_line(buf: &mut[u8], trns: &[u8], entries: usize, channels: usize) {
    let channels = channels as isize;
    let i = (buf.len() as isize / (channels+1) * channels - channels..-(channels)).step_by(-channels);
    let j = (buf.len() as isize - (channels+1)..-(channels+1)).step_by(-(channels+1));
    let channels = channels as usize;
    for (i, j) in i.zip(j) {
        let i_pixel = i as usize;
        let j_chunk = j as usize;
        if &buf[i_pixel..i_pixel+channels] == trns {
            buf[j_chunk+channels] = 0
        } else {
            buf[j_chunk+channels] = 0xFF
        }
        for k in (0..channels).rev() {
            buf[j_chunk+k] = buf[i_pixel+k];
        }
    }
}

fn expand_trns_line_nbits(buf: &mut[u8], trns: u8, entries: usize, bit_depth: u8) {
    let scaling_factor = (255)/((1u16 << bit_depth) - 1) as u8;
    expand_packed(buf, 2, bit_depth, |pixel, chunk| {
        if pixel == trns {
            chunk[1] = 0
        } else {
            chunk[1] = 0xFF
        }
        chunk[0] = pixel * scaling_factor
    })
}

fn expand_palette(buf: &mut[u8], palette: &[(u8, u8, u8)],
                  entries: usize, bit_depth: u8, trns: Option<&[u8]>) {
    let bpp = 8 / bit_depth as usize;
    let extra = if trns.is_some() { entries * bpp } else { 0 };
    assert_eq!(buf.len(), 3 * (entries * bpp - buf.len() % bpp) + extra);
    if let Some(trns) = trns {
        expand_packed(buf, 4, bit_depth, |i, chunk| {
            let ((r, g, b), a) = (
                palette[i as usize],
                *trns.get(i as usize).unwrap_or(&0xFF)
            );
            chunk[0] = r;
            chunk[1] = g;
            chunk[2] = b;
            chunk[3] = a;
        })
    } else {
        expand_packed(buf, 3, bit_depth, |i, chunk| {
            let (r, g, b) = palette[i as usize];
            chunk[0] = r;
            chunk[1] = g;
            chunk[2] = b;
        })
    }
}

pub struct IDATReader<R> {
    pub r: R,
    pub crc: Crc32,

    eof: bool,
    chunk_length: u32,
}

impl<R: Read> IDATReader<R> {
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

impl<R: Read> Read for IDATReader<R> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        if self.eof {
            return Ok(0);
        }

        let len = buf.len();
        let mut start = 0;

        while start < len {
            let m = cmp::min(len - start, self.chunk_length as usize);

            let slice = &mut buf[start..start + m];
            let r = try!(self.r.read(slice));

            start += r;

            self.chunk_length -= r as u32;
            self.crc.update(&*slice);

            if self.chunk_length == 0 {
                let chunk_crc = try!(self.r.read_u32::<BigEndian>());
                let crc = self.crc.checksum();

                if crc != chunk_crc {
                    return Ok(0);
                }

                self.crc.reset();
                self.chunk_length = try!(self.r.read_u32::<BigEndian>());

                let mut v = Vec::with_capacity(4);
                try!(self.r.by_ref().take(4).read_to_end(&mut v));
                self.crc.update(&v);

                match str::from_utf8(&v) {
                    Ok("IDAT") => (),
                    _ => {
                        self.eof = true;
                        break
                    }
                }
            }
        }

        Ok(start)
    }
}