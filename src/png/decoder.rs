use std::io;
use std::cmp;
use std::mem;
use std::iter;
use std::str;
use std::slice;
use std::io::IoResult;
use std::io::MemReader;

use image;
use image::ImageResult;
use image::ImageDecoder;
use color;

use super::filter::unfilter;
use super::hash::Crc32;
use super::zlib::ZlibDecoder;

macro_rules! io_try(
    ($e: expr) => (
        match $e {
            Ok(e) => e,
            Err(_) => return Err(image::IoError)
        }
    )
)

pub static PNGSIGNATURE: [u8, ..8] = [137, 80, 78, 71, 13, 10, 26, 10];

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
    colour_type: u8,
    pixel_type: color::ColorType,

    palette: Option<Vec<(u8, u8, u8)>>,

    interlace_method: u8,

    chunk_length: u32,
    chunk_type: Vec<u8>,

    bpp: uint,
    rlength: uint,
    decoded_rows: u32,
}

impl<R: Reader> PNGDecoder<R> {
    /// Create a new decoder that decodes from the stream ```r```
    pub fn new(r: R) -> PNGDecoder<R> {
        let idat_reader = IDATReader::new(r);

        PNGDecoder {
            pixel_type: color::Grey(1),
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
            decoded_rows: 0,
        }
    }

    ///Returns a reference to the color palette used for indexed
    ///color images.
    ///Each array element is a tuple of RGB values.
    pub fn palette < 'a>(&'a self) -> &'a [(u8, u8, u8)] {
        match self.palette {
            Some(ref p) => p.as_slice(),
            None        => &[]
        }
    }

    fn read_signature(&mut self) -> ImageResult<bool> {
        let png = io_try!(self.z.inner().r.read_exact(8));

        Ok(png.as_slice() == PNGSIGNATURE)
    }

    fn parse_ihdr(&mut self, buf: Vec<u8>) -> ImageResult<()> {
        self.crc.update(buf.as_slice());
        let mut m = MemReader::new(buf);

        self.width = m.read_be_u32().unwrap();
        self.height = m.read_be_u32().unwrap();

        self.bit_depth = m.read_byte().unwrap();
        self.colour_type = m.read_byte().unwrap();

        self.pixel_type = match (self.colour_type, self.bit_depth) {
            (0, 1)  => color::Grey(1),
            (0, 2)  => color::Grey(2),
            (0, 4)  => color::Grey(4),
            (0, 8)  => color::Grey(8),
            (0, 16) => color::Grey(16),
            (2, 8)  => color::RGB(8),
            (2, 16) => color::RGB(16),
            (3, 1)  => color::RGB(8),
            (3, 2)  => color::RGB(8),
            (3, 4)  => color::RGB(8),
            (3, 8)  => color::RGB(8),
            (4, 8)  => color::GreyA(8),
            (4, 16) => color::GreyA(16),
            (6, 8)  => color::RGBA(8),
            (6, 16) => color::RGBA(16),
            (_, _)  => return Err(image::FormatError)
        };

        let compression_method = m.read_byte().unwrap();
        if compression_method != 0 {
            return Err(image::UnsupportedError)
        }

        let filter_method = m.read_byte().unwrap();
        if filter_method != 0 {
            return Err(image::UnsupportedError)
        }

        self.interlace_method = m.read_byte().unwrap();
        if self.interlace_method != 0 {
            return Err(image::UnsupportedError)
        }

        let channels = match self.colour_type {
            0 => 1,
            2 => 3,
            3 => 1,
            4 => 2,
            6 => 4,
            _ => return Err(image::FormatError)
        };

        let bits_per_pixel = channels * self.bit_depth as uint;

        self.rlength = (bits_per_pixel * self.width as uint + 7) / 8;
        self.bpp = (bits_per_pixel + 7) / 8;
        self.previous = Vec::from_elem(self.rlength, 0u8);

        Ok(())
    }

    fn parse_plte(&mut self, buf: Vec<u8>) -> ImageResult<()> {
        self.crc.update(buf.as_slice());

        let len = buf.len() / 3;

        if len > 256 || len > (1 << self.bit_depth as uint) || buf.len() % 3 != 0{
            return Err(image::FormatError)
        }

        let p = Vec::from_fn(256, |i| {
            if i < len {
                let r = buf[3 * i];
                let g = buf[3 * i + 1];
                let b = buf[3 * i + 2];

                (r, g, b)
            } else {
                (0, 0, 0)
            }
        });

        self.palette = Some(p);

        Ok(())
    }

    fn read_metadata(&mut self) -> ImageResult<()> {
        if !try!(self.read_signature()) {
            return Err(image::FormatError)
        }

        self.state = HaveSignature;

        loop {
            let length = io_try!(self.z.inner().r.read_be_u32());
            let chunk  = io_try!(self.z.inner().r.read_exact(4));

            self.chunk_length = length;
            self.chunk_type   = chunk.clone();

            self.crc.update(chunk);
            
            match (self.chunk_type.as_slice(), self.state) {
                (b"IHDR", HaveSignature) => {
                    if length != 13 {
                        return Err(image::FormatError)
                    }

                    let d = io_try!(self.z.inner().r.read_exact(length as uint));
                    try!(self.parse_ihdr(d));

                    self.state = HaveIHDR;
                }

                (b"PLTE", HaveIHDR) => {
                    let d = io_try!(self.z.inner().r.read_exact(length as uint));
                    try!(self.parse_plte(d));
                    self.state = HavePLTE;
                }

                (b"tRNS", HavePLTE) => {
                    return Err(image::UnsupportedError)
                }

                (b"IDAT", HaveIHDR) if self.colour_type != 3 => {
                    self.state = HaveFirstIDat;
                    self.z.inner().set_inital_length(self.chunk_length);
                    self.z.inner().crc.update(self.chunk_type.as_slice());

                    break;
                }

                (b"IDAT", HavePLTE) if self.colour_type == 3 => {
                    self.state = HaveFirstIDat;
                    self.z.inner().set_inital_length(self.chunk_length);
                    self.z.inner().crc.update(self.chunk_type.as_slice());

                    break;
                }

                _ => {
                    let b = io_try!(self.z.inner().r.read_exact(length as uint));
                    self.crc.update(b);
                }
            }

            let chunk_crc = io_try!(self.z.inner().r.read_be_u32());
            let crc = self.crc.checksum();

            if crc != chunk_crc {
                return Err(image::FormatError)
            }

            self.crc.reset();
        }

        Ok(())
    }
}

impl<R: Reader> ImageDecoder for PNGDecoder<R> {
    fn dimensions(&mut self) -> ImageResult<(u32, u32)> {
        if self.state == Start {
            let _ = try!(self.read_metadata());
        }

        Ok((self.width, self.height))
    }

    fn colortype(&mut self) -> ImageResult<color::ColorType> {
        if self.state == Start {
            let _ = try!(self.read_metadata());
        }

        Ok(self.pixel_type)
    }

    fn row_len(&mut self) -> ImageResult<uint> {
        if self.state == Start {
            let _ = try!(self.read_metadata());
        }

        let bits = color::bits_per_pixel(self.pixel_type);

        Ok((bits * self.width as uint + 7) / 8)
    }

    fn read_scanline(&mut self, buf: &mut [u8]) -> ImageResult<u32> {
        if self.state == Start {
            let _ = try!(self.read_metadata());
        }

        let filter_type = match FromPrimitive::from_u8(io_try!(self.z.read_byte())) {
            Some(v) => v,
            _ => return Err(image::FormatError)
        };

        {
            let mut read = 0;
            let read_buffer = buf.mut_slice_to(self.rlength);
            while read < self.rlength {
                let r = io_try!(self.z.read(read_buffer.mut_slice_from(read)));
                read += r;
            }
        }

        unfilter(filter_type, self.bpp, self.previous.as_slice(), buf.mut_slice_to(self.rlength));
        slice::bytes::copy_memory(self.previous.as_mut_slice(), buf.slice_to(self.rlength));

        if self.palette.is_some() {
            let s = (*self.palette.get_ref()).as_slice();
            expand_palette(buf, s, self.rlength, self.bit_depth);
        }

        self.decoded_rows += 1;

        Ok(self.decoded_rows)
    }

    fn read_image(&mut self) -> ImageResult<Vec<u8>> {
        if self.state == Start {
            let _ = try!(self.read_metadata());
        }

        let rowlen  = try!(self.row_len());
        let mut buf = Vec::from_elem(rowlen * self.height as uint, 0u8);

        for chunk in buf.as_mut_slice().mut_chunks(rowlen) {
            let _ = try!(self.read_scanline(chunk));
        }

        Ok(buf)
    }
}

fn expand_palette(buf: &mut[u8], palette: &[(u8, u8, u8)], 
                  entries: uint, bit_depth: u8) {
    assert!(buf.len() == entries * 3 * (8 / bit_depth as uint));
    let mask = (1u8 << bit_depth as uint) - 1;
    // Unsafe copy to be able to create a mutable borrow afterwards
    // This is unproblematic since we are iterating from opposite directions
    // over these slices such that the paletted pixel do not get overwritten
    // before processing them.
    let data = unsafe {
        let view: &mut [u8] = mem::transmute_copy(&buf);
        view.slice_to(entries)
    };
    let pixels = data
        .iter()
        .rev() // Reverse iterator
        .flat_map(|&v|
            // This has to be reversed to
            iter::range_step_inclusive(8i8-(bit_depth as i8), 0, -(bit_depth as i8))
            .zip(iter::iterate(
                |v| v, v
            )
        ))
        .map(|(shift, pixel)| (pixel & mask << shift as uint) >> shift as uint);
    for (chunk, (r, g, b)) in buf.mut_chunks(3).rev().zip(pixels.map(|i| 
        palette[i as uint]
    )) {
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
            let m = cmp::min(len - start, self.chunk_length as uint);

            let slice = buf.mut_slice(start, start + m);
            let r = try!(self.r.read(slice));

            start += r;

            self.chunk_length -= r as u32;
            self.crc.update(slice.as_slice());

            if self.chunk_length == 0 {
                let chunk_crc = try!(self.r.read_be_u32());
                let crc = self.crc.checksum();

                if crc != chunk_crc {
                    return Err(io::standard_error(io::InvalidInput))
                }

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
        }

        Ok(start)
    }
}

#[cfg(test)]
mod tests {
    extern crate glob;
    extern crate core;

    use std::io::File;

    use image::{
        ImageDecoder,
        ImageResult
    };

    use super::PNGDecoder;

    /// Filters the testsuite images for certain features
    fn get_testimages(feature: &str, color_type: &str, test_interlaced: bool) -> Vec<Path> {
        //"./src/png/testdata/pngsuite/*.png"
        let pattern = Path::new(".").join_many(["src", "png", "testdata", "pngsuite", "*.png"]);

        let mut paths = glob::glob(pattern.as_str().unwrap())
            .filter(|ref p| p.filename_str().unwrap().starts_with(feature))
            .filter(|ref p| p.filename_str().unwrap().contains(color_type));

        let ret: Vec<Path> = if test_interlaced {
            paths.collect()
        } else {
            paths.filter(|ref p| !p.filename_str()
                 .unwrap()
                 .slice_from(2)
                 .contains("i"))
                 .collect()
        };

        assert!(ret.len() > 0) // fail if no testimages are available
        ret
    }

    fn load_image(path: &Path) -> ImageResult<Vec<u8>> {
        PNGDecoder::new(File::open(path)).read_image()
    }

    #[test]
    /// Test image filters
    fn test_filters() {
        let images = get_testimages("f", "", false);

        for path in images.iter() {
            assert!(match load_image(path) {
                Ok(_) => true,
                Err(err) => { println!("file {}, failed with {}", path.display(), err); false }
            })
        }
    }
    #[test]
    /// Test basic formats filters
    fn test_basic() {
        let images = get_testimages("b", "2c", false);

        for path in images.iter() {
            assert!(match load_image(path) {
                Ok(_) => true,
                Err(err) => {println!("file {}, failed with {}", path.display(), err); false }
            })
        }
    }

    #[test]
    /// Chunk ordering
    fn test_chunk_ordering() {
        let images = get_testimages("o", "", false);

        for path in images.iter() {
            assert!(match load_image(path) {
                Ok(_) => { true },
                Err(err) => {println!("file {}, failed with {}", path.display(), err); false }
            })
        }
    }

    #[test]
    /// Test corrupted images, they should all fail
    fn test_corrupted() {
        let images = get_testimages("x", "", true);
        let num_images = images.len();
        let mut fails = 0;

        for path in images.iter() {
            match load_image(path) {
                Ok(_) => println!("corrupted file {} did not fail", path.display()),
                Err(_) => {
                    fails += 1;
                }
            }
        }

        assert_eq!(num_images, fails)
    }
}