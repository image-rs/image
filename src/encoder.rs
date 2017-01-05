extern crate deflate;

use std::borrow::Cow;
use std::io::{self, Write};
use std::result;
use std::fmt;
use std::error;

use chunk;
use crc::Crc32;
use common::{Info, ColorType, BitDepth};
use filter::{FilterType, filter};
use traits::{WriteBytesExt, HasParameters, Parameter};

pub type Result<T> = result::Result<T, EncodingError>;

#[derive(Debug)]
pub enum EncodingError {
    IoError(io::Error),
    Format(Cow<'static, str>),
}

impl error::Error for EncodingError {
    fn description(&self) -> &str {
        use self::EncodingError::*;
        match *self {
            IoError(ref err) => err.description(),
            Format(ref desc) => &desc,
        }
    }
}

impl fmt::Display for EncodingError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> result::Result<(), fmt::Error> {
        write!(fmt, "{}", (self as &error::Error).description())
    }
}

impl From<io::Error> for EncodingError {
    fn from(err: io::Error) -> EncodingError {
        EncodingError::IoError(err)
    }
}
impl From<EncodingError> for io::Error {
    fn from(err: EncodingError) -> io::Error {
        io::Error::new(io::ErrorKind::Other, (&err as &error::Error).description())
    }
}

/// PNG Encoder
pub struct Encoder<W: Write> {
    w: W,
    info: Info,
}

impl<W: Write> Encoder<W> {
    pub fn new(w: W, width: u32, height: u32) -> Encoder<W> {
        let mut info = Info::default();
        info.width = width;
        info.height = height;
        Encoder { w: w, info: info }
    }

    pub fn write_header(self) -> Result<Writer<W>> {
        Writer::new(self.w, self.info).init()
    }
}

impl<W: Write> HasParameters for Encoder<W> {}

impl<W: Write> Parameter<Encoder<W>> for ColorType {
    fn set_param(self, this: &mut Encoder<W>) {
        this.info.color_type = self
    }
}

impl<W: Write> Parameter<Encoder<W>> for BitDepth {
    fn set_param(self, this: &mut Encoder<W>) {
        this.info.bit_depth = self
    }
}

/// PNG writer
pub struct Writer<W: Write> {
    w: W,
    info: Info,
}

impl<W: Write> Writer<W> {
    fn new(w: W, info: Info) -> Writer<W> {
        let w = Writer { w: w, info: info };
        w
    }

    fn init(mut self) -> Result<Self> {
        try!(self.w.write(&[137, 80, 78, 71, 13, 10, 26, 10]));
        let mut data = [0; 13];
        try!((&mut data[..]).write_be(self.info.width));
        try!((&mut data[4..]).write_be(self.info.height));
        data[8] = self.info.bit_depth as u8;
        data[9] = self.info.color_type as u8;
        data[12] = if self.info.interlaced { 1 } else { 0 };
        try!(self.write_chunk(chunk::IHDR, &data));
        Ok(self)
    }

    pub fn write_chunk(&mut self, name: [u8; 4], data: &[u8]) -> Result<()> {
        try!(self.w.write_be(data.len() as u32));
        try!(self.w.write(&name));
        try!(self.w.write(data));
        let mut crc = Crc32::new();
        crc.update(&name);
        crc.update(data);
        try!(self.w.write_be(crc.checksum()));
        Ok(())
    }

    /// Writes the image data.
    pub fn write_image_data(&mut self, data: &[u8]) -> Result<()> {
        let bpp = self.info.bytes_per_pixel();
        let in_len = self.info.raw_row_length() - 1;
        let mut prev = vec![0; in_len];
        let mut current = vec![0; in_len];
        let data_size = in_len * self.info.height as usize;
        if data.len() < data_size || data_size == 0 {
            return Err(EncodingError::Format("not enough image data provided".into()));
        }
        let mut zlib = deflate::write::ZlibEncoder::new(Vec::new(), deflate::Compression::Fast);
        let filter_method = FilterType::Sub;
        for line in data.chunks(in_len) {
            ::utils::copy_memory(&line, &mut current);
            try!(zlib.write_all(&[filter_method as u8]));
            filter(filter_method, bpp, &prev, &mut current);
            try!(zlib.write_all(&current));
            ::utils::copy_memory(&current, &mut prev);
        }
        self.write_chunk(chunk::IDAT, &try!(zlib.finish()))
    }
}

impl<W: Write> Drop for Writer<W> {
    fn drop(&mut self) {
        let _ = self.write_chunk(chunk::IEND, &[]);
    }
}

#[test]
fn roundtrip() {
    use std::fs::File;
    // Decode image
    let decoder = ::Decoder::new(File::open("tests/pngsuite/basi0g01.png").unwrap());
    let (info, mut reader) = decoder.read_info().unwrap();
    let mut buf = vec![0; info.buffer_size()];
    reader.next_frame(&mut buf).unwrap();
    // Encode decoded image
    let mut out = Vec::new();
    {
        let mut encoder = Encoder::new(&mut out, info.width, info.height).write_header().unwrap();
        encoder.write_image_data(&buf).unwrap();
    }
    // Decode encoded decoded image
    let decoder = ::Decoder::new(&*out);
    let (info, mut reader) = decoder.read_info().unwrap();
    let mut buf2 = vec![0; info.buffer_size()];
    reader.next_frame(&mut buf2).unwrap();
    // check if the encoded image is ok:
    assert_eq!(buf, buf2);
}
