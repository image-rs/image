extern crate deflate;

use std::borrow::Cow;
use std::error;
use std::fmt;
use std::io::{self, Write};
use std::mem;
use std::result;

use chunk;
use crc::Crc32;
use common::{AnimationControl, FrameControl, Info, ColorType, BitDepth};
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

    pub fn new_animated(w: W, width: u32, height: u32, frames: u32) -> Result<Encoder<W>> {
        if frames > 0 {
            let mut encoder = Encoder::new(w, width, height);

            let animation_ctl = AnimationControl { num_frames: frames, num_plays: 0 };
            let mut frame_ctl = FrameControl::default();
            frame_ctl.width = width;
            frame_ctl.height = height;

            encoder.info.animation_control = Some(animation_ctl);
            encoder.info.frame_control = Some(frame_ctl);

            Ok(encoder)
        } else {
            Err(EncodingError::Format("invalid number of frames for an animated PNG".into()))
        }

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
    separate_default_image: bool,
}

impl<W: Write> Writer<W> {
    fn new(w: W, info: Info) -> Writer<W> {
        let w = Writer { w: w, info: info, separate_default_image: false };
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
        
        match self.info {
            Info { animation_control: Some(anim_ctl), frame_control: Some(_), ..} => {
                let mut data = [0; 8];
                try!((&mut data[..]).write_be(anim_ctl.num_frames));
                try!((&mut data[4..]).write_be(anim_ctl.num_plays));
                try!(self.write_chunk(chunk::acTL, &data));
            }
            _ => {}
        };
        
        Ok(self)
    }

    pub fn write_chunk_with_fields(&mut self, name: [u8; 4], data: &[u8], fields: Option<&[u8]>) -> Result<()> {
        self.w.write_be(data.len() as u32 + (if fields.is_some() { fields.unwrap().len() as u32 } else { 0 }))?;
        self.w.write(&name)?;
        if fields.is_some() { try!(self.w.write(fields.unwrap())); }
        self.w.write(data)?;

        let mut crc = Crc32::new();
        crc.update(&name);
        if fields.is_some() { crc.update(fields.unwrap()); }
        crc.update(data);

        self.w.write_be(crc.checksum())?;
        Ok(())
    }

    pub fn write_chunk(&mut self, name: [u8; 4], data: &[u8]) -> Result<()> {
        self.write_chunk_with_fields(name, data, None)
    }

    /// Writes the image data.
    pub fn write_image_data(&mut self, data: &[u8]) -> Result<()> {
        let zlib = self.get_image_data(data)?;
        self.write_chunk(chunk::IDAT, &try!(zlib.finish()))
    }

    fn get_image_data(&mut self, data: &[u8]) -> Result<deflate::write::ZlibEncoder<Vec<u8>>> {
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
            current.copy_from_slice(&line);
            try!(zlib.write_all(&[filter_method as u8]));
            filter(filter_method, bpp, &prev, &mut current);
            try!(zlib.write_all(&current));
            mem::swap(&mut prev, &mut current);
        }
        Ok(zlib)
    }

    pub fn write_separate_default_image(&mut self, data: &[u8]) -> Result<()> {
        match self.info {
            Info { animation_control: Some(_), frame_control: Some(frame_control), ..} => {
                if frame_control.sequence_number != 0 {
                    Err(EncodingError::Format("separate default image provided after frame sequence has begun".into()))
                } else if self.separate_default_image {
                    Err(EncodingError::Format("default image already written".into()))
                } else {
                    self.separate_default_image = true;
                    self.write_image_data(data)
                }
            }
            _ => {
                Err(EncodingError::Format("default image provided for a non-animated PNG".into()))
            }
        }
    }

    #[allow(non_snake_case)]
    fn write_fcTL(&mut self) -> Result<()> {
        let frame_ctl = self.info.frame_control.ok_or(EncodingError::Format("cannot write fcTL for a non-animated PNG".into()))?;
        let mut data = [0u8; 26];

        (&mut data[..]).write_be(frame_ctl.sequence_number)?;
        (&mut data[4..]).write_be(frame_ctl.width)?;
        (&mut data[8..]).write_be(frame_ctl.height)?;
        (&mut data[12..]).write_be(frame_ctl.x_offset)?;
        (&mut data[16..]).write_be(frame_ctl.y_offset)?;
        (&mut data[20..]).write_be(frame_ctl.delay_num)?;
        (&mut data[22..]).write_be(frame_ctl.delay_den)?;
        data[24] = frame_ctl.dispose_op as u8;
        data[25] = frame_ctl.blend_op as u8;

        self.write_chunk(chunk::fcTL, &data)
    }

    #[allow(non_snake_case)]
    fn write_fdAT(&mut self, data: &[u8]) -> Result<()> {
        // println!("Writing fdAT:{:?}", self.info.frame_control.unwrap().sequence_number+1);

        let zlib = self.get_image_data(data)?;

        let mut data = [0u8; 4];
        (&mut data[..]).write_be(self.info.frame_control
                .ok_or(EncodingError::Format("cannot write fdAT for a non-animated PNG".into()))?.sequence_number+1u32)?;

        self.write_chunk_with_fields(chunk::fdAT, &zlib.finish()?, Some(&data))
    }

    pub fn write_frame(&mut self, data: &[u8]) -> Result<()> {
        // println!("{:?}", self.info.frame_control.unwrap().sequence_number);

        match self.info {
            Info { animation_control: Some(AnimationControl { num_frames: 0, ..}) , frame_control: Some(_), ..} => {
                Err(EncodingError::Format("exceeded number of frames specified".into()))
            },
            Info { animation_control: Some(anim_ctl), frame_control: Some(frame_control), ..} => {
                match frame_control.sequence_number {
                    0 => {
                        let ret = if self.separate_default_image { // If we've already written the default image we can write frames the normal way
                            // fcTL + fdAT

                            self.write_fcTL().and(self.write_fdAT(data))
                        } else { // If not we'll have to set the first frame to be both:
                            // fcTL + first frame (IDAT)

                            self.write_fcTL().and(self.write_image_data(data))
                        };

                        let mut fc = self.info.frame_control.unwrap();
                        fc.inc_seq_num(1);
                        self.info.frame_control = Some(fc);
                        ret
                    },
                    x if x == 2 * anim_ctl.num_frames - 1 => {
                        // println!("We're done, boss");

                        // This is the last frame:
                        // Do the usual and also set AnimationControl to no remaining frames:
                        let ret = self.write_fcTL().and(self.write_fdAT(data));
                        let mut fc = self.info.frame_control.unwrap();
                        fc.set_seq_num(0);
                        self.info.frame_control = Some(fc);
                        ret
                    },
                    _ => {
                        // Usual case:
                        // fcTL + fdAT
                        // println!("Buisness as usual");
                        let ret = self.write_fcTL().and(self.write_fdAT(data));
                        let mut fc = self.info.frame_control.unwrap();
                        fc.inc_seq_num(2);
                        self.info.frame_control = Some(fc);
                        ret
                    }
                }
            },
            _ => {
                Err(EncodingError::Format("frame provided for a non-animated PNG".into()))
            }
        }
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
