use std::io::{self, Read, Seek};
use std::mem;
use num_traits::{FromPrimitive, Num};
use std::collections::HashMap;

use image;
use image::{
    ImageError,
    ImageResult,
    ImageDecoder,
    DecodingResult,
    DecodingBuffer
};

use color::{ColorType};

use self::ifd::Directory;

use self::stream::{
    ByteOrder,
    EndianReader,
    SmartReader,
    LZWReader
};

mod ifd;
mod stream;

enum_from_primitive! {
#[derive(Clone, Copy, Debug, PartialEq)]
enum PhotometricInterpretation {
    WhiteIsZero = 0,
    BlackIsZero = 1,
    RGB = 2,
    RGBPalette = 3,
    TransparencyMask = 4,
    CMYK = 5,
    YCbCr = 6,
    CIELab = 8,
}
}

enum_from_primitive! {
#[derive(Clone, Copy, Debug)]
enum CompressionMethod {
    None = 1,
    Huffman = 2,
    Fax3 = 3,
    Fax4 = 4,
    LZW = 5,
    JPEG = 6,
    PackBits = 32773
}
}

enum_from_primitive! {
#[derive(Clone, Copy, Debug)]
enum PlanarConfiguration {
    Chunky = 1,
    Planar = 2
}
}

enum_from_primitive! {
#[derive(Clone, Copy, Debug)]
enum Predictor {
    None = 1,
    Horizontal = 2
}
}

/// The representation of a TIFF decoder
///
/// Currently does not support decoding of interlaced images
#[derive(Debug)]
pub struct TIFFDecoder<R> where R: Read + Seek {
    reader: SmartReader<R>,
    byte_order: ByteOrder,
    next_ifd: Option<u32>,
    ifd: Option<Directory>,
    width: u32,
    height: u32,
    bits_per_sample: Vec<u8>,
    samples: u8,
    photometric_interpretation: PhotometricInterpretation,
    compression_method: CompressionMethod
}

trait Wrapping {
    fn wrapping_add(&self, other: Self) -> Self;
}

impl Wrapping for u8 {
    fn wrapping_add(&self, other: Self) -> Self {
        u8::wrapping_add(*self, other)
    }
}

impl Wrapping for u16 {
    fn wrapping_add(&self, other: Self) -> Self {
        u16::wrapping_add(*self, other)
    }
}

fn rev_hpredict_nsamp<T>(mut image: Vec<T>,
                         size: (u32, u32),
                         samples: usize)
                         -> Vec<T>
                         where T: Num + Copy + Wrapping {
    let width = size.0 as usize;
    let height = size.1 as usize;
    for row in 0..height {
        for col in samples..width * samples {
            let prev_pixel = image[(row * width * samples + col - samples)];
            let pixel = &mut image[(row * width * samples + col)];
            *pixel = pixel.wrapping_add(prev_pixel);
        }
    }
    image
}

fn rev_hpredict(image: DecodingResult, size: (u32, u32), color_type: ColorType) -> ImageResult<DecodingResult> {
    let samples = match color_type {
        ColorType::Gray(8) | ColorType::Gray(16) => 1,
        ColorType::RGB(8) | ColorType::RGB(16) => 3,
        ColorType::RGBA(8) | ColorType::RGBA(16) => 4,
        _ => return Err(ImageError::UnsupportedError(format!(
            "Horizontal predictor for {:?} is unsupported.", color_type
        )))
    };
    Ok(match image {
        DecodingResult::U8(buf) => {
            DecodingResult::U8(rev_hpredict_nsamp(buf, size, samples))
        },
        DecodingResult::U16(buf) => {
            DecodingResult::U16(rev_hpredict_nsamp(buf, size, samples))
        }
    })
}

impl<R: Read + Seek> TIFFDecoder<R> {
    /// Create a new decoder that decodes from the stream ```r```
    pub fn new(r: R) -> ImageResult<TIFFDecoder<R>> {
        TIFFDecoder {
            reader: SmartReader::wrap(r, ByteOrder::LittleEndian),
            byte_order: ByteOrder::LittleEndian,
            next_ifd: None,
            ifd: None,
            width: 0,
            height: 0,
            bits_per_sample: vec![1],
            samples: 1,
            photometric_interpretation: PhotometricInterpretation::BlackIsZero,
            compression_method: CompressionMethod::None
        }.init()
    }

    fn read_header(&mut self) -> ImageResult<()> {
        let mut endianess = Vec::with_capacity(2);
        try!(self.reader.by_ref().take(2).read_to_end(&mut endianess));
        match &*endianess {
            b"II" => {
                self.byte_order = ByteOrder::LittleEndian;
                self.reader.byte_order = ByteOrder::LittleEndian; },
            b"MM" => {
                self.byte_order = ByteOrder::BigEndian;
                self.reader.byte_order = ByteOrder::BigEndian;  },
            _ => return Err(image::ImageError::FormatError(
                "TIFF signature not found.".to_string()
            ))
        }
        if try!(self.read_short()) != 42 {
            return Err(image::ImageError::FormatError("TIFF signature invalid.".to_string()))
        }
        self.next_ifd = match try!(self.read_long()) {
            0 => None,
            n => Some(n)
        };
        Ok(())
    }

    /// Initializes the decoder.
    pub fn init(self) -> ImageResult<TIFFDecoder<R>> {
        self.next_image()
    }

    /// Reads in the next image.
    /// If there is no further image in the TIFF file a format error is returned.
    /// To determine whether there are more images call `TIFFDecoder::more_images` instead.
    pub fn next_image(mut self) -> ImageResult<TIFFDecoder<R>> {
        try!(self.read_header());
        self.ifd = Some(try!(self.read_ifd()));
        self.width = try!(self.get_tag_u32(ifd::Tag::ImageWidth));
        self.height = try!(self.get_tag_u32(ifd::Tag::ImageLength));
        self.photometric_interpretation = match FromPrimitive::from_u32(
            try!(self.get_tag_u32(ifd::Tag::PhotometricInterpretation))
        ) {
            Some(val) => val,
            None => return Err(image::ImageError::UnsupportedError(
                "The image is using an unknown photometric interpretation.".to_string()
            ))
        };
        match try!(self.find_tag_u32(ifd::Tag::Compression)) {
            Some(val) => match FromPrimitive::from_u32(val) {
                Some(method) =>  {
                    self.compression_method = method
                },
                None => return Err(image::ImageError::UnsupportedError(
                    "Unknown compression method.".to_string()
                ))
            },
            None => {}
        }
        match try!(self.find_tag_u32(ifd::Tag::SamplesPerPixel)) {
            Some(val) => {
                self.samples = val as u8
            },
            None => {}
        }
        match self.samples {
            1 => {
                match try!(self.find_tag_u32(ifd::Tag::BitsPerSample)) {
                    Some(val) => {
                        self.bits_per_sample = vec![val as u8]
                    },
                    None => {}
                }
            }
            3 | 4 => {
                match try!(self.find_tag_u32_vec(ifd::Tag::BitsPerSample)) {
                    Some(val) => {
                        self.bits_per_sample = val.iter().map(|&v| v as u8).collect()
                    },
                    None => {}
                }

            }
            _ => return Err(image::ImageError::UnsupportedError(
                format!("{} samples per pixel is supported.", self.samples)
            ))
        }
        Ok(self)
    }

    /// Returns `true` if there is at least one more image available.
    pub fn more_images(&self) -> bool {
        match self.next_ifd {
            Some(_) => true,
            None => false
        }
    }

    /// Returns the byte_order
    pub fn byte_order(&self) -> ByteOrder {
        self.byte_order
    }

    /// Reads a TIFF short value
    #[inline]
    pub fn read_short(&mut self) -> Result<u16, io::Error> {
        self.reader.read_u16()
    }

    /// Reads a TIFF long value
    #[inline]
    pub fn read_long(&mut self) -> Result<u32, io::Error> {
        self.reader.read_u32()
    }

    /// Reads a TIFF IFA offset/value field
    #[inline]
    pub fn read_offset(&mut self) -> Result<[u8; 4], io::Error> {
        let mut val = [0; 4];
        try!(self.reader.read_exact(&mut val));
        Ok(val)
    }

    /// Moves the cursor to the specified offset
    #[inline]
    pub fn goto_offset(&mut self, offset: u32) -> io::Result<()> {
        self.reader.seek(io::SeekFrom::Start(offset as u64)).map(|_| ())
    }

    /// Reads a IFD entry.
    // An IFD entry has four fields:
    //
    // Tag   2 bytes
    // Type  2 bytes
    // Count 4 bytes
    // Value 4 bytes either a pointer the value itself
    fn read_entry(&mut self) -> ImageResult<Option<(ifd::Tag, ifd::Entry)>> {
        let tag = ifd::Tag::from_u16(try!(self.read_short()));
        let type_: ifd::Type = match FromPrimitive::from_u16(try!(self.read_short())) {
            Some(t) => t,
            None => {
                // Unknown type. Skip this entry according to spec.
                try!(self.read_long());
                try!(self.read_long());
                return Ok(None)

            }
        };
        Ok(Some((tag, ifd::Entry::new(
            type_,
            try!(self.read_long()), // count
            try!(self.read_offset())  // offset
        ))))
    }

    /// Reads the next IFD
    fn read_ifd(&mut self) -> ImageResult<Directory> {
        let mut dir: Directory = HashMap::new();
        match self.next_ifd {
            None => return Err(image::ImageError::FormatError(
                "Image file directory not found.".to_string())
            ),
            Some(offset) => try!(self.goto_offset(offset))
        }
        for _ in 0..try!(self.read_short()) {
            let (tag, entry) = match try!(self.read_entry()) {
                Some(val) => val,
                None => continue // Unknown data type in tag, skip
            };
            dir.insert(tag, entry);
        }
        self.next_ifd = match try!(self.read_long()) {
            0 => None,
            n => Some(n)
        };
        Ok(dir)
    }

    /// Tries to retrieve a tag.
    /// Return `Ok(None)` if the tag is not present.
    fn find_tag(&mut self, tag: ifd::Tag) -> ImageResult<Option<ifd::Value>> {
        let ifd: &Directory = unsafe {
            let ifd = self.ifd.as_ref().unwrap(); // Ok to fail
            // Get a immutable borrow of self
            // This is ok because entry val only changes the stream
            // but not the directory.
            mem::transmute_copy(&ifd)
        };
        match ifd.get(&tag) {
            None => Ok(None),
            Some(entry) => Ok(Some(try!(entry.val(self))))
        }
    }

    /// Tries to retrieve a tag and convert it to the desired type.
    fn find_tag_u32(&mut self, tag: ifd::Tag) -> ImageResult<Option<u32>> {
        match try!(self.find_tag(tag)) {
            Some(val) => Ok(Some(try!(val.as_u32()))),
            None => Ok(None)
        }
    }

    /// Tries to retrieve a tag and convert it to the desired type.
    fn find_tag_u32_vec(&mut self, tag: ifd::Tag) -> ImageResult<Option<Vec<u32>>> {
        match try!(self.find_tag(tag)) {
            Some(val) => Ok(Some(try!(val.as_u32_vec()))),
            None => Ok(None)
        }
    }

    /// Tries to retrieve a tag.
    /// Returns an error if the tag is not present
    fn get_tag(&mut self, tag: ifd::Tag) -> ImageResult<ifd::Value> {
        match try!(self.find_tag(tag)) {
            Some(val) => Ok(val),
            None => Err(::image::ImageError::FormatError(format!(
                "Required tag `{:?}` not found.", tag
            )))
        }
    }

    /// Tries to retrieve a tag and convert it to the desired type.
    fn get_tag_u32(&mut self, tag: ifd::Tag) -> ImageResult<u32> {
        (try!(self.get_tag(tag))).as_u32()
    }

    /// Tries to retrieve a tag and convert it to the desired type.
    fn get_tag_u32_vec(&mut self, tag: ifd::Tag) -> ImageResult<Vec<u32>> {
        (try!(self.get_tag(tag))).as_u32_vec()
    }

    /// Decompresses the strip into the supplied buffer.
    /// Returns the number of bytes read.
    fn expand_strip<'a>(&mut self, buffer: DecodingBuffer<'a>, offset: u32, length: u32) -> ImageResult<usize> {
        let color_type = try!(self.colortype());
        try!(self.goto_offset(offset));
        let (bytes, mut reader): (usize, Box<EndianReader>) = match self.compression_method {
            CompressionMethod::None => {
                let order = self.reader.byte_order;
                (length as usize, Box::new(SmartReader::wrap(&mut self.reader, order)))
            },
            CompressionMethod::LZW => {
                let (bytes, reader) = try!(LZWReader::new(&mut self.reader));
                (bytes, Box::new(reader))
            }
            method => return Err(::image::ImageError::UnsupportedError(format!(
                "Compression method {:?} is unsupported", method
            )))
        };
        Ok(match (color_type, buffer) {
            (ColorType:: RGB(8), DecodingBuffer::U8(ref mut buffer)) |
            (ColorType::RGBA(8), DecodingBuffer::U8(ref mut buffer)) => {
                try!(reader.read(&mut buffer[..bytes]))
            }
            (ColorType::RGBA(16), DecodingBuffer::U16(ref mut buffer)) |
            (ColorType:: RGB(16), DecodingBuffer::U16(ref mut buffer)) => {
                for datum in buffer[..bytes/2].iter_mut() {
                    *datum = try!(reader.read_u16())
                }
                bytes/2
            }
            (ColorType::Gray(16), DecodingBuffer::U16(ref mut buffer)) => {
                for datum in buffer[..bytes/2].iter_mut() {
                    *datum = try!(reader.read_u16());
                    if self.photometric_interpretation == PhotometricInterpretation::WhiteIsZero {
                        *datum = 0xffff - *datum
                    }
                }
                bytes/2
            }
            (ColorType::Gray(n), DecodingBuffer::U8(ref mut buffer)) if n <= 8 => {
                try!(reader.read(&mut buffer[..bytes]));
                if self.photometric_interpretation == PhotometricInterpretation::WhiteIsZero {
                    for byte in buffer[..bytes].iter_mut() {
                        *byte = 0xff - *byte
                    }
                }
                bytes
            }
            (type_, _) => return Err(::image::ImageError::UnsupportedError(format!(
                "Color type {:?} is unsupported", type_
            )))
        })
    }
}

impl<R: Read + Seek> ImageDecoder for TIFFDecoder<R> {
    fn dimensions(&mut self) -> ImageResult<(u32, u32)> {
        Ok((self.width, self.height))

    }

    fn colortype(&mut self) -> ImageResult<ColorType> {
        match self.photometric_interpretation {
            // TODO: catch also [ 8, 8, 8, _] this does not work due to a bug in rust atm
            PhotometricInterpretation::RGB if self.bits_per_sample == [8, 8, 8, 8] => Ok(ColorType::RGBA(8)),
            PhotometricInterpretation::RGB if self.bits_per_sample == [8, 8, 8] => Ok(ColorType::RGB(8)),
            PhotometricInterpretation::RGB if self.bits_per_sample == [16, 16, 16, 16] => Ok(ColorType::RGBA(16)),
            PhotometricInterpretation::RGB if self.bits_per_sample == [16, 16, 16] => Ok(ColorType::RGB(16)),
            PhotometricInterpretation::BlackIsZero | PhotometricInterpretation::WhiteIsZero
                                           if self.bits_per_sample.len() == 1 => Ok(ColorType::Gray(self.bits_per_sample[0])),

            _ => return Err(::image::ImageError::UnsupportedError(format!(
                "{:?} with {:?} bits per sample is unsupported", self.bits_per_sample, self.photometric_interpretation
            ))) // TODO: this is bad we should not fail at this point}
        }
    }

    fn row_len(&mut self) -> ImageResult<usize> {
        unimplemented!()
    }

    fn read_scanline(&mut self, _: &mut [u8]) -> ImageResult<u32> {
        unimplemented!()
    }

    fn read_image(&mut self) -> ImageResult<DecodingResult> {
        let buffer_size =
            self.width  as usize
            * self.height as usize
            * self.bits_per_sample.iter().count();
        let mut result = match (self.bits_per_sample.iter()
                                               .map(|&x| x)
                                               .max()
                                               .unwrap_or(8) as f32/8.0).ceil() as u8 {
            n if n <= 8 => DecodingResult::U8(Vec::with_capacity(buffer_size)),
            n if n <= 16 => DecodingResult::U16(Vec::with_capacity(buffer_size)),
            n => return Err(
                ImageError::UnsupportedError(
                    format!("{} bits per channel not supported", n)
                )
            )
        };
        if let Ok(config) = self.get_tag_u32(ifd::Tag::PlanarConfiguration) {
            match FromPrimitive::from_u32(config) {
                Some(PlanarConfiguration::Chunky) => {},
                config => return Err(ImageError::UnsupportedError(
                    format!("Unsupported planar configuration “{:?}”.", config)
                ))
            }
        }
        // Safe since the uninizialized values are never read.
        match result {
            DecodingResult::U8(ref mut buffer) =>
                unsafe { buffer.set_len(buffer_size) },
            DecodingResult::U16(ref mut buffer) =>
                unsafe { buffer.set_len(buffer_size) },
        }
        let mut units_read = 0;
        for (&offset, &byte_count) in try!(self.get_tag_u32_vec(ifd::Tag::StripOffsets))
        .iter().zip(try!(self.get_tag_u32_vec(ifd::Tag::StripByteCounts)).iter()) {
            units_read += match result {
                DecodingResult::U8(ref mut buffer) => {
                    try!(self.expand_strip(
                        DecodingBuffer::U8(&mut buffer[units_read..]),
                        offset, byte_count
                    ))
                },
                DecodingResult::U16(ref mut buffer) => {
                    try!(self.expand_strip(
                        DecodingBuffer::U16(&mut buffer[units_read..]),
                        offset, byte_count
                    ))
                },
            };
            if units_read == buffer_size {
                break
            }
        }
        // Shrink length such that the uninitialized memory is not exposed.
        if units_read < buffer_size {
            match result {
                DecodingResult::U8(ref mut buffer) =>
                    unsafe { buffer.set_len(units_read) },
                DecodingResult::U16(ref mut buffer) =>
                    unsafe { buffer.set_len(units_read) },
            }
        }
        if let Ok(predictor) = self.get_tag_u32(ifd::Tag::Predictor) {
            result = match FromPrimitive::from_u32(predictor) {
                Some(Predictor::None) => result,
                Some(Predictor::Horizontal) => {
                    try!(rev_hpredict(
                        result,
                        try!(self.dimensions()),
                        try!(self.colortype())
                    ))
                },
                None => return Err(ImageError::FormatError(
                    format!("Unkown predictor “{}” encountered", predictor)
                ))
            }
        }
        Ok(result)
    }
}
