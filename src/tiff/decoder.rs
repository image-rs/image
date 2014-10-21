use std::io;
use std::mem;
use std::raw;
use std::iter::AdditiveIterator;

use std::io::IoResult;

use std::collections::{HashMap};

use image;
use image::ImageResult;
use image::ImageDecoder;
use color;

use super::ifd;
use super::ifd::Directory;

macro_rules! io_try(
    ($e: expr) => (
        match $e {
            Ok(e) => e,
            Err(err) => return Err(image::IoError(err))
        }
    )
)

/// Byte order of the TIFF file.
#[deriving(Show)]
pub enum ByteOrder {
    /// little endian byte order
    LittleEndian,
    /// big endian byte order
    BigEndian
}


/// Reader that is aware of the byte order.
#[deriving(Show)]
pub struct SmartReader<R> {
    reader: R,
    byte_order: ByteOrder
}

impl<R: Reader + Seek> SmartReader<R> {
    /// Wraps a reader
    pub fn wrap(reader: R, byte_order: ByteOrder) -> SmartReader<R> {
        SmartReader {
            reader: reader,
            byte_order: byte_order
        }
    }
    
    /// Reads an u16
    #[inline]
    pub fn read_u16(&mut self) -> IoResult<u16> {
        match self.byte_order {
            LittleEndian => self.reader.read_le_u16(),
            BigEndian => self.reader.read_be_u16()
        }
    }
    
    /// Reads an u32
    #[inline]
    pub fn read_u32(&mut self) -> IoResult<u32> {
        match self.byte_order {
            LittleEndian => self.reader.read_le_u32(),
            BigEndian => self.reader.read_be_u32()
        }
    }
}

impl<R: Reader> Reader for SmartReader<R> {
    #[inline]
    fn read(&mut self, buf: &mut [u8]) -> IoResult<uint> {
        self.reader.read(buf)
    }
}

impl<R: Seek> Seek for SmartReader<R> {
    #[inline]
    fn tell(&self) -> IoResult<u64> {
        self.reader.tell()
    }


    #[inline]
    fn seek(&mut self, pos: i64, style: io::SeekStyle) -> IoResult<()> {
        self.reader.seek(pos, style)
    }
}


#[deriving(Show, FromPrimitive)]
enum PhotometricInterpretation {
    WhiteIsZero = 0,
    BlackIsZero = 1,
    RGB = 2,
    PaletteColor = 3,
    TransparencyMask = 4,
}
#[deriving(Show, FromPrimitive)]
enum CompressionMethod {
    NoCompression = 1,
    Huffman = 2,
    PackBits = 32773
}

/// The representation of a PNG decoder
///
/// Currently does not support decoding of interlaced images
#[deriving(Show)]
pub struct TIFFDecoder<R> {
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

impl<R: Reader + Seek> TIFFDecoder<R> {
    /// Create a new decoder that decodes from the stream ```r```
    pub fn new(r: R) -> ImageResult<TIFFDecoder<R>> {
        TIFFDecoder {
            reader: SmartReader::wrap(r, LittleEndian),
            byte_order: LittleEndian,
            next_ifd: None,
            ifd: None,
            width: 0,
            height: 0,
            bits_per_sample: vec![1],
            samples: 1,
            photometric_interpretation: BlackIsZero,
            compression_method: NoCompression
        }.init()
    }
    
    fn read_header(&mut self) -> ImageResult<()> {
        match io_try!(self.reader.read_exact(2)).as_slice() {
            b"II" => { 
                self.byte_order = LittleEndian;
                self.reader.byte_order = LittleEndian; },
            b"MM" => {
                self.byte_order = BigEndian;
                self.reader.byte_order = BigEndian;  },
            _ => return Err(image::FormatError(
                "TIFF signature not found.".to_string()
            ))
        }
        if io_try!(self.read_short()) != 42 {
            return Err(image::FormatError("TIFF signature invalid.".to_string()))
        }
        self.next_ifd = match io_try!(self.read_long()) {
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
    /// If there is no further image in the TIFF file a format error is return.
    /// To determine whether there are more images call `TIFFDecoder::more_images` instead.
    pub fn next_image(mut self) -> ImageResult<TIFFDecoder<R>> {
        try!(self.read_header());
        self.ifd = Some(try!(self.read_ifd()));
        self.width = try!(self.get_tag_u32(ifd::ImageWidth));
        self.height = try!(self.get_tag_u32(ifd::ImageLength));
        self.photometric_interpretation = match FromPrimitive::from_u32(
            try!(self.get_tag_u32(ifd::PhotometricInterpretation))
        ) {
            Some(val) => val,
            None => return Err(image::UnsupportedError(
                "The image is using an unknown photometric interpretation.".to_string()
            ))
        };
        match try!(self.find_tag_u32(ifd::Compression)) {
            Some(val) => match FromPrimitive::from_u32(val) {
                Some(method) =>  {
                    self.compression_method = method
                },
                None => return Err(image::UnsupportedError(
                    "Unknown compression method.".to_string()
                ))
            },
            None => {}
        }
        match try!(self.find_tag_u32(ifd::SamplesPerPixel)) {
            Some(val) => {
                self.samples = val as u8
            },
            None => {}
        }
        match self.samples {
            1 => {
                match try!(self.find_tag_u32(ifd::BitsPerSample)) {
                    Some(val) => {
                        self.bits_per_sample = vec![val as u8]
                    },
                    None => {}
                }
            }
            n => println!("{} samples", n)
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
    pub fn read_short(&mut self) -> IoResult<u16> {
        self.reader.read_u16()
    }
    
    /// Reads a TIFF long value
    #[inline]
    pub fn read_long(&mut self) -> IoResult<u32> {
        self.reader.read_u32()
    }
    
    /// Reads a TIFF IFA offset/value field
    #[inline]
    pub fn read_offset(&mut self) -> IoResult<[u8, ..4]> {
        let mut val = [0, ..4];
        let _ = try!(self.reader.read_at_least(4, val.as_mut_slice()));
        Ok(val)
    }
    
    /// Moves the cursor to the specified offset
    #[inline]
    pub fn goto_offset(&mut self, offset: u32) -> IoResult<()> {
        self.reader.seek(offset as i64, io::SeekSet)
    }
    
    /// Reads a IFD entry.
    ///
    /// And IFD entry has four fields
    /// Tag   2 bytes 
    /// Type  2 bytes 
    /// Count 4 bytes 
    /// Value 4 bytes either a pointer the value itself
    fn read_entry(&mut self) -> ImageResult<Option<(ifd::Tag, ifd::Entry)>> {
        let tag = ifd::Tag::from_u16(io_try!(self.read_short()));
        let type_: ifd::Type = match FromPrimitive::from_u16(io_try!(self.read_short())) {
            Some(t) => t,
            None => {
                // Unknown type. Skip this entry according to spec.
                io_try!(self.read_long());
                io_try!(self.read_long());
                return Ok(None)
                
            }
        };
        Ok(Some((tag, ifd::Entry::new(
            type_,
            io_try!(self.read_long()), // count
            io_try!(self.read_offset())  // offset
        ))))
    }
    
    /// Reads the next IFD
    fn read_ifd(&mut self) -> ImageResult<Directory> {
        let mut dir: Directory = HashMap::new(); 
        match self.next_ifd {
            None => return Err(image::FormatError(
                "Image file directory not found.".to_string())
            ),
            Some(offset) => io_try!(self.goto_offset(offset))
        }
        for _ in range(0, io_try!(self.read_short())) {
            let (tag, entry) = match try!(self.read_entry()) {
                Some(val) => val,
                None => continue // Unknown data type in tag, skip
            };
            dir.insert(tag, entry);
        }
        self.next_ifd = match io_try!(self.read_long()) {
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
        match ifd.find(&tag) {
            None => Ok(None),
            Some(entry) => Ok(Some(try!(entry.val(self))))
        }
    }
    
    /// Tries to retrieve a tag an convert it to the desired type.
    fn find_tag_u32(&mut self, tag: ifd::Tag) -> ImageResult<Option<u32>> {
        match try!(self.find_tag(tag)) {
            Some(val) => Ok(Some(try!(val.as_u32()))),
            None => Ok(None)
        }
    }
    
    /// Tries to retrieve a tag.
    /// Returns an error if the tag is not present
    fn get_tag(&mut self, tag: ifd::Tag) -> ImageResult<ifd::Value> {
        match try!(self.find_tag(tag)) {
            Some(val) => Ok(val),
            None => Err(::image::FormatError(format!(
                "Required tag `{}` not found.", tag
            )))
        }
    }
    
    /// Tries to retrieve a tag an convert it to the desired type.
    fn get_tag_u32(&mut self, tag: ifd::Tag) -> ImageResult<u32> {
        (try!(self.get_tag(tag))).as_u32()
    }
    /// Tries to retrieve a tag an convert it to the desired type.
    fn get_tag_u32_vec(&mut self, tag: ifd::Tag) -> ImageResult<Vec<u32>> {
        (try!(self.get_tag(tag))).as_u32_vec()
    }
    
    /// Decompresses the strip into the supplied buffer.
    /// Returns the number of bytes read.
    fn expand_strip(&mut self, buffer: &mut [u8], offset: u32, length: u32) -> ImageResult<uint> {
        let color_type = try!(self.colortype());
        io_try!(self.goto_offset(offset));
        let reader = match self.compression_method {
            NoCompression => {
                &mut self.reader
            }
            method => return Err(::image::UnsupportedError(format!(
                "Compression method {} is unsupported", method
            )))
        };
        match color_type {
            color::Grey(16) => {
                // Casting to &[16] makes sure that endian issues are handled
                // automagically by the compiler
                let buffer: &mut [u16] = unsafe { mem::transmute(
                    raw::Slice::<u16> {
                        data: mem::transmute(buffer.as_ptr()),
                        len: buffer.len()/2
                    }
                )};
                for datum in buffer.slice_to_mut(length as uint/2).iter_mut() {
                    *datum = io_try!(reader.read_u16())
                }
            }
            color::Grey(n) if n < 8 => {
                return Ok(io_try!(reader.read(buffer.slice_to_mut(length as uint))))
            }
            type_ => return Err(::image::UnsupportedError(format!(
                "Color type {} is unsupported", type_
            )))
        }
        Ok(length as uint)
    }
}

impl<R: Reader + Seek> ImageDecoder for TIFFDecoder<R> {
    fn dimensions(&mut self) -> ImageResult<(u32, u32)> {
        Ok((self.width, self.height))
        
    }

    fn colortype(&mut self) -> ImageResult<color::ColorType> {
        match (self.bits_per_sample.as_slice(), self.photometric_interpretation) {
            // TODO: catch also [ 8,  8,  8, _] this does not work due to a bug in rust atm
            ([ 8,  8,  8], RGB) => Ok(color::RGB(8)),
            ([16, 16, 16], RGB) => Ok(color::RGB(16)),
            ([ n], BlackIsZero)
            |([ n], WhiteIsZero) => Ok(color::Grey(n)),
            (bits, mode) => return Err(::image::UnsupportedError(format!(
                "{} with {} bits per sample is unsupported", mode, bits
            ))) // TODO: this is bad we should not fail at this point
        }
    }

    fn row_len(&mut self) -> ImageResult<uint> {
        unimplemented!()
    }

    fn read_scanline(&mut self, _: &mut [u8]) -> ImageResult<u32> {
        unimplemented!()
    }

    fn read_image(&mut self) -> ImageResult<Vec<u8>> {
        let buffer_size = 
            self.width  as uint
            * self.height as uint
            * self.bits_per_sample.iter().map(|&x| x).sum() as uint
            / 8;
        let mut buffer = Vec::with_capacity(buffer_size);
        // Safe since the uninizialized values are never read.
        unsafe { buffer.set_len(buffer_size) }
        println!("buffer len {}", buffer_size)
        let mut bytes_read = 0;
        for (&offset, &byte_count) in try!(self.get_tag_u32_vec(ifd::StripOffsets))
        .iter().zip(try!(self.get_tag_u32_vec(ifd::StripByteCounts)).iter()) {
            println!("offset {}, count {}", offset, byte_count)
            bytes_read += try!(self.expand_strip(
                buffer.slice_from_mut(bytes_read), offset, byte_count
            ));
            if bytes_read == buffer_size {
                break
            }
        }
        // Shrink length such that the uninitialized memory is not exposed.
        if bytes_read < buffer_size {
            unsafe { buffer.set_len(bytes_read) }
        }
        Ok(buffer)
    }
}