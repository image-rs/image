//! Function for reading TIFF tags

use std::io::{self, Read, Seek};
use std::collections::{HashMap};

use super::stream::{ByteOrder, SmartReader, EndianReader};

use self::Value::{Unsigned, List};

macro_rules! tags {
    {$(
        $tag:ident
        $val:expr;
    )*} => {

        /// TIFF tag
        #[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
        pub enum Tag {
            $($tag,)*
            Unknown(u16)
        }
        impl Tag {
            pub fn from_u16(n: u16) -> Tag {
                $(if n == $val { Tag::$tag } else)* {
                    Tag::Unknown(n)
                }
            }
        }
    }
}

// Note: These tags appear in the order they are mentioned in the TIFF reference
tags!{
    // Baseline tags:
    Artist 315; // TODO add support
    // grayscale images PhotometricInterpretation 1 or 3
    BitsPerSample 258;
    CellLength 265; // TODO add support
    CellWidth 264; // TODO add support
    // palette-color images (PhotometricInterpretation 3)
    ColorMap 320; // TODO add support
    Compression 259; // TODO add support for 2 and 32773
    Copyright 33432; // TODO add support
    DateTime 306; // TODO add support
    ExtraSamples 338; // TODO add support
    FillOrder 266; // TODO add support
    FreeByteCounts 289; // TODO add support
    FreeOffsets 288; // TODO add support
    GrayResponseCurve 291; // TODO add support
    GrayResponseUnit 290; // TODO add support
    HostComputer 316; // TODO add support
    ImageDescription 270; // TODO add support
    ImageLength 257;
    ImageWidth 256;
    Make 271; // TODO add support
    MaxSampleValue 281; // TODO add support
    MinSampleValue 280; // TODO add support
    Model 272; // TODO add support
    NewSubfileType 254; // TODO add support
    Orientation 274; // TODO add support
    PhotometricInterpretation 262;
    PlanarConfiguration 284;
    ResolutionUnit 296; // TODO add support
    RowsPerStrip 278;
    SamplesPerPixel 277;
    Software 305;
    StripByteCounts 279;
    StripOffsets 273;
    SubfileType 255; // TODO add support
    Threshholding 263; // TODO add support
    XResolution 282;
    YResolution 283;
    // Advanced tags
    Predictor 317;
}

enum_from_primitive! {
#[derive(Clone, Copy, Debug)]
pub enum Type {
    BYTE = 1,
    ASCII = 2,
    SHORT = 3,
    LONG = 4,
    RATIONAL = 5,
}
}


#[allow(unused_qualifications)]
#[derive(Debug)]
pub enum Value {
    //Signed(i32),
    Unsigned(u32),
    List(Vec<Value>)
}

impl Value {
    pub fn as_u32(self) -> ::image::ImageResult<u32> {
        match self {
            Unsigned(val) => Ok(val),
            val => Err(::image::ImageError::FormatError(format!(
                "Expected unsigned integer, {:?} found.", val
            )))
        }
    }
    pub fn as_u32_vec(self) -> ::image::ImageResult<Vec<u32>> {
        match self {
            List(vec) => {
                let mut new_vec = Vec::with_capacity(vec.len());
                for v in vec.into_iter() {
                    new_vec.push(try!(v.as_u32()))
                }
                Ok(new_vec)
            },
            Unsigned(val) => Ok(vec![val]),
            //_ => Err(::image::FormatError("Tag data malformed.".to_string()))
        }
    }
}

pub struct Entry {
    type_: Type,
    count: u32,
    offset: [u8; 4],
}

impl ::std::fmt::Debug for Entry {
    fn fmt(&self, fmt: &mut ::std::fmt::Formatter) -> Result<(), ::std::fmt::Error> {
        fmt.write_str(&format!("Entry {{ type_: {:?}, count: {:?}, offset: {:?} }}",
            self.type_,
            self.count,
            &self.offset
        ))
    }
}

impl Entry {
    pub fn new(type_: Type, count: u32, offset: [u8; 4]) -> Entry {
        Entry {
            type_: type_,
            count: count,
            offset: offset
        }
    }

    /// Returns a mem_reader for the offset/value field
    fn r(&self, byte_order: ByteOrder) -> SmartReader<io::Cursor<Vec<u8>>> {
        SmartReader::wrap(
            io::Cursor::new(self.offset.to_vec()),
            byte_order
        )
    }

    pub fn val<R: Read + Seek>(&self, decoder: &mut super::TIFFDecoder<R>)
    -> ::image::ImageResult<Value> {
        let bo = decoder.byte_order();
        match (self.type_, self.count) {
            // TODO check if this could give wrong results
            // at a different endianess of file/computer.
            (Type::BYTE, 1) => Ok(Unsigned(self.offset[0] as u32)),
            (Type::SHORT, 1) => Ok(Unsigned(try!(self.r(bo).read_u16()) as u32)),
            (Type::SHORT, 2) => {
                let mut r = self.r(bo);
                Ok(List(vec![
                    Unsigned(try!(r.read_u16()) as u32),
                    Unsigned(try!(r.read_u16()) as u32)
                ]))
            },
            (Type::SHORT, n) => {
                let mut v = Vec::with_capacity(n as usize);
                try!(decoder.goto_offset(try!(self.r(bo).read_u32())));
                for _ in 0 .. n {
                    v.push(Unsigned(try!(decoder.read_short()) as u32))
                }
                Ok(List(v))
            },
            (Type::LONG, 1) => Ok(Unsigned(try!(self.r(bo).read_u32()))),
            (Type::LONG, n) => {
                let mut v = Vec::with_capacity(n as usize);
                try!(decoder.goto_offset(try!(self.r(bo).read_u32())));
                for _ in 0 .. n {
                    v.push(Unsigned(try!(decoder.read_long())))
                }
                Ok(List(v))
            }
            _ => Err(::image::ImageError::UnsupportedError("Unsupported data type.".to_string()))
        }
    }
}

/// Type representing an Image File Directory
pub type Directory = HashMap<Tag, Entry>;
