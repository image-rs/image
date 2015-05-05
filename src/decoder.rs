use std::borrow::Cow;
use std::error;
use std::fmt;
use std::mem;
use std::io::{self, Read};
use std::cmp::min;
use std::convert::{From, AsRef};

use num::FromPrimitive;

use deflate::{Inflater, Flush};

use crc::Crc32;
use traits::ReadBytesExt;

use chunks::*;

/// TODO check if these size are reasonable
const CHUNCK_BUFFER_SIZE: usize = 10*1024;
const IMAGE_BUFFER_SIZE: usize = 30*1024;

#[derive(Debug)]
enum U32Value {
    // CHUNKS
    Length,
    Type(u32),
    Crc(ChunkType)
}

#[derive(Debug)]
enum ByteValue {
    BitDepth,
    ColorType,
    CompressionMethod,
    FilterMethod,
    InterlaceMethod
}

enum_from_primitive! {
#[derive(Debug, Clone, Copy)]
pub enum ColorType {
    Grayscale = 0,
    RGB = 2,
    Indexed = 3,
    GrayscaleAlpha = 4,
    RGBA = 6
}
}

    
#[derive(Debug)]
enum State {
    Signature(u8, [u8; 7]),
    U32Byte3(U32Value, u32),
    U32Byte2(U32Value, u32),
    U32Byte1(U32Value, u32),
    U32(U32Value),
    ReadChunk(u32, ChunkType, bool),
    PartialChunk(u32, ChunkType),
    DecodeData(u32, ChunkType, usize),
    //Byte(ByteValue),
}

#[derive(Debug)]
pub enum DecodingResult<'a> {
    None,
    Header(u32, u32, u8, ColorType, bool),
    ChunkBegin(u32, ChunkType),
    ChunkComplete(u32, ChunkType),
    ImageData(&'a [u8]),
    ImageEnd,
}

#[derive(Debug)]
pub enum DecodingError {
    IoError(io::Error),
    Format(::std::borrow::Cow<'static, str>),
    InvalidSignature,
    CrcMismatch {
        /// bytes to skip to try to recover from this error
        recover: usize,
        /// Stored CRC32 value
        crc_val: u32,
        /// Calculated CRC32 sum
        crc_sum: u32,
        chunk: ChunkType
    },
    CorruptFlateStream
}

impl error::Error for DecodingError {
    fn description(&self) -> &str {
        use self::DecodingError::*;
        match *self {
            IoError(ref err) => err.description(),
            Format(ref desc) => &desc,
            InvalidSignature => "invalid signature",
            CrcMismatch { .. } => "CRC error",
            CorruptFlateStream => "compressed data stream corrupted"
        }
    }
}

impl fmt::Display for DecodingError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(fmt, "{}", (self as &error::Error).description())
    }
}

impl From<io::Error> for DecodingError {
    fn from(err: io::Error) -> DecodingError {
        DecodingError::IoError(err)
    }
}

/// PNG info struct
pub struct Info;

pub struct Decoder {
    state: Option<State>,
    width: u32,
    height: u32,
    color_type: ColorType,
    bit_depth: u8,
    interlaced: bool,
    current_chunk: (Crc32, Vec<u8>),
    inflater: Inflater,
    image_data: Vec<u8>,
}


impl Decoder {
    /// Creates a new decoder
    ///
    /// Allocates the internal buffers (40 KiB) needed for decoding the image.
    pub fn new() -> Decoder {
        Decoder {
            state: Some(State::Signature(0, [0; 7])),
            width: 0,
            height: 0,
            color_type: ColorType::Grayscale,
            bit_depth: 0,
            interlaced: false,
            current_chunk: (Crc32::new(), Vec::with_capacity(CHUNCK_BUFFER_SIZE)),
            inflater: Inflater::new(),
            image_data: vec![0; IMAGE_BUFFER_SIZE]
        }
    }
    
    /// Low level decoder interface.
    ///
    /// Allows to stream partial data to the encoder. Returns a tuple containing the 
    /// bytes that have been consumed from the input buffer and the latest decoding
    /// result.
    pub fn update<'a>(&'a mut self, mut buf: &[u8])
    -> Result<(usize, DecodingResult<'a>), DecodingError> {
        // NOTE: Do not change the function signature without double-checking the
        //       unsafe block!
        let len = buf.len();
        while buf.len() > 0 && self.state.is_some() {
            match self.next_state(buf) {
                Ok((bytes, DecodingResult::None)) => {
                    buf = &buf[bytes..]
                }
                Ok((bytes, result)) => {
                    buf = &buf[bytes..];
                    return Ok(
                        (len-buf.len(), 
                        // This transmute just casts the lifetime away. Since Rust only 
                        // has SESE regions, this early return cannot be worked out and
                        // such that the borrow region of self includes the whole block.
                        // The explixit lifetimes in the function signature ensure that
                        // this is safe.
                        unsafe { 
                            mem::transmute::<DecodingResult, DecodingResult>(result)
                        }
                    ))
                }
                Err(err) => return Err(err)
            }
        }
        Ok((len-buf.len(), DecodingResult::None))
    }
    
    fn next_state<'a>(&'a mut self, buf: &[u8])
    -> Result<(usize, DecodingResult<'a>), DecodingError> {
        use self::State::*;
        
        macro_rules! goto (
            ($n:expr, $state:expr) => ({
                self.state = Some($state); 
                Ok(($n, DecodingResult::None))
            });
            ($state:expr) => ({
                self.state = Some($state); 
                Ok((1, DecodingResult::None))
            });
            ($n:expr, $state:expr, emit $res:expr) => ({
                self.state = Some($state); 
                Ok(($n, $res))
            });
            ($state:expr, emit $res:expr) => ({
                self.state = Some($state); 
                Ok((1, $res))
            })
        );
        
        let current_byte = buf[0];
        
        // Driver should ensure that state is never None
        let state = self.state.take().unwrap();
        //println!("{:?}", state);

        match state {
            Signature(i, mut signature) => if i < 7 {
                signature[i as usize] = current_byte;
                goto!(Signature(i+1, signature))
            } else {
                if signature == [137, 80, 78, 71, 13, 10, 26] && current_byte == 10 {
                    goto!(U32(U32Value::Length))
                } else {
                    Err(DecodingError::InvalidSignature)
                }
            },
            PartialChunk(remaining, type_str) => {
                match type_str {
                    IDAT => {
                        goto!(0, DecodeData(remaining, type_str, 0))
                    },
                    // Skip other chunks
                    _ => {
                        let (state, res) = if remaining == 0 {
                            try!(self.parse_chunk(type_str))
                        } else {
                            (ReadChunk(remaining, type_str, true), DecodingResult::None)
                        };
                        goto!(0, state, emit res)
                    }
                }
                
            },
            U32Byte3(type_, mut val) => {
                use self::U32Value::*;
                val |= current_byte as u32;
                match type_ {
                    Length => goto!(U32(Type(val))),
                    Type(length) => {
                        let type_str = [
                            (val >> 24) as u8,
                            (val >> 16) as u8,
                            (val >> 8) as u8,
                            val as u8
                        ];
                        self.current_chunk.0.reset();
                        self.current_chunk.0.update(&type_str);
                        goto!(
                            ReadChunk(length, type_str, true),
                            emit DecodingResult::ChunkBegin(length, type_str)
                        )
                    },
                    Crc(type_str) => {
                        if val == self.current_chunk.0.checksum() {
                            goto!(
                                State::U32(U32Value::Length),
                                emit if type_str == IEND {
                                    DecodingResult::ImageEnd
                                } else {
                                    DecodingResult::ChunkComplete(val, type_str)
                                }
                            )
                        } else {
                            Err(DecodingError::CrcMismatch {
                                recover: 1,
                                crc_val: val, 
                                crc_sum: self.current_chunk.0.checksum(), 
                                chunk: type_str
                            })
                        }
                    },
                }
            },
            U32Byte2(type_, val) => {
                goto!(U32Byte3(type_, val | (current_byte as u32) << 8))
            },
            U32Byte1(type_, val) => {
                goto!(U32Byte2(type_, val | (current_byte as u32) << 16))
            },
            U32(type_) => {
                goto!(U32Byte1(type_,       (current_byte as u32) << 24))
            },
            ReadChunk(remaining, type_str, clear) => {
                if clear {
                    self.current_chunk.1.clear();
                }
                if remaining > 0 {
                    let (ref mut crc, ref mut c_buf) = self.current_chunk;
                    let buf_avail = c_buf.capacity() - c_buf.len();
                    let bytes_avail = min(buf.len(), buf_avail);
                    let n = min(remaining, bytes_avail as u32);
                    if buf_avail == 0 {
                        goto!(0, PartialChunk(
                            remaining, type_str
                        ))
                    } else {
                        let buf = &buf[..n as usize];
                        crc.update(buf);
                        c_buf.push_all(buf);
                        let left = remaining - n;
                        if left == 0 {
                            goto!(n as usize, PartialChunk(
                                left, type_str
                            ))
                        } else {
                            goto!(n as usize, ReadChunk(left, type_str, false))
                        }
                        
                    }
                } else {
                    goto!(0, U32(U32Value::Crc(type_str)))
                }
            }
            DecodeData(remaining, type_str, mut n) => {
                let (eof, c, data) = try!(self.inflater.inflate(
                    &self.current_chunk.1[n..],
                    &mut self.image_data,
                    Flush::None
                ));
                n += c;
                if eof && n != self.current_chunk.1.len() {
                    Err(DecodingError::CorruptFlateStream)
                } else if n == self.current_chunk.1.len() {
                    goto!(
                        0,
                        ReadChunk(remaining, type_str, true),
                        emit DecodingResult::ImageData(data)
                    )
                } else {
                    goto!(
                        0,
                        DecodeData(remaining, type_str, n),
                        emit DecodingResult::ImageData(data)
                    )
                }
            }
        }
    }
    
    fn parse_chunk(&mut self, type_str: [u8; 4])
    -> Result<(State, DecodingResult<'static>), DecodingError> {
        let result = match type_str {
            IHDR => {
                try!(self.parse_ihdr())
            }
            // Skip unknown chunks:
            _ => DecodingResult::None
        };
        Ok((State::U32(U32Value::Crc(type_str)), result))
    }
    
    fn parse_ihdr(&mut self)
    -> Result<DecodingResult<'static>, DecodingError> {
        let mut buf = &self.current_chunk.1[..];
        self.width = try!(buf.read_be());
        self.height = try!(buf.read_be());
        self.bit_depth = try!(buf.read_be());
        let color_type = try!(buf.read_be());
        self.color_type = match FromPrimitive::from_u8(color_type) {
            Some(color_type) => color_type,
            None => return Err(DecodingError::Format(Cow::Owned(format!(
                "invalid color type ({})", color_type
            ))))
        };
        let _: u8 = try!(buf.read_be()); // compression method
        let _: u8 = try!(buf.read_be()); // filter method
        self.interlaced = match try!(buf.read_be()) {
            0u8 => false,
            1 => true,
            _ => return Err(DecodingError::Format(
                Cow::Borrowed("invalid interlace method")
            ))
        };
        Ok(DecodingResult::Header(
            self.width,
            self.height,
            self.bit_depth,
            self.color_type,
            self.interlaced
        ))
    }
}

/// PNG reader
///
/// Provides a high level interface by wrapping a `Read` that iterates over lines
/// or whole images.
pub struct Reader<R: Read> {
    r: R,
    d: Decoder,
    /// Read buffer
    buf: Vec<u8>,
    /// Buffer position
    pos: usize,
    /// Buffer length
    end: usize,
    info: Option<Info>
}

impl<R: Read> Reader<R> {
    /// Creates a new reader
    fn new(r: R) -> Reader<R> {
        Reader {
            r: r,
            d: Decoder::new(),
            buf: vec![0; CHUNCK_BUFFER_SIZE],
            pos: 0,
            end: 0,
            info: None
        }
    }
    
    /// Reads all meta data until the first IDAT chunk
    pub fn read_info(&mut self) -> Result<&Info, DecodingError> {
        use DecodingResult::*;
        if let Some(ref info) = self.info {
            Ok(info)
        } else {
            let info = Info;
            while let Some(val) = try!(self.decode_next()) {
                match val {
                    ChunkBegin(_, IDAT) => break,
                    _ => ()
                }
            }
            self.info = Some(info);
            Ok(self.info.as_ref().unwrap())
        }
    }
    
    /// Returns the next decoded block
    pub fn decode_next(&mut self) -> Result<Option<DecodingResult>, DecodingError> {
        loop {
            if self.pos == self.end {
                self.end = try!(self.r.read(&mut self.buf));
                self.pos = 0;
            }
            match try!(self.d.update(&self.buf[self.pos..self.end])) {
                (n, DecodingResult::None) => self.pos += n,
                (_, DecodingResult::ImageEnd) => return Ok(None),
                (n, result) => {
                    self.pos += n;
                    return Ok(Some(unsafe {
                        // This transmute just casts the lifetime away. See comment
                        // in Decoder::update for more information.
                        mem::transmute::<DecodingResult, DecodingResult>(result)
                    }))
                }
            }
        }
    }
}

#[test]
fn test() {
    use std::fs::File;
    let mut reader = Reader::new(File::open("tests/samples/lenna_fragment_interlaced.png").unwrap());
    while let Some(obj) = reader.decode_next().unwrap() {
        match obj {
            DecodingResult::ImageData(data) => {
                //for (i, line) in data.chunks(4 * (decoder.width as usize) +1).enumerate() {
                //    println!("{}: {}", i, line[0])
                //}
                panic!()
            }
            _ => ()
        }
    }
    //File::open("tests/samples/bug.png").unwrap().read_to_end(&mut data).unwrap();
    //File::open("tests/samples/PNG_transparency_demonstration_1.png").unwrap().read_to_end(&mut data).unwrap();    
}
