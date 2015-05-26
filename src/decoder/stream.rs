use std::borrow::Cow;
use std::default::Default;
use std::error;
use std::fmt;
use std::mem;
use std::io::{self, Read, Write};
use std::cmp::min;
use std::convert::{From, AsRef};

use deflate::{Inflater, Flush};

use crc::Crc32;
use traits::ReadBytesExt;
use common::{ColorType, Info};
use chunk::{self, ChunkType, IHDR, IDAT, IEND};
use utils;

/// TODO check if these size are reasonable
pub const CHUNCK_BUFFER_SIZE: usize = 10*1024;
pub const IMAGE_BUFFER_SIZE: usize = 30*1024;

#[derive(Debug)]
enum U32Value {
    // CHUNKS
    Length,
    Type(u32),
    Crc(ChunkType)
}
    
#[derive(Debug)]
enum State {
    Signature(u8, [u8; 7]),
    U32Byte3(U32Value, u32),
    U32Byte2(U32Value, u32),
    U32Byte1(U32Value, u32),
    U32(U32Value),
    ReadChunk(ChunkType, bool),
    PartialChunk(ChunkType),
    DecodeData(ChunkType, usize),
}

#[derive(Debug)]
/// Result of the decoding process
pub enum Decoded<'a> {
    /// Nothing decoded yet
    Nothing,
    Header(u32, u32, u8, ColorType, bool),
    ChunkBegin(u32, ChunkType),
    ChunkComplete(u32, ChunkType),
    /// Decoded raw image data.
    /// 
    /// The buffer is guaranteed not to span over
    /// line boundaries.
    ImageData(&'a [u8]),
    ImageEnd,
}

#[derive(Debug)]
pub enum DecodingError {
    IoError(io::Error),
    Format(Cow<'static, str>),
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
    Other(Cow<'static, str>),
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
            Other(ref desc) => &desc,
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

/// PNG StreamingDecoder (low-level interface)
pub struct StreamingDecoder {
    state: Option<State>,
    current_chunk: (Crc32, u32, Vec<u8>),
    inflater: Inflater,
    image_data: Vec<u8>,
    row_remaining: usize,
    adam7: Option<utils::Adam7Iterator>,
    info: Option<Info>,
}

impl StreamingDecoder {
    /// Creates a new StreamingDecoder
    ///
    /// Allocates the internal buffers.
    pub fn new() -> StreamingDecoder {
        StreamingDecoder {
            state: Some(State::Signature(0, [0; 7])),
            current_chunk: (Crc32::new(), 0, Vec::with_capacity(CHUNCK_BUFFER_SIZE)),
            inflater: Inflater::new(),
            image_data: vec![0; IMAGE_BUFFER_SIZE],
            row_remaining: 0,
            adam7: None,
            info: None
        }
    }
    
    /// Resets the StreamingDecoder
    pub fn reset(&mut self) {
        self.state = Some(State::Signature(0, [0; 7]));
        self.current_chunk.0 = Crc32::new();
        self.current_chunk.2.clear();
        self.inflater = Inflater::new();
        self.row_remaining = 0;
        self.info = None;
    }
    
    /// Low level StreamingDecoder interface.
    ///
    /// Allows to stream partial data to the encoder. Returns a tuple containing the 
    /// bytes that have been consumed from the input buffer and the current decoding
    /// result.
    pub fn update<'a>(&'a mut self, mut buf: &[u8])
    -> Result<(usize, Decoded<'a>), DecodingError> {
        // NOTE: Do not change the function signature without double-checking the
        //       unsafe block!
        let len = buf.len();
        while buf.len() > 0 && self.state.is_some() {
            match self.next_state(buf) {
                Ok((bytes, Decoded::Nothing)) => {
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
                        // ### NOTE
                        // To check that everything is sound, return the result without
                        // the match (e.g. `return Ok(try!(self.next_state(buf)))`). If
                        // it compiles the returned lifetime is correct.
                        unsafe { 
                            mem::transmute::<Decoded, Decoded>(result)
                        }
                    ))
                }
                Err(err) => return Err(err)
            }
        }
        Ok((len-buf.len(), Decoded::Nothing))
    }
    
    fn next_state<'a>(&'a mut self, buf: &[u8])
    -> Result<(usize, Decoded<'a>), DecodingError> {
        use self::State::*;
        
        macro_rules! goto (
            ($n:expr, $state:expr) => ({
                self.state = Some($state); 
                Ok(($n, Decoded::Nothing))
            });
            ($state:expr) => ({
                self.state = Some($state); 
                Ok((1, Decoded::Nothing))
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
                        self.current_chunk.1 = length;
                        goto!(
                            ReadChunk(type_str, true),
                            emit Decoded::ChunkBegin(length, type_str)
                        )
                    },
                    Crc(type_str) => {
                        if val == self.current_chunk.0.checksum() {
                            goto!(
                                State::U32(U32Value::Length),
                                emit if type_str == IEND {
                                    Decoded::ImageEnd
                                } else {
                                    Decoded::ChunkComplete(val, type_str)
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
            PartialChunk(type_str) => {
                match type_str {
                    IDAT => {
                        goto!(0, DecodeData(type_str, 0))
                    },
                    // Skip other chunks
                    _ => {
                        if self.current_chunk.1 == 0 { // complete chunk
                            Ok((0, try!(self.parse_chunk(type_str))))
                        } else {
                            goto!(0, ReadChunk(type_str, true), emit Decoded::Nothing)
                        }
                    }
                }
                
            },
            ReadChunk(type_str, clear) => { 
                if clear {
                    self.current_chunk.2.clear();
                }
                if self.current_chunk.1 > 0 {
                    let (ref mut crc, ref mut remaining, ref mut c_buf) = self.current_chunk;
                    let buf_avail = c_buf.capacity() - c_buf.len();
                    let bytes_avail = min(buf.len(), buf_avail);
                    let n = min(*remaining, bytes_avail as u32);
                    if buf_avail == 0 {
                        goto!(0, PartialChunk(type_str))
                    } else {
                        let buf = &buf[..n as usize];
                        crc.update(buf);
                        c_buf.extend(buf.iter().map(|&v| v));
                        *remaining -= n;
                        if *remaining == 0 {
                            goto!(n as usize, PartialChunk(type_str
                            ))
                        } else {
                            goto!(n as usize, ReadChunk(type_str, false))
                        }
                        
                    }
                } else {
                    goto!(0, U32(U32Value::Crc(type_str)))
                }
            }
            DecodeData(type_str, mut n) => {
                let chunk_len = self.current_chunk.2.len();
                let remaining = self.current_chunk.1;
                if self.row_remaining == 0 {
                    self.row_remaining = if let Some(ref mut adam7) = self.adam7 {
                        match adam7.next() {
                            Some((_, _, width)) => {
                                self.info.as_ref().unwrap().raw_row_length_from_width(width)
                            },
                            None => -1 as isize as usize // TODO: return at this point
                        }
                    } else if let Some(ref info) = self.info {
                        info.raw_row_length()
                    } else {
                        return Err(DecodingError::Format(
                            "IHDR chunk missing".into()
                        ))
                    }
                }
                let m = min(self.image_data.len(), self.row_remaining);
                let (eof, c, data) = try!(self.inflater.inflate(
                    &self.current_chunk.2[n..],
                    &mut self.image_data[..m],
                    Flush::None
                ));
                n += c;
                self.row_remaining -= data.len();
                if eof && n != chunk_len {
                    Err(DecodingError::CorruptFlateStream)
                } else if n == chunk_len && (data.len() == 0 || remaining != 0) {
                    goto!(
                        0,
                        ReadChunk(type_str, true),
                        emit Decoded::ImageData(data)
                    )
                } else {
                    goto!(
                        0,
                        DecodeData(type_str, n),
                        emit Decoded::ImageData(data)
                    )
                }
            }
        }
    }
    
    fn parse_chunk(&mut self, type_str: [u8; 4])
    -> Result<Decoded, DecodingError> {
        self.state = Some(State::U32(U32Value::Crc(type_str)));
        let state_ptr: *mut _ = &mut self.state;
        match match type_str {
            IHDR => {
                self.parse_ihdr()
            },
            chunk::PLTE => {
                self.parse_plte()
            },
            chunk::tRNS => {
                self.parse_trns()
            }
            // Skip other and unknown chunks:
            _ => Ok(Decoded::Nothing)
        } {
            Err(err) =>{
                // Borrow of self ends here, because Decoding error does not borrow self.
                *unsafe { &mut *state_ptr } = None;
                Err(err)
            },
            ok => ok
        }
    }
    
    fn get_info_or_err(&self) -> Result<&Info, DecodingError> {
        self.info.as_ref().ok_or(DecodingError::Format(
            "IHDR chunk missing".into()
        ))
    }
    
    fn parse_plte(&mut self)
    -> Result<Decoded, DecodingError> {
        let mut vec = Vec::new();
        vec.extend(self.current_chunk.2.iter().map(|&v| v));
        self.info.as_mut().map(
            |info| info.palette = Some(vec)
        );
        Ok(Decoded::Nothing)
    }
    
    fn parse_trns(&mut self)
    -> Result<Decoded, DecodingError> {
        use common::ColorType::*;
        let (color_type, bit_depth) = {
            let info = try!(self.get_info_or_err());
            (info.color_type, info.bit_depth)
        };
        let mut vec = Vec::new();
        vec.extend(self.current_chunk.2.iter().map(|&v| v));
        let len = vec.len();
        let info = match self.info {
            Some(ref mut info) => info,
            None => return Err(DecodingError::Format(
              "tRNS chunk occured before IHDR chunk".into()
            ))
        };
        info.trns = Some(vec);
        let vec = info.trns.as_mut().unwrap();
        match color_type {
            Grayscale => {
                if len < 2 {
                    return Err(DecodingError::Format(
                        "not enought palette entries".into()
                    ))
                }
                if bit_depth < 16 {
                    vec[0] = vec[1];
                    vec.truncate(1);
                }
                Ok(Decoded::Nothing)
            },
            RGB => {
                if len < 6 {
                    return Err(DecodingError::Format(
                        "not enought palette entries".into()
                    ))
                }
                if bit_depth < 16 {
                    vec[0] = vec[1];
                    vec[1] = vec[3];
                    vec[2] = vec[5];
                    vec.truncate(3);
                }
                Ok(Decoded::Nothing)
            },
            Indexed => {
                let _ = info.palette.as_ref().ok_or(DecodingError::Format(
                    "tRNS chunk occured before PLTE chunk".into()
                ));
                Ok(Decoded::Nothing)
            },
            c => Err(DecodingError::Format(
                format!("tRNS chunk found for color type ({})", c as u8).into()
            ))
        }
        
    }
    
    
    fn parse_ihdr(&mut self)
    -> Result<Decoded, DecodingError> {
        // TODO: check if color/bit depths combination is valid
        let mut buf = &self.current_chunk.2[..];
        let width = try!(buf.read_be());
        let height = try!(buf.read_be());
        let bit_depth = try!(buf.read_be());
        match bit_depth {
            1 | 2 | 4 | 8 | 16 => (),
            n => return Err(DecodingError::Format(
                format!("invalid bit depth ({})", n).into()
            ))
        }
        let color_type = try!(buf.read_be());
        let color_type = match ColorType::from_u8(color_type) {
            Some(color_type) => color_type,
            None => return Err(DecodingError::Format(
                format!("invalid color type ({})", color_type).into()
            ))
        };
        match try!(buf.read_be()) { // compression method
            0u8 => (),
            n => return Err(DecodingError::Format(
                format!("unknown compression method ({})", n).into()
            ))
        }
        match try!(buf.read_be()) { // filter method
            0u8 => (),
            n => return Err(DecodingError::Format(
                format!("unknown filter method ({})", n).into()
            ))
        }
        let interlaced = match try!(buf.read_be()) {
            0u8 => false,
            1 => {
                self.adam7 = Some(utils::Adam7Iterator::new(width, height));
                true
            },
            n => return Err(DecodingError::Format(
                format!("unknown interlace method ({})", n).into()
            ))
        };
        let mut info = Info::default();

        info.width = width;
        info.height = height;
        info.bit_depth = bit_depth;
        info.color_type = color_type;
        info.interlaced = interlaced;
        self.info = Some(info);
        Ok(Decoded::Header(
            width,
            height,
            bit_depth,
            color_type,
            interlaced
        ))
    }
}

#[inline(always)]
pub fn get_info(d: &StreamingDecoder) -> Option<&Info> {
    d.info.as_ref()
}