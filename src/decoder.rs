use std::io;
use std::cmp::min;
use std::convert::From;

use crc::Crc32;
use traits::ReadBytesExt;

const EXTENSION_BUFFER: usize = 1024;

#[derive(Debug)]
enum U32Value {
    // CHUNKS
    Length,
    Type(u32),
    Crc([u8; 4])
}

#[derive(Debug)]
enum ByteValue {
    BitDepth,
    ColorType,
    CompressionMethod,
    FilterMethod,
    InterlaceMethod
}

#[derive(Debug)]
enum StopPosition {
    ChunkComplete([u8; 4]),
    PartialChunk(u32, [u8; 4])
}
    
#[derive(Debug)]
enum State {
    Signature(u8, [u8; 7]),
    StoppedAt(StopPosition),
    U32Byte3(U32Value, u32),
    U32Byte2(U32Value, u32),
    U32Byte1(U32Value, u32),
    U32(U32Value),
    ReadChunk(u32, [u8; 4]),
    //Byte(ByteValue),
}

#[derive(Debug)]
pub enum DecodingError {
    InvalidSignature,
    CrcMismatch,
    IoError(io::Error)
}

impl From<io::Error> for DecodingError {
    fn from(err: io::Error) -> DecodingError {
        DecodingError::IoError(err)
    }
}

#[derive(Debug)]
pub struct Decoder {
    state: Option<State>,
    width: u32,
    height: u32,
    current_chunk: (Crc32, Vec<u8>)
}

impl Decoder {
    pub fn new() -> Decoder {
        Decoder {
            state: Some(State::Signature(0, [0; 7])),
            width: 0,
            height: 0,
            current_chunk: (Crc32::new(), Vec::with_capacity(EXTENSION_BUFFER))
        }
    }
    
    pub fn update(&mut self, mut buf: &[u8])
    -> Result<usize, DecodingError> {
        let len = buf.len();
        while buf.len() > 0 && self.state.is_some() {
            match self.next_state(buf) {
                Ok(bytes) => {
                    buf = &buf[bytes..]
                }
                Err(err) => return Err(err)
            }
        }
        Ok(len-buf.len())
    }
    
    fn next_state(&mut self, buf: &[u8]) -> Result<usize, DecodingError> {
        use self::State::*;
        
        macro_rules! goto (
            ($n:expr, $state:expr) => ({
                self.state = Some($state); 
                Ok($n)
            });
            ($state:expr) => ({
                self.state = Some($state); 
                Ok(1)
            })
        );
        
        let current_byte = buf[0];
        
        // Driver should ensure that state is never None
        let state = self.state.take().unwrap();
        println!("{:?}", state);

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
            StoppedAt(pos) => {
                use self::StopPosition::*;
                match pos {
                    ChunkComplete(type_str) => goto!(
                        0, try!(self.parse_chunk(type_str))
                    ),
                    PartialChunk(remaining, type_str) => {
                        // TODO handle this
                        self.current_chunk.1.clear();
                        goto!(0, ReadChunk(remaining, type_str))
                    },
                }
            }
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
                        self.current_chunk.1.clear();
                        goto!(ReadChunk(length, type_str))
                    },
                    Crc(type_str) => {
                        if val == self.current_chunk.0.checksum() {
                            goto!(StoppedAt(StopPosition::ChunkComplete(type_str)))
                        } else {
                            Err(DecodingError::CrcMismatch)
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
            ReadChunk(remaining, type_str) => {
                let (ref mut crc, ref mut c_buf) = self.current_chunk;
                let buf_avail = c_buf.capacity() - c_buf.len();
                let bytes_avail = min(buf.len(), buf_avail);
                let n = min(remaining, bytes_avail as u32);
                if remaining > 0 {
                    if buf_avail == 0 {
                        goto!(0, StoppedAt(StopPosition::PartialChunk(
                            remaining, type_str
                        )))
                    } else {
                        let buf = &buf[..n as usize];
                        crc.update(buf);
                        c_buf.push_all(buf);
                        goto!(n as usize, ReadChunk(remaining - n, type_str))
                    }
                } else {
                    goto!(0, U32(U32Value::Crc(type_str)))
                }
            }
        }
    }
    
    fn parse_chunk(&mut self, type_str: [u8; 4]) -> Result<State, DecodingError> {
        match &type_str {
            b"IHDR" => {
                self.parse_ihdr()
            }
            // Skip unknown chunks:
            _ => Ok(State::U32(U32Value::Length))
        }
    }
    
    fn parse_ihdr(&mut self) -> Result<State, DecodingError> {
        let mut buf = &self.current_chunk.1[..];
        self.width = try!(buf.read_be());
        self.height = try!(buf.read_be());
        Ok(State::U32(U32Value::Length))
    }
}

#[test]
fn test() {
    use std::io::prelude::*;
    use std::fs::File;
    let mut data = Vec::new();
    File::open("tests/samples/lenna_fragment_interlaced.png").unwrap().read_to_end(&mut data).unwrap();
    let mut decoder = Decoder::new();
    decoder.update(&*data).unwrap();
    println!("{:?}", decoder);
    panic!()
}
