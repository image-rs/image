//! All IO functionality needed for TIFF decoding

use std::io;
use std::io::IoResult;
use utils::{lzw, bitstream};

/// Byte order of the TIFF file.
#[derive(Copy, Debug)]
pub enum ByteOrder {
    /// little endian byte order
    LittleEndian,
    /// big endian byte order
    BigEndian
}


/// Reader that is aware of the byte order.
pub trait EndianReader: Reader {
    /// Byte order that should be adhered to
    fn byte_order(&self) -> ByteOrder;
    
    /// Reads an u16
    #[inline(always)]
    fn read_u16(&mut self) -> IoResult<u16> {
        match self.byte_order() {
            ByteOrder::LittleEndian => self.read_le_u16(),
            ByteOrder::BigEndian => self.read_be_u16()
        }
    }
    
    /// Reads an u32
    #[inline(always)]
    fn read_u32(&mut self) -> IoResult<u32> {
        match self.byte_order() {
            ByteOrder::LittleEndian => self.read_le_u32(),
            ByteOrder::BigEndian => self.read_be_u32()
        }
    }
}

/// Reader that decompresses LZW streams
pub struct LZWReader {
    buffer: io::MemReader,
    byte_order: ByteOrder
}

impl LZWReader {
    /// Wraps a reader
    pub fn new<R>(reader: &mut SmartReader<R>) -> IoResult<(usize, LZWReader)> where R: Reader {
        let mut buffer = Vec::new();
        let order = reader.byte_order;
        try!(lzw::decode_early_change(bitstream::MsbReader::new(reader), &mut buffer, 8));
        let bytes = buffer.len();
        Ok((bytes, LZWReader {
            buffer: io::MemReader::new(buffer),
            byte_order: order
        }))
    }
}

impl Reader for LZWReader {
    #[inline]
    fn read(&mut self, buf: &mut [u8]) -> IoResult<usize> {
        self.buffer.read(buf)
    }
}

impl EndianReader for LZWReader {
    #[inline(always)]
    fn byte_order(&self) -> ByteOrder {
        self.byte_order
    }
}

/// Reader that is aware of the byte order.
#[derive(Debug)]
pub struct SmartReader<R> where R: Reader + Seek {
    reader: R,
    pub byte_order: ByteOrder
}

impl<R> SmartReader<R> where R: Reader + Seek {
    /// Wraps a reader
    pub fn wrap(reader: R, byte_order: ByteOrder) -> SmartReader<R> {
        SmartReader {
            reader: reader,
            byte_order: byte_order
        }
    }
}

impl<R> EndianReader for SmartReader<R> where R: Reader {
    #[inline(always)]
    fn byte_order(&self) -> ByteOrder {
        self.byte_order
    }
}

impl<R: Reader> Reader for SmartReader<R> {
    #[inline]
    fn read(&mut self, buf: &mut [u8]) -> IoResult<usize> {
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

impl<'a, R: Reader> Reader for &'a mut SmartReader<R> {
    #[inline]
    fn read(&mut self, buf: &mut [u8]) -> IoResult<usize> {
        self.reader.read(buf)
    }
}

impl<'a, R: Seek> Seek for &'a mut SmartReader<R> {
    #[inline]
    fn tell(&self) -> IoResult<u64> {
        self.reader.tell()
    }
    
    #[inline]
    fn seek(&mut self, pos: i64, style: io::SeekStyle) -> IoResult<()> {
        self.reader.seek(pos, style)
    }
}