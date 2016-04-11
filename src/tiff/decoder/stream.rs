//! All IO functionality needed for TIFF decoding

use std::io;
use std::io::{Read, Seek};
use byteorder::{ReadBytesExt, BigEndian, LittleEndian};
use utils::{lzw, bitstream};

/// Byte order of the TIFF file.
#[derive(Clone, Copy, Debug)]
pub enum ByteOrder {
    /// little endian byte order
    LittleEndian,
    /// big endian byte order
    BigEndian
}


/// Reader that is aware of the byte order.
pub trait EndianReader: Read {
    /// Byte order that should be adhered to
    fn byte_order(&self) -> ByteOrder;

    /// Reads an u16
    #[inline(always)]
    fn read_u16(&mut self) -> Result<u16, io::Error> {
        match self.byte_order() {
            ByteOrder::LittleEndian => <Self as ReadBytesExt>::read_u16::<LittleEndian>(self),
            ByteOrder::BigEndian => <Self as ReadBytesExt>::read_u16::<BigEndian>(self)
        }
    }

    /// Reads an u32
    #[inline(always)]
    fn read_u32(&mut self) -> Result<u32, io::Error> {
        match self.byte_order() {
            ByteOrder::LittleEndian => <Self as ReadBytesExt>::read_u32::<LittleEndian>(self),
            ByteOrder::BigEndian => <Self as ReadBytesExt>::read_u32::<BigEndian>(self)
        }
    }
}

/// Reader that decompresses LZW streams
pub struct LZWReader {
    buffer: io::Cursor<Vec<u8>>,
    byte_order: ByteOrder
}

impl LZWReader {
    /// Wraps a reader
    pub fn new<R>(reader: &mut SmartReader<R>) -> io::Result<(usize, LZWReader)> where R: Read + Seek {
        let mut buffer = Vec::new();
        let order = reader.byte_order;
        try!(lzw::decode_early_change(bitstream::MsbReader::new(reader), &mut buffer, 8));
        let bytes = buffer.len();
        Ok((bytes, LZWReader {
            buffer: io::Cursor::new(buffer),
            byte_order: order
        }))
    }
}

impl Read for LZWReader {
    #[inline]
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
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
pub struct SmartReader<R> where R: Read + Seek {
    reader: R,
    pub byte_order: ByteOrder
}

impl<R> SmartReader<R> where R: Read + Seek {
    /// Wraps a reader
    pub fn wrap(reader: R, byte_order: ByteOrder) -> SmartReader<R> {
        SmartReader {
            reader: reader,
            byte_order: byte_order
        }
    }
}

impl<R> EndianReader for SmartReader<R> where R: Read + Seek {
    #[inline(always)]
    fn byte_order(&self) -> ByteOrder {
        self.byte_order
    }
}

impl<R: Read + Seek> Read for SmartReader<R> {
    #[inline]
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.reader.read(buf)
    }
}

impl<R: Read + Seek> Seek for SmartReader<R> {
    #[inline]
    fn seek(&mut self, pos: io::SeekFrom) -> io::Result<u64> {
        self.reader.seek(pos)
    }
}
