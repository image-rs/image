//! All IO functionality needed for TIFF decoding

use std::io::{
    self,
    Read
};
use utils::{
    lzw,
    bitstream
};

/// Byte order of the TIFF file.
#[derive(Copy, Debug)]
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
    fn read_u16(&mut self) -> io::Result<u16> {
        match self.byte_order() {
            ByteOrder::LittleEndian => self.read_le_u16(),
            ByteOrder::BigEndian => self.read_be_u16()
        }
    }

    /// Reads an u32
    #[inline(always)]
    fn read_u32(&mut self) -> io::Result<u32> {
        match self.byte_order() {
            ByteOrder::LittleEndian => self.read_le_u32(),
            ByteOrder::BigEndian => self.read_be_u32()
        }
    }
}

/// Reader that decompresses LZW streams
pub struct LZWReader<'a> {
    buffer: io::Cursor<&'a [u8]>,
    byte_order: ByteOrder
}

impl<'a> LZWReader<'a> {
    /// Wraps a reader
    pub fn new<R>(reader: &mut SmartReader<R>) -> io::Result<(usize, LZWReader)>
        where R: Read + Seek {
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

impl<'a> Read for LZWReader<'a> {
    #[inline]
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.buffer.read(buf)
    }
}

impl<'a> EndianReader for LZWReader<'a> {
    #[inline(always)]
    fn byte_order(&self) -> ByteOrder {
        self.byte_order
    }
}

/// Reader that is aware of the byte order.
#[derive(Debug)]
pub struct SmartReader<R>
where R: Read + Seek {
    reader: R,
    pub byte_order: ByteOrder
}

impl<R> SmartReader<R>
where R: Read + Seek {
    /// Wraps a reader
    pub fn wrap(reader: R, byte_order: ByteOrder) -> SmartReader<R> {
        SmartReader {
            reader: reader,
            byte_order: byte_order
        }
    }
}

impl<R> EndianReader for SmartReader<R>
where R: Read + Seek {
    #[inline(always)]
    fn byte_order(&self) -> ByteOrder {
        self.byte_order
    }
}

impl<R: Read + Seek> Read for SmartReader<R> {
    // #[inline]
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.reader.read(buf)
    }
}

impl<R: Read + Seek> Seek for SmartReader<R> {
    #[inline]
    fn tell(&self) -> io::Result<u64> {
        self.reader.tell()
    }

    #[inline]
    fn seek(&mut self, pos: io::SeekFrom) -> io::Result<u64> {
        self.reader.seek(pos)
    }
}

impl<'a, R: Read + Seek> Seek for &'a mut SmartReader<R> {
    #[inline]
    fn tell(&self) -> io::Result<u64> {
        self.reader.tell()
    }

    #[inline]
    fn seek(&mut self, pos: io::SeekFrom) -> io::Result<u64> {
        self.reader.seek(pos)
    }
}
