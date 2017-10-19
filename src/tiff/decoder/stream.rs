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

/// Reader that unpacks Apple's PackBits format
pub struct PackBitsReader {
    buffer: io::Cursor<Vec<u8>>,
    byte_order: ByteOrder
}

impl PackBitsReader {
    /// Wraps a reader
    pub fn new<R: Read + Seek>(mut reader: R, byte_order: ByteOrder, length: usize) -> io::Result<(usize, PackBitsReader)> {
        let mut buffer = Vec::new();
        let mut read: usize = 0;
        while read < length {
            read += try!(read_packbits_run(&mut reader, &mut buffer));
        }
        Ok((buffer.len(), PackBitsReader {
            buffer: io::Cursor::new(buffer),
            byte_order: byte_order
        }))
    }
}

fn read_packbits_run<R: Read + Seek>(reader: &mut R, buffer: &mut Vec<u8>) -> io::Result<usize> {
    let mut header: [u8; 1] = [0];

    let bytes = try!(reader.read(&mut header));

    match bytes {
        0 => Ok(0),
        _ => match header[0] as i8 {
            -128 => Ok(1),
            h if h >= -127 && h <= -1 => {
                let new_len = buffer.len() + (1 - h as isize) as usize;
                try!(reader.read_exact(&mut header));
                buffer.resize(new_len, header[0]);
                Ok(2)
            },
            h => {
                let num_vals = h as usize + 1;
                let start = buffer.len();
                buffer.resize(start + num_vals, 0);
                try!(reader.read_exact(&mut buffer[start..]));
                Ok(num_vals + 1)
            }
        }
    }
}

impl Read for PackBitsReader {
    #[inline]
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.buffer.read(buf)
    }
}

impl EndianReader for PackBitsReader {
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_packbits() {
        let encoded = vec![0xFE, 0xAA, 0x02, 0x80, 0x00, 0x2A, 0xFD, 0xAA,
                           0x03, 0x80, 0x00, 0x2A, 0x22, 0xF7, 0xAA];
        let encoded_len = encoded.len();

        let buff = io::Cursor::new(encoded);
        let (_, mut decoder) = PackBitsReader::new(buff, ByteOrder::LittleEndian, encoded_len).unwrap();

        let mut decoded = Vec::new();
        decoder.read_to_end(&mut decoded).unwrap();

        let expected = vec![0xAA, 0xAA, 0xAA, 0x80, 0x00, 0x2A, 0xAA, 0xAA,
                            0xAA, 0xAA, 0x80, 0x00, 0x2A, 0x22, 0xAA, 0xAA,
                            0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA];
        assert_eq!(decoded, expected);
    }
}
