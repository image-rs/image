//! This module provides a bit reader

use std::io;

/// Bit reader
pub trait BitReader: Reader {
    /// Returns the next `n` bits.
    fn read_bits(&mut self, n: u8) -> io::IoResult<u16>;
}

macro_rules! define_bit_readers {
    {$(
        $name:ident, #[$doc:meta];
    )*} => {

$( // START Structure definitions

#[$doc]
pub struct $name<R> where R: Reader {
    r: R,
    bits: u8,
    acc: u32,
}

impl<R: Reader> $name<R> {

    /// Creates a new bit reader
    pub fn new(reader: R) -> $name<R> {
        $name {
            r: reader,
            bits: 0,
            acc: 0,
        }
    }

    /// Returns true if the reader is aligned to a byte of the underlying byte stream.
    #[inline(always)]
    fn is_aligned(&self) -> bool {
        self.bits == 0
    }


}

impl<R: Reader> Reader for $name<R> {
    fn read(&mut self, buf: &mut [u8]) -> io::IoResult<usize> {
        if self.is_aligned() {
            self.r.read(buf)
        } else {
            let mut i = 0;
            for (j, byte) in buf.iter_mut().enumerate() {
                *byte = try!(self.read_bits(8)) as u8;
                i = j;
            }
            Ok(i)
        }
    }
}

)* // END Structure definitions

    }
}

define_bit_readers!{
    LsbReader, #[doc = "Reads bits from a byte stream, LSB first."];
    MsbReader, #[doc = "Reads bits from a byte stream, MSB first."];
}

impl<R> BitReader for LsbReader<R> where R: Reader {

    fn read_bits(&mut self, n: u8) -> io::IoResult<u16> {
        if n > 16 {
            return Err(io::IoError {
                kind: io::InvalidInput,
                desc: "Cannot read more than 16 bits",
                detail: None
            })
        }
        while self.bits < n {
            self.acc |= (try!(self.r.read_u8()) as u32) << self.bits;
            self.bits += 8;
        }
        let res = self.acc & ((1 << n) - 1);
        self.acc >>= n;
        self.bits -= n;
        Ok(res as u16)
    }

}

impl<R> BitReader for MsbReader<R> where R: Reader {

    fn read_bits(&mut self, n: u8) -> io::IoResult<u16> {
        if n > 16 {
            return Err(io::IoError {
                kind: io::InvalidInput,
                desc: "Cannot read more than 16 bits",
                detail: None
            })
        }
        while self.bits < n {
            self.acc |= (try!(self.r.read_u8()) as u32) << (24 - self.bits);
            self.bits += 8;
        }
        let res = self.acc >> (32 - n);
        self.acc <<= n;
        self.bits -= n;
        Ok(res as u16)
    }
}

/// A bit writer.
///
/// Write bits to a byte stream, LSB first.
#[allow(dead_code)]
pub struct BitWriter<'a, W> where W: Writer + 'a {
    w: &'a mut W,
    bits: u8,
    buf: u8,
}
#[allow(dead_code)]
impl<'a, W> BitWriter<'a, W> where W: Writer + 'a {
    /// Creates a new bit reader
    pub fn new(writer: &'a mut W) -> BitWriter<'a, W> {
        BitWriter {
            w: writer,
            bits: 0,
            buf: 0,
        }
    }

    /// Returns the next `n` bits.
    pub fn write_bits(&mut self, mut v: u32, mut n: u8) -> io::IoResult<()> {
        while n > 0 {
            self.buf |= (v as u8) << self.bits as usize;
            let missing = 8u8 - self.bits;
            if n >= missing {
                n -= missing;
                v >>= missing as usize;
                try!(self.w.write_u8(self.buf));
                self.bits = 0;
                self.buf = 0;
            } else {
                self.bits += n;
                break
            }
        }
        Ok(())
    }


    /// Flushes the internal accumulator fill missing bits with 0
    pub fn flush(&mut self) -> io::IoResult<()> {
        if self.bits > 0 {
            try!(self.w.write_u8(self.buf));
            self.bits = 0;
            self.buf = 0;
        }
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::BitReader;

    #[test]
    fn reader_writer() {
        let data = [255, 20, 40, 120, 128];
        let mut expanded_data = Vec::new();
        let mut reader = super::LsbReader::new(&data[]);
        while let Ok(b) = reader.read_bits(10) {
            expanded_data.push(b as u32)
        }
        let mut compressed_data = Vec::new();
        {
            let mut writer = super::BitWriter::new(&mut compressed_data);
            for &datum in expanded_data.iter() {
                let _  = writer.write_bits(datum, 10);
            }   
        }
        assert_eq!(&data[], &compressed_data[])
    }
}
