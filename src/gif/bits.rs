//! This module provides a bit reader

use std::io;

/// A bit reader.
///
/// Reads bits from a byte stream, LSB first.
pub struct BitReader<R> where R: Reader{
    r: R,
    bits: u8,
    buf: u64,
}

impl<R: Reader> BitReader<R> {
    /// Creates a new bit reader
    pub fn new(reader: R) -> BitReader<R> {
        BitReader {
            r: reader,
            bits: 0,
            buf: 0,
        }
    }
    
    fn fill_cache(&mut self, n: u8) -> io::IoResult<()> {
        if n > 64 {
            return Err(io::IoError {
                kind: io::InvalidInput,
                desc: "Cannot read more than 64 bits",
                detail: None
            })
        }
        // FIXME: 64bit won't work this way
        while self.bits < n {
            self.buf |= try!(self.r.read_byte()) as u64 << self.bits as uint;
            self.bits += 8;
        }
        Ok(())
    }
    
    /// Returns the next `n` bits without consuming them.
    pub fn peek_bits(&mut self, n: u8) -> io::IoResult<u64> {
        try!(self.fill_cache(n));
        let mask = (1 << n as uint) - 1;
        Ok(self.buf & mask)
    }
    
    fn consume(&mut self, n: u8) {
        self.buf >>= n as uint;
        self.bits -= n;
    }

    /// Returns the next `n` bits.
    pub fn read_bits(&mut self, n: u8) -> io::IoResult<u64> {
        let res = try!(self.peek_bits(n));
        self.consume(n);
        Ok(res)
    }

    /// Returns true if the reader is aligned to a byte of the underlying byte stream.
    #[inline(always)]
    fn is_aligned(&self) -> bool {
        self.bits == 0
    }
}

impl<R: Reader> Reader for BitReader<R> {
    fn read(&mut self, buf: &mut [u8]) -> io::IoResult<uint> {
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

/// A bit writer.
///
/// Reads bits from a byte stream, LSB first.
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
            self.buf |= v as u8 << self.bits as uint;
            let missing = 8u8 - self.bits;
            if n >= missing {
                n -= missing;
                v >>= missing as uint;
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
    #[test]
    fn reader_writer() {
        let data = [255, 20, 40, 120, 128];
        let mut expanded_data = Vec::new();
        let mut reader = super::BitReader::new(data.as_slice());
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
        assert_eq!(data.as_slice(), compressed_data.as_slice())
    }
}