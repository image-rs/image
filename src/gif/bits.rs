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