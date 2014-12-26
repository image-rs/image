//! This modules provides an implementation of the Lempel–Ziv–Welch Compression Algorithm

use std::io;
use std::collections::HashMap;
use std::collections::hash_map::Entry::{Vacant, Occupied};

use super::bits::BitReader;

const MAX_CODESIZE: u8 = 12;

/// Alias for a LZW code point
type Code = u16;

/// Decoding dictionary
/// It is not generic due to current limitations of Rust
/// Inspired by http://www.cplusplus.com/articles/iL18T05o/
struct DecodingDict {
    min_size: u8,
    table: Vec<(Option<Code>, u8)>,
    buffer: Vec<u8>,
}

impl DecodingDict {
    /// Creates a new dict
    fn new(min_size: u8) -> DecodingDict {
        DecodingDict {
            min_size: min_size,
            table: Vec::with_capacity(512),
            buffer: Vec::with_capacity((1 << MAX_CODESIZE as uint) - 1)
        }
    }

    /// Resets the dictionary
    fn reset(&mut self) {
        self.table.clear();
        for i in range(0, (1u16 << self.min_size as uint)) {
            self.table.push((None, i as u8));
        }
    }

    /// Inserts a value into the dict
    fn push(&mut self, key: Option<Code>, value: u8) {
        self.table.push((key, value))
    }

    /// Reconstructs the data for the corresponding code
    fn reconstruct(&mut self, code: Option<Code>) -> &[u8] {
        self.buffer.clear();
        let mut code = code;
        let mut cha;
        while let Some(k) = code {
            //(code, cha) = self.table[k as uint];
            let entry = self.table[k as uint]; code = entry.0; cha = entry.1;
            self.buffer.push(cha);
        }
        self.buffer.reverse();
        self.buffer.as_slice()
    }

    /// Returns the buffer constructed by the last reconstruction
    fn buffer(&self) -> &[u8] {
        self.buffer.as_slice()
    }

    /// Number of entries in the dictionary
    fn next_code(&self) -> u16 {
        self.table.len() as u16
    }
}

/// LZW decoder
pub struct LZWReader<R: Reader> {
    r: BitReader<R>,
    table: DecodingDict,
    code_size: u8,
    minimal_size: u8,
    clear_code: u16,
    end_code: u16,

}

impl<R: Reader> LZWReader<R> {
    /// Creates a new LZW reader with a minimum code size of `size`
    pub fn new(reader: R, size: u8) -> LZWReader<R> {
        let clear_code = 1 << size as uint;
        let end_code = clear_code + 1;
        LZWReader {
            r: BitReader::new(reader),
            table: DecodingDict::new(size),
            code_size: size + 1,
            minimal_size: size,
            clear_code: clear_code,
            end_code: end_code,
        }
    }

    /// Resets the code table
    fn reset(&mut self) {
        self.table.reset();
        self.table.push(None, 0); // clear code
        self.table.push(None, 0); // end code
        self.code_size = self.minimal_size + 1;
    }

    /// Decodes the data into a `Writer`
    pub fn decode<W: Writer>(&mut self, stream: &mut W) -> io::IoResult<()> {
        let mut prev = None;
        loop {
            let code = try!(self.r.read_bits(self.code_size)) as u16;
            if code == self.clear_code {
                self.reset();
                prev = None;
            } else if code == self.end_code {
                return Ok(())
            } else {
                let next_code = self.table.next_code();
                let data = if code == next_code {
                    let cha = self.table.reconstruct(prev)[0];
                    self.table.push(prev, cha);
                    self.table.reconstruct(Some(code))
                } else if code > next_code {
                    return Err(io::IoError {
                        kind: io::InvalidInput,
                        desc: "Invalid code",
                        detail: None
                    })
                } else {
                    let cha = self.table.reconstruct(Some(code))[0];
                    self.table.push(prev, cha);
                    self.table.buffer()
                };
                try!(stream.write(data));
                if next_code == (1 << self.code_size as uint) 
                   && self.code_size < MAX_CODESIZE {
                    self.code_size += 1;
                }
                prev = Some(code);
            }
        }
    }
}