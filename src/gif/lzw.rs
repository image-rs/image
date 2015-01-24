//! This modules provides an implementation of the Lempel–Ziv–Welch Compression Algorithm

use std::io;

use super::bits::{BitReader, LsbReader};

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
            buffer: Vec::with_capacity((1 << MAX_CODESIZE as usize) - 1)
        }
    }

    /// Resets the dictionary
    fn reset(&mut self) {
        self.table.clear();
        for i in (0..(1u16 << self.min_size as usize)) {
            self.table.push((None, i as u8));
        }
    }

    /// Inserts a value into the dict
    #[inline(always)]
    fn push(&mut self, key: Option<Code>, value: u8) {
        self.table.push((key, value))
    }

    /// Reconstructs the data for the corresponding code
    fn reconstruct(&mut self, code: Option<Code>) -> io::IoResult<&[u8]> {
        self.buffer.clear();
        let mut code = code;
        let mut cha;
        // Check the first access more thoroughly since a bad code
        // could occur if the data is malformed
        if let Some(k) = code {
            match self.table.get(k as usize) {
                Some(&(code_, cha_)) => {
                    code = code_;
                    cha = cha_;
                }
                None => return Err(io::IoError {
                    kind: io::InvalidInput,
                    desc: "invalid code occured",
                    detail: Some(format!("{} < {} expected", k, self.table.len()))
                })
            }
            self.buffer.push(cha);
        }
        while let Some(k) = code {
            //(code, cha) = self.table[k as usize];
            // Node this could possibly replaced by unsafe access because this
            // struct had been contructed by this algorithm correctly
            let entry = self.table[k as usize]; code = entry.0; cha = entry.1;
            self.buffer.push(cha);
        }
        self.buffer.reverse();
        Ok(&self.buffer[])
    }

    /// Returns the buffer constructed by the last reconstruction
    #[inline(always)]
    fn buffer(&self) -> &[u8] {
        &self.buffer[]
    }

    /// Number of entries in the dictionary
    #[inline(always)]
    fn next_code(&self) -> u16 {
        self.table.len() as u16
    }
}

/// Decodes a lzw compressed stream
pub fn decode<R, W>(mut r: R, w: &mut W, min_code_size: u8) -> io::IoResult<()>
where R: BitReader, W: Writer {
    let mut prev = None;
    let clear_code = 1 << min_code_size as usize;
    let end_code = clear_code + 1;
    let mut table = DecodingDict::new(min_code_size);
    let mut code_size = min_code_size + 1;
    loop {
        let code = try!(r.read_bits(code_size));
        if code == clear_code {
            table.reset();
            table.push(None, 0); // clear code
            table.push(None, 0); // end code
            code_size = min_code_size + 1;
            prev = None;
        } else if code == end_code {
            return Ok(())
        } else {
            let next_code = table.next_code();
            if prev.is_none() {
                try!(w.write_u8(code as u8));
            } else {
                let data = if code == next_code {
                    let cha = try!(table.reconstruct(prev))[0];
                    table.push(prev, cha);
                    try!(table.reconstruct(Some(code)))
                } else if code < next_code {
                    let cha = try!(table.reconstruct(Some(code)))[0];
                    table.push(prev, cha);
                    table.buffer()
                } else {
                    return Err(io::IoError {
                        kind: io::InvalidInput,
                        desc: "Invalid code",
                        detail: Some(format!("expected {} <= {}", 
                                     code,
                                     next_code)
                                )
                    })
                };
                try!(w.write(data));
            }
            if next_code == (1 << code_size as usize) - 1
               && code_size < MAX_CODESIZE {
                code_size += 1;
            }
            prev = Some(code);
        }
    }
}
