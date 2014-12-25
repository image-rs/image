//! This modules provides an implementation of the Lempel–Ziv–Welch Compression Algorithm

use std::io;
use std::collections::HashMap;
use std::collections::hash_map::Entry::{Vacant, Occupied};

use super::bits::BitReader;

const MAX_CODESIZE: u8 = 12;

/// LZW decoder
pub struct LZWReader<R: Reader> {
    r: BitReader<R>,
    table: HashMap<u16, Vec<u8>>,
    code_size: u8,
    next_code: u16,
    prev_code: Option<u16>,
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
            table: HashMap::new(),
            code_size: size + 1,
            next_code: end_code + 1,
            prev_code: None,
            minimal_size: size,
            clear_code: clear_code,
            end_code: end_code,
        }
    }

    /// Resets the code table
    fn reset(&mut self) {
        self.table.clear();
        self.code_size = self.minimal_size + 1;
        self.next_code = self.end_code + 1;
        self.prev_code = None;
        for i in range(0, (1 << self.minimal_size as uint)) {
            self.table.insert(i, vec![i as u8]);
        }
    }

    /// Decodes the data into a `Writer`
    pub fn decode<W: Writer>(&mut self, stream: &mut W) -> io::IoResult<()> {
        //let code = try!(self.r.read_bits(self.code_size)) as u16;
        //let mut prev = Some(if code == self.clear_code {
        //    self.reset();
        //    (try!(self.r.read_bits(self.code_size)) as u8)
        //} else {
        //    code as u8
        //} as u16);
        //if let Some(prev) = prev {
        //    try!(stream.write_u8(prev as u8));   
        //}
        loop {
            let code = try!(self.r.read_bits(self.code_size)) as u16;
            //println!("code {} ({} bits), prev {}", code, self.code_size, self.prev_code);
            if code == self.clear_code {
                self.reset();
                //println!("reset");
            } else if code == self.end_code {
                return Ok(())
            } else {
                self.prev_code = if let Some(prev) = self.prev_code {
                    let mut prefix = self.table[prev].clone();
                    let k = match self.table.entry(code) {
                        Occupied(entry) => {
                            try!(stream.write(entry.get().as_slice()));
                            entry.get()[0]
                        },
                        Vacant(_) => {
                            try!(stream.write(prefix.as_slice()));
                            try!(stream.write(prefix.slice_to(1)));
                            prefix[0]
                        },
                    };
                    prefix.push(k);
                    self.table.insert(self.next_code, prefix);
                    if self.next_code == (1 << self.code_size as uint) - 1 
                       && self.code_size < MAX_CODESIZE {
                        self.code_size += 1;
                    }
                    self.next_code += 1;
                    Some(code)
                } else {
                    try!(stream.write_u8(code as u8));
                    Some(code)
                }
            }
        }
    }
}