//! This modules provides an implementation of the Lempel–Ziv–Welch Compression Algorithm

// FIXME remove as soon as gif encoder is done
#![allow(dead_code)]

// Note: This implementation borrows heavily from the work of Julius Pettersson
// See http://www.cplusplus.com/articles/iL18T05o/ for his extensive explanations
// and a C++ implementatation

use std::io;
use std::io::{Read, Write};

use utils::bitstream::{BitReader, BitWriter};

const MAX_CODESIZE: u8 = 12;
const MAX_ENTRIES: usize = 1 << MAX_CODESIZE as usize;

/// Alias for a LZW code point
type Code = u16;

/// Decoding dictionary
///
/// It is not generic due to current limitations of Rust
///
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
        for i in 0..(1u16 << self.min_size as usize) {
            self.table.push((None, i as u8));
        }
    }

    /// Inserts a value into the dict
    #[inline(always)]
    fn push(&mut self, key: Option<Code>, value: u8) {
        self.table.push((key, value))
    }

    /// Reconstructs the data for the corresponding code
    fn reconstruct(&mut self, code: Option<Code>) -> io::Result<&[u8]> {
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
                None => return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    &format!("invalid code occured: {} < {} expected", k, self.table.len())[..],
                ))
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
        Ok(&self.buffer)
    }

    /// Returns the buffer constructed by the last reconstruction
    #[inline(always)]
    fn buffer(&self) -> &[u8] {
        &self.buffer
    }

    /// Number of entries in the dictionary
    #[inline(always)]
    fn next_code(&self) -> u16 {
        self.table.len() as u16
    }
}


macro_rules! define_decoder_function {
    {$(
        $name:ident, $offset:expr, #[$doc:meta];
    )*} => {

$( // START function definition

#[$doc]
pub fn $name<R, W>(mut r: R, w: &mut W, min_code_size: u8) -> io::Result<()>
where R: BitReader, W: Write {
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
                try!(w.write_all(&[code as u8]));
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
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidInput,
                        &format!("Invalid code: expected {} <= {}", code, next_code)[..],
                    ))
                };
                try!(w.write(data));
            }
            if next_code == (1 << code_size as usize) - 1 - $offset
               && code_size < MAX_CODESIZE {
                code_size += 1;
            }
            prev = Some(code);
        }
    }
}

)* // END function definition

    }
}

define_decoder_function!{
    decode, 0, #[doc = "Decodes a lzw compressed stream."];
    decode_early_change, 1, #[doc = "Decodes a lzw compressed stream using an “early change” algorithm."];
}


struct Node {
    prefix: Option<Code>,
    c: u8,
    left: Option<Code>,
    right: Option<Code>,
}

impl Node {
    #[inline(always)]
    fn new(c: u8) -> Node {
        Node {
            prefix: None,
            c: c,
            left: None,
            right: None
        }
    }
}

struct EncodingDict {
    table: Vec<Node>,
    min_size: u8,

}

/// Encoding dictionary based on a binary tree
impl EncodingDict {
    fn new(min_size: u8) -> EncodingDict {
        let mut this = EncodingDict {
            table: Vec::with_capacity(MAX_ENTRIES),
            min_size: min_size
        };
        this.reset();
        this
    }

    fn reset(&mut self) {
        self.table.clear();
        for i in 0 .. (1u16 << self.min_size as usize) {
            self.push_node(Node::new(i as u8));
        }
    }

    #[inline(always)]
    fn push_node(&mut self, node: Node) {
        self.table.push(node)
    }

    #[inline(always)]
    fn clear_code(&self) -> Code {
        1u16 << self.min_size
    }

    #[inline(always)]
    fn end_code(&self) -> Code {
        self.clear_code() + 1
    }

    // Searches for a new prefix
    fn search_and_insert(&mut self, i: Option<Code>, c: u8) -> Option<Code> {
        if let Some(i) = i.map(|v| v as usize) {
            let table_size = self.table.len() as Code;
            if let Some(mut j) = self.table[i].prefix {
                loop {
                    let entry = &mut self.table[j as usize];
                    if c < entry.c {
                        if let Some(k) = entry.left {
                            j = k
                        } else {
                            entry.left = Some(table_size);
                            break
                        }
                    } else if c > entry.c {
                        if let Some(k) = entry.right {
                            j = k
                        } else {
                            entry.right = Some(table_size);
                            break
                        }
                    } else {
                        return Some(j)
                    }
                }
            } else {
                self.table[i].prefix = Some(table_size);
            }
            self.table.push(Node::new(c));
            None
        } else {
            Some(self.search_initials(c as Code))
        }
    }

    fn next_code(&self) -> usize {
        self.table.len()
    }

    fn search_initials(&self, i: Code) -> Code {
        self.table[i as usize].c as Code
    }
}

pub fn encode<R, W>(r: R, mut w: W, min_code_size: u8) -> io::Result<()>
where R: Read, W: BitWriter {
    let mut dict = EncodingDict::new(min_code_size);
    dict.push_node(Node::new(0)); // clear code
    dict.push_node(Node::new(0)); // end code
    let mut code_size = min_code_size + 1;
    let mut i = None;
    // gif spec: first clear code
    try!(w.write_bits(dict.clear_code(), code_size));
    let mut r = r.bytes();
    while let Some(Ok(c)) = r.next() {
        let prev = i;
        i = dict.search_and_insert(prev, c);
        if i.is_none() {
            if let Some(code) = prev {
                try!(w.write_bits(code, code_size));
            }
            i = Some(dict.search_initials(c as Code))
        }
        // There is a hit: do not write out code but continue
        let next_code = dict.next_code();
        if next_code > (1 << code_size as usize)
           && code_size < MAX_CODESIZE {
            code_size += 1;
        }
        if next_code > MAX_ENTRIES {
            dict.reset();
            dict.push_node(Node::new(0)); // clear code
            dict.push_node(Node::new(0)); // end code
            try!(w.write_bits(dict.clear_code(), code_size));
            code_size = min_code_size + 1;
        }

    }
    if let Some(code) = i {
        try!(w.write_bits(code, code_size));
    }
    try!(w.write_bits(dict.end_code(), code_size));
    try!(w.flush());
    Ok(())
}
