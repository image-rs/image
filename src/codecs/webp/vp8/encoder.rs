//! An implementation of the VP8 Video Codec
//!
//! This module contains a partial implementation of the
//! VP8 video format as defined in RFC-6386.
//!
//! It encodes Keyframes only sans Loop Filtering.
//! VP8 is the underpinning of the WebP image format
//!
//! # Related Links
//! * [rfc-6386](http://tools.ietf.org/html/rfc6386) - The VP8 Data Format and Decoding Guide
//! * [VP8.pdf](http://static.googleusercontent.com/media/research.google.com/en//pubs/archive/37073.pdf) - An overview of
//! of the VP8 format
//!

use std::{convert::TryInto, io::Write};

type Prob = u8;

pub(crate) struct BoolEncoder {
    buffer: Vec<u8>,
    range: u32,
    bottom: u32,
    bit_count: i32,
}

impl BoolEncoder {
    pub(crate) fn new() -> BoolEncoder {
        BoolEncoder {
            buffer: Vec::new(),
            range: 255,
            bottom: 0,
            bit_count: 24,
        }
    }

    pub(crate) fn add_bool(&mut self, value: bool, prob: Prob) {
        let split = 1 + (((self.range - 1) * u32::from(prob)) >> 8);

        if value {
            self.bottom += split;
            self.range -= split;
        } else {
            self.range = split;
        }

        while self.range < 128 {
            self.range *= 2;

            //if first bit in u32 is a 1
            //if self.bottom & (1 << 31) != 0 {
            if self.bottom >= (1 << 31) {
                add_one_to_output(&mut self.buffer);
            }

            self.bottom *= 2;

            self.bit_count -= 1;
            if self.bit_count == 0 {
                self.buffer.push((self.bottom >> 24).try_into().unwrap()); //push the highest 8 bits into buffer

                self.bottom &= (1 << 24) - 1; //zero the highest 8 bits

                self.bit_count = 8;
            }
        }
    }

    pub(crate) fn add_literal(&mut self, n: u8, val: u8) {
        let mut i = 0u8;

        while i < n {
            self.add_bool(val & (1 << i) == (1 << i), 128u8);
            i += 1;
        }
    }

    pub(crate) fn write_to_file<W: Write>(&mut self, writer: &mut W) {
        let mut c = self.bit_count;
        let mut v = self.bottom;

        if v & (1 << (32 - c)) > 0 { 
            add_one_to_output(&mut self.buffer);
        }
        v <<= c & 7;               
        c >>= 3;                   
        c -= 1;
        while c >= 0 {
            v <<= 8;
            c -= 1;
        }
        c = 3;
        while c >= 0 { 
            self.buffer.push((v >> 24) as u8);
            v <<= 8;
            c -= 1;
        }

        writer.write(self.buffer.as_slice()).unwrap();
    }


}

fn add_one_to_output(val: &mut Vec<u8>) {
    let mut index = val.len()-1;
    while val[index] == 255 {
        val[index] = 0;
        index -= 1;
    }
    val[index] += 1;
}

/* pub(crate) fn encode(bools: &[bool], prob_zero: u8) {

    let mut write_file = std::fs::File::create("bools_test").unwrap();

    let mut encoder = BoolEncoder::new();

    for value in bools.iter() {
        encoder.add_bool(*value, prob_zero);
    }

    encoder.write_to_file(&mut write_file);
} */