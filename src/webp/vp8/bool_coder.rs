use ::{ImageError, ImageResult};
use super::Prob;

pub struct BoolReader {
    buf: Vec<u8>,
    index: usize,

    range: u32,
    value: u32,
    bit_count: u8,
}

impl BoolReader {
    pub fn new() -> BoolReader {
        BoolReader {
            buf: Vec::new(),
            range: 0,
            value: 0,
            bit_count: 0,
            index: 0,
        }
    }

    pub fn init(&mut self, buf: Vec<u8>) -> ImageResult<()> {
        if buf.len() < 2 {
            return Err(ImageError::FormatError(
                "Expected at least 2 bytes of decoder initialization data".into()));
        }

        self.buf = buf;
        // Direct access safe, since length has just been validated.
        self.value = (u32::from(self.buf[0]) << 8) | u32::from(self.buf[1]);
        self.index = 2;
        self.range = 255;
        self.bit_count = 0;

        Ok(())
    }

    pub fn read_bool(&mut self, probability: u8) -> bool {
        let split = 1 + (((self.range - 1) * u32::from(probability)) >> 8);
        let bigsplit = split << 8;

        let retval = if self.value >= bigsplit {
            self.range -= split;
            self.value -= bigsplit;
            true
        } else {
            self.range = split;
            false
        };

        while self.range < 128 {
            self.value <<= 1;
            self.range <<= 1;
            self.bit_count += 1;

            if self.bit_count == 8 {
                self.bit_count = 0;

                // If no more bits are available, just don't do anything.
                // This strategy is suggested in the reference implementation of RFC6386 (p.135)
                if self.index < self.buf.len() {
                    self.value |= u32::from(self.buf[self.index]);
                    self.index += 1;
                }
            }
        }

        retval
    }

    pub fn read_literal(&mut self, n: u8) -> u8 {
        let mut v = 0u8;
        let mut n = n;

        while n != 0 {
            v = (v << 1) + self.read_bool(128u8) as u8;
            n -= 1;
        }

        v
    }

    pub fn read_magnitude_and_sign(&mut self, n: u8) -> i32 {
        let magnitude = self.read_literal(n);
        let sign = self.read_literal(1);

        if sign == 1 {
            -i32::from(magnitude)
        } else {
            i32::from(magnitude)
        }
    }

    pub fn read_with_tree(&mut self, tree: &[i8], probs: &[Prob], start: isize) -> i8 {
        let mut index = start;

        loop {
            let a = self.read_bool(probs[index as usize >> 1]);
            let b = index + a as isize;
            index = tree[b as usize] as isize;

            if index <= 0 {
                break;
            }
        }

        -index as i8
    }

    pub fn read_flag(&mut self) -> bool {
        0 != self.read_literal(1)
    }
}
