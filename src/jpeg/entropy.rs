use std::iter::repeat;

use image;
use image::ImageResult;

#[derive(Default, Clone)]
pub struct HuffTable {
    lut: Vec<(u8, u8)>,
    valptr: Vec<isize>,
    huffval: Vec<u8>,
    maxcode: Vec<isize>,
    mincode: Vec<isize>,
}

pub struct HuffDecoder {
    pub bits: u32,
    pub num_bits: u8,
    pub end: bool,
    pub marker: u8,
}

impl HuffDecoder {
    pub fn new() -> HuffDecoder {
        HuffDecoder {
            bits: 0,
            num_bits: 0,
            end: false,
            marker: 0
        }
    }

    fn guarantee<R: Reader>(&mut self, r: &mut R, n: u8) -> ImageResult<()> {
        while self.num_bits < n && !self.end {
            let byte = try!(r.read_u8());

            if byte == 0xFF {
                let byte2 = try!(r.read_u8());
                if byte2 != 0 {
                    self.marker = byte2;
                    self.end = true;
                }
            }

            self.bits |= ((byte as u32) << (32 - 8)) >> self.num_bits as usize;
            self.num_bits += 8;
        }

        Ok(())
    }

    pub fn read_bit<R: Reader>(&mut self, r: &mut R) -> ImageResult<u8> {
        let _   = try!(self.guarantee(r, 1));
        let bit = (self.bits & (1 << 31)) >> 31;
        self.consume(1);

        Ok(bit as u8)
    }

    // Section F.2.2.4
    // Figure F.17
    pub fn receive<R: Reader>(&mut self, r: &mut R, ssss: u8) -> ImageResult<i32> {
        let _ = try!(self.guarantee(r, ssss));
        let bits = (self.bits & (0xFFFFFFFFu32 << (32 - ssss as usize))) >> (32 - ssss) as usize;
        self.consume(ssss);

        Ok(bits as i32)
    }

    fn consume(&mut self, n: u8) {
        self.bits <<= n as usize;
        self.num_bits -= n;
    }

    pub fn decode_symbol<R: Reader>(&mut self, r: &mut R, table: &HuffTable) -> ImageResult<u8> {
        let _ = try!(self.guarantee(r, 8));

        let index = (self.bits & 0xFF000000) >> (32 - 8);
        let (val, size) = table.lut[index as usize];

        if index < 256 && size < 9 {
        self.consume(size);
            Ok(val)
        } else {
            let mut code = 0us;

            for i in (0us..16) {
                let b = try!(self.read_bit(r));
                code |= b as usize;

                if (code as isize) <= table.maxcode[i] {
                    let index = table.valptr[i] +
                                code as isize -
                                table.mincode[i];

                    return Ok(table.huffval[index as usize])
                }

                code <<= 1;
            }

            Err(image::ImageError::FormatError("Could not decode symbol.".to_string()))
        }
    }
}

/// Given an array containing the number of codes of each code length,
/// this function generates the huffman codes lengths and their respective
/// code lengths as specified by the JPEG spec.
fn derive_codes_and_sizes(bits: &[u8]) -> (Vec<u8>, Vec<u16>) {
    let mut huffsize = repeat(0u8).take(256).collect::<Vec<u8>>();
    let mut huffcode = repeat(0u16).take(256).collect::<Vec<u16>>();

    let mut k = 0;
    let mut j;

    // Annex C.2
    // Figure C.1
    // Generate table of individual code lengths
    for i in (0us..16) {
        j = 0;

        while j < bits[i] {
            huffsize[k] = i as u8 + 1;
            k += 1;
            j += 1;
        }
    }

    huffsize[k] = 0;

    // Annex C.2
    // Figure C.2
    // Generate table of huffman codes
    k = 0;
    let mut code = 0u16;
    let mut size = huffsize[0];

    while huffsize[k] != 0 {
        huffcode[k] = code;
        code += 1;
        k += 1;

        if huffsize[k] == size {
            continue
        }

        let diff = huffsize[k] - size;
        code <<= diff as usize;

        size += diff
    }

    (huffsize, huffcode)
}

pub fn build_huff_lut(bits: &[u8], huffval: &[u8]) -> Vec<(u8, u16)> {
    let mut lut = repeat((17u8, 0u16)).take(256).collect::<Vec<(u8, u16)>>();
    let (huffsize, huffcode) = derive_codes_and_sizes(bits);

    for (i, &v) in huffval.iter().enumerate() {
        lut[v as usize] = (huffsize[i], huffcode[i]);
    }

    lut
}

pub fn derive_tables(bits: Vec<u8>, huffval: Vec<u8>) -> HuffTable {
    let mut mincode = repeat(-1is).take(16).collect::<Vec<isize>>();
    let mut maxcode = repeat(-1is).take(16).collect::<Vec<isize>>();
    let mut valptr  = repeat(-1is).take(16).collect::<Vec<isize>>();
    let mut lut     = repeat((0u8, 17u8)).take(256).collect::<Vec<(u8, u8)>>();

    let (huffsize, huffcode) = derive_codes_and_sizes(&bits[]);

    // Annex F.2.2.3
    // Figure F.15
    let mut j = 0;

    for i in (0us..16) {
        if bits[i] != 0 {
            valptr[i] = j;
            mincode[i] = huffcode[j as usize] as isize;
            j += bits[i] as isize - 1;
            maxcode[i] = huffcode[j as usize] as isize;

            j += 1;
        }
    }

    for (i, v) in huffval.iter().enumerate() {
        if huffsize[i] > 8 {
            break
        }

        let r = 8 - huffsize[i] as usize;

        for j in (0us..1 << r) {
            let index = (huffcode[i] << r) + j as u16;
            lut[index as usize] = (*v, huffsize[i]);
        }
    }

    HuffTable {
        lut: lut,
        huffval: huffval,
        maxcode: maxcode,
        mincode: mincode,
        valptr: valptr
    }
}
