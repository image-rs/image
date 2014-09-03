use image;
use image::ImageResult;

macro_rules! io_try(
    ($e: expr) => (
        match $e {
            Ok(e) => e,
            Err(err) => return Err(image::IoError(err))
        }
    )
)

#[deriving(Default, Clone)]
pub struct HuffTable {
    lut: Vec<(u8, u8)>,
    valptr: Vec<int>,
    huffval: Vec<u8>,
    maxcode: Vec<int>,
    mincode: Vec<int>,
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
            let byte = io_try!(r.read_u8());

            if byte == 0xFF {
                let byte2 = io_try!(r.read_u8());
                if byte2 != 0 {
                    self.marker = byte2;
                    self.end = true;
                }
            }

            self.bits |= (byte as u32 << (32 - 8)) >> self.num_bits as uint;
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

    //Section F.2.2.4
    //Figure F.17
    pub fn receive<R: Reader>(&mut self, r: &mut R, ssss: u8) -> ImageResult<i32> {
        let _ = try!(self.guarantee(r, ssss));
        let bits = (self.bits & (0xFFFFFFFFu32 << (32 - ssss as uint))) >> (32 - ssss) as uint;
        self.consume(ssss);

        Ok(bits as i32)
    }

    fn consume(&mut self, n: u8) {
        self.bits <<= n as uint;
        self.num_bits -= n;
    }

    pub fn decode_symbol<R: Reader>(&mut self, r: &mut R, table: &HuffTable) -> ImageResult<u8> {
        let _ = try!(self.guarantee(r, 8));

        let index = (self.bits & 0xFF000000) >> (32 - 8);
        let (val, size) = table.lut[index as uint];

        if index < 256 && size < 9 {
        self.consume(size);
            Ok(val)
        } else {
            let mut code = 0u;

            for i in range(0u, 16) {
                let b = try!(self.read_bit(r));
                code |= b as uint;

                if (code as int) <= table.maxcode[i] {
                    let index = table.valptr[i] +
                                code as int -
                                table.mincode[i];

                    return Ok(table.huffval[index as uint])
                }

                code <<= 1;
            }

            Err(image::FormatError("Could not decode symbol.".to_string()))
        }
    }
}

/// Given an array containing the number of codes of each code length,
/// this function generates the huffman codes lengths and their respective
/// code lengths as specified by the JPEG spec.
fn derive_codes_and_sizes(bits: &[u8]) -> (Vec<u8>, Vec<u16>) {
    let mut huffsize = Vec::from_elem(256, 0u8);
    let mut huffcode = Vec::from_elem(256, 0u16);

    let mut k = 0;
    let mut j;

    //Annex C.2
    //Figure C.1
    //Generate table of individual code lengths
    for i in range(0u, 16) {
        j = 0;

        while j < bits[i] {
            huffsize.as_mut_slice()[k] = i as u8 + 1;
            k += 1;
            j += 1;
        }
    }

    huffsize.as_mut_slice()[k] = 0;

    //Annex C.2
    //Figure C.2
    //Generate table of huffman codes
    k = 0;
    let mut code = 0u16;
    let mut size = huffsize[0];

    while huffsize[k] != 0 {
        huffcode.as_mut_slice()[k] = code;
        code += 1;
        k += 1;

        if huffsize[k] == size {
            continue
        }

        let diff = huffsize[k] - size;
        code <<= diff as uint;

        size += diff
    }

    (huffsize, huffcode)
}

pub fn build_huff_lut(bits: &[u8], huffval: &[u8]) -> Vec<(u8, u16)> {
    let mut lut = Vec::from_elem(256, (17u8, 0u16));
    let (huffsize, huffcode) = derive_codes_and_sizes(bits);

    for (i, &v) in huffval.iter().enumerate() {
        lut.as_mut_slice()[v as uint] = (huffsize[i], huffcode[i]);
    }

    lut
}

pub fn derive_tables(bits: Vec<u8>, huffval: Vec<u8>) -> HuffTable {
    let mut mincode = Vec::from_elem(16, -1i);
    let mut maxcode = Vec::from_elem(16, -1i);
    let mut valptr  = Vec::from_elem(16, -1i);
    let mut lut     = Vec::from_elem(256, (0u8, 17u8));

    let (huffsize, huffcode) = derive_codes_and_sizes(bits.as_slice());

    //Annex F.2.2.3
    //Figure F.15
    let mut j = 0;

    for i in range(0u, 16) {
        if bits[i] != 0 {
            valptr.as_mut_slice()[i] = j;
            mincode.as_mut_slice()[i] = huffcode[j as uint] as int;
            j += bits[i] as int - 1;
            maxcode.as_mut_slice()[i] = huffcode[j as uint] as int;

            j += 1;
        }
    }

    for (i, v) in huffval.iter().enumerate() {
        if huffsize[i] > 8 {
            break
        }

        let r = 8 - huffsize[i] as uint;

        for j in range(0u, 1 << r) {
            let index = (huffcode[i] << r) + j as u16;
            lut.as_mut_slice()[index as uint] = (*v, huffsize[i]);
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