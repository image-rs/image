//! Decoding and Encoding of JPEG Images
//!
//! JPEG (Joint Photographic Experts Group) is an image format
//! that supports lossy compression.
//! This module implements the Baseline JPEG standard.
//!
//! #Related Links
//! * http://www.w3.org/Graphics/JPEG/itu-t81.pdf - The JPEG specification
//!

pub use self::decoder::JPEGDecoder;
pub use self::encoder::JPEGEncoder;

mod encoder;
mod decoder;

/// A representation of a JPEG component
#[deriving(Clone)]
pub struct Component {
    /// The Component's identifier
    pub id: u8,

    /// Horizontal sampling factor
    pub h: u8,

    /// Vertical sampling factor
    pub v: u8,

    /// The quantization table selector
    pub tq: u8,

    /// Index to the Huffman DC Table
    pub dc_table: u8,

    /// Index to the AC Huffman Table
    pub ac_table: u8,

    /// The dc prediction of the component
    pub dc_pred: i32
}

/// The permutation of dct coefficients.
pub static UNZIGZAG: [u8, ..64] = [
     0,  1,  8, 16,  9,  2,  3, 10,
    17, 24, 32, 25, 18, 11,  4,  5,
    12, 19, 26, 33, 40, 48, 41, 34,
    27, 20, 13,  6,  7, 14, 21, 28,
    35, 42, 49, 56, 57, 50, 43, 36,
    29, 22, 15, 23, 30, 37, 44, 51,
    58, 59, 52, 45, 38, 31, 39, 46,
    53, 60, 61, 54, 47, 55, 62, 63,
];

/// Given an array containing the number of codes of each code length,
/// this function generates the huffman codes lengths and their respective
/// code lengths as specified by the JPEG spec.
pub fn derive_codes_and_sizes(bits: &[u8]) -> (Vec<u8>, Vec<u16>) {
    let mut huffsize = Vec::from_elem(256, 0u8);
    let mut huffcode = Vec::from_elem(256, 0u16);

    let mut k = 0;
    let mut j;

    //Annex C.2
    //Figure C.1
    //Generate table of individual code lengths
    for i in range(0u, 16) {
        j = 0;
        while j < bits.as_slice()[i] {
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
    let mut size = huffsize.as_slice()[0];

    while huffsize.as_slice()[k] != 0 {
        huffcode.as_mut_slice()[k] = code;
        code += 1;
        k += 1;

        if huffsize.as_slice()[k] == size {
            continue
        }

        let diff = huffsize.as_slice()[k] - size;
        code <<= diff as uint;

        size += diff
    }

    (huffsize, huffcode)
}