#![feature(const_fn)]

/// Given an array containing the number of codes of each code length,
/// this function generates the huffman codes lengths and their respective
/// code lengths as specified by the JPEG spec.
const fn derive_codes_and_sizes_const(bits: &[u8]) -> ([u8; 256], [u16; 256]) {
    let mut huffsize = [0u8; 256];
    let mut huffcode = [0u16; 256];

    let mut k = 0;

    // Annex C.2
    // Figure C.1
    // Generate table of individual code lengths
    let mut i = 0;
    while i < 16 {
        let mut j = 0;
        while j < bits[i as usize] {
            huffsize[k] = i + 1;
            k += 1;
            j += 1;
        }
        i += 1;
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
            continue;
        }

        // FIXME there is something wrong with this code
        let diff = huffsize[k].wrapping_sub(size);
        code = if diff < 16 { code << diff as usize } else { 0 };

        size = size.wrapping_add(diff);
    }

    (huffsize, huffcode)
}

pub(crate) const fn build_huff_lut_const(bits: &[u8], huffval: &[u8]) -> [(u8, u16); 256] {
    let mut lut = [(17u8, 0u16); 256];
    let (huffsize, huffcode) = derive_codes_and_sizes_const(bits);

    let mut i = 0;
    while i < huffval.len() {
        lut[huffval[i] as usize] = (huffsize[i], huffcode[i]);
        i += 1;
    }

    lut
}

/// Given an array containing the number of codes of each code length,
/// this function generates the huffman codes lengths and their respective
/// code lengths as specified by the JPEG spec.
fn derive_codes_and_sizes(bits: &[u8]) -> (Vec<u8>, Vec<u16>) {
    let mut huffsize = vec![0u8; 256];
    let mut huffcode = vec![0u16; 256];

    let mut k = 0;
    let mut j;

    // Annex C.2
    // Figure C.1
    // Generate table of individual code lengths
    for i in 0u8..16 {
        j = 0;

        while j < bits[usize::from(i)] {
            huffsize[k] = i + 1;
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
            continue;
        }

        // FIXME there is something wrong with this code
        let diff = huffsize[k].wrapping_sub(size);
        code = if diff < 16 { code << diff as usize } else { 0 };

        size = size.wrapping_add(diff);
    }

    (huffsize, huffcode)
}

pub(crate) fn build_huff_lut(bits: &[u8], huffval: &[u8]) -> Vec<(u8, u16)> {
    let mut lut = vec![(17u8, 0u16); 256];
    let (huffsize, huffcode) = derive_codes_and_sizes(bits);

    for (i, &v) in huffval.iter().enumerate() {
        lut[v as usize] = (huffsize[i], huffcode[i]);
    }

    lut
}

#[cfg(test)]
mod test {
    // Code lengths and values for table K.4
    static STD_CHROMA_DC_CODE_LENGTHS: [u8; 16] = [
        0x00, 0x03, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00,
    ];

    static STD_CHROMA_DC_VALUES: [u8; 12] = [
        0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0A, 0x0B,
    ];

    use super::{build_huff_lut, build_huff_lut_const};

    #[test]
    fn test_build_huff_lut() {
        let cd = build_huff_lut(&STD_CHROMA_DC_CODE_LENGTHS, &STD_CHROMA_DC_VALUES);
        let cd_const = build_huff_lut_const(&STD_CHROMA_DC_CODE_LENGTHS, &STD_CHROMA_DC_VALUES).to_vec();
        assert_eq!(cd_const, cd);
    }
}