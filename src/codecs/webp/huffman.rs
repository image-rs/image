use super::lossless::BitReader;

const CODE_LENGTH_CODES: usize = 19;
const CODE_LENGTH_CODE_ORDER: [usize; 19] = [
    17, 18, 0, 1, 2, 3, 4, 5, 16, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15
];

pub(crate) fn get_code_lengths(bit_reader: &mut BitReader) -> Vec<u16> {
    let simple = bit_reader.read_bits::<u8>(1) == 1;

    let mut code_lengths = Vec::new();

    if simple {
        let num_code_lengths = bit_reader.read_bits::<u8>(1) + 1;
        let is_first_8bits = bit_reader.read_bits::<u8>(1);
        code_lengths.push(bit_reader.read_bits::<u16>(1 + 7 * is_first_8bits));
        if num_code_lengths == 2 {
            code_lengths.push(bit_reader.read_bits::<u16>(8));
        }
    } else {
        code_lengths = vec![0; CODE_LENGTH_CODES];
        let num_code_lengths = 4 + bit_reader.read_bits::<usize>(4);
        for i in 0..num_code_lengths {
            code_lengths[CODE_LENGTH_CODE_ORDER[i]] = bit_reader.read_bits(3);
        }
    }

    code_lengths
}

pub(crate) fn read_meta_huffman_codes(bit_reader: &mut BitReader) {
    let multiple = bit_reader.read_bits::<u8>(1) == 1;

    if multiple {
        let huffman_bits = bit_reader.read_bits::<u8>(3) + 2;
        
    }
}