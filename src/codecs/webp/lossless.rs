//! Decoding of lossless WebP images
//! 
//! [Lossless spec](https://developers.google.com/speed/webp/docs/webp_lossless_bitstream_specification#522_decoding_entropy-coded_image_data)


use std::{error, fmt, io::Read, ops::{AddAssign, Shl}};

use byteorder::{LittleEndian, ReadBytesExt};

use crate::{ImageError, ImageFormat, ImageResult, error::DecodingError};

const CODE_LENGTH_CODES: usize = 19;
const CODE_LENGTH_CODE_ORDER: [usize; 19] = [
    17, 18, 0, 1, 2, 3, 4, 5, 16, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15
];

const DISTANCE_MAP: [(i8, i8); 120] = [
    (0, 1),  (1, 0),  (1, 1),  (-1, 1), (0, 2),  (2, 0),  (1, 2),  (-1, 2),
    (2, 1),  (-2, 1), (2, 2),  (-2, 2), (0, 3),  (3, 0),  (1, 3),  (-1, 3),
    (3, 1),  (-3, 1), (2, 3),  (-2, 3), (3, 2),  (-3, 2), (0, 4),  (4, 0),
    (1, 4),  (-1, 4), (4, 1),  (-4, 1), (3, 3),  (-3, 3), (2, 4),  (-2, 4),
    (4, 2),  (-4, 2), (0, 5),  (3, 4),  (-3, 4), (4, 3),  (-4, 3), (5, 0),
    (1, 5),  (-1, 5), (5, 1),  (-5, 1), (2, 5),  (-2, 5), (5, 2),  (-5, 2),
    (4, 4),  (-4, 4), (3, 5),  (-3, 5), (5, 3),  (-5, 3), (0, 6),  (6, 0),
    (1, 6),  (-1, 6), (6, 1),  (-6, 1), (2, 6),  (-2, 6), (6, 2),  (-6, 2),
    (4, 5),  (-4, 5), (5, 4),  (-5, 4), (3, 6),  (-3, 6), (6, 3),  (-6, 3),
    (0, 7),  (7, 0),  (1, 7),  (-1, 7), (5, 5),  (-5, 5), (7, 1),  (-7, 1),
    (4, 6),  (-4, 6), (6, 4),  (-6, 4), (2, 7),  (-2, 7), (7, 2),  (-7, 2),
    (3, 7),  (-3, 7), (7, 3),  (-7, 3), (5, 6),  (-5, 6), (6, 5),  (-6, 5),
    (8, 0),  (4, 7),  (-4, 7), (7, 4),  (-7, 4), (8, 1),  (8, 2),  (6, 6),
    (-6, 6), (8, 3),  (5, 7),  (-5, 7), (7, 5),  (-7, 5), (8, 4),  (6, 7),
    (-6, 7), (7, 6),  (-7, 6), (8, 5),  (7, 7),  (-7, 7), (8, 6),  (8, 7)
];

const GREEN: usize = 0;
const RED: usize = 1;
const BLUE: usize = 2;
const ALPHA: usize = 3;
const DIST: usize = 4;

#[derive(Debug, Clone, Copy, Default)]
struct HuffmanCode {
    bits: u8,
    value: u16,
}

#[derive(Debug, Clone, Default)]
struct HuffmanTreeGroup {
    huffman_trees: Vec<[HuffmanCode; 5]>,
}

#[inline]
pub(crate) fn DIV_ROUND_UP(num: u16, den: u16) -> u16 {
    (num + den - 1) / den 
}

#[derive(Debug, Clone, Copy)]
enum DecoderError {
    /// Signature of 0x2f not found
    LosslessSignatureInvalid(u8),
    /// Version Number must be 0
    VersionNumberInvalid(u8),

    ///
    InvalidColorCacheBits(u8)
}

impl fmt::Display for DecoderError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DecoderError::LosslessSignatureInvalid(sig) => 
                f.write_fmt(format_args!("Invalid lossless signature: {}", sig)),
            DecoderError::VersionNumberInvalid(num) => 
                f.write_fmt(format_args!("Invalid version number: {}", num)),
            DecoderError::InvalidColorCacheBits(num) => 
                f.write_fmt(format_args!("Invalid color cache(must be between 1-11): {}", num))
        }
    }
}

impl From<DecoderError> for ImageError {
    fn from(e: DecoderError) -> ImageError {
        ImageError::Decoding(DecodingError::new(ImageFormat::WebP.into(), e))
    }
}

impl error::Error for DecoderError {}

#[derive(Debug, Clone, Copy)]
enum TransformType {
    PredictorTransform(u8),
    ColorTransform(u8),
    SubtractGreen,
    ColorIndexingTransform(u8),
}

//Decodes lossless WebP images
#[derive(Debug)]
pub(crate) struct LosslessDecoder<R> {
    r: R,
    bit_reader: BitReader,
    frame: LosslessFrame,
    transforms: Vec<TransformType>,
}

impl<R: Read> LosslessDecoder<R> {
    /// Create a new decoder
    pub(crate) fn new(r: R) -> LosslessDecoder<R> {

        LosslessDecoder {
            r,
            bit_reader: BitReader::new(),
            frame: Default::default(),
            transforms: Vec::new(),
        }
    }

    pub(crate) fn dimensions(&self) -> (u32, u32) {
        (u32::from(self.frame.width), u32::from(self.frame.height))
    }

    /// Reads the frame
    pub(crate) fn decode_frame(&mut self) -> ImageResult<()> {
        let len = self.r.read_u32::<LittleEndian>()?;
        let signature = self.r.read_u8()?;

        if signature != 0x2f {
            return Err(DecoderError::LosslessSignatureInvalid(signature).into());
        }

        let mut buf = Vec::new();
        self.r.read_to_end(&mut buf)?;
        self.bit_reader.init(buf);

        self.frame.width = self.bit_reader.read_bits::<u16>(14)+1;
        self.frame.height = self.bit_reader.read_bits::<u16>(14)+1;

        let _alpha_used = self.bit_reader.read_bits::<u8>(1);

        let version_num = self.bit_reader.read_bits::<u8>(3);

        if version_num != 0 {
            return Err(DecoderError::VersionNumberInvalid(version_num).into());
        }

        self.decode_image_stream(self.frame.width, self.frame.height, true)?;
        Ok(())
    }

    fn decode_image_stream(&mut self, xsize: u16, ysize: u16, base: bool) -> ImageResult<Vec<u32>> {
        
        let trans_xsize = xsize;
        let trans_ysize = ysize;

        if base {
            self.read_transforms()?;
        }

        let color_cache = self.read_color_cache()?;

        self.read_huffman_codes(true, xsize, ysize)?;

        let color_cache_size = match color_cache {
            Some(color_cache_bits) => {
                1 << color_cache_bits
            }
            None => 0,
        };

        if base {
            return Ok(vec![]);
        }

        //decode LZ77 encoded data
        let mut data = vec![0; (trans_xsize * trans_ysize).into()];

        self.decode_image_data(trans_xsize, trans_ysize, &mut data);

        Ok(data)
    }

    fn read_transforms(&mut self) -> ImageResult<()> {
        while self.bit_reader.read_bits::<u8>(1) == 1 {
            let transform_type_val = self.bit_reader.read_bits::<u8>(2);
            
            let transform_type = match transform_type_val {
                0 => {
                    //predictor

                    let size_bits = self.bit_reader.read_bits::<u8>(3) + 2;

                    TransformType::PredictorTransform(size_bits)
                }
                1 => {
                    //color transform

                    let size_bits = self.bit_reader.read_bits::<u8>(3) + 2;

                    TransformType::ColorTransform(size_bits)
                }
                2 => {
                    //subtract green

                    TransformType::SubtractGreen
                }
                3 => {
                    let color_table_size = self.bit_reader.read_bits::<u8>(8) + 1;

                    TransformType::ColorIndexingTransform(color_table_size)
                }
                _ => unreachable!(),
            };
            

            //self.transform = Some(transform_type);
        }

        Ok(())
    }

    fn read_huffman_codes(
        &mut self, 
        read_meta: bool,
        xsize: u16,
        ysize: u16) -> ImageResult<HuffmanTreeGroup> {
        
        let mut num_huff_groups = 1;
    
        if read_meta && self.bit_reader.read_bits::<u8>(1) == 1 {
            //meta huffman codes
            let huffman_bits = self.bit_reader.read_bits::<u8>(3) + 2;
            let huffman_xsize = DIV_ROUND_UP(xsize, 1 << huffman_bits);
            let huffman_ysize = DIV_ROUND_UP(ysize, 1 << huffman_bits);
            let huffman_pixels = usize::from(huffman_xsize * huffman_ysize);
            let entropy_image = self.decode_image_stream(huffman_xsize, huffman_ysize, false)?;
            
            
            for i in 0..huffman_pixels {
                let meta_huff_code = (entropy_image[i] >> 8) & 0xffff;

                if meta_huff_code >= num_huff_groups {
                    num_huff_groups = meta_huff_code + 1;
                }
            }
        }

        let tree_group = HuffmanTreeGroup {
            huffman_trees: Vec::with_capacity(num_huff_groups as usize)
        };

        for i in 0..num_huff_groups {
            
            for j in 0..5 {
                tree_group.huffman_trees[j]
            }
        }
    
        
    
        Ok(tree_group)
    }

    //decodes a single huffman code
    fn read_huffman_code(&mut self) -> Vec<HuffmanCode> {
        let simple = self.bit_reader.read_bits::<u8>(1) == 1;
    
        let mut code_lengths = Vec::new();
    
        if simple {
            let num_code_lengths = self.bit_reader.read_bits::<u8>(1) + 1;
            let is_first_8bits = self.bit_reader.read_bits::<u8>(1);
            code_lengths.push(self.bit_reader.read_bits::<u8>(1 + 7 * is_first_8bits));
            if num_code_lengths == 2 {
                code_lengths.push(self.bit_reader.read_bits::<u8>(8));
            }
        } else {
            code_lengths = vec![0; CODE_LENGTH_CODES];
            let num_code_lengths = 4 + self.bit_reader.read_bits::<usize>(4);
            for i in 0..num_code_lengths {
                code_lengths[CODE_LENGTH_CODE_ORDER[i]] = self.bit_reader.read_bits(3);
            }
        }
    
        code_lengths
    }

    fn decode_image_data(&mut self, width: u16, height: u16, data: &mut Vec<u32>) -> ImageResult<()> {
        
        for index in 0..data.len() {

            //let code = 

        }

        Ok(())
    }

    fn read_blocks(&mut self) -> ImageResult<()> {

        Ok(())
    }

    fn read_color_cache(&mut self) -> ImageResult<Option<u8>> {
        if self.bit_reader.read_bits::<u8>(1) == 1 {
            let code_bits = self.bit_reader.read_bits::<u8>(4);
            
            if !(1..=11).contains(&code_bits) {
                return Err(DecoderError::InvalidColorCacheBits(code_bits).into());
            }
            
            Ok(Some(code_bits))
        } else {
            Ok(None)
        }
    }

    fn entropy_coded_image(&mut self) -> ImageResult<()> {
        //let size = self.color_cache_size()?;

        /* match size {
            Some(size) => {
                //
                let color_cache = vec![0u32; usize::from(size)];
            }
            None => {

            }
        } */

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub(crate) struct BitReader {
    buf: Vec<u8>,
    index: usize,
    bit_count: u8,
}

impl BitReader {
    fn new() -> BitReader {
        BitReader {
            buf: Vec::new(),
            index: 0,
            bit_count: 0,
        }
    }

    fn init(&mut self, buf: Vec<u8>) {
        self.buf = buf;
    }

    pub(crate) fn read_bits<T>(&mut self, num: u8) -> T 
        where T: num_traits::Unsigned + Shl<u8, Output = T> +
            AddAssign<T> + From<bool> {
        let mut value: T = T::zero();
        
        for i in 0..num {
            let bit_true = self.buf[self.index] & (1 << self.bit_count) != 0;
            value += T::from(bit_true) << i;
            self.bit_count = if self.bit_count == 7 {
                self.index += 1;
                0
            } else {
                self.bit_count + 1
            };
        }

        value

    }
}



#[derive(Debug, Clone, Default)]
struct LosslessFrame {
    width: u16,
    height: u16,

    buf: Vec<u8>,
}

#[cfg(test)]
mod test {

    use super::BitReader;

    #[test]
    fn bit_read_test() {
        let mut bit_reader = BitReader::new();

        //10011100 01000001 11100001
        let buf = vec![0x9C, 0x41, 0xE1];

        bit_reader.init(buf);

        assert_eq!(bit_reader.read_bits::<u8>(3), 4); //100
        assert_eq!(bit_reader.read_bits::<u8>(2), 3); //11
        assert_eq!(bit_reader.read_bits::<u8>(6), 12); //001100
        assert_eq!(bit_reader.read_bits::<u16>(10), 40); //0000101000
        assert_eq!(bit_reader.read_bits::<u8>(3), 7); //111
    }
}