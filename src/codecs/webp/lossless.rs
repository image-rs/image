//! Decoding of lossless WebP images
//!
//! [Lossless spec](https://developers.google.com/speed/webp/docs/webp_lossless_bitstream_specification)
//!

use std::{
    convert::TryFrom,
    convert::TryInto,
    error, fmt,
    io::Read,
    ops::{AddAssign, Shl},
};

use byteorder::ReadBytesExt;

use crate::{error::DecodingError, ImageError, ImageFormat, ImageResult};

use super::huffman::HuffmanTree;
use super::lossless_transform::{add_pixels, TransformType};

const CODE_LENGTH_CODES: usize = 19;
const CODE_LENGTH_CODE_ORDER: [usize; CODE_LENGTH_CODES] = [
    17, 18, 0, 1, 2, 3, 4, 5, 16, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
];

#[rustfmt::skip]
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

const HUFFMAN_CODES_PER_META_CODE: usize = 5;

type HuffmanCodeGroup = [HuffmanTree; HUFFMAN_CODES_PER_META_CODE];

const ALPHABET_SIZE: [u16; HUFFMAN_CODES_PER_META_CODE] = [256 + 24, 256, 256, 256, 40];

#[inline]
pub(crate) fn subsample_size(size: u16, bits: u8) -> u16 {
    ((u32::from(size) + (1u32 << bits) - 1) >> bits)
        .try_into()
        .unwrap()
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum DecoderError {
    /// Signature of 0x2f not found
    LosslessSignatureInvalid(u8),
    /// Version Number must be 0
    VersionNumberInvalid(u8),

    ///
    InvalidColorCacheBits(u8),

    HuffmanError,

    BitStreamError,

    TransformError,
}

impl fmt::Display for DecoderError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DecoderError::LosslessSignatureInvalid(sig) => {
                f.write_fmt(format_args!("Invalid lossless signature: {}", sig))
            }
            DecoderError::VersionNumberInvalid(num) => {
                f.write_fmt(format_args!("Invalid version number: {}", num))
            }
            DecoderError::InvalidColorCacheBits(num) => f.write_fmt(format_args!(
                "Invalid color cache(must be between 1-11): {}",
                num
            )),
            DecoderError::HuffmanError => f.write_fmt(format_args!("Error building Huffman Tree")),
            DecoderError::BitStreamError => {
                f.write_fmt(format_args!("Error while reading bitstream"))
            }
            DecoderError::TransformError => {
                f.write_fmt(format_args!("Error while reading or writing transforms"))
            }
        }
    }
}

impl From<DecoderError> for ImageError {
    fn from(e: DecoderError) -> ImageError {
        ImageError::Decoding(DecodingError::new(ImageFormat::WebP.into(), e))
    }
}

impl error::Error for DecoderError {}

const NUM_TRANSFORM_TYPES: usize = 4;

//Decodes lossless WebP images
#[derive(Debug)]
pub(crate) struct LosslessDecoder<R> {
    r: R,
    bit_reader: BitReader,
    frame: LosslessFrame,
    transforms: [Option<TransformType>; NUM_TRANSFORM_TYPES],
    transform_order: Vec<u8>,
}

impl<R: Read> LosslessDecoder<R> {
    /// Create a new decoder
    pub(crate) fn new(r: R) -> LosslessDecoder<R> {
        LosslessDecoder {
            r,
            bit_reader: BitReader::new(),
            frame: Default::default(),
            transforms: [None, None, None, None],
            transform_order: Vec::new(),
        }
    }

    /// Reads the frame
    pub(crate) fn decode_frame(&mut self) -> ImageResult<&LosslessFrame> {
        let signature = self.r.read_u8()?;

        if signature != 0x2f {
            return Err(DecoderError::LosslessSignatureInvalid(signature).into());
        }

        let mut buf = Vec::new();
        self.r.read_to_end(&mut buf)?;
        self.bit_reader.init(buf);

        self.frame.width = self.bit_reader.read_bits::<u16>(14)? + 1;
        self.frame.height = self.bit_reader.read_bits::<u16>(14)? + 1;

        let _alpha_used = self.bit_reader.read_bits::<u8>(1)?;

        let version_num = self.bit_reader.read_bits::<u8>(3)?;

        if version_num != 0 {
            return Err(DecoderError::VersionNumberInvalid(version_num).into());
        }

        let mut data = self.decode_image_stream(self.frame.width, self.frame.height, true)?;

        for &trans_index in self.transform_order.iter().rev() {
            let trans = self.transforms[usize::from(trans_index)].as_ref().unwrap();
            trans.apply_transform(&mut data, self.frame.width, self.frame.height)?;
        }

        self.frame.buf = data;
        Ok(&self.frame)
    }

    //used for alpha data in extended decoding
    pub(crate) fn decode_frame_implicit_dims(
        &mut self,
        width: u16,
        height: u16,
    ) -> ImageResult<&LosslessFrame> {
        let mut buf = Vec::new();
        self.r.read_to_end(&mut buf)?;
        self.bit_reader.init(buf);

        self.frame.width = width;
        self.frame.height = height;

        let mut data = self.decode_image_stream(self.frame.width, self.frame.height, true)?;

        //transform_order is vector of indices(0-3) into transforms in order decoded
        for &trans_index in self.transform_order.iter().rev() {
            let trans = self.transforms[usize::from(trans_index)].as_ref().unwrap();
            trans.apply_transform(&mut data, self.frame.width, self.frame.height)?;
        }

        self.frame.buf = data;
        Ok(&self.frame)
    }

    /// Reads Image data from the bitstream
    /// Can be in any of the 5 roles described in the Specification
    /// ARGB Image role has different behaviour to the other 4
    /// xsize and ysize describe the size of the blocks where each block has its own entropy code
    fn decode_image_stream(
        &mut self,
        xsize: u16,
        ysize: u16,
        is_argb_img: bool,
    ) -> ImageResult<Vec<u32>> {
        let trans_xsize = if is_argb_img {
            self.read_transforms()?
        } else {
            xsize
        };

        let color_cache_bits = self.read_color_cache()?;

        let color_cache = color_cache_bits.map(|bits| {
            let size = 1 << bits;
            let cache = vec![0u32; size];
            ColorCache {
                color_cache_bits: bits,
                color_cache: cache,
            }
        });

        let huffman_info = self.read_huffman_codes(is_argb_img, trans_xsize, ysize, color_cache)?;

        //decode data
        let data = self.decode_image_data(trans_xsize, ysize, huffman_info)?;

        Ok(data)
    }

    /// Reads transforms and their data from the bitstream
    fn read_transforms(&mut self) -> ImageResult<u16> {
        let mut xsize = self.frame.width;

        while self.bit_reader.read_bits::<u8>(1)? == 1 {
            let transform_type_val = self.bit_reader.read_bits::<u8>(2)?;

            if self.transforms[usize::from(transform_type_val)].is_some() {
                //can only have one of each transform, error
                return Err(DecoderError::TransformError.into());
            }

            self.transform_order.push(transform_type_val);

            let transform_type = match transform_type_val {
                0 => {
                    //predictor

                    let size_bits = self.bit_reader.read_bits::<u8>(3)? + 2;

                    let block_xsize = subsample_size(xsize, size_bits);
                    let block_ysize = subsample_size(self.frame.height, size_bits);

                    let data = self.decode_image_stream(block_xsize, block_ysize, false)?;

                    TransformType::PredictorTransform {
                        size_bits,
                        predictor_data: data,
                    }
                }
                1 => {
                    //color transform

                    let size_bits = self.bit_reader.read_bits::<u8>(3)? + 2;

                    let block_xsize = subsample_size(xsize, size_bits);
                    let block_ysize = subsample_size(self.frame.height, size_bits);

                    let data = self.decode_image_stream(block_xsize, block_ysize, false)?;

                    TransformType::ColorTransform {
                        size_bits,
                        transform_data: data,
                    }
                }
                2 => {
                    //subtract green

                    TransformType::SubtractGreen
                }
                3 => {
                    let color_table_size = self.bit_reader.read_bits::<u16>(8)? + 1;

                    let mut color_map = self.decode_image_stream(color_table_size, 1, false)?;

                    let bits = if color_table_size <= 2 {
                        3
                    } else if color_table_size <= 4 {
                        2
                    } else if color_table_size <= 16 {
                        1
                    } else {
                        0
                    };
                    xsize = subsample_size(xsize, bits);

                    Self::adjust_color_map(&mut color_map);

                    TransformType::ColorIndexingTransform {
                        table_size: color_table_size,
                        table_data: color_map,
                    }
                }
                _ => unreachable!(),
            };

            self.transforms[usize::from(transform_type_val)] = Some(transform_type);
        }

        Ok(xsize)
    }

    /// Adjusts the color map since it's subtraction coded
    fn adjust_color_map(color_map: &mut Vec<u32>) {
        for i in 1..color_map.len() {
            color_map[i] = add_pixels(color_map[i], color_map[i - 1]);
        }
    }

    /// Reads huffman codes associated with an image
    fn read_huffman_codes(
        &mut self,
        read_meta: bool,
        xsize: u16,
        ysize: u16,
        color_cache: Option<ColorCache>,
    ) -> ImageResult<HuffmanInfo> {
        let mut num_huff_groups = 1;

        let mut huffman_bits = 0;
        let mut huffman_xsize = 1;
        let mut huffman_ysize = 1;
        let mut entropy_image = Vec::new();

        if read_meta && self.bit_reader.read_bits::<u8>(1)? == 1 {
            //meta huffman codes
            huffman_bits = self.bit_reader.read_bits::<u8>(3)? + 2;
            huffman_xsize = subsample_size(xsize, huffman_bits);
            huffman_ysize = subsample_size(ysize, huffman_bits);

            entropy_image = self.decode_image_stream(huffman_xsize, huffman_ysize, false)?;

            for pixel in entropy_image.iter_mut() {
                let meta_huff_code = (*pixel >> 8) & 0xffff;

                *pixel = meta_huff_code;

                if meta_huff_code >= num_huff_groups {
                    num_huff_groups = meta_huff_code + 1;
                }
            }
        }

        let mut hufftree_groups = Vec::new();

        for _i in 0..num_huff_groups {
            let mut group: HuffmanCodeGroup = Default::default();
            for j in 0..HUFFMAN_CODES_PER_META_CODE {
                let mut alphabet_size = ALPHABET_SIZE[j];
                if j == 0 {
                    if let Some(color_cache) = color_cache.as_ref() {
                        alphabet_size += 1 << color_cache.color_cache_bits;
                    }
                }

                let tree = self.read_huffman_code(alphabet_size)?;
                group[j] = tree;
            }
            hufftree_groups.push(group);
        }

        let huffman_mask = if huffman_bits == 0 {
            !0
        } else {
            (1 << huffman_bits) - 1
        };

        let info = HuffmanInfo {
            xsize: huffman_xsize,
            _ysize: huffman_ysize,
            color_cache,
            image: entropy_image,
            bits: huffman_bits,
            mask: huffman_mask,
            huffman_code_groups: hufftree_groups,
        };

        Ok(info)
    }

    /// Decodes and returns a single huffman tree
    fn read_huffman_code(&mut self, alphabet_size: u16) -> ImageResult<HuffmanTree> {
        let simple = self.bit_reader.read_bits::<u8>(1)? == 1;

        if simple {
            let num_symbols = self.bit_reader.read_bits::<u8>(1)? + 1;

            let mut code_lengths = vec![u16::from(num_symbols - 1)];
            let mut codes = vec![0];
            let mut symbols = Vec::new();

            let is_first_8bits = self.bit_reader.read_bits::<u8>(1)?;
            symbols.push(self.bit_reader.read_bits::<u16>(1 + 7 * is_first_8bits)?);

            if num_symbols == 2 {
                symbols.push(self.bit_reader.read_bits::<u16>(8)?);
                code_lengths.push(1);
                codes.push(1);
            }

            HuffmanTree::build_explicit(code_lengths, codes, symbols)
        } else {
            let mut code_length_code_lengths = vec![0; CODE_LENGTH_CODES];

            let num_code_lengths = 4 + self.bit_reader.read_bits::<usize>(4)?;
            for i in 0..num_code_lengths {
                code_length_code_lengths[CODE_LENGTH_CODE_ORDER[i]] =
                    self.bit_reader.read_bits(3)?;
            }

            let new_code_lengths =
                self.read_huffman_code_lengths(code_length_code_lengths, alphabet_size)?;

            HuffmanTree::build_implicit(new_code_lengths)
        }
    }

    /// Reads huffman code lengths
    fn read_huffman_code_lengths(
        &mut self,
        code_length_code_lengths: Vec<u16>,
        num_symbols: u16,
    ) -> ImageResult<Vec<u16>> {
        let table = HuffmanTree::build_implicit(code_length_code_lengths)?;

        let mut max_symbol = if self.bit_reader.read_bits::<u8>(1)? == 1 {
            let length_nbits = 2 + 2 * self.bit_reader.read_bits::<u8>(3)?;
            2 + self.bit_reader.read_bits::<u16>(length_nbits)?
        } else {
            num_symbols
        };

        let mut code_lengths = vec![0; usize::from(num_symbols)];
        let mut prev_code_len = 8; //default code length

        let mut symbol = 0;
        while symbol < num_symbols {
            if max_symbol == 0 {
                break;
            }
            max_symbol -= 1;

            let code_len = table.read_symbol(&mut self.bit_reader)?;

            if code_len < 16 {
                code_lengths[usize::from(symbol)] = code_len;
                symbol += 1;
                if code_len != 0 {
                    prev_code_len = code_len;
                }
            } else {
                let use_prev = code_len == 16;
                let slot = code_len - 16;
                let extra_bits = match slot {
                    0 => 2,
                    1 => 3,
                    2 => 7,
                    _ => return Err(DecoderError::BitStreamError.into()),
                };
                let repeat_offset = match slot {
                    0 | 1 => 3,
                    2 => 11,
                    _ => return Err(DecoderError::BitStreamError.into()),
                };

                let mut repeat = self.bit_reader.read_bits::<u16>(extra_bits)? + repeat_offset;

                if symbol + repeat > num_symbols {
                    return Err(DecoderError::BitStreamError.into());
                } else {
                    let length = if use_prev { prev_code_len } else { 0 };
                    while repeat > 0 {
                        repeat -= 1;
                        code_lengths[usize::from(symbol)] = length;
                        symbol += 1;
                    }
                }
            }
        }

        Ok(code_lengths)
    }

    /// Decodes the image data using the huffman trees and either of the 3 methods of decoding
    fn decode_image_data(
        &mut self,
        width: u16,
        height: u16,
        mut huffman_info: HuffmanInfo,
    ) -> ImageResult<Vec<u32>> {
        let num_values = usize::from(width) * usize::from(height);
        let mut data = vec![0; num_values];

        let huff_index = huffman_info.get_huff_index(0, 0);
        let mut tree = &huffman_info.huffman_code_groups[huff_index];
        let mut last_cached = 0;
        let mut index = 0;
        let mut x = 0;
        let mut y = 0;
        while index < num_values {
            if (x & huffman_info.mask) == 0 {
                let index = huffman_info.get_huff_index(x, y);
                tree = &huffman_info.huffman_code_groups[index];
            }

            let code = tree[GREEN].read_symbol(&mut self.bit_reader)?;

            //check code
            if code < 256 {
                //literal, so just use huffman codes and read as argb
                let red = tree[RED].read_symbol(&mut self.bit_reader)?;
                let blue = tree[BLUE].read_symbol(&mut self.bit_reader)?;
                let alpha = tree[ALPHA].read_symbol(&mut self.bit_reader)?;

                data[index] = (u32::from(alpha) << 24)
                    + (u32::from(red) << 16)
                    + (u32::from(code) << 8)
                    + u32::from(blue);

                index += 1;
                x += 1;
                if x >= width {
                    x = 0;
                    y += 1;
                }
            } else if code < 256 + 24 {
                //backward reference, so go back and use that to add image data
                let length_symbol = code - 256;
                let length = Self::get_copy_distance(&mut self.bit_reader, length_symbol)?;

                let dist_symbol = tree[DIST].read_symbol(&mut self.bit_reader)?;
                let dist_code = Self::get_copy_distance(&mut self.bit_reader, dist_symbol)?;
                let dist = Self::plane_code_to_distance(width, dist_code);

                if index < dist || num_values - index < length {
                    return Err(DecoderError::BitStreamError.into());
                }

                for i in 0..length {
                    data[index + i] = data[index + i - dist];
                }
                index += length;
                x += u16::try_from(length).unwrap();
                while x >= width {
                    x -= width;
                    y += 1;
                }
                if index < num_values {
                    let index = huffman_info.get_huff_index(x, y);
                    tree = &huffman_info.huffman_code_groups[index];
                }
            } else {
                //color cache, so use previously stored pixels to get this pixel
                let key = code - 256 - 24;

                if let Some(color_cache) = huffman_info.color_cache.as_mut() {
                    //cache old colors
                    while last_cached < index {
                        color_cache.insert(data[last_cached]);
                        last_cached += 1;
                    }
                    data[index] = color_cache.lookup(key.into())?;
                } else {
                    return Err(DecoderError::BitStreamError.into());
                }
                index += 1;
                x += 1;
                if x >= width {
                    x = 0;
                    y += 1;
                }
            }
        }

        Ok(data)
    }

    /// Reads color cache data from the bitstream
    fn read_color_cache(&mut self) -> ImageResult<Option<u8>> {
        if self.bit_reader.read_bits::<u8>(1)? == 1 {
            let code_bits = self.bit_reader.read_bits::<u8>(4)?;

            if !(1..=11).contains(&code_bits) {
                return Err(DecoderError::InvalidColorCacheBits(code_bits).into());
            }

            Ok(Some(code_bits))
        } else {
            Ok(None)
        }
    }

    /// Gets the copy distance from the prefix code and bitstream
    fn get_copy_distance(bit_reader: &mut BitReader, prefix_code: u16) -> ImageResult<usize> {
        if prefix_code < 4 {
            return Ok(usize::from(prefix_code + 1));
        }
        let extra_bits: u8 = ((prefix_code - 2) >> 1).try_into().unwrap();
        let offset = (2 + (usize::from(prefix_code) & 1)) << extra_bits;

        Ok(offset + bit_reader.read_bits::<usize>(extra_bits)? + 1)
    }

    /// Gets distance to pixel
    fn plane_code_to_distance(xsize: u16, plane_code: usize) -> usize {
        if plane_code > 120 {
            plane_code - 120
        } else {
            let (xoffset, yoffset) = DISTANCE_MAP[plane_code - 1];

            let dist = i32::from(xoffset) + i32::from(yoffset) * i32::from(xsize);
            if dist < 1 {
                return 1;
            }
            dist.try_into().unwrap()
        }
    }
}

#[derive(Debug, Clone)]
struct HuffmanInfo {
    xsize: u16,
    _ysize: u16,
    color_cache: Option<ColorCache>,
    image: Vec<u32>,
    bits: u8,
    mask: u16,
    huffman_code_groups: Vec<HuffmanCodeGroup>,
}

impl HuffmanInfo {
    fn get_huff_index(&self, x: u16, y: u16) -> usize {
        if self.bits == 0 {
            return 0;
        }
        let position = usize::from((y >> self.bits) * self.xsize + (x >> self.bits));
        let meta_huff_code: usize = self.image[position].try_into().unwrap();
        meta_huff_code
    }
}

#[derive(Debug, Clone)]
struct ColorCache {
    color_cache_bits: u8,
    color_cache: Vec<u32>,
}

impl ColorCache {
    fn insert(&mut self, color: u32) {
        let index = (0x1e35a7bdu32.overflowing_mul(color).0) >> (32 - self.color_cache_bits);
        self.color_cache[index as usize] = color;
    }

    fn lookup(&self, index: usize) -> ImageResult<u32> {
        match self.color_cache.get(index) {
            Some(&value) => Ok(value),
            None => Err(DecoderError::BitStreamError.into()),
        }
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

    pub(crate) fn read_bits<T>(&mut self, num: u8) -> ImageResult<T>
    where
        T: num_traits::Unsigned + Shl<u8, Output = T> + AddAssign<T> + From<bool>,
    {
        let mut value: T = T::zero();

        for i in 0..num {
            if self.buf.len() <= self.index {
                return Err(DecoderError::BitStreamError.into());
            }
            let bit_true = self.buf[self.index] & (1 << self.bit_count) != 0;
            value += T::from(bit_true) << i;
            self.bit_count = if self.bit_count == 7 {
                self.index += 1;
                0
            } else {
                self.bit_count + 1
            };
        }

        Ok(value)
    }
}

#[derive(Debug, Clone, Default)]
pub(crate) struct LosslessFrame {
    pub(crate) width: u16,
    pub(crate) height: u16,

    pub(crate) buf: Vec<u32>,
}

impl LosslessFrame {
    /// Fills a buffer by converting from argb to rgba
    pub(crate) fn fill_rgba(&self, buf: &mut [u8]) {
        for (&argb_val, chunk) in self.buf.iter().zip(buf.chunks_exact_mut(4)) {
            chunk[0] = ((argb_val >> 16) & 0xff).try_into().unwrap();
            chunk[1] = ((argb_val >> 8) & 0xff).try_into().unwrap();
            chunk[2] = (argb_val & 0xff).try_into().unwrap();
            chunk[3] = ((argb_val >> 24) & 0xff).try_into().unwrap();
        }
    }

    /// Get buffer size from the image
    pub(crate) fn get_buf_size(&self) -> usize {
        usize::from(self.width) * usize::from(self.height) * 4
    }

    /// Fills a buffer with just the green values from the lossless decoding
    /// Used in extended alpha decoding
    pub(crate) fn fill_green(&self, buf: &mut [u8]) {
        for (&argb_val, buf_value) in self.buf.iter().zip(buf.iter_mut()) {
            *buf_value = ((argb_val >> 8) & 0xff).try_into().unwrap();
        }
    }
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

        assert_eq!(bit_reader.read_bits::<u8>(3).unwrap(), 4); //100
        assert_eq!(bit_reader.read_bits::<u8>(2).unwrap(), 3); //11
        assert_eq!(bit_reader.read_bits::<u8>(6).unwrap(), 12); //001100
        assert_eq!(bit_reader.read_bits::<u16>(10).unwrap(), 40); //0000101000
        assert_eq!(bit_reader.read_bits::<u8>(3).unwrap(), 7); //111
    }

    #[test]
    fn bit_read_error_test() {
        let mut bit_reader = BitReader::new();

        //01101010
        let buf = vec![0x6A];

        bit_reader.init(buf);

        assert_eq!(bit_reader.read_bits::<u8>(3).unwrap(), 2); //010
        assert_eq!(bit_reader.read_bits::<u8>(5).unwrap(), 13); //01101
        assert!(bit_reader.read_bits::<u8>(4).is_err()); //error
    }
}
