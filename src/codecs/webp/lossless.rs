//! Decoding of lossless WebP images
//! 
//! [Lossless spec](https://developers.google.com/speed/webp/docs/webp_lossless_bitstream_specification#522_decoding_entropy-coded_image_data)


use std::{error, fmt, io::Read, ops::{AddAssign, Shl}};

use byteorder::{LittleEndian, ReadBytesExt};

use crate::{ImageError, ImageFormat, ImageResult, error::DecodingError};

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

#[derive(Debug, Clone, Copy)]
enum DecoderError {
    /// Signature of 0x2f not found
    LosslessSignatureInvalid(u8),
    /// Version Number must be 0
    VersionNumberInvalid(u8),

    InvalidTransformType(u8),
}

impl fmt::Display for DecoderError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DecoderError::LosslessSignatureInvalid(sig) => 
                f.write_fmt(format_args!("Invalid lossless signature: {}", sig)),
            DecoderError::VersionNumberInvalid(num) => 
                f.write_fmt(format_args!("Invalid version number: {}", num)),
            DecoderError::InvalidTransformType(val) => 
                f.write_fmt(format_args!("Invalid transform type: {}", val)),
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
    transform: Option<TransformType>,
}

impl<R: Read> LosslessDecoder<R> {
    /// Create a new decoder
    pub(crate) fn new(r: R) -> LosslessDecoder<R> {

        LosslessDecoder {
            r,
            bit_reader: BitReader::new(),
            frame: Default::default(),
            transform: None,
        }
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

        self.read_transforms()?;
        self.read_image_data()?;

        Ok(())
    }

    fn read_transforms(&mut self) -> ImageResult<()> {
        while self.bit_reader.read_bits::<u8>(1) != 0 {
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
                _ => return Err(DecoderError::InvalidTransformType(transform_type_val).into()),
            };
            

            //self.transform = Some(transform_type);
        }

        Ok(())
    }

    fn read_image_data(&mut self) -> ImageResult<()> {


        Ok(())
    }
}

#[derive(Debug, Clone)]
struct BitReader {
    buf: Vec<u8>,
    index: usize,
    bit_count: u8,
}

impl BitReader {
    fn new() -> BitReader {
        BitReader {
            buf: Vec::new(),
            index: 0,
            bit_count: 7,
        }
    }

    fn init(&mut self, buf: Vec<u8>) {
        self.buf = buf;
    }

    fn read_bits<T>(&mut self, num: u8) -> T 
        where T: num_traits::Unsigned + Shl<u8, Output = T> +
            AddAssign<T> + From<bool> {
        let mut value: T = T::zero();
        
        for i in 0..num {
            let bit_true = self.buf[self.index] & (1 << self.bit_count) != 0;
            value += T::from(bit_true) << i;
            self.bit_count = if self.bit_count == 0 {
                self.index += 1;
                7
            } else {
                self.bit_count - 1
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

        assert_eq!(bit_reader.read_bits::<u8>(3), 1); //100
        assert_eq!(bit_reader.read_bits::<u8>(2), 3); //11
        assert_eq!(bit_reader.read_bits::<u8>(6), 17); //100010
        assert_eq!(bit_reader.read_bits::<u16>(10), 240); //0000111100
        assert_eq!(bit_reader.read_bits::<u8>(3), 4); //001
    }
}