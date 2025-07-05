use super::header::{Header, ImageType, ALPHA_BIT_MASK};
use crate::error::DecodingError;
use crate::io::ReadExt;
use crate::utils::vec_try_with_capacity;
use crate::{
    color::{ColorType, ExtendedColorType},
    error::{
        ImageError, ImageResult, LimitError, LimitErrorKind, UnsupportedError, UnsupportedErrorKind,
    },
    ImageDecoder, ImageFormat,
};
use byteorder_lite::ReadBytesExt;
use std::io::{self, Read};

struct ColorMap {
    /// sizes in bytes
    start_offset: usize,
    entry_size: usize,
    bytes: Vec<u8>,
}

impl ColorMap {
    /// Get one entry from the color map
    pub(crate) fn get(&self, index: usize) -> Option<&[u8]> {
        let entry = self.start_offset + self.entry_size * index;
        self.bytes.get(entry..entry + self.entry_size)
    }
}

/// The representation of a TGA decoder
pub struct TgaDecoder<R> {
    r: R,

    width: usize,
    height: usize,
    bytes_per_pixel: usize,

    image_type: ImageType,
    color_type: ColorType,
    original_color_type: Option<ExtendedColorType>,

    header: Header,
    color_map: Option<ColorMap>,
}

#[derive(Copy, Clone, Debug, PartialOrd, PartialEq)]
enum TgaOrientation {
    TopLeft,
    TopRight,
    BottomRight,
    BottomLeft,
}

impl TgaOrientation {
    fn from_image_desc_byte(value: u8) -> Self {
        // Set bits 4 and 5 indicates direction, if bit 4 is set then pixel order right -> left,
        // when bit 5 is set it indicates rows top -> bottom direction.
        // Sources:
        // https://en.wikipedia.org/wiki/Truevision_TGA ; Image specification (field 5)
        if value & (1u8 << 4) == 0 {
            // Left -> Right
            if value & (1u8 << 5) == 0 {
                TgaOrientation::BottomLeft
            } else {
                TgaOrientation::TopLeft
            }
        } else {
            // Right -> Left
            if value & (1u8 << 5) == 0 {
                TgaOrientation::BottomRight
            } else {
                TgaOrientation::TopRight
            }
        }
    }
}

impl<R: Read> TgaDecoder<R> {
    /// Create a new decoder that decodes from the stream `r`
    pub fn new(mut r: R) -> ImageResult<TgaDecoder<R>> {
        // Read header
        let header = Header::from_reader(&mut r)?;
        let image_type = ImageType::new(header.image_type);
        let width = header.image_width as usize;
        let height = header.image_height as usize;
        let bytes_per_pixel = (header.pixel_depth as usize).div_ceil(8);
        let num_alpha_bits = header.image_desc & ALPHA_BIT_MASK;

        // Validate header
        if ![8, 16, 24, 32].contains(&header.pixel_depth) || ![0, 8].contains(&num_alpha_bits) {
            return Err(ImageError::Unsupported(
                UnsupportedError::from_format_and_kind(
                    ImageFormat::Tga.into(),
                    UnsupportedErrorKind::Color(ExtendedColorType::Unknown(header.pixel_depth)),
                ),
            ));
        }

        // TODO: validate the rest of the fields in the header.

        // Read image ID (and ignore it)
        let mut tmp = [0u8; 256];
        r.read_exact(&mut tmp[0..header.id_length as usize])?;

        // Read color map
        let mut color_map = None;
        if header.map_type == 1 {
            if ![16, 24, 32].contains(&header.map_entry_size) {
                return Err(ImageError::Unsupported(
                    UnsupportedError::from_format_and_kind(
                        ImageFormat::Tga.into(),
                        UnsupportedErrorKind::GenericFeature(
                            "Unsupported color map entry size".into(),
                        ),
                    ),
                ));
            }

            let mut bytes = Vec::new();
            let bytes_per_entry = (header.map_entry_size as usize).div_ceil(8);
            r.read_exact_vec(&mut bytes, bytes_per_entry * header.map_length as usize)?;

            color_map = Some(ColorMap {
                entry_size: bytes_per_entry,
                start_offset: header.map_origin as usize,
                bytes,
            });
        }

        // Compute color information
        let num_other_bits = if header.map_type != 0 {
            header.map_entry_size
        } else {
            header
                .pixel_depth
                .checked_sub(num_alpha_bits)
                .ok_or_else(|| {
                    ImageError::Decoding(DecodingError::new(
                        ImageFormat::Tga.into(),
                        "Inconsistent values for pixel depth and alpha bits",
                    ))
                })?
        };

        let color_type;
        let mut original_color_type = None;
        match (num_alpha_bits, num_other_bits, image_type.is_color()) {
            // really, the encoding is BGR and BGRA, this is fixed up with
            // `TgaDecoder::reverse_encoding`.
            (0, 32, true) => color_type = ColorType::Rgba8,
            (8, 24, true) => color_type = ColorType::Rgba8,
            (0, 24, true) => color_type = ColorType::Rgb8,
            (8, 8, false) => color_type = ColorType::La8,
            (0, 8, false) => color_type = ColorType::L8,
            (8, 0, false) => {
                // alpha-only image is treated as L8
                color_type = ColorType::L8;
                original_color_type = Some(ExtendedColorType::A8);
            }
            _ => {
                return Err(ImageError::Unsupported(
                    UnsupportedError::from_format_and_kind(
                        ImageFormat::Tga.into(),
                        UnsupportedErrorKind::Color(ExtendedColorType::Unknown(header.pixel_depth)),
                    ),
                ))
            }
        }

        Ok(TgaDecoder {
            r,

            width,
            height,
            bytes_per_pixel,

            image_type,
            color_type,
            original_color_type,

            header,
            color_map,
        })
    }

    /// Expands indices into its mapped color
    fn expand_color_map(&self, pixel_data: &[u8]) -> io::Result<Vec<u8>> {
        #[inline]
        fn bytes_to_index(bytes: &[u8]) -> usize {
            let mut result = 0usize;
            for byte in bytes {
                result = (result << 8) | *byte as usize;
            }
            result
        }

        let bytes_per_entry = (self.header.map_entry_size as usize).div_ceil(8);
        let mut result = vec_try_with_capacity(self.width * self.height * bytes_per_entry)?;

        if self.bytes_per_pixel == 0 {
            return Err(io::ErrorKind::Other.into());
        }

        let color_map = self.color_map.as_ref().ok_or(io::ErrorKind::Other)?;

        for chunk in pixel_data.chunks(self.bytes_per_pixel) {
            let index = bytes_to_index(chunk);
            let color = color_map
                .get(index)
                .and_then(|slice| slice.get(..bytes_per_entry));
            debug_assert!(color.is_some());
            result.extend_from_slice(color.unwrap_or_default());
        }

        Ok(result)
    }

    /// Reads a run length encoded data for given number of bytes
    fn read_encoded_data(&mut self, num_bytes: usize) -> io::Result<Vec<u8>> {
        let mut pixel_data = vec_try_with_capacity(num_bytes)?;

        if self.bytes_per_pixel > 16 {
            debug_assert!(false, "the size shoudl be valid");
            return Err(io::ErrorKind::InvalidInput.into());
        }
        let mut repeat_buf = [0; 16];
        let repeat_buf = &mut repeat_buf[..self.bytes_per_pixel];

        while pixel_data.len() < num_bytes {
            let run_packet = self.r.read_u8()?;
            // If the highest bit in `run_packet` is set, then we repeat pixels
            //
            // Note: the TGA format adds 1 to both counts because having a count
            // of 0 would be pointless.
            if (run_packet & 0x80) != 0 {
                // high bit set, so we will repeat the data
                let repeat_count = ((run_packet & !0x80) + 1) as usize;
                self.r.read_exact(repeat_buf)?;

                // get the repeating pixels from the bytes of the pixel stored in `repeat_buf`
                let data = repeat_buf
                    .iter()
                    .cycle()
                    .take(repeat_count * self.bytes_per_pixel);
                pixel_data.extend(data);
            } else {
                // not set, so `run_packet+1` is the number of non-encoded pixels
                let num_raw_bytes = (run_packet + 1) as usize * self.bytes_per_pixel;
                self.r
                    .by_ref()
                    .take(num_raw_bytes as u64)
                    .read_to_end(&mut pixel_data)?;
            }
        }

        if pixel_data.len() > num_bytes {
            // FIXME: the last packet contained more data than we asked for!
            // This is at least a warning. We truncate the data since some methods rely on the
            // length to be accurate in the success case.
            pixel_data.truncate(num_bytes);
        }

        Ok(pixel_data)
    }

    /// Reads a run length encoded packet
    fn read_all_encoded_data(&mut self) -> ImageResult<Vec<u8>> {
        let num_bytes = self.width * self.height * self.bytes_per_pixel;

        Ok(self.read_encoded_data(num_bytes)?)
    }

    /// Reverse from BGR encoding to RGB encoding
    ///
    /// TGA files are stored in the BGRA encoding. This function swaps
    /// the blue and red bytes in the `pixels` array.
    fn reverse_encoding_in_output(&mut self, pixels: &mut [u8]) {
        // We only need to reverse the encoding of color images
        match self.color_type {
            ColorType::Rgb8 | ColorType::Rgba8 => {
                for chunk in pixels.chunks_mut(self.color_type.bytes_per_pixel().into()) {
                    chunk.swap(0, 2);
                }
            }
            _ => {}
        }
    }

    /// Change image orientation depending on the flags set
    fn fixup_orientation(&mut self, pixels: &mut [u8]) {
        let orientation = TgaOrientation::from_image_desc_byte(self.header.image_desc);

        // Flip image if bottom->top direction
        if (orientation == TgaOrientation::BottomLeft || orientation == TgaOrientation::BottomRight)
            && self.height > 1
        {
            let row_stride = self.width * self.bytes_per_pixel;

            let (left_part, right_part) = pixels.split_at_mut(self.height / 2 * row_stride);

            for (src, dst) in left_part
                .chunks_exact_mut(row_stride)
                .zip(right_part.chunks_exact_mut(row_stride).rev())
            {
                for (src, dst) in src.iter_mut().zip(dst.iter_mut()) {
                    std::mem::swap(src, dst);
                }
            }
        }

        // Flop image if right->left direction
        if (orientation == TgaOrientation::BottomRight || orientation == TgaOrientation::TopRight)
            && self.width > 1
        {
            for row in pixels.chunks_exact_mut(self.width * self.bytes_per_pixel) {
                let (left_part, right_part) =
                    row.split_at_mut(self.width / 2 * self.bytes_per_pixel);
                for (src, dst) in left_part
                    .chunks_exact_mut(self.bytes_per_pixel)
                    .zip(right_part.chunks_exact_mut(self.bytes_per_pixel).rev())
                {
                    for (src, dst) in src.iter_mut().zip(dst.iter_mut()) {
                        std::mem::swap(dst, src);
                    }
                }
            }
        }
    }
}

impl<R: Read> ImageDecoder for TgaDecoder<R> {
    fn dimensions(&self) -> (u32, u32) {
        (self.width as u32, self.height as u32)
    }

    fn color_type(&self) -> ColorType {
        self.color_type
    }

    fn original_color_type(&self) -> ExtendedColorType {
        self.original_color_type
            .unwrap_or_else(|| self.color_type().into())
    }

    fn read_image(mut self, buf: &mut [u8]) -> ImageResult<()> {
        assert_eq!(u64::try_from(buf.len()), Ok(self.total_bytes()));

        // In indexed images, we might need more bytes than pixels to read them. That's nonsensical
        // to encode but we'll not want to crash.
        let mut fallback_buf = vec![];
        // read the pixels from the data region
        let rawbuf = if self.image_type.is_encoded() {
            let pixel_data = self.read_all_encoded_data()?;
            if self.bytes_per_pixel <= usize::from(self.color_type.bytes_per_pixel()) {
                buf[..pixel_data.len()].copy_from_slice(&pixel_data);
                &buf[..pixel_data.len()]
            } else {
                fallback_buf = pixel_data;
                &fallback_buf[..]
            }
        } else {
            let num_raw_bytes = self.width * self.height * self.bytes_per_pixel;
            if self.bytes_per_pixel <= usize::from(self.color_type.bytes_per_pixel()) {
                self.r.by_ref().read_exact(&mut buf[..num_raw_bytes])?;
                &buf[..num_raw_bytes]
            } else {
                fallback_buf.resize(num_raw_bytes, 0u8);
                self.r
                    .by_ref()
                    .read_exact(&mut fallback_buf[..num_raw_bytes])?;
                &fallback_buf[..num_raw_bytes]
            }
        };

        // expand the indices using the color map if necessary
        if self.image_type.is_color_mapped() {
            let pixel_data = self.expand_color_map(rawbuf)?;
            // not enough data to fill the buffer, or would overflow the buffer
            if pixel_data.len() != buf.len() {
                return Err(ImageError::Limits(LimitError::from_kind(
                    LimitErrorKind::DimensionError,
                )));
            }
            buf.copy_from_slice(&pixel_data);
        }

        self.reverse_encoding_in_output(buf);

        self.fixup_orientation(buf);

        Ok(())
    }

    fn read_image_boxed(self: Box<Self>, buf: &mut [u8]) -> ImageResult<()> {
        (*self).read_image(buf)
    }
}
