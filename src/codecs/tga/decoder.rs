use super::header::{Header, ImageType, ALPHA_BIT_MASK};
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
    pub(crate) fn from_reader(
        r: &mut dyn Read,
        start_offset: u16,
        num_entries: u16,
        bits_per_entry: u8,
    ) -> ImageResult<ColorMap> {
        let bytes_per_entry = (bits_per_entry as usize + 7) / 8;

        let mut bytes = vec![0; bytes_per_entry * num_entries as usize];
        r.read_exact(&mut bytes)?;

        Ok(ColorMap {
            entry_size: bytes_per_entry,
            start_offset: start_offset as usize,
            bytes,
        })
    }

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
    has_loaded_metadata: bool,

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
    pub fn new(r: R) -> ImageResult<TgaDecoder<R>> {
        let mut decoder = TgaDecoder {
            r,

            width: 0,
            height: 0,
            bytes_per_pixel: 0,
            has_loaded_metadata: false,

            image_type: ImageType::Unknown,
            color_type: ColorType::L8,
            original_color_type: None,

            header: Header::default(),
            color_map: None,
        };
        decoder.read_metadata()?;
        Ok(decoder)
    }

    fn read_header(&mut self) -> ImageResult<()> {
        self.header = Header::from_reader(&mut self.r)?;
        self.image_type = ImageType::new(self.header.image_type);
        self.width = self.header.image_width as usize;
        self.height = self.header.image_height as usize;
        self.bytes_per_pixel = (self.header.pixel_depth as usize + 7) / 8;
        Ok(())
    }

    fn read_metadata(&mut self) -> ImageResult<()> {
        if !self.has_loaded_metadata {
            self.read_header()?;
            self.read_image_id()?;
            self.read_color_map()?;
            self.read_color_information()?;
            self.has_loaded_metadata = true;
        }
        Ok(())
    }

    /// Loads the color information for the decoder
    ///
    /// To keep things simple, we won't handle bit depths that aren't divisible
    /// by 8 and are larger than 32.
    fn read_color_information(&mut self) -> ImageResult<()> {
        if self.header.pixel_depth % 8 != 0 || self.header.pixel_depth > 32 {
            // Bit depth must be divisible by 8, and must be less than or equal
            // to 32.
            return Err(ImageError::Unsupported(
                UnsupportedError::from_format_and_kind(
                    ImageFormat::Tga.into(),
                    UnsupportedErrorKind::Color(ExtendedColorType::Unknown(
                        self.header.pixel_depth,
                    )),
                ),
            ));
        }

        let num_alpha_bits = self.header.image_desc & ALPHA_BIT_MASK;

        let other_channel_bits = if self.header.map_type != 0 {
            self.header.map_entry_size
        } else {
            if num_alpha_bits > self.header.pixel_depth {
                return Err(ImageError::Unsupported(
                    UnsupportedError::from_format_and_kind(
                        ImageFormat::Tga.into(),
                        UnsupportedErrorKind::Color(ExtendedColorType::Unknown(
                            self.header.pixel_depth,
                        )),
                    ),
                ));
            }

            self.header.pixel_depth - num_alpha_bits
        };
        let color = self.image_type.is_color();

        match (num_alpha_bits, other_channel_bits, color) {
            // really, the encoding is BGR and BGRA, this is fixed
            // up with `TgaDecoder::reverse_encoding`.
            (0, 32, true) => self.color_type = ColorType::Rgba8,
            (8, 24, true) => self.color_type = ColorType::Rgba8,
            (0, 24, true) => self.color_type = ColorType::Rgb8,
            (8, 8, false) => self.color_type = ColorType::La8,
            (0, 8, false) => self.color_type = ColorType::L8,
            (8, 0, false) => {
                // alpha-only image is treated as L8
                self.color_type = ColorType::L8;
                self.original_color_type = Some(ExtendedColorType::A8);
            }
            _ => {
                return Err(ImageError::Unsupported(
                    UnsupportedError::from_format_and_kind(
                        ImageFormat::Tga.into(),
                        UnsupportedErrorKind::Color(ExtendedColorType::Unknown(
                            self.header.pixel_depth,
                        )),
                    ),
                ))
            }
        }
        Ok(())
    }

    /// Read the image id field
    ///
    /// We're not interested in this field, so this function skips it if it
    /// is present
    fn read_image_id(&mut self) -> ImageResult<()> {
        self.r
            .read_exact(&mut vec![0; self.header.id_length as usize])?;
        Ok(())
    }

    fn read_color_map(&mut self) -> ImageResult<()> {
        if self.header.map_type == 1 {
            // FIXME: we could reverse the map entries, which avoids having to reverse all pixels
            // in the final output individually.
            self.color_map = Some(ColorMap::from_reader(
                &mut self.r,
                self.header.map_origin,
                self.header.map_length,
                self.header.map_entry_size,
            )?);
        }
        Ok(())
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

        let bytes_per_entry = (self.header.map_entry_size as usize + 7) / 8;
        let mut result = Vec::with_capacity(self.width * self.height * bytes_per_entry);

        if self.bytes_per_pixel == 0 {
            return Err(io::ErrorKind::Other.into());
        }

        let color_map = self
            .color_map
            .as_ref()
            .ok_or_else(|| io::Error::from(io::ErrorKind::Other))?;

        for chunk in pixel_data.chunks(self.bytes_per_pixel) {
            let index = bytes_to_index(chunk);
            if let Some(color) = color_map.get(index) {
                result.extend_from_slice(color);
            } else {
                return Err(io::ErrorKind::Other.into());
            }
        }

        Ok(result)
    }

    /// Reads a run length encoded data for given number of bytes
    fn read_encoded_data(&mut self, num_bytes: usize) -> io::Result<Vec<u8>> {
        let mut pixel_data = Vec::with_capacity(num_bytes);
        let mut repeat_buf = Vec::with_capacity(self.bytes_per_pixel);

        while pixel_data.len() < num_bytes {
            let run_packet = self.r.read_u8()?;
            // If the highest bit in `run_packet` is set, then we repeat pixels
            //
            // Note: the TGA format adds 1 to both counts because having a count
            // of 0 would be pointless.
            if (run_packet & 0x80) != 0 {
                // high bit set, so we will repeat the data
                let repeat_count = ((run_packet & !0x80) + 1) as usize;
                self.r
                    .by_ref()
                    .take(self.bytes_per_pixel as u64)
                    .read_to_end(&mut repeat_buf)?;

                // get the repeating pixels from the bytes of the pixel stored in `repeat_buf`
                let data = repeat_buf
                    .iter()
                    .cycle()
                    .take(repeat_count * self.bytes_per_pixel);
                pixel_data.extend(data);
                repeat_buf.clear();
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
