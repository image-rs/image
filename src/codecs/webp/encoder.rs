//! Encoding of WebP images.
use std::collections::BinaryHeap;
///
/// Uses the simple encoding API from the [libwebp] library.
///
/// [libwebp]: https://developers.google.com/speed/webp/docs/api#simple_encoding_api
use std::io::{self, Write};
use std::slice::ChunksExact;

#[cfg(feature = "webp-encoder")]
use libwebp::{Encoder, PixelLayout, WebPMemory};

use crate::error::{ParameterError, ParameterErrorKind, UnsupportedError, UnsupportedErrorKind};
use crate::flat::SampleLayout;
use crate::{ColorType, ImageEncoder, ImageError, ImageFormat, ImageResult};

/// WebP Encoder.
pub struct WebPEncoder<W> {
    writer: W,
    quality: WebPQuality,

    chunk_buffer: Vec<u8>,
    buffer: u64,
    nbits: u8,
}

/// WebP encoder quality.
#[derive(Debug, Copy, Clone)]
pub struct WebPQuality(Quality);

#[derive(Debug, Copy, Clone)]
enum Quality {
    Lossless,
    #[allow(unused)]
    #[deprecated = "Lossy encoding will be removed in a future version. See: https://github.com/image-rs/image/issues/1984"]
    Lossy(u8),
}

impl WebPQuality {
    /// Minimum lossy quality value (0).
    pub const MIN: u8 = 0;
    /// Maximum lossy quality value (100).
    pub const MAX: u8 = 100;
    /// Default lossy quality (80), providing a balance of quality and file size.
    pub const DEFAULT: u8 = 80;

    /// Lossless encoding.
    pub fn lossless() -> Self {
        Self(Quality::Lossless)
    }

    /// Lossy encoding. 0 = low quality, small size; 100 = high quality, large size.
    ///
    /// Values are clamped from 0 to 100.
    #[deprecated = "Lossy encoding will be removed in a future version. See: https://github.com/image-rs/image/issues/1984"]
    pub fn lossy(quality: u8) -> Self {
        #[allow(deprecated)]
        Self(Quality::Lossy(quality.clamp(Self::MIN, Self::MAX)))
    }
}

impl Default for WebPQuality {
    fn default() -> Self {
        #[allow(deprecated)]
        Self::lossy(WebPQuality::DEFAULT)
    }
}

impl<W: Write> WebPEncoder<W> {
    /// Create a new encoder that writes its output to `w`.
    ///
    /// Defaults to lossy encoding, see [`WebPQuality::DEFAULT`].
    #[deprecated = "Use `new_lossless` instead. Lossy encoding will be removed in a future version. See: https://github.com/image-rs/image/issues/1984"]
    #[cfg(feature = "webp-encoder")]
    pub fn new(w: W) -> Self {
        #[allow(deprecated)]
        WebPEncoder::new_with_quality(w, WebPQuality::default())
    }

    /// Create a new encoder with the specified quality, that writes its output to `w`.
    #[deprecated = "Use `new_lossless` instead. Lossy encoding will be removed in a future version. See: https://github.com/image-rs/image/issues/1984"]
    #[cfg(feature = "webp-encoder")]
    pub fn new_with_quality(w: W, quality: WebPQuality) -> Self {
        Self {
            writer: w,
            quality,
            chunk_buffer: Vec::new(),
            buffer: 0,
            nbits: 0,
        }
    }

    /// Create a new encoder that writes its output to `w`.
    ///
    /// Uses "VP8L" lossless encoding.
    pub fn new_lossless(w: W) -> Self {
        Self {
            writer: w,
            quality: WebPQuality::lossless(),
            chunk_buffer: Vec::new(),
            buffer: 0,
            nbits: 0,
        }
    }

    fn write_bits(&mut self, bits: u64, nbits: u8) -> io::Result<()> {
        debug_assert!(nbits <= 64);

        self.buffer |= bits << self.nbits;
        self.nbits += nbits;

        if self.nbits >= 64 {
            self.chunk_buffer.write_all(&self.buffer.to_le_bytes())?;
            self.nbits -= 64;
            self.buffer = bits.checked_shr((nbits - self.nbits) as u32).unwrap_or(0);
        }
        debug_assert!(self.nbits < 64);
        Ok(())
    }

    fn flush(&mut self) -> io::Result<()> {
        if self.nbits % 8 != 0 {
            self.write_bits(0, 8 - self.nbits % 8)?;
        }
        if self.nbits > 0 {
            self.chunk_buffer
                .write_all(&self.buffer.to_le_bytes()[..self.nbits as usize / 8])
                .unwrap();
            self.buffer = 0;
            self.nbits = 0;
        }
        Ok(())
    }

    fn write_single_entry_huffman_tree(&mut self, symbol: u8) -> io::Result<()> {
        self.write_bits(1, 2)?;
        if symbol <= 1 {
            self.write_bits(0, 1)?;
            self.write_bits(symbol as u64, 1)?;
        } else {
            self.write_bits(1, 1)?;
            self.write_bits(symbol as u64, 8)?;
        }
        Ok(())
    }

    fn build_huffman_tree(
        &mut self,
        frequencies: &[u32],
        lengths: &mut [u8],
        codes: &mut [u16],
        length_limit: u8,
    ) -> bool {
        assert_eq!(frequencies.len(), lengths.len());
        assert_eq!(frequencies.len(), codes.len());

        if frequencies.iter().filter(|&&f| f > 0).count() <= 1 {
            lengths.fill(0);
            codes.fill(0);
            return false;
        }

        #[derive(Eq, PartialEq, Copy, Clone, Debug)]
        struct Item(u32, u16);
        impl Ord for Item {
            fn cmp(&self, other: &Self) -> std::cmp::Ordering {
                other.0.cmp(&self.0)
            }
        }
        impl PartialOrd for Item {
            fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
                Some(self.cmp(other))
            }
        }

        // Build a huffman tree
        let mut internal_nodes = Vec::new();
        let mut nodes = BinaryHeap::from_iter(
            frequencies
                .iter()
                .enumerate()
                .filter(|(_, &frequency)| frequency > 0)
                .map(|(i, &frequency)| Item(frequency, i as u16)),
        );
        while nodes.len() > 1 {
            let Item(frequency1, index1) = nodes.pop().unwrap();
            let mut root = nodes.peek_mut().unwrap();
            internal_nodes.push((index1, root.1));
            *root = Item(
                frequency1 + root.0,
                internal_nodes.len() as u16 + frequencies.len() as u16 - 1,
            );
        }

        // Walk the tree to assign code lengths
        lengths.fill(0);
        let mut stack = Vec::new();
        stack.push((nodes.pop().unwrap().1, 0));
        while let Some((node, depth)) = stack.pop() {
            let node = node as usize;
            if node < frequencies.len() {
                lengths[node] = depth as u8;
            } else {
                let (left, right) = internal_nodes[node - frequencies.len()];
                stack.push((left, depth + 1));
                stack.push((right, depth + 1));
            }
        }

        // Limit the codes to length length_limit
        let mut max_length = 0;
        for &length in lengths.iter() {
            max_length = max_length.max(length);
        }
        if max_length > length_limit {
            let mut counts = [0u32; 16];
            for &length in lengths.iter() {
                counts[length.min(length_limit) as usize] += 1;
            }

            let mut total = 0;
            for (i, count) in counts
                .iter()
                .enumerate()
                .skip(1)
                .take(length_limit as usize)
            {
                total += count << (length_limit as usize - i);
            }

            while total > 1u32 << length_limit {
                let mut i = length_limit as usize - 1;
                while counts[i] == 0 {
                    i -= 1;
                }
                counts[i] -= 1;
                counts[length_limit as usize] -= 1;
                counts[i + 1] += 2;
                total -= 1;
            }

            // assign new lengths
            let mut len = length_limit;
            let mut indexes = frequencies.iter().copied().enumerate().collect::<Vec<_>>();
            indexes.sort_unstable_by_key(|&(_, frequency)| frequency);
            for &(i, frequency) in indexes.iter() {
                if frequency > 0 {
                    while counts[len as usize] == 0 {
                        len -= 1;
                    }
                    lengths[i] = len;
                    counts[len as usize] -= 1;
                }
            }
        }

        // Assign codes
        codes.fill(0);
        let mut code = 0u32;
        for len in 1..=length_limit {
            for (i, &length) in lengths.iter().enumerate() {
                if length == len {
                    codes[i] = (code as u16).reverse_bits() >> (16 - len);
                    code += 1;
                }
            }
            code <<= 1;
        }
        assert_eq!(code, 2 << length_limit);

        true
    }

    fn write_huffman_tree(
        &mut self,
        frequencies: &[u32],
        lengths: &mut [u8],
        codes: &mut [u16],
    ) -> io::Result<()> {
        if !self.build_huffman_tree(frequencies, lengths, codes, 15) {
            let symbol = frequencies
                .iter()
                .position(|&frequency| frequency > 0)
                .unwrap_or(0);
            return self.write_single_entry_huffman_tree(symbol as u8);
        }

        let mut code_length_lengths = [0u8; 16];
        let mut code_length_codes = [0u16; 16];
        let mut code_length_frequencies = [0u32; 16];
        for &length in lengths.iter() {
            code_length_frequencies[length as usize] += 1;
        }
        let single_code_length_length = !self.build_huffman_tree(
            &code_length_frequencies,
            &mut code_length_lengths,
            &mut code_length_codes,
            7,
        );

        const CODE_LENGTH_ORDER: [usize; 19] = [
            17, 18, 0, 1, 2, 3, 4, 5, 16, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
        ];

        // Write the huffman tree
        self.write_bits(0, 1)?; // normal huffman tree
        self.write_bits(19 - 4, 4)?; // num_code_lengths - 4

        for &i in CODE_LENGTH_ORDER.iter() {
            if i > 15 || code_length_frequencies[i] == 0 {
                self.write_bits(0, 3)?;
            } else if single_code_length_length {
                self.write_bits(1, 3)?;
            } else {
                self.write_bits(code_length_lengths[i] as u64, 3)?;
            }
        }

        match lengths.len() {
            256 => {
                self.write_bits(1, 1)?; // max_symbol is stored
                self.write_bits(3, 3)?; // max_symbol_nbits / 2 - 2
                self.write_bits(254, 8)?; // max_symbol - 2
            }
            280 => self.write_bits(0, 1)?,
            _ => unreachable!(),
        }

        // Write the huffman codes
        if !single_code_length_length {
            for &len in lengths.iter() {
                self.write_bits(
                    code_length_codes[len as usize] as u64,
                    code_length_lengths[len as usize],
                )?;
            }
        }

        Ok(())
    }

    fn length_to_symbol(len: u16) -> (u16, u8) {
        let len = len - 1;
        let highest_bit = 15 - len.leading_zeros() as u16; // TODO: use ilog2 once MSRV >= 1.67
        let second_highest_bit = (len >> (highest_bit - 1)) & 1;
        let extra_bits = highest_bit - 1;
        let symbol = 2 * highest_bit + second_highest_bit;
        (symbol, extra_bits as u8)
    }

    #[inline(always)]
    fn count_run(
        pixel: &[u8],
        it: &mut std::iter::Peekable<ChunksExact<u8>>,
        frequencies1: &mut [u32; 280],
    ) {
        let mut run_length = 0;
        while run_length < 4096 && it.peek() == Some(&pixel) {
            run_length += 1;
            it.next();
        }
        if run_length > 0 {
            if run_length <= 4 {
                let symbol = 256 + run_length - 1;
                frequencies1[symbol] += 1;
            } else {
                let (symbol, _extra_bits) = Self::length_to_symbol(run_length as u16);
                frequencies1[256 + symbol as usize] += 1;
            }
        }
    }

    #[inline(always)]
    fn write_run(
        &mut self,
        pixel: &[u8],
        it: &mut std::iter::Peekable<ChunksExact<u8>>,
        codes1: &[u16; 280],
        lengths1: &[u8; 280],
    ) -> io::Result<()> {
        let mut run_length = 0;
        while run_length < 4096 && it.peek() == Some(&pixel) {
            run_length += 1;
            it.next();
        }
        if run_length > 0 {
            if run_length <= 4 {
                let symbol = 256 + run_length - 1;
                self.write_bits(codes1[symbol] as u64, lengths1[symbol])?;
            } else {
                let (symbol, extra_bits) = Self::length_to_symbol(run_length as u16);
                self.write_bits(
                    codes1[256 + symbol as usize] as u64,
                    lengths1[256 + symbol as usize],
                )?;
                self.write_bits(
                    (run_length as u64 - 1) & ((1 << extra_bits) - 1),
                    extra_bits,
                )?;
            }
        }
        Ok(())
    }

    fn encode_lossless(
        mut self,
        data: &[u8],
        width: u32,
        height: u32,
        color: ColorType,
    ) -> ImageResult<()> {
        if width == 0
            || width > 16384
            || height == 0
            || height > 16384
            || !SampleLayout::row_major_packed(color.channel_count(), width, height)
                .fits(data.len())
        {
            return Err(ImageError::Parameter(ParameterError::from_kind(
                ParameterErrorKind::DimensionMismatch,
            )));
        }

        let (is_color, is_alpha) = match color {
            ColorType::L8 => (false, false),
            ColorType::La8 => (false, true),
            ColorType::Rgb8 => (true, false),
            ColorType::Rgba8 => (true, true),
            _ => {
                return Err(ImageError::Unsupported(
                    UnsupportedError::from_format_and_kind(
                        ImageFormat::WebP.into(),
                        UnsupportedErrorKind::Color(color.into()),
                    ),
                ))
            }
        };

        self.write_bits(0x2f, 8)?; // signature
        self.write_bits(width as u64 - 1, 14)?;
        self.write_bits(height as u64 - 1, 14)?;

        self.write_bits(is_alpha as u64, 1)?; // alpha used
        self.write_bits(0x0, 3)?; // version

        // subtract green transform
        self.write_bits(0b101, 3)?;

        // predictor transform
        self.write_bits(0b111001, 6)?;
        self.write_bits(0x0, 1)?; // no color cache
        self.write_single_entry_huffman_tree(2)?;
        for _ in 0..4 {
            self.write_single_entry_huffman_tree(0)?;
        }

        // transforms done
        self.write_bits(0x0, 1)?;

        // color cache
        self.write_bits(0x0, 1)?;

        // meta-huffman codes
        self.write_bits(0x0, 1)?;

        // expand to RGBA
        let mut pixels = match color {
            ColorType::L8 => data.iter().flat_map(|&p| [p, p, p, 255]).collect(),
            ColorType::La8 => data
                .chunks_exact(2)
                .flat_map(|p| [p[0], p[0], p[0], p[1]])
                .collect(),
            ColorType::Rgb8 => data
                .chunks_exact(3)
                .flat_map(|p| [p[0], p[1], p[2], 255])
                .collect(),
            ColorType::Rgba8 => data.to_vec(),
            _ => unreachable!(),
        };

        // compute subtract green transform
        for pixel in pixels.chunks_exact_mut(4) {
            pixel[0] = pixel[0].wrapping_sub(pixel[1]);
            pixel[2] = pixel[2].wrapping_sub(pixel[1]);
        }

        // compute predictor transform
        let row_bytes = width as usize * 4;
        for y in (1..height as usize).rev() {
            let (prev, current) =
                pixels[(y - 1) * row_bytes..][..row_bytes * 2].split_at_mut(row_bytes);
            for (c, p) in current.iter_mut().zip(prev) {
                *c = c.wrapping_sub(*p);
            }
        }
        for i in (4..row_bytes).rev() {
            pixels[i] = pixels[i].wrapping_sub(pixels[i - 4]);
        }
        pixels[3] = pixels[3].wrapping_sub(255);

        // compute frequencies
        let mut frequencies0 = [0u32; 256];
        let mut frequencies1 = [0u32; 280];
        let mut frequencies2 = [0u32; 256];
        let mut frequencies3 = [0u32; 256];
        let mut it = pixels.chunks_exact(4).peekable();
        match color {
            ColorType::L8 => {
                frequencies0[0] = 1;
                frequencies2[0] = 1;
                frequencies3[0] = 1;
                while let Some(pixel) = it.next() {
                    frequencies1[pixel[1] as usize] += 1;
                    Self::count_run(pixel, &mut it, &mut frequencies1);
                }
            }
            ColorType::La8 => {
                frequencies0[0] = 1;
                frequencies2[0] = 1;
                while let Some(pixel) = it.next() {
                    frequencies1[pixel[1] as usize] += 1;
                    frequencies3[pixel[3] as usize] += 1;
                    Self::count_run(pixel, &mut it, &mut frequencies1);
                }
            }
            ColorType::Rgb8 => {
                frequencies3[0] = 1;
                while let Some(pixel) = it.next() {
                    frequencies0[pixel[0] as usize] += 1;
                    frequencies1[pixel[1] as usize] += 1;
                    frequencies2[pixel[2] as usize] += 1;
                    Self::count_run(pixel, &mut it, &mut frequencies1);
                }
            }
            ColorType::Rgba8 => {
                while let Some(pixel) = it.next() {
                    frequencies0[pixel[0] as usize] += 1;
                    frequencies1[pixel[1] as usize] += 1;
                    frequencies2[pixel[2] as usize] += 1;
                    frequencies3[pixel[3] as usize] += 1;
                    Self::count_run(pixel, &mut it, &mut frequencies1);
                }
            }
            _ => unreachable!(),
        }

        // compute and write huffman codes
        let mut lengths0 = [0u8; 256];
        let mut lengths1 = [0u8; 280];
        let mut lengths2 = [0u8; 256];
        let mut lengths3 = [0u8; 256];
        let mut codes0 = [0u16; 256];
        let mut codes1 = [0u16; 280];
        let mut codes2 = [0u16; 256];
        let mut codes3 = [0u16; 256];
        self.write_huffman_tree(&frequencies1, &mut lengths1, &mut codes1)?;
        if is_color {
            self.write_huffman_tree(&frequencies0, &mut lengths0, &mut codes0)?;
            self.write_huffman_tree(&frequencies2, &mut lengths2, &mut codes2)?;
        } else {
            self.write_single_entry_huffman_tree(0)?;
            self.write_single_entry_huffman_tree(0)?;
        }
        if is_alpha {
            self.write_huffman_tree(&frequencies3, &mut lengths3, &mut codes3)?;
        } else {
            self.write_single_entry_huffman_tree(0)?;
        }
        self.write_single_entry_huffman_tree(1)?;

        // Write image data
        let mut it = pixels.chunks_exact(4).peekable();
        match color {
            ColorType::L8 => {
                while let Some(pixel) = it.next() {
                    self.write_bits(
                        codes1[pixel[1] as usize] as u64,
                        lengths1[pixel[1] as usize],
                    )?;
                    self.write_run(pixel, &mut it, &codes1, &lengths1)?;
                }
            }
            ColorType::La8 => {
                while let Some(pixel) = it.next() {
                    let len1 = lengths1[pixel[1] as usize];
                    let len3 = lengths3[pixel[3] as usize];

                    let code = codes1[pixel[1] as usize] as u64
                        | (codes3[pixel[3] as usize] as u64) << len1;

                    self.write_bits(code, len1 + len3)?;
                    self.write_run(pixel, &mut it, &codes1, &lengths1)?;
                }
            }
            ColorType::Rgb8 => {
                while let Some(pixel) = it.next() {
                    let len1 = lengths1[pixel[1] as usize];
                    let len0 = lengths0[pixel[0] as usize];
                    let len2 = lengths2[pixel[2] as usize];

                    let code = codes1[pixel[1] as usize] as u64
                        | (codes0[pixel[0] as usize] as u64) << len1
                        | (codes2[pixel[2] as usize] as u64) << (len1 + len0);

                    self.write_bits(code, len1 + len0 + len2)?;
                    self.write_run(pixel, &mut it, &codes1, &lengths1)?;
                }
            }
            ColorType::Rgba8 => {
                while let Some(pixel) = it.next() {
                    let len1 = lengths1[pixel[1] as usize];
                    let len0 = lengths0[pixel[0] as usize];
                    let len2 = lengths2[pixel[2] as usize];
                    let len3 = lengths3[pixel[3] as usize];

                    let code = codes1[pixel[1] as usize] as u64
                        | (codes0[pixel[0] as usize] as u64) << len1
                        | (codes2[pixel[2] as usize] as u64) << (len1 + len0)
                        | (codes3[pixel[3] as usize] as u64) << (len1 + len0 + len2);

                    self.write_bits(code, len1 + len0 + len2 + len3)?;
                    self.write_run(pixel, &mut it, &codes1, &lengths1)?;
                }
            }
            _ => unreachable!(),
        }

        // flush writer
        self.flush()?;
        if self.chunk_buffer.len() % 2 == 1 {
            self.chunk_buffer.push(0);
        }

        // write container
        self.writer.write_all(b"RIFF")?;
        self.writer
            .write_all(&(self.chunk_buffer.len() as u32 + 12).to_le_bytes())?;
        self.writer.write_all(b"WEBP")?;
        self.writer.write_all(b"VP8L")?;
        self.writer
            .write_all(&(self.chunk_buffer.len() as u32).to_le_bytes())?;
        self.writer.write_all(&self.chunk_buffer)?;

        Ok(())
    }

    #[cfg(feature = "webp-encoder")]
    fn encode_lossy(
        mut self,
        data: &[u8],
        width: u32,
        height: u32,
        color: ColorType,
    ) -> ImageResult<()> {
        // TODO: convert color types internally?
        let layout = match color {
            ColorType::Rgb8 => PixelLayout::Rgb,
            ColorType::Rgba8 => PixelLayout::Rgba,
            _ => {
                return Err(ImageError::Unsupported(
                    UnsupportedError::from_format_and_kind(
                        ImageFormat::WebP.into(),
                        UnsupportedErrorKind::Color(color.into()),
                    ),
                ))
            }
        };

        // Validate dimensions upfront to avoid panics.
        if width == 0
            || height == 0
            || !SampleLayout::row_major_packed(color.channel_count(), width, height)
                .fits(data.len())
        {
            return Err(ImageError::Parameter(ParameterError::from_kind(
                ParameterErrorKind::DimensionMismatch,
            )));
        }

        // Call the native libwebp library to encode the image.
        let encoder = Encoder::new(data, layout, width, height);
        let encoded: WebPMemory = match self.quality.0 {
            Quality::Lossless => encoder.encode_lossless(),
            #[allow(deprecated)]
            Quality::Lossy(quality) => encoder.encode(quality as f32),
        };

        // The simple encoding API in libwebp does not return errors.
        if encoded.is_empty() {
            return Err(ImageError::Encoding(crate::error::EncodingError::new(
                ImageFormat::WebP.into(),
                "encoding failed, output empty",
            )));
        }

        self.writer.write_all(&encoded)?;
        Ok(())
    }

    /// Encode image data with the indicated color type.
    ///
    /// The encoder requires image data be Rgb8 or Rgba8.
    ///
    /// # Panics
    ///
    /// Panics if `width * height * color.bytes_per_pixel() != data.len()`.
    #[track_caller]
    pub fn encode(self, data: &[u8], width: u32, height: u32, color: ColorType) -> ImageResult<()> {
        let expected_buffer_len =
            (width as u64 * height as u64).saturating_mul(color.bytes_per_pixel() as u64);
        assert_eq!(
            expected_buffer_len,
            data.len() as u64,
            "Invalid buffer length: expected {expected_buffer_len} got {} for {width}x{height} image",
            data.len(),
        );

        if let WebPQuality(Quality::Lossless) = self.quality {
            self.encode_lossless(data, width, height, color)
        } else {
            #[cfg(feature = "webp-encoder")]
            return self.encode_lossy(data, width, height, color);
            #[cfg(not(feature = "webp-encoder"))]
            unreachable!()
        }
    }
}

impl<W: Write> ImageEncoder for WebPEncoder<W> {
    #[track_caller]
    fn write_image(
        self,
        buf: &[u8],
        width: u32,
        height: u32,
        color_type: ColorType,
    ) -> ImageResult<()> {
        self.encode(buf, width, height, color_type)
    }
}

#[cfg(test)]
mod tests {
    use crate::{ImageEncoder, RgbaImage};

    #[test]
    fn write_webp() {
        let img = RgbaImage::from_raw(10, 6, (0..240).collect()).unwrap();

        let mut output = Vec::new();
        super::WebPEncoder::new_lossless(&mut output)
            .write_image(
                img.inner_pixels(),
                img.width(),
                img.height(),
                crate::ColorType::Rgba8,
            )
            .unwrap();

        let img2 = crate::load_from_memory_with_format(&output, crate::ImageFormat::WebP)
            .unwrap()
            .to_rgba8();

        assert_eq!(img, img2);
    }
}

#[cfg(test)]
#[cfg(feature = "webp-encoder")]
mod native_tests {
    use crate::codecs::webp::{WebPEncoder, WebPQuality};
    use crate::{ColorType, ImageEncoder};

    #[derive(Debug, Clone)]
    struct MockImage {
        width: u32,
        height: u32,
        color: ColorType,
        data: Vec<u8>,
    }

    impl quickcheck::Arbitrary for MockImage {
        fn arbitrary(g: &mut quickcheck::Gen) -> Self {
            // Limit to small, non-empty images <= 512x512.
            let width = u32::arbitrary(g) % 512 + 1;
            let height = u32::arbitrary(g) % 512 + 1;
            let (color, stride) = if bool::arbitrary(g) {
                (ColorType::Rgb8, 3)
            } else {
                (ColorType::Rgba8, 4)
            };
            let size = width * height * stride;
            let data: Vec<u8> = (0..size).map(|_| u8::arbitrary(g)).collect();
            MockImage {
                width,
                height,
                color,
                data,
            }
        }
    }

    quickcheck! {
        fn fuzz_webp_valid_image(image: MockImage, quality: u8) -> bool {
            // Check valid images do not panic.
            let mut buffer = Vec::<u8>::new();
            for webp_quality in [WebPQuality::lossless(), #[allow(deprecated)] WebPQuality::lossy(quality)] {
                buffer.clear();
                #[allow(deprecated)]
                let encoder = WebPEncoder::new_with_quality(&mut buffer, webp_quality);
                if encoder
                    .write_image(&image.data, image.width, image.height, image.color).is_err() {
                    return false;
                }
            }
            true
        }

        fn fuzz_webp_no_panic(data: Vec<u8>, width: u8, height: u8, quality: u8) -> bool {
            // Check random (usually invalid) parameters do not panic.

            if data.len() < width as usize * height as usize * 4 {
                return true;
            }

            let mut buffer = Vec::<u8>::new();
            for color in [ColorType::Rgb8, ColorType::Rgba8] {
                for webp_quality in [WebPQuality::lossless(), #[allow(deprecated)] WebPQuality::lossy(quality)] {
                    buffer.clear();
                    #[allow(deprecated)]
                    let encoder = WebPEncoder::new_with_quality(&mut buffer, webp_quality);
                    // Ignore errors.
                    let _ = encoder.write_image(
                        &data[..width as usize * height as usize * color.bytes_per_pixel() as usize],
                        width as u32,
                        height as u32,
                        color,
                    );
                }
            }
            true
        }
    }
}
