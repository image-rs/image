use std::convert::TryInto;

use crate::color::{ExtendedColorType, ExtendedColorType::*};

/// Error from trying to convert a buffer of pixels.
#[derive(Copy, Clone, Debug)]
pub enum ConvertError {
    /// In input pixels are invalid for the given color type.
    CorruptInput,
    /// The alignment of the provided buffers doesn't match the required alignment for the
    /// converter.
    BadAlignment,
    /// The parameters specifying width and row pitch are inconsistent.
    BadDimensions,
}

/// An object that can convert raw buffers of pixels from one format to another.
pub trait Converter {
    /// Returns the (source, destination) color types that this converts between.
    fn color_types(&self) -> (ExtendedColorType, ExtendedColorType);

    /// Returns the require alignments for source and destination buffers.
    fn required_alignments(&self) -> (u8, u8);

    /// Perform the conversion.
    ///
    /// * `source` contains the input pixels and must have alignment of at least
    ///   `require_alignment().0`.
    ///
    /// * `source_width` is the number of pixels in a source row (the number of rows is computed
    ///   based on the length of the buffer).
    ///
    /// * `source_row_pitch` is the number of bytes between the start of successive rows in the
    ///   source buffer, or None if the pixels are tighly packed. Must be divisible by the source
    ///   buffer's required alignment.
    ///
    /// * `destination` is the output buffer to use which must be aligned to at least
    ///   `required_alignment().1`.
    ///
    /// * `destination_row_pitch` is the number of bytes between the start of successive rows in the
    ///   destination buffer, or None if the pixels are tightly packed. Must be divisible by the
    ///   destination buffer's required alignment.
    fn convert(
        &self,
        source: &[u8],
        source_width: u32,
        source_row_pitch: usize,
        destination: &mut [u8],
        destination_row_pitch: usize,
    ) -> Result<(), ConvertError>;
}

/// A naive converter that converts any `ExtendedColorType` into some variant of `ColorType` without
/// considering colorspaces.
#[derive(Copy, Clone)]
pub struct NormalizeConverter(ExtendedColorType);
impl NormalizeConverter {
    /// Create a new converter. Succeeds for all color types except `ExtendedColorType::Unknown`.
    pub fn new(t: ExtendedColorType) -> Option<Self> {
        if let Unknown(_) = t {
            return None;
        }
        Some(Self(t))
    }
}
impl Converter for NormalizeConverter {
    fn color_types(&self) -> (ExtendedColorType, ExtendedColorType) {
        (
            self.0,
            match self.0 {
                L1 | L2 | L4 | L8 => L8,
                La1 | La2 | La4 | La8 => La8,
                Rgb1 | Rgb2 | Rgb4 | Rgb8 => Rgb8,
                Rgba1 | Rgba2 | Rgba4 | Rgba8 => Rgba8,
                Bgr8 => Bgr8,
                Bgra8 => Bgra8,
                L16 => L16,
                La16 => La16,
                Rgb16 => Rgb16,
                Rgba16 => Rgba16,
                Unknown(_) => unreachable!(),
                __NonExhaustive(m) => match m._private {},
            },
        )
    }
    fn required_alignments(&self) -> (u8, u8) {
        match self.0 {
            L1 | L2 | L4 | L8 | La1 | La2 | La4 | La8 | Rgb1 | Rgb2 | Rgb4 | Rgb8 | Rgba1
            | Rgba2 | Rgba4 | Rgba8 | Bgr8 | Bgra8 | L16 | La16 | Rgb16 | Rgba16 => (1, 1),
            Unknown(_) => unreachable!(),
            __NonExhaustive(m) => match m._private {},
        }
    }
    fn convert(
        &self,
        source: &[u8],
        src_width: u32,
        src_row_pitch: usize,
        destination: &mut [u8],
        dst_row_pitch: usize,
    ) -> Result<(), ConvertError> {
        let src_bpp = self.0.bits_per_pixel() as u64;
        let dst_bpp = self.color_types().1.bits_per_pixel() as u64;

        let src_row_bytes = ((src_bpp.saturating_mul(src_width as u64) + 7) / 8)
            .try_into()
            .unwrap_or(usize::MAX);
        let dst_row_bytes = ((dst_bpp.saturating_mul(src_width as u64) + 7) / 8)
            .try_into()
            .unwrap_or(usize::MAX);

        if src_row_pitch < src_row_bytes || dst_row_pitch < dst_row_bytes {
            return Err(ConvertError::BadDimensions);
        }

        let src_rows = source
            .chunks(src_row_pitch)
            .map(|s| &s[..src_row_bytes.min(s.len())]);
        let dst_rows = destination.chunks_mut(dst_row_pitch).map(|s| {
            let len = dst_row_bytes.min(s.len());
            &mut s[..len]
        });

        match self.0 {
            L1 | La1 | Rgb1 | Rgba1 => {
                for (src, dst) in src_rows.zip(dst_rows) {
                    for (s, ds) in src.iter().zip(dst.chunks_mut(8)) {
                        for (i, d) in ds.iter_mut().enumerate() {
                            *d = match (s >> (7-i)) & 0b1 {
                                0b0 => 0x0,
                                0b1 => 0xff,
                                _ => unreachable!(),
                            };
                        }
                    }
                }
            }
            L2 | La2 | Rgb2 | Rgba2 => {
                for (src, dst) in src_rows.zip(dst_rows) {
                    for (s, ds) in src.iter().zip(dst.chunks_mut(4)) {
                        for (i, d) in ds.iter_mut().enumerate() {
                            *d = match (s >> (2 * (3-i))) & 0b11 {
                                0b00 => 0b00000000,
                                0b01 => 0b01010101,
                                0b10 => 0b10101010,
                                0b11 => 0b11111111,
                                _ => unreachable!(),
                            };
                        }
                    }
                }
            }
            L4 | La4 | Rgb4 | Rgba4 => {
                for (src, dst) in src_rows.zip(dst_rows) {
                    for (s, ds) in src.iter().zip(dst.chunks_mut(2)) {
                        for (i, d) in ds.iter_mut().enumerate() {
                            let b = (s >> (4 * (1-i))) & 0b1111;
                            *d = b | (b << 4);
                        }
                    }
                }
            }
            L8 | La8 | Rgb8 | Rgba8 | Bgr8 | Bgra8 | L16 | La16 | Rgb16 | Rgba16 => {
                for (src, dst) in src_rows.zip(dst_rows) {
                    dst.copy_from_slice(src);
                }
            }
            Unknown(_) => unreachable!(),
            __NonExhaustive(m) => match m._private {},
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_1_bit() {
        let source = [0b1110_1000, 0b0101_1010];
        let expanded = [0xff, 0xff, 0xff, 0, 0xff, 0, 0, 0,
                        0, 0xff, 0, 0xff, 0xff, 0, 0xff, 0];

        let mut destination = [0; 16];
        let converter = NormalizeConverter::new(ExtendedColorType::La1).unwrap();
        assert_eq!(converter.required_alignments(), (1, 1));
        assert_eq!(converter.color_types(), (ExtendedColorType::La1, ExtendedColorType::La8));
        converter.convert(&source, 4, 1, &mut destination, 8).unwrap();
        assert_eq!(destination, expanded);

        let mut destination = [0; 16];
        let converter = NormalizeConverter::new(ExtendedColorType::Rgb1).unwrap();
        assert_eq!(converter.required_alignments(), (1, 1));
        assert_eq!(converter.color_types(), (ExtendedColorType::Rgb1, ExtendedColorType::Rgb8));
        converter.convert(&source, 1, 2, &mut destination, 8).unwrap();
        assert_eq!(destination[0..3], expanded[0..3]);
        assert_eq!(destination[3..], [0u8; 13]);
    }
}
