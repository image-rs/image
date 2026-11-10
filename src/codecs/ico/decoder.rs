use byteorder_lite::ReadBytesExt;
use std::io::{BufRead, Read, Seek};
use std::{error, fmt};

use crate::color::ColorType;
use crate::error::{
    DecodingError, ImageError, ImageResult, UnsupportedError, UnsupportedErrorKind,
};
use crate::io::image_reader_type::SpecCompliance;
use crate::io::{
    DecodedAnimationAttributes, DecodedImageAttributes, DecoderPreparedImage, FormatAttributes,
};
use crate::utils::seek_start_with_offset;
use crate::{ImageDecoder, ImageFormat};

use self::InnerDecoder::*;
use crate::codecs::bmp::BmpDecoder;
use crate::codecs::png::{PngDecoder, PNG_SIGNATURE};

/// Errors that can occur during decoding and parsing an ICO image or one of its enclosed images.
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
enum DecoderError {
    /// The ICO directory is empty
    NoEntries,
    /// The number of color planes (0 or 1), or the horizontal coordinate of the hotspot for CUR files too big.
    IcoEntryTooManyPlanesOrHotspot,
    /// The bit depth (may be 0 meaning unspecified), or the vertical coordinate of the hotspot for CUR files too big.
    IcoEntryTooManyBitsPerPixelOrHotspot,

    /// The entry is in PNG format and specified a length that is shorter than PNG header.
    PngShorterThanHeader,
    /// The enclosed PNG is not in RGBA, which is invalid: <https://blogs.msdn.microsoft.com/oldnewthing/20101022-00/?p=12473>/.
    PngNotRgba,

    /// The entry is in BMP format and specified a data size that is not correct for the image and optional mask data.
    InvalidDataSize,

    /// The dimensions specified by the entry does not match the dimensions in the header of the enclosed image.
    ImageEntryDimensionMismatch {
        /// The mismatched subimage's type
        format: IcoEntryImageFormat,
        /// The dimensions specified by the entry
        entry: (u16, u16),
        /// The dimensions of the image itself
        image: (u32, u32),
    },
}

impl fmt::Display for DecoderError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DecoderError::NoEntries => f.write_str("ICO directory contains no image"),
            DecoderError::IcoEntryTooManyPlanesOrHotspot => {
                f.write_str("ICO image entry has too many color planes or too large hotspot value")
            }
            DecoderError::IcoEntryTooManyBitsPerPixelOrHotspot => f.write_str(
                "ICO image entry has too many bits per pixel or too large hotspot value",
            ),
            DecoderError::PngShorterThanHeader => {
                f.write_str("Entry specified a length that is shorter than PNG header!")
            }
            DecoderError::PngNotRgba => f.write_str("The PNG is not in RGBA format!"),
            DecoderError::InvalidDataSize => {
                f.write_str("ICO image data size did not match expected size")
            }
            DecoderError::ImageEntryDimensionMismatch {
                format,
                entry,
                image,
            } => f.write_fmt(format_args!(
                "Entry{entry:?} and {format}{image:?} dimensions do not match!"
            )),
        }
    }
}

impl From<DecoderError> for ImageError {
    fn from(e: DecoderError) -> ImageError {
        ImageError::Decoding(DecodingError::new(ImageFormat::Ico.into(), e))
    }
}

impl error::Error for DecoderError {}

/// The image formats an ICO may contain
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
enum IcoEntryImageFormat {
    /// PNG in ARGB
    Png,
    /// BMP with optional alpha mask
    Bmp,
}

impl fmt::Display for IcoEntryImageFormat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            IcoEntryImageFormat::Png => "PNG",
            IcoEntryImageFormat::Bmp => "BMP",
        })
    }
}

impl From<IcoEntryImageFormat> for ImageFormat {
    fn from(val: IcoEntryImageFormat) -> Self {
        match val {
            IcoEntryImageFormat::Png => ImageFormat::Png,
            IcoEntryImageFormat::Bmp => ImageFormat::Bmp,
        }
    }
}

/// An ico decoder
pub struct IcoDecoder<R: BufRead + Seek> {
    selected_entry: DirEntry,
    reader_offset: u64,
    inner_decoder: InnerDecoder<R>,
    spec_strictness: SpecCompliance,
}

enum InnerDecoder<R: BufRead + Seek> {
    Bmp(BmpDecoder<R>),
    Png(Box<PngDecoder<R>>),
}

#[derive(Clone, Copy, Default)]
struct DirEntry {
    width: u8,
    height: u8,
    // We ignore some header fields as they will be replicated in the PNG, BMP and they are not
    // necessary for determining the best_entry.
    #[allow(unused)]
    color_count: u8,
    // Wikipedia has this to say:
    // Although Microsoft's technical documentation states that this value must be zero, the icon
    // encoder built into .NET (System.Drawing.Icon.Save) sets this value to 255. It appears that
    // the operating system ignores this value altogether.
    #[allow(unused)]
    reserved: u8,

    // We ignore some header fields as they will be replicated in the PNG, BMP and they are not
    // necessary for determining the best_entry.
    #[allow(unused)]
    num_color_planes: u16,
    bits_per_pixel: u16,

    image_length: u32,
    image_offset: u32,
}

impl<R: BufRead + Seek> IcoDecoder<R> {
    /// Create a new decoder that decodes from the stream ```r```
    pub fn new(r: R) -> ImageResult<IcoDecoder<R>> {
        Self::with_spec_compliance(r, SpecCompliance::default())
    }

    /// Create a new decoder with the given spec compliance mode.
    pub(crate) fn with_spec_compliance(
        mut r: R,
        spec: SpecCompliance,
    ) -> ImageResult<IcoDecoder<R>> {
        let reader_offset = r.stream_position()?;
        let entries = read_entries(&mut r, spec)?;
        let entry = best_entry(entries)?;
        let decoder = entry.decoder(r, reader_offset)?;

        Ok(IcoDecoder {
            selected_entry: entry,
            reader_offset,
            inner_decoder: decoder,
            spec_strictness: spec,
        })
    }
}

fn read_entries<R: Read>(r: &mut R, spec: SpecCompliance) -> ImageResult<Vec<DirEntry>> {
    let mut header = [0u8; 6];
    r.read_exact(&mut header)?;
    // header[0..2] = reserved, header[2..4] = type, header[4..6] = count
    let count = u16::from_le_bytes(header[4..6].try_into().unwrap());
    (0..count).map(|_| read_entry(r, spec)).collect()
}

fn read_entry<R: Read>(r: &mut R, spec: SpecCompliance) -> ImageResult<DirEntry> {
    let mut buf = [0u8; 16];
    r.read_exact(&mut buf)?;

    // Parse fields from buffer
    // buf[4..6]: may be color planes (0 or 1) or horizontal hotspot for CUR files
    let num_color_planes = u16::from_le_bytes(buf[4..6].try_into().unwrap());
    if spec == SpecCompliance::Strict && num_color_planes > 256 {
        return Err(DecoderError::IcoEntryTooManyPlanesOrHotspot.into());
    }

    // buf[6..8]: may be bit depth (0 = unspecified) or vertical hotspot for CUR files
    let mut bits_per_pixel = u16::from_le_bytes(buf[6..8].try_into().unwrap());
    if spec == SpecCompliance::Strict && bits_per_pixel > 256 {
        return Err(DecoderError::IcoEntryTooManyBitsPerPixelOrHotspot.into());
    }

    let color_count = buf[2];

    // Some icons don't have a bit depth, only a color count. Convert the color count
    // to the minimum necessary bit depth.
    if bits_per_pixel == 0 {
        let count = match color_count {
            0 => 256,
            c => u16::from(c),
        };
        bits_per_pixel = count.next_power_of_two().trailing_zeros() as u16;
    }

    Ok(DirEntry {
        width: buf[0],
        height: buf[1],
        color_count,
        reserved: buf[3],
        num_color_planes,
        bits_per_pixel,
        image_length: u32::from_le_bytes(buf[8..12].try_into().unwrap()),
        image_offset: u32::from_le_bytes(buf[12..16].try_into().unwrap()),
    })
}

/// Find the entry with the highest (size, color depth).
///
/// If two entries have the same size and color depth, pick the first one.
/// While ICO files with multiple identical size and bpp entries are rare, they
/// do exist. Since we can't make an educated guess which one is best, picking
/// the first one is a reasonable default.
fn best_entry(entries: Vec<DirEntry>) -> ImageResult<DirEntry> {
    entries
        .into_iter()
        .rev() // ties should pick the first entry, not the last
        .max_by_key(|entry| {
            (
                u32::from(entry.real_width()) * u32::from(entry.real_height()),
                entry.bits_per_pixel,
            )
        })
        .ok_or(DecoderError::NoEntries.into())
}

impl DirEntry {
    fn real_width(&self) -> u16 {
        match self.width {
            0 => 256,
            w => u16::from(w),
        }
    }

    fn real_height(&self) -> u16 {
        match self.height {
            0 => 256,
            h => u16::from(h),
        }
    }

    fn matches_dimensions(&self, width: u32, height: u32) -> bool {
        u32::from(self.real_width()) == width.min(256)
            && u32::from(self.real_height()) == height.min(256)
    }

    fn seek_to_start<R: Read + Seek>(&self, r: &mut R, reader_offset: u64) -> ImageResult<()> {
        seek_start_with_offset(r, reader_offset, u64::from(self.image_offset))?;
        Ok(())
    }

    fn is_png<R: Read + Seek>(&self, r: &mut R, reader_offset: u64) -> ImageResult<bool> {
        self.seek_to_start(r, reader_offset)?;

        // Read the first 8 bytes to sniff the image.
        let mut signature = [0u8; 8];
        r.read_exact(&mut signature)?;

        Ok(signature == PNG_SIGNATURE)
    }

    fn decoder<R: BufRead + Seek>(
        &self,
        mut r: R,
        reader_offset: u64,
    ) -> ImageResult<InnerDecoder<R>> {
        let is_png = self.is_png(&mut r, reader_offset)?;
        self.seek_to_start(&mut r, reader_offset)?;

        if is_png {
            Ok(Png(Box::new(PngDecoder::new(r))))
        } else {
            Ok(Bmp(BmpDecoder::new_with_ico_format(r)?))
        }
    }
}

// We forward everything to png or bmp decoder.
#[deny(clippy::missing_trait_methods)]
impl<R: BufRead + Seek> ImageDecoder for IcoDecoder<R> {
    fn format_attributes(&self) -> FormatAttributes {
        match &self.inner_decoder {
            Bmp(decoder) => decoder.format_attributes(),
            Png(decoder) => decoder.format_attributes(),
        }
    }

    fn prepare_image(&mut self) -> ImageResult<DecoderPreparedImage> {
        match &mut self.inner_decoder {
            Bmp(decoder) => decoder.prepare_image(),
            Png(decoder) => decoder.prepare_image(),
        }
    }

    fn animation_attributes(&mut self) -> Option<DecodedAnimationAttributes> {
        match &mut self.inner_decoder {
            Bmp(decoder) => decoder.animation_attributes(),
            Png(decoder) => decoder.animation_attributes(),
        }
    }

    fn read_image(&mut self, buf: &mut [u8]) -> ImageResult<DecodedImageAttributes> {
        match &mut self.inner_decoder {
            Png(decoder) => {
                if self.selected_entry.image_length < PNG_SIGNATURE.len() as u32 {
                    return Err(DecoderError::PngShorterThanHeader.into());
                }

                let layout = decoder.prepare_image()?;
                // Check if the image dimensions match the ones in the image data.
                let (width, height) = layout.layout.dimensions();
                if self.spec_strictness == SpecCompliance::Strict
                    && !self.selected_entry.matches_dimensions(width, height)
                {
                    return Err(DecoderError::ImageEntryDimensionMismatch {
                        format: IcoEntryImageFormat::Png,
                        entry: (
                            self.selected_entry.real_width(),
                            self.selected_entry.real_height(),
                        ),
                        image: (width, height),
                    }
                    .into());
                }

                // > The format of a PNG-compressed image consists simply of a PNG image, starting
                // > with the PNG file signature. The image must be in 32bpp ARGB format [...].
                // https://devblogs.microsoft.com/oldnewthing/20101022-00/?p=12473
                //
                // This requirement was added to better support older software, which might crash for arbitrary PNGs.
                // We have a state-of-the-art PNG decoder, so allowing any PNG format is easy for us.
                // However, we still want to enforce this restriction is strict mode for compatibility with other decoders.
                if self.spec_strictness == SpecCompliance::Strict
                    && layout.layout.color != ColorType::Rgba8
                {
                    return Err(DecoderError::PngNotRgba.into());
                }

                decoder.read_image(buf)
            }
            Bmp(decoder) => {
                let layout = decoder.prepare_image()?;
                let (width, height) = layout.layout.dimensions();
                if self.spec_strictness == SpecCompliance::Strict
                    && !self.selected_entry.matches_dimensions(width, height)
                {
                    return Err(DecoderError::ImageEntryDimensionMismatch {
                        format: IcoEntryImageFormat::Bmp,
                        entry: (
                            self.selected_entry.real_width(),
                            self.selected_entry.real_height(),
                        ),
                        image: (width, height),
                    }
                    .into());
                }

                // The ICO decoder needs an alpha channel to apply the AND mask.
                if layout.layout.color != ColorType::Rgba8 {
                    return Err(ImageError::Unsupported(
                        UnsupportedError::from_format_and_kind(
                            ImageFormat::Bmp.into(),
                            UnsupportedErrorKind::Color(layout.layout.color.into()),
                        ),
                    ));
                }

                decoder.read_image_data(buf)?;

                let r = decoder.reader();
                let image_end = r.stream_position()?;
                let data_end = self.reader_offset
                    + u64::from(self.selected_entry.image_offset)
                    + u64::from(self.selected_entry.image_length);

                let mask_row_bytes = width.div_ceil(32) * 4;
                let mask_length = u64::from(mask_row_bytes) * u64::from(height);

                // data_end should be image_end + the mask length (mask_row_bytes * height).
                // According to
                // https://devblogs.microsoft.com/oldnewthing/20101021-00/?p=12483
                // the mask is required, but according to Wikipedia
                // https://en.wikipedia.org/wiki/ICO_(file_format)
                // the mask is not required. Unfortunately, Wikipedia does not have a citation
                // for that claim, so we can't be sure which is correct.
                if data_end >= image_end + mask_length {
                    if width == 0 {
                        return Ok(DecodedImageAttributes::default());
                    }

                    // 32bpp BMPs already have a native alpha channel, so the
                    // AND mask is ignored.
                    // For lower bit depths, read and apply the AND mask.
                    if self.selected_entry.bits_per_pixel < 32 {
                        let rgba = buf.as_chunks_mut::<4>().0;
                        let rows = rgba.chunks_exact_mut(width as usize);

                        if rows.len() != height as usize {
                            return Err(DecoderError::InvalidDataSize.into());
                        }

                        // If there's an AND mask following the image, read and apply it.
                        // This from the bottom up (in terms of our coordinates).
                        for row in rows.rev() {
                            let mut x = 0;

                            // Apply the bits of each byte until we reach the end of the row.
                            for _ in 0..mask_row_bytes {
                                let mask_byte = r.read_u8()?;
                                for bit in (0..8).rev() {
                                    if x >= width {
                                        break;
                                    }

                                    if mask_byte & (1 << bit) != 0 {
                                        // Set pixel to fully transparent.
                                        row[x as usize] = [0, 0, 0, 0];
                                    }

                                    x += 1;
                                }
                            }
                        }
                    }

                    Ok(DecodedImageAttributes::default())
                } else if data_end == image_end {
                    // accept images with no mask data
                    Ok(DecodedImageAttributes::default())
                } else if self.spec_strictness == SpecCompliance::Lenient
                    && self.selected_entry.bits_per_pixel >= 32
                {
                    // In lenient mode, we accept truncated mask data for 32bpp images
                    // since they already have an alpha channel and we ignore the AND mask anyway.
                    Ok(DecodedImageAttributes::default())
                } else {
                    Err(DecoderError::InvalidDataSize.into())
                }
            }
        }
    }

    fn icc_profile(&mut self) -> ImageResult<Option<Vec<u8>>> {
        match &mut self.inner_decoder {
            Bmp(decoder) => decoder.icc_profile(),
            Png(decoder) => decoder.icc_profile(),
        }
    }

    fn exif_metadata(&mut self) -> ImageResult<Option<Vec<u8>>> {
        match &mut self.inner_decoder {
            Bmp(decoder) => decoder.exif_metadata(),
            Png(decoder) => decoder.exif_metadata(),
        }
    }

    fn xmp_metadata(&mut self) -> ImageResult<Option<Vec<u8>>> {
        match &mut self.inner_decoder {
            Bmp(decoder) => decoder.xmp_metadata(),
            Png(decoder) => decoder.xmp_metadata(),
        }
    }

    fn iptc_metadata(&mut self) -> ImageResult<Option<Vec<u8>>> {
        match &mut self.inner_decoder {
            Bmp(decoder) => decoder.iptc_metadata(),
            Png(decoder) => decoder.iptc_metadata(),
        }
    }

    fn set_limits(&mut self, limits: crate::Limits) -> ImageResult<()> {
        match &mut self.inner_decoder {
            Bmp(decoder) => decoder.set_limits(limits),
            Png(decoder) => decoder.set_limits(limits),
        }
    }

    fn more_images(&self) -> crate::io::SequenceControl {
        // ICO files only provide a single image for now. This may change in the future, we might
        // want to yield the others as thumbnails.
        match &self.inner_decoder {
            Bmp(decoder) => decoder.more_images(),
            Png(decoder) => decoder.more_images(),
        }
    }

    fn finish(&mut self) -> ImageResult<()> {
        match &mut self.inner_decoder {
            Bmp(decoder) => decoder.finish(),
            Png(decoder) => decoder.finish(),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    // Test if BMP images without alpha channel inside ICOs don't panic.
    // Because the test data is invalid decoding should produce an error.
    #[test]
    fn bmp_16_with_missing_alpha_channel() {
        let data = vec![
            0x00, 0x00, 0x01, 0x00, 0x01, 0x00, 0x0e, 0x04, 0xc3, 0x7e, 0x00, 0x00, 0x00, 0x00,
            0x7c, 0x00, 0x00, 0x00, 0x0e, 0x00, 0x00, 0x00, 0xf8, 0xff, 0xff, 0xff, 0x01, 0x00,
            0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x12, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x8f, 0xf6, 0xff, 0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x20, 0x66, 0x74, 0x83, 0x70, 0x61, 0x76, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x02,
            0x00, 0x00, 0x00, 0x00, 0xff, 0xff, 0xff, 0xeb, 0x00, 0x9b, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x4e, 0x47, 0x0d,
            0x0a, 0x1a, 0x0a, 0x00, 0x00, 0x00, 0x62, 0x49, 0x48, 0x44, 0x52, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x02, 0x0c,
            0x00, 0x00, 0x00, 0xc3, 0x3f, 0x94, 0x61, 0xaa, 0x17, 0x4d, 0x8d, 0x79, 0x1d, 0x8b,
            0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x14, 0x2e, 0x28, 0x40, 0xe5, 0x9f,
            0x4b, 0x4d, 0xe9, 0x87, 0xd3, 0xda, 0xd6, 0x89, 0x81, 0xc5, 0xa4, 0xa1, 0x60, 0x98,
            0x31, 0xc7, 0x1d, 0xb6, 0x8f, 0x20, 0xc8, 0x3e, 0xee, 0xd8, 0xe4, 0x8f, 0xee, 0x7b,
            0x48, 0x9b, 0x88, 0x25, 0x13, 0xda, 0xa4, 0x13, 0xa4, 0x00, 0x00, 0x00, 0x00, 0x40,
            0x16, 0x01, 0xff, 0xff, 0xff, 0xff, 0xe9, 0x0c, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0xa3, 0x66, 0x64, 0x41, 0x54, 0xa3, 0xa3, 0x00, 0x00, 0x00, 0xb8, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xa3, 0x66, 0x64, 0x41, 0x54, 0xa3, 0xa3,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x8f, 0xf6, 0xff, 0xff,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x20, 0x66, 0x74, 0x83, 0x70, 0x61, 0x76,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x00, 0xff, 0xff, 0xff,
            0xeb, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a, 0x00, 0x00, 0x00, 0x62, 0x49,
            0x48, 0x44, 0x52, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x00, 0x00,
            0x00, 0x00, 0x00, 0xff, 0xff, 0x94, 0xc8, 0x00, 0x02, 0x0c, 0x00, 0xff, 0xff, 0xc6,
            0x84, 0x00, 0x2a, 0x75, 0x03, 0xa3, 0x05, 0xfb, 0xe1, 0x6e, 0xe8, 0x27, 0xd6, 0xd3,
            0x96, 0xc1, 0xe4, 0x30, 0x0c, 0x05, 0xb9, 0xa3, 0x8b, 0x29, 0xda, 0xa4, 0xf1, 0x4d,
            0xf3, 0xb2, 0x98, 0x2b, 0xe6, 0x93, 0x07, 0xf9, 0xca, 0x2b, 0xc2, 0x39, 0x20, 0xba,
            0x7c, 0xa0, 0xb1, 0x43, 0xe6, 0xf9, 0xdc, 0xd1, 0xc2, 0x52, 0xdc, 0x41, 0xc1, 0x2f,
            0x29, 0xf7, 0x46, 0x32, 0xda, 0x1b, 0x72, 0x8c, 0xe6, 0x2b, 0x01, 0xe5, 0x49, 0x21,
            0x89, 0x89, 0xe4, 0x3d, 0xa1, 0xdb, 0x3b, 0x4a, 0x0b, 0x52, 0x86, 0x52, 0x33, 0x9d,
            0xb2, 0xcf, 0x4a, 0x86, 0x53, 0xd7, 0xa9, 0x4b, 0xaf, 0x62, 0x06, 0x49, 0x53, 0x00,
            0xc3, 0x3f, 0x94, 0x61, 0xaa, 0x17, 0x4d, 0x8d, 0x79, 0x1d, 0x8b, 0x10, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x14, 0x2e, 0x28, 0x40, 0xe5, 0x9f, 0x4b, 0x4d, 0xe9,
            0x87, 0xd3, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xe7, 0xc5, 0x00,
            0x02, 0x00, 0x00, 0x00, 0x06, 0x00, 0x0b, 0x00, 0x50, 0x31, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x76, 0x76, 0x01, 0x00, 0x00, 0x00, 0x76, 0x00,
            0x00, 0x23, 0x3f, 0x52, 0x41, 0x44, 0x49, 0x41, 0x4e, 0x43, 0x45, 0x61, 0x50, 0x35,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x03, 0x4d, 0x47, 0x49, 0x46, 0x38, 0x37, 0x61, 0x05,
            0x50, 0x37, 0x00, 0x00, 0x00, 0x00, 0x00, 0xc7, 0x37, 0x61,
        ];

        let mut decoder = IcoDecoder::new(std::io::Cursor::new(&data)).unwrap();
        let bytes = decoder.prepare_image().unwrap().total_bytes();
        let mut buf = vec![0; usize::try_from(bytes).unwrap()];
        assert!(decoder.read_image(&mut buf).is_err());
    }

    #[test]
    fn dimension_mismatch_strict_vs_lenient() {
        // Minimal 2x2 32-bit BMP inside an ICO where the directory entry says 3x3.
        let data = vec![
            0x00, 0x00, 0x01, 0x00, 0x01, 0x00, 0x03, 0x03, 0x00, 0x00, 0x01, 0x00, 0x20, 0x00,
            0x40, 0x00, 0x00, 0x00, 0x16, 0x00, 0x00, 0x00, 0x28, 0x00, 0x00, 0x00, 0x02, 0x00,
            0x00, 0x00, 0x04, 0x00, 0x00, 0x00, 0x01, 0x00, 0x20, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
            0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00,
        ];

        let mut decoder =
            IcoDecoder::with_spec_compliance(std::io::Cursor::new(&data), SpecCompliance::Lenient)
                .unwrap();
        let bytes = decoder.prepare_image().unwrap().total_bytes();
        let mut buf = vec![0; usize::try_from(bytes).unwrap()];
        assert!(decoder.read_image(&mut buf).is_ok());

        let mut decoder =
            IcoDecoder::with_spec_compliance(std::io::Cursor::new(&data), SpecCompliance::Strict)
                .unwrap();
        let bytes = decoder.prepare_image().unwrap().total_bytes();
        let mut buf = vec![0; usize::try_from(bytes).unwrap()];
        assert!(decoder.read_image(&mut buf).is_err());
    }

    #[test]
    fn truncated_mask_32bpp_lenient() {
        let data = std::fs::read("tests/images/ico/images/truncated_mask_32bpp.ico").unwrap();

        let mut decoder =
            IcoDecoder::with_spec_compliance(std::io::Cursor::new(&data), SpecCompliance::Lenient)
                .unwrap();
        let bytes = decoder.prepare_image().unwrap().total_bytes();
        let mut buf = vec![0; usize::try_from(bytes).unwrap()];
        assert!(decoder.read_image(&mut buf).is_ok());

        let mut decoder =
            IcoDecoder::with_spec_compliance(std::io::Cursor::new(&data), SpecCompliance::Strict)
                .unwrap();
        let bytes = decoder.prepare_image().unwrap().total_bytes();
        let mut buf = vec![0; usize::try_from(bytes).unwrap()];
        assert!(decoder.read_image(&mut buf).is_err());
    }

    // Verify that the AND mask is ignored for 32bpp BMP images in ICO files.
    #[test]
    fn bmp_32bpp_and_mask_ignored() {
        let data =
            std::fs::read("tests/images/ico/images/bmp-32bpp-conflicting-and-mask.ico").unwrap();

        let mut decoder = IcoDecoder::new(std::io::Cursor::new(&data)).unwrap();
        let layout = decoder.prepare_image().unwrap();
        let mut buf = vec![0u8; layout.total_bytes() as usize];
        decoder.read_image(&mut buf).unwrap();

        // Every pixel should have alpha=128 (the native alpha from the BMP data).
        // If the AND mask were incorrectly applied, alpha would be 0.
        for (i, pixel) in buf.as_chunks::<4>().0.iter().enumerate() {
            assert_eq!(
                pixel[3], 128,
                "pixel {i}: expected alpha=128, got {}",
                pixel[3]
            );
        }
    }

    #[test]
    fn format_error_ico_strict_vs_lenient() {
        let data = std::fs::read("tests/images/ico/images/lenient-bpp.ico").unwrap();

        let mut decoder =
            IcoDecoder::with_spec_compliance(std::io::Cursor::new(&data), SpecCompliance::Lenient)
                .unwrap();
        let bytes = decoder.prepare_image().unwrap().total_bytes();
        let mut buf = vec![0; usize::try_from(bytes).unwrap()];
        assert!(decoder.read_image(&mut buf).is_ok());

        let decoder_strict =
            IcoDecoder::with_spec_compliance(std::io::Cursor::new(&data), SpecCompliance::Strict);
        assert!(decoder_strict.is_err());
    }

    #[test]
    fn best_entry_selection() {
        let data = vec![
            0x00, 0x00, 0x01, 0x00, 0x02, 0x00, // Entry 1: 16x16, 32 bpp
            16, 16, 0, 0, 1, 0, 32, 0, 8, 0, 0, 0, 38, 0, 0, 0,
            // Entry 2: 32x32, 0 bpp, 0 color_count
            32, 32, 0, 0, 1, 0, 0, 0, 8, 0, 0, 0, 46, 0, 0, 0,
        ];

        let mut r = std::io::Cursor::new(&data);
        let entries = read_entries(&mut r, SpecCompliance::Lenient).unwrap();

        assert_eq!(entries.len(), 2);
        assert_eq!(entries[0].width, 16);
        assert_eq!(entries[0].bits_per_pixel, 32);

        assert_eq!(entries[1].width, 32);
        assert_eq!(entries[1].bits_per_pixel, 8);

        let best = best_entry(entries).unwrap();
        assert_eq!(best.width, 32);
    }

    #[test]
    fn lenient_too_many_planes() {
        // The `planes` field (buf[4..6]) holds the horizontal hotspot coordinate for CUR files,
        // which may legitimately exceed 256. Mirroring the `bits_per_pixel` handling, the
        // `num_color_planes > 256` validation only applies in strict mode. (Sibling of the
        // `bits_per_pixel`/vertical-hotspot relaxation.)
        let data = vec![
            0x00, 0x00, 0x02, 0x00, 0x01, 0x00, // ICONDIR: type=2 (CUR), 1 entry
            // DIRENTRY: planes (buf[4..6]) = 0x0101 = 257 (>256)
            16, 16, 0, 0, 0x01, 0x01, 8, 0, 8, 0, 0, 0, 22, 0, 0, 0,
        ];

        // Lenient mode accepts the oversized value.
        let mut r = std::io::Cursor::new(&data);
        let entries = read_entries(&mut r, SpecCompliance::Lenient).unwrap();
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].num_color_planes, 257);

        // Strict mode still rejects it.
        let mut r = std::io::Cursor::new(&data);
        assert!(read_entries(&mut r, SpecCompliance::Strict).is_err());
    }
}
