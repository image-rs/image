use byteorder::{LittleEndian, ReadBytesExt};
use std::convert::TryFrom;
use std::io::{self, Cursor, Read, Seek, SeekFrom};
use std::marker::PhantomData;
use std::{error, fmt, mem};

use crate::color::ColorType;
use crate::error::{DecodingError, ImageError, ImageResult, UnsupportedError, UnsupportedErrorKind};
use crate::image::{self, ImageDecoder, ImageFormat};

use self::InnerDecoder::*;
use crate::bmp::BmpDecoder;
use crate::png::PngDecoder;

// http://www.w3.org/TR/PNG-Structure.html
// The first eight bytes of a PNG file always contain the following (decimal) values:
const PNG_SIGNATURE: [u8; 8] = [137, 80, 78, 71, 13, 10, 26, 10];

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
    /// The enclosed PNG is not in RGBA, which is invalid: https://blogs.msdn.microsoft.com/oldnewthing/20101022-00/?p=12473/.
    PngNotRgba,

    /// The optional mask row, containing 1 bit per pixel, padded to 4 bytes, was too short for this image.
    BmpIcoMaskTooShortForImage,

    /// The dimensions specified by the entry does not match the dimensions in the header of the enclosed image.
    ImageEntryDimensionMismatch {
        /// The mismatched subimage's type
        format: IcoEntryImageFormat,
        /// The dimensions specified by the entry
        entry: (u16, u16),
        /// The dimensions of the image itself
        image: (u32, u32)
    },
}

impl fmt::Display for DecoderError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DecoderError::NoEntries =>
                f.write_str("ICO directory contains no image"),
            DecoderError::IcoEntryTooManyPlanesOrHotspot =>
                f.write_str("ICO image entry has too many color planes or too large hotspot value"),
            DecoderError::IcoEntryTooManyBitsPerPixelOrHotspot =>
                f.write_str("ICO image entry has too many bits per pixel or too large hotspot value"),
            DecoderError::PngShorterThanHeader =>
                f.write_str("Entry specified a length that is shorter than PNG header!"),
            DecoderError::PngNotRgba =>
                f.write_str("The PNG is not in RGBA format!"),
            DecoderError::BmpIcoMaskTooShortForImage =>
                f.write_str("ICO mask too short for the image"),
            DecoderError::ImageEntryDimensionMismatch { format, entry, image } =>
                f.write_fmt(format_args!("Entry{:?} and {}{:?} dimensions do not match!", entry, format, image)),
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

impl Into<ImageFormat> for IcoEntryImageFormat {
    fn into(self) -> ImageFormat {
        match self {
            IcoEntryImageFormat::Png => ImageFormat::Png,
            IcoEntryImageFormat::Bmp => ImageFormat::Bmp,
        }
    }
}

/// An ico decoder
pub struct IcoDecoder<R: Read> {
    selected_entry: DirEntry,
    inner_decoder: InnerDecoder<R>,
}

enum InnerDecoder<R: Read> {
    BMP(BmpDecoder<R>),
    PNG(PngDecoder<R>),
}

#[derive(Clone, Copy, Default)]
struct DirEntry {
    width: u8,
    height: u8,
    color_count: u8,
    reserved: u8,

    num_color_planes: u16,
    bits_per_pixel: u16,

    image_length: u32,
    image_offset: u32,
}

impl<R: Read + Seek> IcoDecoder<R> {
    /// Create a new decoder that decodes from the stream ```r```
    pub fn new(mut r: R) -> ImageResult<IcoDecoder<R>> {
        let entries = read_entries(&mut r)?;
        let entry = best_entry(entries)?;
        let decoder = entry.decoder(r)?;

        Ok(IcoDecoder {
            selected_entry: entry,
            inner_decoder: decoder,
        })
    }
}

fn read_entries<R: Read>(r: &mut R) -> ImageResult<Vec<DirEntry>> {
    let _reserved = r.read_u16::<LittleEndian>()?;
    let _type = r.read_u16::<LittleEndian>()?;
    let count = r.read_u16::<LittleEndian>()?;
    (0..count).map(|_| read_entry(r)).collect()
}

fn read_entry<R: Read>(r: &mut R) -> ImageResult<DirEntry> {
    let mut entry = DirEntry::default();

    entry.width = r.read_u8()?;
    entry.height = r.read_u8()?;
    entry.color_count = r.read_u8()?;
    // Reserved value (not used)
    entry.reserved = r.read_u8()?;

    // This may be either the number of color planes (0 or 1), or the horizontal coordinate
    // of the hotspot for CUR files.
    entry.num_color_planes = r.read_u16::<LittleEndian>()?;
    if entry.num_color_planes > 256 {
        return Err(DecoderError::IcoEntryTooManyPlanesOrHotspot.into());
    }

    // This may be either the bit depth (may be 0 meaning unspecified),
    // or the vertical coordinate of the hotspot for CUR files.
    entry.bits_per_pixel = r.read_u16::<LittleEndian>()?;
    if entry.bits_per_pixel > 256 {
        return Err(DecoderError::IcoEntryTooManyBitsPerPixelOrHotspot.into());
    }

    entry.image_length = r.read_u32::<LittleEndian>()?;
    entry.image_offset = r.read_u32::<LittleEndian>()?;

    Ok(entry)
}

/// Find the entry with the highest (color depth, size).
fn best_entry(mut entries: Vec<DirEntry>) -> ImageResult<DirEntry> {
    let mut best = entries.pop().ok_or_else(|| DecoderError::NoEntries)?;

    let mut best_score = (
        best.bits_per_pixel,
        u32::from(best.real_width()) * u32::from(best.real_height()),
    );

    for entry in entries {
        let score = (
            entry.bits_per_pixel,
            u32::from(entry.real_width()) * u32::from(entry.real_height()),
        );
        if score > best_score {
            best = entry;
            best_score = score;
        }
    }
    Ok(best)
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
        u32::from(self.real_width()) == width && u32::from(self.real_height()) == height
    }

    fn seek_to_start<R: Read + Seek>(&self, r: &mut R) -> ImageResult<()> {
        r.seek(SeekFrom::Start(u64::from(self.image_offset)))?;
        Ok(())
    }

    fn is_png<R: Read + Seek>(&self, r: &mut R) -> ImageResult<bool> {
        self.seek_to_start(r)?;

        // Read the first 8 bytes to sniff the image.
        let mut signature = [0u8; 8];
        r.read_exact(&mut signature)?;

        Ok(signature == PNG_SIGNATURE)
    }

    fn decoder<R: Read + Seek>(&self, mut r: R) -> ImageResult<InnerDecoder<R>> {
        let is_png = self.is_png(&mut r)?;
        self.seek_to_start(&mut r)?;

        if is_png {
            Ok(PNG(PngDecoder::new(r)?))
        } else {
            Ok(BMP(BmpDecoder::new_with_ico_format(r)?))
        }
    }
}

/// Wrapper struct around a `Cursor<Vec<u8>>`
pub struct IcoReader<R>(Cursor<Vec<u8>>, PhantomData<R>);
impl<R> Read for IcoReader<R> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.0.read(buf)
    }
    fn read_to_end(&mut self, buf: &mut Vec<u8>) -> io::Result<usize> {
        if self.0.position() == 0 && buf.is_empty() {
            mem::swap(buf, self.0.get_mut());
            Ok(buf.len())
        } else {
            self.0.read_to_end(buf)
        }
    }
}

impl<'a, R: 'a + Read + Seek> ImageDecoder<'a> for IcoDecoder<R> {
    type Reader = IcoReader<R>;

    fn dimensions(&self) -> (u32, u32) {
        match self.inner_decoder {
            BMP(ref decoder) => decoder.dimensions(),
            PNG(ref decoder) => decoder.dimensions(),
        }
    }

    fn color_type(&self) -> ColorType {
        match self.inner_decoder {
            BMP(ref decoder) => decoder.color_type(),
            PNG(ref decoder) => decoder.color_type(),
        }
    }

    fn into_reader(self) -> ImageResult<Self::Reader> {
        Ok(IcoReader(Cursor::new(image::decoder_to_vec(self)?), PhantomData))
    }

    fn read_image(self, buf: &mut [u8]) -> ImageResult<()> {
        assert_eq!(u64::try_from(buf.len()), Ok(self.total_bytes()));
        match self.inner_decoder {
            PNG(decoder) => {
                if self.selected_entry.image_length < PNG_SIGNATURE.len() as u32 {
                    return Err(DecoderError::PngShorterThanHeader.into());
                }

                // Check if the image dimensions match the ones in the image data.
                let (width, height) = decoder.dimensions();
                if !self.selected_entry.matches_dimensions(width, height) {
                    return Err(DecoderError::ImageEntryDimensionMismatch {
                        format: IcoEntryImageFormat::Png,
                        entry: (self.selected_entry.real_width(), self.selected_entry.real_height()),
                        image: (width, height)
                    }.into());
                }

                // Embedded PNG images can only be of the 32BPP RGBA format.
                // https://blogs.msdn.microsoft.com/oldnewthing/20101022-00/?p=12473/
                if decoder.color_type() != ColorType::Rgba8 {
                    return Err(DecoderError::PngNotRgba.into());
                }

                decoder.read_image(buf)
            }
            BMP(mut decoder) => {
                let (width, height) = decoder.dimensions();
                if !self.selected_entry.matches_dimensions(width, height) {
                    return Err(DecoderError::ImageEntryDimensionMismatch {
                        format: IcoEntryImageFormat::Bmp,
                        entry: (self.selected_entry.real_width(), self.selected_entry.real_height()),
                        image: (width, height)
                    }.into());
                }

                // The ICO decoder needs an alpha channel to apply the AND mask.
                if decoder.color_type() != ColorType::Rgba8 {
                    return Err(ImageError::Unsupported(UnsupportedError::from_format_and_kind(
                        ImageFormat::Bmp.into(),
                        UnsupportedErrorKind::Color(decoder.color_type().into()),
                    )));
                }

                decoder.read_image_data(buf)?;

                // If there's an AND mask following the image, read and apply it.
                let r = decoder.reader();
                let mask_start = r.seek(SeekFrom::Current(0))?;
                let mask_end =
                    u64::from(self.selected_entry.image_offset + self.selected_entry.image_length);
                let mask_length = mask_end - mask_start;

                if mask_length > 0 {
                    // A mask row contains 1 bit per pixel, padded to 4 bytes.
                    let mask_row_bytes = ((width + 31) / 32) * 4;
                    let expected_length = u64::from(mask_row_bytes) * u64::from(height);
                    if mask_length < expected_length {
                        return Err(DecoderError::BmpIcoMaskTooShortForImage.into());
                    }

                    for y in 0..height {
                        let mut x = 0;
                        for _ in 0..mask_row_bytes {
                            // Apply the bits of each byte until we reach the end of the row.
                            let mask_byte = r.read_u8()?;
                            for bit in (0..8).rev() {
                                if x >= width {
                                    break;
                                }
                                if mask_byte & (1 << bit) != 0 {
                                    // Set alpha channel to transparent.
                                    buf[((height - y - 1) * width + x) as usize * 4 + 3] = 0;
                                }
                                x += 1;
                            }
                        }
                    }
                }
                Ok(())
            }
        }
    }
}
