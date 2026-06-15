use byteorder_lite::{LittleEndian, WriteBytesExt};
use std::borrow::Cow;
use std::io::{self, Write};

use crate::codecs::png::PngEncoder;
use crate::error::{
    EncodingError, ImageError, ImageResult, ParameterError, ParameterErrorKind, UnsupportedError,
};
use crate::{ExtendedColorType, ImageEncoder, ImageFormat};

// Enum value indicating an ICO image (as opposed to a CUR image):
const ICO_IMAGE_TYPE: u16 = 1;
// The length of an ICO file ICONDIR structure, in bytes:
const ICO_ICONDIR_SIZE: u32 = 6;
// The length of an ICO file DIRENTRY structure, in bytes:
const ICO_DIRENTRY_SIZE: u32 = 16;
// The length of a BITMAPINFOHEADER structure, in bytes:
const BITMAPINFOHEADER_SIZE: u32 = 40;

/// ICO encoder
pub struct IcoEncoder<W: Write> {
    w: W,
}

/// An ICO image entry
pub struct IcoFrame<'a> {
    // Pre-encoded PNG or BMP
    encoded_image: Cow<'a, [u8]>,
    // Stored as `0 => 256, n => n`
    width: u8,
    // Stored as `0 => 256, n => n`
    height: u8,
    color_type: ExtendedColorType,
}

impl<'a> IcoFrame<'a> {
    /// Construct a new `IcoFrame` using a pre-encoded PNG or BMP
    ///
    /// The `width` and `height` must be between 1 and 256 (inclusive).
    pub fn with_encoded(
        encoded_image: impl Into<Cow<'a, [u8]>>,
        width: u32,
        height: u32,
        color_type: ExtendedColorType,
    ) -> ImageResult<Self> {
        let encoded_image = encoded_image.into();

        if !(1..=256).contains(&width) {
            return Err(ImageError::Parameter(ParameterError::from_kind(
                ParameterErrorKind::Generic(format!(
                    "the image width must be `1..=256`, instead width {width} was provided",
                )),
            )));
        }

        if !(1..=256).contains(&height) {
            return Err(ImageError::Parameter(ParameterError::from_kind(
                ParameterErrorKind::Generic(format!(
                    "the image height must be `1..=256`, instead height {height} was provided",
                )),
            )));
        }

        Ok(Self {
            encoded_image,
            width: width as u8,
            height: height as u8,
            color_type,
        })
    }

    /// Construct a new `IcoFrame` by encoding `buf` as a PNG
    ///
    /// The `width` and `height` must be between 1 and 256 (inclusive)
    pub fn as_png(
        buf: &[u8],
        width: u32,
        height: u32,
        color_type: ExtendedColorType,
    ) -> ImageResult<Self> {
        let mut image_data: Vec<u8> = Vec::new();
        PngEncoder::new(&mut image_data).write_image(buf, width, height, color_type)?;

        let frame = Self::with_encoded(image_data, width, height, color_type)?;
        Ok(frame)
    }

    /// Construct a new `IcoFrame` by encoding `buf` as a BMP
    ///
    /// The `width` and `height` must be between 1 and 256 (inclusive)
    pub fn as_bmp(
        buf: &[u8],
        width: u32,
        height: u32,
        color_type: ExtendedColorType,
    ) -> ImageResult<Self> {
        if width == 0 || height == 0 {
            return Err(ImageError::Parameter(ParameterError::from_kind(
                ParameterErrorKind::Generic(format!(
                    "the image width and height must be non-zero, but width {width} and height {height} were provided",
                )),
            )));
        }
        // same soft limit as BMP decoder
        if width > u16::MAX as u32 || height > u16::MAX as u32 {
            return Err(ImageError::Parameter(ParameterError::from_kind(
                ParameterErrorKind::Generic("Dimensions too big.".into()),
            )));
        }

        let mut image_data: Vec<u8> = Vec::new();

        // https://learn.microsoft.com/en-us/previous-versions/ms997538(v=msdn.10)?redirectedfrom=MSDN
        // > Only the following members are used: biSize, biWidth, biHeight, biPlanes, biBitCount,
        // > biSizeImage. All other members must be 0.
        // https://learn.microsoft.com/en-us/windows/win32/api/wingdi/ns-wingdi-bitmapinfoheader
        image_data.write_u32::<LittleEndian>(BITMAPINFOHEADER_SIZE)?; // biSize
        image_data.write_i32::<LittleEndian>(width as i32)?; // biWidth
        image_data.write_i32::<LittleEndian>(height as i32 * 2)?; // biHeight
        image_data.write_u16::<LittleEndian>(1)?; // biPlanes must be 1
        image_data.write_u16::<LittleEndian>(32)?; // biBitCount (we only support 32-bit for now)
        image_data.write_u32::<LittleEndian>(0)?; // biCompression must be 0 (BI_RGB) for uncompressed
        image_data.write_u32::<LittleEndian>(0)?; // biSizeImage
        image_data.extend_from_slice(&[0; 16]); // remaining unused fields
        debug_assert_eq!(image_data.len(), BITMAPINFOHEADER_SIZE as usize);

        match color_type {
            ExtendedColorType::Rgba8 => {
                let pixels = buf.as_chunks::<4>().0;
                // BMP format requires the alpha channel to be stored as BGRA, so we need to convert it from RGBA.
                for row in pixels.chunks_exact(width as usize).rev() {
                    for [r, g, b, a] in row {
                        image_data.extend_from_slice(&[*b, *g, *r, *a]);
                    }
                }

                // AND mask
                // For 32-bit BMP the AND mask is typically ignored, but it should be provided nonetheless.
                // We use a threshold of 128 for the alpha channel to determine whether a pixel is transparent or opaque in the AND mask.
                for row in pixels.chunks_exact(width as usize).rev() {
                    let mut mask_byte: u8 = 0;
                    for (x, a) in row.iter().map(|pixel| pixel[3]).enumerate() {
                        let pos = x % 8;
                        let bit_pos = 7 - pos;
                        mask_byte |= if a < 128 { 1 } else { 0 } << bit_pos;

                        if pos == 7 {
                            image_data.push(mask_byte);
                            mask_byte = 0;
                        }
                    }
                    if !width.is_multiple_of(8) {
                        image_data.push(mask_byte);
                    }

                    // Each row of the AND mask must be padded to a multiple of 4 bytes.
                    let mask_row_len = width.div_ceil(8);
                    let mask_row_padded = mask_row_len.div_ceil(4) * 4;
                    let padding = &[0_u8; 4][..(mask_row_padded - mask_row_len) as usize];
                    image_data.extend_from_slice(padding);
                }
            }
            ExtendedColorType::Rgb8 => {
                let pixels = buf.as_chunks::<3>().0;
                // ICO doesn't allow 24-bit RGB, so we have to use the 0RGB 32-bit format.
                // This is the same as the 32-bit RGBA format but with the alpha channel set to 0 for all pixels.
                for row in pixels.chunks_exact(width as usize).rev() {
                    for [r, g, b] in row {
                        image_data.extend_from_slice(&[*b, *g, *r, 0]);
                    }
                }

                // AND mask
                // Since there is no transparency, we can set all pixels to 0 (opaque) in the AND mask.
                for _ in 0..height {
                    let chunks_per_row = width.div_ceil(32);
                    for _ in 0..chunks_per_row {
                        image_data.extend_from_slice(&[0_u8; 4]);
                    }
                }
            }
            _ => {
                return Err(ImageError::Unsupported(
                    UnsupportedError::from_format_and_kind(
                        ImageFormat::Ico.into(),
                        crate::error::UnsupportedErrorKind::Color(color_type),
                    ),
                ));
            }
        }

        // Set biSizeImage properly
        // This isn't strictly necessary since 0 is allowed for uncompressed images (which ICO requires),
        // but we set it nonetheless of compatibility.
        let size = image_data.len() as u32 - BITMAPINFOHEADER_SIZE;
        image_data[20..24].copy_from_slice(&size.to_le_bytes());

        // color type has to be Rgba8, because the `bit_per_pixel` field in the ICO directory entry has to be set to 32.
        let frame = Self::with_encoded(image_data, width, height, ExtendedColorType::Rgba8)?;
        Ok(frame)
    }
}

impl<W: Write> IcoEncoder<W> {
    /// Create a new encoder that writes its output to ```w```.
    pub fn new(w: W) -> IcoEncoder<W> {
        IcoEncoder { w }
    }

    /// Takes some [`IcoFrame`]s and encodes them into an ICO.
    ///
    /// `images` is a list of images, usually ordered by dimension, which
    /// must be between 1 and 65535 (inclusive) in length.
    pub fn encode_images(mut self, images: &[IcoFrame<'_>]) -> ImageResult<()> {
        if !(1..=usize::from(u16::MAX)).contains(&images.len()) {
            return Err(ImageError::Parameter(ParameterError::from_kind(
                ParameterErrorKind::Generic(format!(
                    "the number of images must be `1..=u16::MAX`, instead {} images were provided",
                    images.len(),
                )),
            )));
        }

        write_icondir(&mut self.w, images.len() as u16)?;

        let mut offset = ICO_ICONDIR_SIZE + ICO_DIRENTRY_SIZE * images.len() as u32;
        for (i, image) in images.iter().enumerate() {
            let Ok(data_size) = u32::try_from(image.encoded_image.len()) else {
                return Err(ImageError::Encoding(EncodingError::new(
                    ImageFormat::Ico.into(),
                    "the encoded image data must be at most 4 GiB",
                )));
            };

            write_direntry(
                &mut self.w,
                image.width,
                image.height,
                image.color_type,
                offset,
                data_size,
            )?;

            // The offset is always calculated for the next frame. So we want
            // to skip it on the last frame since there is no next frame.
            // This has the effect of allowing the last frame's content to go
            // beyond the 4 GiB in the underlying writer.
            if i == images.len() - 1 {
                break;
            }

            offset = offset.checked_add(data_size).ok_or_else(|| {
                ImageError::Encoding(EncodingError::new(
                    ImageFormat::Ico.into(),
                    "the total size of the ICO file must be at most 4 GiB",
                ))
            })?;
        }
        for image in images {
            self.w.write_all(&image.encoded_image)?;
        }
        Ok(())
    }
}

impl<W: Write> ImageEncoder for IcoEncoder<W> {
    /// Write an ICO image with the specified width, height, and color type.
    ///
    /// For color types with 16-bit per channel or larger, the contents of `buf` should be in
    /// native endian.
    ///
    /// WARNING: In image 0.23.14 and earlier this method erroneously expected buf to be in big endian.
    #[track_caller]
    fn write_image(
        self,
        buf: &[u8],
        width: u32,
        height: u32,
        color_type: ExtendedColorType,
    ) -> ImageResult<()> {
        let expected_buffer_len = color_type.buffer_size(width, height);
        assert_eq!(
            expected_buffer_len,
            buf.len() as u64,
            "Invalid buffer length: expected {expected_buffer_len} got {} for {width}x{height} image",
            buf.len(),
        );

        let image = IcoFrame::as_png(buf, width, height, color_type)?;
        self.encode_images(&[image])
    }
}

fn write_icondir<W: Write>(w: &mut W, num_images: u16) -> io::Result<()> {
    // Reserved field (must be zero):
    w.write_u16::<LittleEndian>(0)?;
    // Image type (ICO or CUR):
    w.write_u16::<LittleEndian>(ICO_IMAGE_TYPE)?;
    // Number of images in the file:
    w.write_u16::<LittleEndian>(num_images)?;
    Ok(())
}

fn write_direntry<W: Write>(
    w: &mut W,
    width: u8,
    height: u8,
    color: ExtendedColorType,
    data_start: u32,
    data_size: u32,
) -> io::Result<()> {
    // Image dimensions:
    w.write_u8(width)?;
    w.write_u8(height)?;
    // Number of colors in palette (or zero for no palette):
    w.write_u8(0)?;
    // Reserved field (must be zero):
    w.write_u8(0)?;
    // Color planes:
    w.write_u16::<LittleEndian>(0)?;
    // Bits per pixel:
    w.write_u16::<LittleEndian>(color.bits_per_pixel())?;
    // Image data size, in bytes:
    w.write_u32::<LittleEndian>(data_size)?;
    // Image data offset, in bytes:
    w.write_u32::<LittleEndian>(data_start)?;
    Ok(())
}

#[cfg(test)]
mod test {
    use crate::{DynamicImage, ImageBuffer, PixelWithColorType, Rgb, Rgba, RgbaImage};

    use super::*;

    // Test that the encoder allows image where all frames have offsets < 4GiB
    // (even if the total file size might be larger than 4 GiB), but disallows
    // image where any frame has an offset >= 4 GiB.
    #[test]
    fn ico_larger_than_4_gib() {
        // Allocate a 1 MiB ""image"" and make 4096 frames with it.
        // The last frame will peek beyond the 4 GiB mark, since the header also takes a bit of memory.
        let data = vec![0; 1024 * 1024];
        let create_frame =
            || IcoFrame::with_encoded(data.as_slice(), 256, 256, ExtendedColorType::Rgba8).unwrap();

        let mut frames: Vec<IcoFrame> = (0..4096).map(|_| create_frame()).collect();

        let encoder = IcoEncoder::new(io::sink());
        let res = encoder.encode_images(&frames);
        assert!(res.is_ok());

        // adding just one more frame will cause the offset of the last frame to go beyond 4 GiB, which should cause an error.
        frames.push(create_frame());
        let encoder = IcoEncoder::new(io::sink());
        let res = encoder.encode_images(&frames);
        assert!(res.is_err());
    }

    #[test]
    fn roundtrip() {
        fn encode_bmp_decode<P: PixelWithColorType<Subpixel = u8>>(
            image: &ImageBuffer<P, Vec<P::Subpixel>>,
        ) -> DynamicImage {
            let frame = IcoFrame::as_bmp(
                image.subpixels(),
                image.width(),
                image.height(),
                P::COLOR_TYPE,
            )
            .unwrap();

            let mut encoded_data: Vec<u8> = Vec::new();
            IcoEncoder::new(&mut encoded_data)
                .encode_images(&[frame])
                .unwrap();

            let mut reader = crate::ImageReaderOptions::with_format(
                io::Cursor::new(encoded_data),
                ImageFormat::Ico,
            );
            reader.set_spec_compliance(crate::SpecCompliance::Strict);
            reader.decode().unwrap()
        }

        let rgba = RgbaImage::from_fn(32, 32, |x, y| {
            Rgba([
                x as u8 * 8,
                y as u8 * 8,
                0,
                if y < 24 {
                    255
                } else {
                    (x + y * 32 - 256 * 3) as u8
                },
            ])
        });
        let rgb = rgba.convert();
        let round_rgba = encode_bmp_decode(&rgba);
        let round_rgb = encode_bmp_decode::<Rgb<u8>>(&rgb);
        assert_eq!(round_rgba.into_rgba8(), rgba);
        assert_eq!(round_rgb.into_rgba8(), rgb.convert());
    }
}
