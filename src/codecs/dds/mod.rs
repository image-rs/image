//!  Decoding of DDS images
//!
//!  DDS (DirectDraw Surface) is a container format for storing DXT (S3TC) compressed images.
//!
//!  # Related Links
//!  * <https://docs.microsoft.com/en-us/windows/win32/direct3ddds/dx-graphics-dds-pguide> - Description of the DDS format.

mod bc;
mod convert;
mod decoder;
mod error;
mod format;
mod header;

use std::io::{BufRead, Read};

use crate::color::ColorType;
use crate::error::{ImageError, ImageResult, UnsupportedError, UnsupportedErrorKind};
use crate::image::{ImageDecoder, ImageFormat};

use decoder::DX10Decoder;
use error::DecoderError;
use format::detect_format;
use header::{BitFlags, Caps, Caps2, Flags, Header};

/// The representation of a DDS decoder
pub struct DdsDecoder<R> {
    inner: DX10Decoder<R>,
}

impl<R> DdsDecoder<R> {
    /// Create a new decoder that decodes from the stream `r`
    pub fn new(mut r: R) -> ImageResult<Self>
    where
        R: BufRead,
    {
        let mut magic = [0; 4];
        r.read_exact(&mut magic)?;
        if magic != b"DDS "[..] {
            return Err(DecoderError::DdsSignatureInvalid.into());
        }

        let header = Header::from_reader(&mut r)?;

        // Don't allow empty images. Decoders should be allowed to assume that
        // the image has at least one pixel.
        if header.width == 0 || header.height == 0 {
            return Err(ImageError::Unsupported(
                UnsupportedError::from_format_and_kind(
                    ImageFormat::Dds.into(),
                    UnsupportedErrorKind::GenericFeature("Empty images are not supported".into()),
                ),
            ));
        }

        // Don't allow images that are too large.
        // Doing this check here prevents overflows later on.
        const MAX_SIZE: u32 = 1 << 24;
        if header.width > MAX_SIZE || header.height > MAX_SIZE {
            return Err(ImageError::Unsupported(
                UnsupportedError::from_format_and_kind(
                    ImageFormat::Dds.into(),
                    UnsupportedErrorKind::GenericFeature(format!(
                        "Image dimensions ({}x{}) are larger than the maximum allowed size",
                        header.width, header.height
                    )),
                ),
            ));
        }

        if header.caps2.has_bits(Caps2::VOLUME) {
            return Err(ImageError::Unsupported(
                UnsupportedError::from_format_and_kind(
                    ImageFormat::Dds.into(),
                    UnsupportedErrorKind::GenericFeature(
                        "Volume textures are not supported".into(),
                    ),
                ),
            ));
        }

        let format = detect_format(&header);

        if let Some(format) = format {
            let cube = header.caps2.has_bits(Caps2::CUBEMAP)
                && header.caps2.has_bits(Caps2::CUBEMAP_ALL_FACES);
            let mip_count = if (header.flags.has_bits(Flags::MIPMAP_COUNT)
                || header.caps.has_bits(Caps::COMPLEX)
                || header.caps.has_bits(Caps::MIPMAP))
                && header.mipmap_count >= 1
            {
                header.mipmap_count
            } else {
                1
            };

            let decoder = DX10Decoder {
                width: header.width,
                height: header.height,
                format,
                mip_count,
                cube,
                inner: r,
            };
            let (width, height) = decoder.dimensions();

            if crate::utils::check_dimension_overflow(
                width,
                height,
                format.color_type().bytes_per_pixel(),
            ) {
                return Err(ImageError::Unsupported(
                    UnsupportedError::from_format_and_kind(
                        ImageFormat::Dds.into(),
                        UnsupportedErrorKind::GenericFeature(format!(
                            "Image dimensions ({}x{}) are too large",
                            width, height
                        )),
                    ),
                ));
            }

            return Ok(Self { inner: decoder });
        }

        // The DDS file is not supported.
        // Try to give a good error message
        let details = if let Some(dx10_header) = header.dx10 {
            format!("DDS DXGI Format {:?}", dx10_header.dxgi_format)
        } else if let Some(four_cc) = header.four_cc() {
            format!("DDS FourCC {:?}", four_cc)
        } else {
            format!("DDS PixelFormat {:?}", header.pixel_format)
        };

        Err(ImageError::Unsupported(
            UnsupportedError::from_format_and_kind(
                ImageFormat::Dds.into(),
                UnsupportedErrorKind::GenericFeature(details),
            ),
        ))
    }
}

impl<R: Read> ImageDecoder for DdsDecoder<R> {
    fn dimensions(&self) -> (u32, u32) {
        self.inner.dimensions()
    }

    fn color_type(&self) -> ColorType {
        self.inner.color_type()
    }

    fn read_image(self, buf: &mut [u8]) -> ImageResult<()> {
        self.inner.read_image(buf)
    }

    fn read_image_boxed(self: Box<Self>, buf: &mut [u8]) -> ImageResult<()> {
        (*self).read_image(buf)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn dimension_overflow() {
        // A DXT1 header set to 0xFFFF_FFFC width and height (the highest u32%4 == 0)
        let header = [
            0x44, 0x44, 0x53, 0x20, 0x7C, 0x0, 0x0, 0x0, 0x7, 0x10, 0x8, 0x0, 0xFC, 0xFF, 0xFF,
            0xFF, 0xFC, 0xFF, 0xFF, 0xFF, 0x0, 0xC0, 0x12, 0x0, 0x0, 0x0, 0x0, 0x0, 0x1, 0x0, 0x0,
            0x0, 0x49, 0x4D, 0x41, 0x47, 0x45, 0x4D, 0x41, 0x47, 0x49, 0x43, 0x4B, 0x0, 0x0, 0x0,
            0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
            0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x20, 0x0, 0x0, 0x0,
            0x4, 0x0, 0x0, 0x0, 0x44, 0x58, 0x54, 0x31, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
            0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x10, 0x0, 0x0, 0x0,
            0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
        ];

        assert!(DdsDecoder::new(&header[..]).is_err());
    }
}
