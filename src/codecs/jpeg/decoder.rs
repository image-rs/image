use std::io::{BufRead, Seek};
use std::marker::PhantomData;

use crate::color::ColorType;
use crate::error::{
    DecodingError, ImageError, ImageResult, LimitError, UnsupportedError, UnsupportedErrorKind,
};
use crate::io::decoder::DecodedMetadataHint;
use crate::io::image_reader_type::SpecCompliance;
use crate::io::{DecodedImageAttributes, DecoderPreparedImage, FormatAttributes};
use crate::{ImageDecoder, ImageFormat, Limits};

type ZuneColorSpace = zune_core::colorspace::ColorSpace;

/// JPEG decoder
pub struct JpegDecoder<R> {
    decoder: zune_jpeg::JpegDecoder<R>,
    spec_compliance: SpecCompliance,
    limits: Limits,
    header: Option<HeaderData>,
    // For API compatibility with the previous jpeg_decoder wrapper.
    // Can be removed later, which would be an API break.
    phantom: PhantomData<R>,
}

struct HeaderData {
    orig_color_space: ZuneColorSpace,
    width: u16,
    height: u16,
}

impl<R: BufRead + Seek> JpegDecoder<R> {
    /// Create a new decoder that decodes from the stream ```r```
    pub fn new(r: R) -> JpegDecoder<R> {
        let options = zune_core::options::DecoderOptions::default()
            .set_strict_mode(false)
            .set_max_width(usize::MAX)
            .set_max_height(usize::MAX);

        let decoder = zune_jpeg::JpegDecoder::new_with_options(r, options);
        // Limits are disabled by default in the constructor for all decoders
        let limits = Limits::no_limits();

        JpegDecoder {
            decoder,
            header: None,
            spec_compliance: SpecCompliance::default(),
            limits,
            phantom: PhantomData,
        }
    }

    /// Create a new decoder with the given spec compliance mode.
    pub(crate) fn new_with_spec_compliance(r: R, spec: SpecCompliance) -> JpegDecoder<R> {
        let mut this = Self::new(r);

        this.spec_compliance = spec;
        this.decoder.set_options({
            this.decoder
                .options()
                .set_strict_mode(matches!(spec, SpecCompliance::Strict))
        });

        this
    }

    fn ensure_headers(&mut self) -> ImageResult<(&mut zune_jpeg::JpegDecoder<R>, &HeaderData)> {
        if self.header.is_none() {
            // Adjust ensure_headers if we do not run this in the constructor!
            self.decoder
                .decode_headers()
                .map_err(ImageError::from_jpeg)?;

            // now that we've decoded the headers we can `.unwrap()`
            // all these functions that only fail if called before decoding the headers
            let (width, height) = self.decoder.dimensions().unwrap();
            // JPEG can only express dimensions up to 65535x65535, so this conversion cannot fail
            let width: u16 = width.try_into().unwrap();
            let height: u16 = height.try_into().unwrap();

            let orig_color_space = self
                .decoder
                .input_colorspace()
                .expect("headers were decoded");

            // configure the decoder color output based on the header information. By default it
            // would do its own color space defaults and we want to setup those that are compatible
            // with our wish of sRGB outputs.
            self.decoder.set_options({
                let requested_color = match orig_color_space {
                    ZuneColorSpace::RGB
                    | ZuneColorSpace::RGBA
                    | ZuneColorSpace::Luma
                    | ZuneColorSpace::LumaA => orig_color_space,
                    // Late failure
                    _ => ZuneColorSpace::RGB,
                };

                self.decoder
                    .options()
                    .jpeg_set_out_colorspace(requested_color)
            });

            self.header = Some(HeaderData {
                orig_color_space,
                width,
                height,
            });
        }

        // `unwrap` instead of match is used here due to lifetime overlaps in the codeflow
        // otherwise, we could not quick return anyways.
        let header = self.header.as_ref().unwrap();
        // Headers are already decoded in `new()` right now, so this is a no-op.
        Ok((&mut self.decoder, header))
    }
}

impl<R: BufRead + Seek> ImageDecoder for JpegDecoder<R> {
    fn format_attributes(&self) -> FormatAttributes {
        FormatAttributes {
            // As per specification, once we start with MCUs we can only have restarts. Also all
            // our methods currently seek of their own accord anyways, it's just important to
            // uphold this if we do not buffer the whole file.
            icc: DecodedMetadataHint::InHeader,
            exif: DecodedMetadataHint::InHeader,
            xmp: DecodedMetadataHint::InHeader,
            iptc: DecodedMetadataHint::InHeader,
            ..FormatAttributes::default()
        }
    }

    fn prepare_image(&mut self) -> ImageResult<DecoderPreparedImage> {
        let (_, header) = self.ensure_headers()?;
        Ok(DecoderPreparedImage::new(
            header.width.into(),
            header.height.into(),
            ColorType::from_jpeg(header.orig_color_space),
        ))
    }

    fn icc_profile(&mut self) -> ImageResult<Option<Vec<u8>>> {
        let (decoder, _) = self.ensure_headers()?;
        Ok(decoder.icc_profile())
    }

    fn exif_metadata(&mut self) -> ImageResult<Option<Vec<u8>>> {
        let (decoder, _) = self.ensure_headers()?;
        Ok(decoder.exif().cloned())
    }

    fn xmp_metadata(&mut self) -> ImageResult<Option<Vec<u8>>> {
        let (decoder, _) = self.ensure_headers()?;
        Ok(decoder.xmp().cloned())
    }

    fn iptc_metadata(&mut self) -> ImageResult<Option<Vec<u8>>> {
        let (decoder, _) = self.ensure_headers()?;
        Ok(decoder.iptc().cloned())
    }

    fn read_image(&mut self, buf: &mut [u8]) -> ImageResult<DecodedImageAttributes> {
        let layout = self.prepare_image()?;

        let advertised_len = layout.total_bytes();
        let actual_len = buf.len() as u64;

        if actual_len != advertised_len {
            return Err(ImageError::Decoding(DecodingError::new(
                ImageFormat::Jpeg.into(),
                format!(
                    "Length of the decoded data {actual_len} \
                    doesn't match the advertised dimensions of the image \
                    that imply length {advertised_len}"
                ),
            )));
        }

        let (decoder, _) = self.ensure_headers()?;
        decoder.decode_into(buf).map_err(|err| {
            ImageError::Decoding(DecodingError::new(ImageFormat::Jpeg.into(), err))
        })?;

        Ok(DecodedImageAttributes {
            ..DecodedImageAttributes::default()
        })
    }

    fn set_limits(&mut self, limits: Limits) -> ImageResult<()> {
        limits.check_support(&crate::LimitSupport::default())?;
        let layout = self.prepare_image()?;
        limits.check_layout_dimensions(&layout)?;
        self.limits = limits;
        Ok(())
    }
}

impl ColorType {
    fn from_jpeg(colorspace: ZuneColorSpace) -> ColorType {
        let colorspace = to_supported_color_space(colorspace);
        use zune_core::colorspace::ColorSpace::*;
        match colorspace {
            // As of zune-jpeg 0.3.13 the output is always 8-bit,
            // but support for 16-bit JPEG might be added in the future.
            RGB => ColorType::Rgb8,
            RGBA => ColorType::Rgba8,
            Luma => ColorType::L8,
            LumaA => ColorType::La8,
            // to_supported_color_space() doesn't return any of the other variants
            _ => unreachable!(),
        }
    }
}

fn to_supported_color_space(orig: ZuneColorSpace) -> ZuneColorSpace {
    use zune_core::colorspace::ColorSpace::*;
    match orig {
        RGB | RGBA | Luma | LumaA => orig,
        // the rest is not supported by `image` so it will be converted to RGB during decoding
        _ => RGB,
    }
}

impl ImageError {
    fn from_jpeg(err: zune_jpeg::errors::DecodeErrors) -> ImageError {
        use zune_jpeg::errors::DecodeErrors::*;
        match err {
            Unsupported(desc) => ImageError::Unsupported(UnsupportedError::from_format_and_kind(
                ImageFormat::Jpeg.into(),
                UnsupportedErrorKind::GenericFeature(format!("{desc:?}")),
            )),
            LargeDimensions(_) => ImageError::Limits(LimitError::from_kind(
                crate::error::LimitErrorKind::DimensionError,
            )),
            err => ImageError::Decoding(DecodingError::new(ImageFormat::Jpeg.into(), err)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::{fs, io::Cursor};

    #[test]
    fn test_exif_orientation() {
        let data = fs::read("tests/images/jpg/portrait_2.jpg").unwrap();
        let decoder = JpegDecoder::new(Cursor::new(data));

        let mut image = crate::DynamicImage::new_luma8(0, 0);
        let mut reader = crate::ImageReader::from_decoder(Box::new(decoder));
        let meta = reader.decode_to_dynimage(&mut image).unwrap();

        assert_eq!(
            meta.attributes().orientation.unwrap(),
            crate::metadata::Orientation::FlipHorizontal
        );
    }

    #[test]
    fn test_strict_vs_lenient_spec_compliance() {
        let mut image = fs::read("tests/images/jpg/progressive/cat.jpg").unwrap();
        image.truncate(image.len() - 1000); // simulate a truncated image

        // Default (lenient) mode: truncated image should be accepted
        let mut decoder = JpegDecoder::new(Cursor::new(&image));
        let layout = decoder.prepare_image().unwrap();
        let mut buffer = vec![0u8; layout.total_bytes() as usize];
        assert!(decoder.read_image(&mut buffer).is_ok());

        // Strict mode: truncated image should be rejected
        let mut decoder =
            JpegDecoder::new_with_spec_compliance(Cursor::new(&image), SpecCompliance::Strict);
        let layout = decoder.prepare_image().unwrap();
        let mut buffer = vec![0u8; layout.total_bytes() as usize];
        assert!(decoder.read_image(&mut buffer).is_err());
    }
}
