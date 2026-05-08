use crate::error::ImageResult;
use crate::io::DecoderPreparedImage;
use crate::metadata::{LoopCount, Orientation};
use crate::Delay;

/// The interface for `image` to utilize in reading image files.
///
/// Please carefully consider consuming this interface directly and prefer interaction with an
/// [`ImageReader`](crate::ImageReader). This is one directional of a protocol between `image` and format decoders. In
/// the general case, an implementation can expect calls to be made in the following order:
///
/// ```text,bnf
/// decoding sequence = configure, { decode image }, "finish", { metadata }
///
/// decode image =
///    "prepare_image", { metadata | "prepare_image" }, "read_image"
///
/// configure = "set_limits"
///
/// metadata = "xmp_metadata" | "icc_profile" | "exif_metadata" | "iptc_metadata"
/// ```
///
/// Deviation from this order can be treated as an error. Future changes to the protocol may
/// introduce additional methods. Decoders will indicate support for sequent variants in their
/// [`ImageDecoder::format_attributes`].
///
/// Metadata (`icc_profile`, `exif_metadata`, etc.) is handled different for different image
/// containers. The should apply to the previous image.
pub trait ImageDecoder {
    /// Set the decoder to have the specified limits. See [`Limits`] for the different kinds of
    /// limits that is possible to set.
    ///
    /// Note to implementors: make sure you call [`Limits::check_support`] so that
    /// decoding fails if any unsupported strict limits are set. Also make sure
    /// you call [`Limits::check_dimensions`] to check the `max_image_width` and
    /// `max_image_height` limits.
    ///
    /// **Note**: By default, _no_ limits are defined. This may be changed in future major version
    /// increases.
    ///
    /// [`Limits`]: ./io/struct.Limits.html
    /// [`Limits::check_support`]: ./io/struct.Limits.html#method.check_support
    /// [`Limits::check_dimensions`]: ./io/struct.Limits.html#method.check_dimensions
    fn set_limits(&mut self, limits: crate::Limits) -> ImageResult<()> {
        limits.check_support(&crate::LimitSupport::default())?;
        let layout = self.prepare_image()?;
        limits.check_layout_dimensions(&layout)?;
        Ok(())
    }

    /// Retrieve general information about the decoder / its format itself.
    ///
    /// This hints which methods should be called while decoding (a sequence of) images from this
    /// decoder, e.g. when metadata is available and when it will be overridden. It also provides
    /// basic capability information about the format. If, in the future, we added different basic
    /// methods of retrieving color data then the attributes would indicate the preferred and/or
    /// possible choices.
    fn format_attributes(&self) -> FormatAttributes {
        FormatAttributes::default()
    }

    /// Retrieve animation attributes.
    ///
    /// You should check [`FormatAttributes::supports_animation`] before calling this method. A
    /// value will only be available on animated images. Additionally, most file formats store the
    /// metadata in the header which might not be read until after calling
    /// [`ImageDecoder::prepare_image`].
    ///
    /// The value here is expected to remain constant when it is present.
    fn animation_attributes(&mut self) -> Option<DecodedAnimationAttributes> {
        None
    }

    /// Consume the header of the image, determining the (next) image's layout.
    ///
    /// This shall be called before a call to [`ImageDecoder::read_image`] to ensure that the
    /// initial metadata has been read. The returned layout indicates the expected buffer of
    /// [`ImageDecoder::read_image`]. The caller is responsible for passing a buffer of the
    /// appropriate size.
    ///
    /// This method should be idempotent on success, calling it multiple times in a row should
    /// produce equivalent results. The decoder must _not_ advance to another image descriptor when
    /// it is called when it has already reached one.
    ///
    /// In contrast to a constructor it can be called multiple times, even after reconfiguring
    /// limits and context which avoids resource issues for formats that buffer metadata.
    fn prepare_image(&mut self) -> ImageResult<DecoderPreparedImage>;

    /// Read all the bytes in the image into a buffer.
    ///
    /// This function takes a slice of bytes and writes the pixel data of the image into it.
    /// `buf` must not be assumed to be aligned to any byte boundaries. However,
    /// alignment to 2 or 4 byte boundaries may result in small performance
    /// improvements for certain decoder implementations.
    ///
    /// The returned pixel data will always be in native endian. This allows
    /// `[u16]` and `[f32]` slices to be cast to `[u8]` and used for this method.
    ///
    /// # Panics
    ///
    /// This function should panic if `buf.len() != self.prepare_image().total_bytes()`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use image::ImageDecoder;
    /// fn read_16bit_image(mut decoder: impl ImageDecoder) -> Vec<u16> {
    ///     let layout = decoder.prepare_image().unwrap();
    ///     let mut buf: Vec<u16> = vec![0; (layout.total_bytes() / 2) as usize];
    ///     decoder.read_image(bytemuck::cast_slice_mut(&mut buf)).unwrap();
    ///     buf
    /// }
    /// ```
    fn read_image(&mut self, buf: &mut [u8]) -> ImageResult<DecodedImageAttributes>;

    /// Returns the ICC color profile embedded in the image, or `Ok(None)` if the image does not have one.
    ///
    /// For formats that don't support embedded profiles this function should always return
    /// `Ok(None)`. Decoders for formats with non-standard color profiles may create a synthetic
    /// profile, see our [`bmp`](`crate::codecs::bmp`) module for an example.
    ///
    /// A decoder that encounters tags which contain a color profile whose encoding it does not
    /// support should return [`UnsupportedError`](`crate::error::UnsupportedError`). This allows a
    /// reader to continue while differentiating from missing metadata.
    fn icc_profile(&mut self) -> ImageResult<Option<Vec<u8>>> {
        Ok(None)
    }

    /// Returns the raw [Exif](https://en.wikipedia.org/wiki/Exif) chunk, if it is present.
    /// A third-party crate such as [`kamadak-exif`](https://docs.rs/kamadak-exif/) is required to actually parse it.
    ///
    /// For formats that don't support embedded profiles this function should always return `Ok(None)`.
    ///
    /// A decoder that encounters tags which contain XMP metadata whose encoding it does not
    /// support should return [`UnsupportedError`](crate::error::UnsupportedError). This allows a
    /// reader to continue while differentiating from missing metadata.
    fn exif_metadata(&mut self) -> ImageResult<Option<Vec<u8>>> {
        Ok(None)
    }

    /// Returns the raw [XMP](https://en.wikipedia.org/wiki/Extensible_Metadata_Platform) chunk, if it is present.
    /// A third-party crate such as [`roxmltree`](https://docs.rs/roxmltree/) is required to actually parse it.
    ///
    /// For formats that don't support embedded profiles this function should always return `Ok(None)`.
    ///
    /// A decoder that encounters tags which contain XMP metadata whose encoding it does not
    /// support should return [`UnsupportedError`](crate::error::UnsupportedError). This allows a
    /// reader to continue while differentiating from missing metadata.
    fn xmp_metadata(&mut self) -> ImageResult<Option<Vec<u8>>> {
        Ok(None)
    }

    /// Returns the raw [IPTC](https://en.wikipedia.org/wiki/IPTC_Information_Interchange_Model) chunk, if it is present.
    ///
    /// For formats that don't support embedded profiles this function should always return `Ok(None)`.
    ///
    /// A decoder that encounters tags which contain XMP metadata whose encoding it does not
    /// support should return [`UnsupportedError`](crate::error::UnsupportedError). This allows a
    /// reader to continue while differentiating from missing metadata.
    fn iptc_metadata(&mut self) -> ImageResult<Option<Vec<u8>>> {
        Ok(None)
    }

    /// Called to determine if there may be more images to decode.
    ///
    /// This ends the decoding loop early when it indicates `None`. Otherwise, termination can only
    /// be handled through errors. See also
    /// [`ImageReader::into_frames`](crate::ImageReader::into_frames).
    fn more_images(&self) -> SequenceControl {
        SequenceControl::MaybeMore
    }

    /// Consume the rest of the file, including any trailer.
    fn finish(&mut self) -> ImageResult<()> {
        Ok(())
    }
}

/// Information meant to steer the protocol usage with the decoder.
#[derive(Clone, Debug, Default)]
#[non_exhaustive]
pub struct FormatAttributes {
    /// Could there be multiple images in this file that form an animation?
    pub supports_animation: bool,
    /// Could there be multiple images in this file, as an unrelated sequence?
    pub supports_sequence: bool,
    /// When should ICC profiles be retrieved.
    pub icc: DecodedMetadataHint,
    /// A hint for polling EXIF metadata.
    pub exif: DecodedMetadataHint,
    /// A hint for polling XMP metadata.
    pub xmp: DecodedMetadataHint,
    /// A hint for polling IPTC metadata.
    pub iptc: DecodedMetadataHint,
}

/// Additional attributes of animated image sequences.
#[derive(Clone, Debug)]
#[non_exhaustive]
pub struct DecodedAnimationAttributes {
    /// Loop count of the animated image.
    pub loop_count: LoopCount,
}

impl Default for DecodedAnimationAttributes {
    fn default() -> Self {
        Self {
            loop_count: LoopCount::Infinite,
        }
    }
}

/// Additional attributes of an image available after decoding.
///
/// The [`Default`] is implemented and returns a value suitable for very basic images from formats
/// that contain only one raster graphic.
#[derive(Clone, Debug, Default)]
#[non_exhaustive]
pub struct DecodedImageAttributes {
    /// The x-coordinate of the top-left rectangle of the image relative to canvas indicated by the
    /// sequence of frames.
    pub x: u32,
    /// The y-coordinate of the top-left rectangle of the image relative to canvas indicated by the
    /// sequence of frames.
    pub y: u32,
    /// A suggested presentation offset relative to the previous image.
    pub delay: Option<Delay>,
    /// Orientation of the image, not relayed through EXIF metadata.
    pub orientation: Option<Orientation>,
    /// Is the underlying data converted into a different pixel format or color model?
    ///
    /// This field is currently an optional hint for encoding. The authoritative source for the
    /// memory size required for [`ImageDecoder::read_image`][`crate::ImageDecoder`] remains the
    /// [`color`][`crate::ImageLayout::color`] field, you do not need to time travel this
    /// information.
    pub original_color_type: Option<crate::ExtendedColorType>,
}

/// A hint when metadata corresponding to the image is decoded.
///
/// Note that while this is a hint, different variants give contradictory indication on when they
/// should be polled. When a metadatum is tagged as [`DecodedMetadataHint::PerImage`] it MUST be
/// polled after each image to ensure all are retrieved, iterating to the next image without
/// polling MAY reset and skip some metadata.
///
/// # Design consideration
///
/// Each variant describes a way to fetch accurate and complete metadata for an individual image in
/// a file. This offloads some responsibility to the decoder, streamed formats that may contain
/// parts after the file need to be peek forwards to aggregate all metadata and then seek
/// backwards. During design we had a sequence that relied on [`ImageDecoder::finish`] but that did
/// not allow the reader to ensure all data was present. This call would also be destructive with
/// regards to the other kind of metadata.
#[derive(Clone, Debug, Default)]
#[non_exhaustive]
pub enum DecodedMetadataHint {
    /// The decoder does not support this datum. It will return `None` but there is no guarantee
    /// that the datum is truly absent in the file.
    ///
    /// This is the default.
    #[default]
    Unsupported,
    /// Metadata is available in the header and will be valid after the first call to
    /// [`ImageDecoder::prepare_image`] and will remain valid for all subsequent images.
    InHeader,
    /// Metadata exists for each image in this file, it must be retrieved between peeking the
    /// layout and reading the image.
    PerImage,
    /// There's no metadata of this type, the decoder would return `None` or an error.
    None,
}

/// Indicate if there may be more images to decode.
///
/// More concrete indications may be added in the future.
#[non_exhaustive]
#[derive(Default)]
pub enum SequenceControl {
    /// The format can not certainly say if there are more images. The caller should try to decode
    /// more images until an error occurs (specifically
    /// [`ParameterErrorKind::NoMoreData`](crate::error::ParameterErrorKind::NoMoreData)).
    #[default]
    MaybeMore,
    /// The decoder is sure that no more images are present.
    ///
    /// Further attempts to decode images should not be made, but no strong guarantee is made about
    /// returning an error in these cases. In particular, further attempts may further read the
    /// image file and check for errors in trailing data.
    None,
}

#[deny(clippy::missing_trait_methods)]
impl<T: ?Sized + ImageDecoder> ImageDecoder for Box<T> {
    fn format_attributes(&self) -> FormatAttributes {
        (**self).format_attributes()
    }
    fn prepare_image(&mut self) -> ImageResult<DecoderPreparedImage> {
        (**self).prepare_image()
    }
    fn animation_attributes(&mut self) -> Option<DecodedAnimationAttributes> {
        (**self).animation_attributes()
    }
    fn icc_profile(&mut self) -> ImageResult<Option<Vec<u8>>> {
        (**self).icc_profile()
    }
    fn exif_metadata(&mut self) -> ImageResult<Option<Vec<u8>>> {
        (**self).exif_metadata()
    }
    fn xmp_metadata(&mut self) -> ImageResult<Option<Vec<u8>>> {
        (**self).xmp_metadata()
    }
    fn iptc_metadata(&mut self) -> ImageResult<Option<Vec<u8>>> {
        (**self).iptc_metadata()
    }
    fn read_image(&mut self, buf: &mut [u8]) -> ImageResult<DecodedImageAttributes> {
        (**self).read_image(buf)
    }
    fn set_limits(&mut self, limits: crate::Limits) -> ImageResult<()> {
        (**self).set_limits(limits)
    }
    fn more_images(&self) -> SequenceControl {
        (**self).more_images()
    }
    fn finish(&mut self) -> ImageResult<()> {
        (**self).finish()
    }
}

#[cfg(test)]
mod tests {
    use super::{DecodedImageAttributes, DecoderPreparedImage, ImageDecoder, ImageResult};
    use crate::ColorType;

    #[test]
    fn total_bytes_overflow() {
        struct D;

        impl ImageDecoder for D {
            fn prepare_image(&mut self) -> ImageResult<DecoderPreparedImage> {
                Ok(DecoderPreparedImage::new(
                    0xffff_ffff,
                    0xffff_ffff,
                    ColorType::Rgb8,
                ))
            }

            fn read_image(&mut self, _buf: &mut [u8]) -> ImageResult<DecodedImageAttributes> {
                unreachable!("Must not be called in this test")
            }
        }

        assert_eq!(D.prepare_image().unwrap().total_bytes(), u64::MAX);
        let v = crate::DynamicImage::from_decoder(D);
        assert!(v.is_err());
    }
}
