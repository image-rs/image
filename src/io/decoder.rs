use crate::animation::Frames;
use crate::color::{ColorType, ExtendedColorType};
use crate::error::ImageResult;
use crate::metadata::{LoopCount, Orientation};

/// The interface for `image` to utilize in reading image files.
///
/// This should be thought of as one side of protocol between `image` and a specific file format.
/// In the general case, the calls are expected to be made in the following order:
///
/// ```text,bnf
/// set_limits*
/// > (peek_layout+ > read_image)*
/// > finish
/// ```
///
/// Metadata (`icc_profile`, `exif_metadata`, etc.)is handled different for different image
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
        let (width, height) = self.dimensions();
        limits.check_dimensions(width, height)?;
        Ok(())
    }

    /// Consume the header of the image, determining the image's layout.
    ///
    /// This must be called before a call to [`Self::read_image`] to ensure that the initial
    /// metadata has been read. In contrast to a constructor it can be called after configuring
    /// limits and context which avoids resource issues for formats that buffer metadata.
    ///
    /// The layout returned by an implementation of [`ImageDecoder::peek_layout`] must match the
    /// buffer expected in [`ImageDecoder::read_image`].
    fn peek_layout(&mut self) -> ImageResult<crate::ImageLayout> {
        let (width, height) = self.dimensions();

        Ok(crate::ImageLayout {
            color: self.color_type(),
            width,
            height,
        })
    }

    /// Returns a tuple containing the width and height of the image
    fn dimensions(&self) -> (u32, u32);

    /// Returns the color type of the image data produced by this decoder
    fn color_type(&self) -> ColorType;

    /// Returns the color type of the image file before decoding
    fn original_color_type(&self) -> ExtendedColorType {
        self.color_type().into()
    }

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
    /// This function should panic if `buf.len() != self.peek_layout().total_bytes()`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use image::ImageDecoder;
    /// fn read_16bit_image(mut decoder: impl ImageDecoder) -> Vec<u16> {
    ///     let layout = decoder.next_layout().unwrap();
    ///     let mut buf: Vec<u16> = vec![0; (layout.total_bytes() / 2) as usize];
    ///     decoder.read_image(bytemuck::cast_slice_mut(&mut buf)).unwrap();
    ///     buf
    /// }
    /// ```
    fn read_image(&mut self, buf: &mut [u8]) -> ImageResult<()>;

    /// Returns the ICC color profile embedded in the image, or `Ok(None)` if the image does not have one.
    ///
    /// For formats that don't support embedded profiles this function should always return `Ok(None)`.
    fn icc_profile(&mut self) -> ImageResult<Option<Vec<u8>>> {
        Ok(None)
    }

    /// Returns the raw [Exif](https://en.wikipedia.org/wiki/Exif) chunk, if it is present.
    /// A third-party crate such as [`kamadak-exif`](https://docs.rs/kamadak-exif/) is required to actually parse it.
    ///
    /// For formats that don't support embedded profiles this function should always return `Ok(None)`.
    fn exif_metadata(&mut self) -> ImageResult<Option<Vec<u8>>> {
        Ok(None)
    }

    /// Returns the raw [XMP](https://en.wikipedia.org/wiki/Extensible_Metadata_Platform) chunk, if it is present.
    /// A third-party crate such as [`roxmltree`](https://docs.rs/roxmltree/) is required to actually parse it.
    ///
    /// For formats that don't support embedded profiles this function should always return `Ok(None)`.
    fn xmp_metadata(&mut self) -> ImageResult<Option<Vec<u8>>> {
        Ok(None)
    }

    /// Returns the raw [IPTC](https://en.wikipedia.org/wiki/IPTC_Information_Interchange_Model) chunk, if it is present.
    ///
    /// For formats that don't support embedded profiles this function should always return `Ok(None)`.
    fn iptc_metadata(&mut self) -> ImageResult<Option<Vec<u8>>> {
        Ok(None)
    }

    /// Returns the orientation of the image.
    ///
    /// This is usually obtained from the Exif metadata, if present. Formats that don't support
    /// indicating orientation in their image metadata will return `Ok(Orientation::NoTransforms)`.
    fn orientation(&mut self) -> ImageResult<Orientation> {
        Ok(self
            .exif_metadata()?
            .and_then(|chunk| Orientation::from_exif_chunk(&chunk))
            .unwrap_or(Orientation::NoTransforms))
    }
}

#[deny(clippy::missing_trait_methods)]
impl<T: ?Sized + ImageDecoder> ImageDecoder for Box<T> {
    fn peek_layout(&mut self) -> ImageResult<crate::ImageLayout> {
        (**self).peek_layout()
    }
    fn dimensions(&self) -> (u32, u32) {
        (**self).dimensions()
    }
    fn color_type(&self) -> ColorType {
        (**self).color_type()
    }
    fn original_color_type(&self) -> ExtendedColorType {
        (**self).original_color_type()
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
    fn orientation(&mut self) -> ImageResult<Orientation> {
        (**self).orientation()
    }
    fn read_image(&mut self, buf: &mut [u8]) -> ImageResult<()> {
        (**self).read_image(buf)
    }
    fn set_limits(&mut self, limits: crate::Limits) -> ImageResult<()> {
        (**self).set_limits(limits)
    }
}

/// `AnimationDecoder` trait
pub trait AnimationDecoder<'a> {
    /// Consume the decoder producing a series of frames.
    fn into_frames(self) -> Frames<'a>;
    /// Loop count of the animated image.
    fn loop_count(&self) -> LoopCount;
}

#[cfg(test)]
mod tests {
    use super::{ColorType, ImageDecoder, ImageResult};

    #[test]
    fn total_bytes_overflow() {
        struct D;

        impl ImageDecoder for D {
            fn color_type(&self) -> ColorType {
                ColorType::Rgb8
            }
            fn dimensions(&self) -> (u32, u32) {
                (0xffff_ffff, 0xffff_ffff)
            }
            fn read_image(&mut self, _buf: &mut [u8]) -> ImageResult<()> {
                unreachable!("Must not be called in this test")
            }
        }

        assert_eq!(D.peek_layout().unwrap().total_bytes(), u64::MAX);
        let v: ImageResult<Vec<u8>> = crate::io::free_functions::decoder_to_vec(&mut D);
        assert!(v.is_err());
    }
}
