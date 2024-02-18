use std::convert::TryFrom;
use std::io::{Read, Seek};

use crate::buffer::ConvertBuffer;
use crate::error::{DecodingError, ImageError, ImageResult};
use crate::image::{ImageDecoder, ImageFormat};
use crate::{AnimationDecoder, ColorType, Delay, Frame, Frames, RgbImage, Rgba, RgbaImage};

impl From<image_webp::DecodingError> for ImageError {
    fn from(e: image_webp::DecodingError) -> ImageError {
        ImageError::Decoding(DecodingError::new(ImageFormat::WebP.into(), e))
    }
}

/// WebP Image format decoder. Currently only supports lossy RGB images or lossless RGBA images.
pub struct WebPDecoder<R> {
    inner: image_webp::WebPDecoder<R>,
}

impl<R: Read + Seek> WebPDecoder<R> {
    /// Create a new WebPDecoder from the Reader ```r```.
    /// This function takes ownership of the Reader.
    pub fn new(r: R) -> ImageResult<Self> {
        Ok(Self {
            inner: image_webp::WebPDecoder::new(r)?,
        })
    }

    /// Returns true if the image as described by the bitstream is animated.
    pub fn has_animation(&self) -> bool {
        self.inner.is_animated()
    }

    /// Sets the background color if the image is an extended and animated webp.
    pub fn set_background_color(&mut self, color: Rgba<u8>) -> ImageResult<()> {
        Ok(self.inner.set_background_color(color.0)?)
    }
}

impl<R: Read + Seek> ImageDecoder for WebPDecoder<R> {
    fn dimensions(&self) -> (u32, u32) {
        self.inner.dimensions()
    }

    fn color_type(&self) -> ColorType {
        if self.inner.has_alpha() {
            ColorType::Rgba8
        } else {
            ColorType::Rgb8
        }
    }

    fn read_image(mut self, buf: &mut [u8]) -> ImageResult<()> {
        assert_eq!(u64::try_from(buf.len()), Ok(self.total_bytes()));

        Ok(self.inner.read_image(buf)?)
    }

    fn read_image_boxed(self: Box<Self>, buf: &mut [u8]) -> ImageResult<()> {
        (*self).read_image(buf)
    }

    fn icc_profile(&mut self) -> ImageResult<Option<Vec<u8>>> {
        Ok(self.inner.icc_profile()?)
    }
}

impl<'a, R: 'a + Read + Seek> AnimationDecoder<'a> for WebPDecoder<R> {
    fn into_frames(self) -> Frames<'a> {
        struct FramesInner<R: Read + Seek> {
            decoder: WebPDecoder<R>,
        }
        impl<R: Read + Seek> Iterator for FramesInner<R> {
            type Item = ImageResult<Frame>;

            fn next(&mut self) -> Option<Self::Item> {
                let (width, height) = self.decoder.inner.dimensions();

                let (img, delay) = if self.decoder.inner.has_alpha() {
                    let mut img = RgbaImage::new(width, height);
                    match self.decoder.inner.read_frame(&mut *img) {
                        Ok(delay) => (img, delay),
                        Err(e) => return Some(Err(e.into())),
                    }
                } else {
                    let mut img = RgbImage::new(width, height);
                    match self.decoder.inner.read_frame(&mut *img) {
                        Ok(delay) => (img.convert(), delay),
                        Err(e) => return Some(Err(e.into())),
                    }
                };

                Some(Ok(Frame::from_parts(
                    img,
                    0,
                    0,
                    Delay::from_numer_denom_ms(delay, 1),
                )))
            }
        }

        Frames::new(Box::new(FramesInner { decoder: self }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn add_with_overflow_size() {
        let bytes = vec![
            0x52, 0x49, 0x46, 0x46, 0xaf, 0x37, 0x80, 0x47, 0x57, 0x45, 0x42, 0x50, 0x6c, 0x64,
            0x00, 0x00, 0xff, 0xff, 0xff, 0xff, 0xfb, 0x7e, 0x73, 0x00, 0x06, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x05, 0x00, 0x00, 0x00, 0x65, 0x65, 0x65, 0x65, 0x65, 0x65,
            0x40, 0xfb, 0xff, 0xff, 0x65, 0x65, 0x65, 0x65, 0x65, 0x65, 0x65, 0x65, 0x65, 0x65,
            0x00, 0x00, 0x00, 0x00, 0x62, 0x00, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x49,
            0x49, 0x54, 0x55, 0x50, 0x4c, 0x54, 0x59, 0x50, 0x45, 0x33, 0x37, 0x44, 0x4d, 0x46,
        ];

        let data = std::io::Cursor::new(bytes);

        let _ = WebPDecoder::new(data);
    }
}
