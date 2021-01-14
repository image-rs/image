//! Decoding of AVIF images.
///
/// The [AVIF] specification defines an image derivative of the AV1 bitstream, an open video codec.
///
/// [AVIF]: https://aomediacodec.github.io/av1-avif/
use std::convert::TryFrom;
use std::error::Error;
use std::io::{self, Cursor, Read};
use std::marker::PhantomData;
use std::mem;

use crate::error::DecodingError;
use crate::{ColorType, ImageDecoder, ImageError, ImageFormat, ImageResult};

use dav1d::{PixelLayout, PlanarImageComponent};
use dcv_color_primitives as dcp;
use mp4parse::read_avif;

fn error_map<E: Into<Box<dyn Error + Send + Sync>>>(err: E) -> ImageError {
    ImageError::Decoding(DecodingError::new(ImageFormat::Avif.into(), err))
}

/// AVIF Decoder.
///
/// Reads one image into the chosen input.
pub struct AvifDecoder<R> {
    inner: PhantomData<R>,
    picture: dav1d::Picture,
    alpha_picture: Option<dav1d::Picture>,
}

impl<R: Read> AvifDecoder<R> {
    /// Create a new decoder that reads its input from `r`.
    pub fn new(mut r: R) -> ImageResult<Self> {
        let ctx = read_avif(&mut r).map_err(error_map)?;
        let mut primary_decoder = dav1d::Decoder::new();
        primary_decoder
            .send_data(ctx.primary_item(), None, None, None)
            .map_err(error_map)?;
        let picture = primary_decoder.get_picture().map_err(error_map)?;
        let alpha_picture = if let Some(alpha_item) = ctx.alpha_item() {
            let mut alpha_decoder = dav1d::Decoder::new();
            alpha_decoder
                .send_data(alpha_item, None, None, None)
                .map_err(error_map)?;
            Some(alpha_decoder.get_picture().map_err(error_map)?)
        } else {
            None
        };
        assert_eq!(picture.bit_depth(), 8);
        Ok(AvifDecoder {
            inner: PhantomData,
            picture,
            alpha_picture,
        })
    }
}

/// Wrapper struct around a `Cursor<Vec<u8>>`
pub struct AvifReader<R>(Cursor<Vec<u8>>, PhantomData<R>);
impl<R> Read for AvifReader<R> {
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

impl<'a, R: 'a + Read> ImageDecoder<'a> for AvifDecoder<R> {
    type Reader = AvifReader<R>;

    fn dimensions(&self) -> (u32, u32) {
        (self.picture.width(), self.picture.height())
    }

    fn color_type(&self) -> ColorType {
        ColorType::Bgra8
    }

    fn into_reader(self) -> ImageResult<Self::Reader> {
        let plane = self.picture.plane(PlanarImageComponent::Y);
        Ok(AvifReader(
            Cursor::new(plane.as_ref().to_vec()),
            PhantomData,
        ))
    }

    fn read_image(self, buf: &mut [u8]) -> ImageResult<()> {
        assert_eq!(u64::try_from(buf.len()), Ok(self.total_bytes()));

        dcp::initialize();

        if self.picture.pixel_layout() != PixelLayout::I400 {
            let pixel_format = match self.picture.pixel_layout() {
                PixelLayout::I400 => todo!(),
                PixelLayout::I420 => dcp::PixelFormat::I420,
                PixelLayout::I422 => dcp::PixelFormat::I422,
                PixelLayout::I444 => dcp::PixelFormat::I444,
                PixelLayout::Unknown => panic!("Unknown pixel layout"),
            };
            let src_format = dcp::ImageFormat {
                pixel_format,
                color_space: dcp::ColorSpace::Bt601,
                num_planes: 3,
            };
            let dst_format = dcp::ImageFormat {
                pixel_format: dcp::PixelFormat::Bgra,
                color_space: dcp::ColorSpace::Lrgb,
                num_planes: 1,
            };
            let (width, height) = self.dimensions();
            let planes = &[
                self.picture.plane(PlanarImageComponent::Y),
                self.picture.plane(PlanarImageComponent::U),
                self.picture.plane(PlanarImageComponent::V),
            ];
            let src_buffers = planes.iter().map(AsRef::as_ref).collect::<Vec<_>>();
            let strides = &[
                self.picture.stride(PlanarImageComponent::Y) as usize,
                self.picture.stride(PlanarImageComponent::U) as usize,
                self.picture.stride(PlanarImageComponent::V) as usize,
            ];
            let dst_buffers = &mut [&mut buf[..]];
            dcp::convert_image(
                width,
                height,
                &src_format,
                Some(strides),
                &src_buffers,
                &dst_format,
                None,
                dst_buffers,
            )
            .map_err(error_map)?;
        } else {
            let plane = self.picture.plane(PlanarImageComponent::Y);
            buf.copy_from_slice(plane.as_ref());
        }

        if let Some(picture) = self.alpha_picture {
            assert_eq!(picture.pixel_layout(), PixelLayout::I400);
            let stride = picture.stride(PlanarImageComponent::Y) as usize;
            let plane = picture.plane(PlanarImageComponent::Y);
            let width = picture.width();
            for (buf, slice) in Iterator::zip(
                buf.chunks_exact_mut(width as usize * 4),
                plane.as_ref().chunks_exact(stride),
            ) {
                for i in 0..width as usize {
                    buf[3 + i * 4] = slice[i];
                }
            }
        }

        Ok(())
    }
}
