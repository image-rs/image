//! Decoding of AVIF images.
///
/// The [AVIF] specification defines an image derivative of the AV1 bitstream, an open video codec.
///
/// [AVIF]: https://aomediacodec.github.io/av1-avif/
use std::error::Error;
use std::io::Read;
use std::marker::PhantomData;

use crate::error::{DecodingError, ImageFormatHint, UnsupportedError, UnsupportedErrorKind};
use crate::{ColorType, ImageDecoder, ImageError, ImageFormat, ImageResult};

use crate::codecs::avif::yuv::*;
use dav1d::{PixelLayout, PlanarImageComponent};
use mp4parse::{read_avif, ParseStrictness};

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
    icc_profile: Option<Vec<u8>>,
}

impl<R: Read> AvifDecoder<R> {
    /// Create a new decoder that reads its input from `r`.
    pub fn new(mut r: R) -> ImageResult<Self> {
        let ctx = read_avif(&mut r, ParseStrictness::Normal).map_err(error_map)?;
        let coded = ctx.primary_item_coded_data().unwrap_or_default();

        let mut primary_decoder = dav1d::Decoder::new().map_err(error_map)?;
        primary_decoder
            .send_data(coded.to_vec(), None, None, None)
            .map_err(error_map)?;
        let picture = read_until_ready(&mut primary_decoder)?;
        let alpha_item = ctx.alpha_item_coded_data().unwrap_or_default();
        let alpha_picture = if !alpha_item.is_empty() {
            let mut alpha_decoder = dav1d::Decoder::new().map_err(error_map)?;
            alpha_decoder
                .send_data(alpha_item.to_vec(), None, None, None)
                .map_err(error_map)?;
            Some(read_until_ready(&mut alpha_decoder)?)
        } else {
            None
        };
        let icc_profile = ctx
            .icc_colour_information()
            .map(|x| x.ok().unwrap_or_default())
            .map(|x| x.to_vec());

        match picture.bit_depth() {
            8 => (),
            10 | 12 => (),
            _ => {
                return ImageResult::Err(ImageError::Decoding(DecodingError::new(
                    ImageFormatHint::Exact(ImageFormat::Avif),
                    format!(
                        "Avif format does not support {} bit depth",
                        picture.bit_depth()
                    ),
                )))
            }
        };
        Ok(AvifDecoder {
            inner: PhantomData,
            picture,
            alpha_picture,
            icc_profile,
        })
    }
}

fn reshape_plane(source: &[u8], stride: usize, width: usize, height: usize) -> Vec<u16> {
    let mut target_plane = vec![0u16; width * height];
    for (shaped_row, src_row) in target_plane
        .chunks_exact_mut(width)
        .zip(source.chunks_exact(stride))
    {
        for (dst, src) in shaped_row.iter_mut().zip(src_row.chunks_exact(2)) {
            *dst = u16::from_le_bytes([src[0], src[1]]);
        }
    }
    target_plane
}

impl<R: Read> ImageDecoder for AvifDecoder<R> {
    fn dimensions(&self) -> (u32, u32) {
        (self.picture.width(), self.picture.height())
    }

    fn color_type(&self) -> ColorType {
        if self.picture.bit_depth() == 8 {
            ColorType::Rgba8
        } else {
            ColorType::Rgba16
        }
    }

    fn icc_profile(&mut self) -> ImageResult<Option<Vec<u8>>> {
        Ok(self.icc_profile.clone())
    }

    fn read_image(self, buf: &mut [u8]) -> ImageResult<()> {
        assert_eq!(u64::try_from(buf.len()), Ok(self.total_bytes()));

        let (width, height) = self.dimensions();

        let yuv_range = match self.picture.color_range() {
            dav1d::pixel::YUVRange::Limited => YuvIntensityRange::Tv,
            dav1d::pixel::YUVRange::Full => YuvIntensityRange::Pc,
        };
        let color_matrix = match self.picture.color_primaries() {
            dav1d::pixel::ColorPrimaries::Reserved0 | dav1d::pixel::ColorPrimaries::Reserved => {
                return Err(ImageError::Unsupported(
                    UnsupportedError::from_format_and_kind(
                        ImageFormat::Avif.into(),
                        UnsupportedErrorKind::GenericFeature(
                            "Using 'Reserved' color matrix is not supported".to_string(),
                        ),
                    ),
                ));
            }
            dav1d::pixel::ColorPrimaries::BT709 => YuvStandardMatrix::Bt709,
            // This is arguable, some applications prefer to go with Bt.709 as default some applications as Bt.601
            // For ex. chrome always prefer Bt.709 even for SD content
            // However, nowadays standard should be Bt.709 for HD+ size otherwise Bt.601
            dav1d::pixel::ColorPrimaries::Unspecified => YuvStandardMatrix::Bt709,
            dav1d::pixel::ColorPrimaries::BT470M => YuvStandardMatrix::Bt470_6,
            dav1d::pixel::ColorPrimaries::BT470BG => YuvStandardMatrix::Bt601,
            dav1d::pixel::ColorPrimaries::ST170M => YuvStandardMatrix::Smpte240,
            dav1d::pixel::ColorPrimaries::ST240M => YuvStandardMatrix::Smpte240,
            dav1d::pixel::ColorPrimaries::Film => YuvStandardMatrix::Bt2020,
            dav1d::pixel::ColorPrimaries::BT2020 => YuvStandardMatrix::Bt2020,
            dav1d::pixel::ColorPrimaries::ST428 => YuvStandardMatrix::Bt709,
            dav1d::pixel::ColorPrimaries::P3DCI => YuvStandardMatrix::Bt709,
            dav1d::pixel::ColorPrimaries::P3Display => YuvStandardMatrix::Bt709,
            dav1d::pixel::ColorPrimaries::Tech3213 => {
                return Err(ImageError::Unsupported(
                    UnsupportedError::from_format_and_kind(
                        ImageFormat::Avif.into(),
                        UnsupportedErrorKind::GenericFeature("Unknown color matrix".to_string()),
                    ),
                ));
            }
        };

        if self.picture.bit_depth() == 8 {
            if self.picture.pixel_layout() != PixelLayout::I400 {
                let worker = match self.picture.pixel_layout() {
                    PixelLayout::I400 => unreachable!(),
                    PixelLayout::I420 => yuv420_to_rgba,
                    PixelLayout::I422 => yuv422_to_rgba,
                    PixelLayout::I444 => yuv444_to_rgba,
                };

                let ref_y = self.picture.plane(PlanarImageComponent::Y);
                let ref_u = self.picture.plane(PlanarImageComponent::U);
                let ref_v = self.picture.plane(PlanarImageComponent::V);
                let image = YuvPlanarImage::new(
                    ref_y.as_ref(),
                    self.picture.stride(PlanarImageComponent::Y) as usize,
                    ref_u.as_ref(),
                    self.picture.stride(PlanarImageComponent::U) as usize,
                    ref_v.as_ref(),
                    self.picture.stride(PlanarImageComponent::V) as usize,
                    width as usize,
                    height as usize,
                );

                let res = worker(image, buf, 8, yuv_range, color_matrix);

                if let Err(err) = res {
                    return Err(ImageError::Unsupported(
                        UnsupportedError::from_format_and_kind(
                            ImageFormat::Avif.into(),
                            UnsupportedErrorKind::GenericFeature(err),
                        ),
                    ));
                }
            } else {
                let plane = self.picture.plane(PlanarImageComponent::Y);

                let gray_image = YuvGrayImage::new(
                    plane.as_ref(),
                    self.picture.stride(PlanarImageComponent::Y) as usize,
                    width as usize,
                    height as usize,
                );

                let cr = yuv400_to_rgba(gray_image, buf, 8, yuv_range, color_matrix);
                if let Err(err) = cr {
                    return Err(ImageError::Unsupported(
                        UnsupportedError::from_format_and_kind(
                            ImageFormat::Avif.into(),
                            UnsupportedErrorKind::GenericFeature(err),
                        ),
                    ));
                }
            }

            if let Some(picture) = self.alpha_picture {
                if picture.pixel_layout() != PixelLayout::I400 {
                    return Err(ImageError::Unsupported(
                        UnsupportedError::from_format_and_kind(
                            ImageFormat::Avif.into(),
                            UnsupportedErrorKind::GenericFeature(format!(
                                "Alpha must be PixelLayout::I400 but was: {:?}",
                                picture.pixel_layout() // PixelLayout does not implement display
                            )),
                        ),
                    ));
                }
                let stride = picture.stride(PlanarImageComponent::Y) as usize;
                let plane = picture.plane(PlanarImageComponent::Y);
                let width = picture.width();
                for (buf, slice) in Iterator::zip(
                    buf.chunks_exact_mut(width as usize * 4),
                    plane.as_ref().chunks_exact(stride),
                ) {
                    for (rgba, a_src) in buf.chunks_exact_mut(4).zip(slice) {
                        rgba[3] = *a_src;
                    }
                }
            }
        } else {
            // 8+ bit-depth case
            let rgba16_buf: &mut [u16] = match bytemuck::try_cast_slice_mut(buf) {
                Ok(slice) => slice,
                Err(_) => {
                    return Err(ImageError::Unsupported(
                        UnsupportedError::from_format_and_kind(
                            ImageFormat::Avif.into(),
                            UnsupportedErrorKind::GenericFeature(
                                "Incorrectly determined image type".to_string(),
                            ),
                        ),
                    ));
                }
            };

            // dav1d may return not aligned and not correctly constrained data,
            // or at least I can't find guarantees on that
            // so if it is happened, instead casting we'll need to reshape it into a target slice
            // required criteria: bytemuck allows this data align, and stride must be dividable by 2

            let mut y_plane_stride = self.picture.stride(PlanarImageComponent::Y) >> 1;

            let ref_y = self.picture.plane(PlanarImageComponent::Y);
            let mut _bind_y = vec![];

            let mut shape_y_plane = || {
                y_plane_stride = width;
                _bind_y = reshape_plane(
                    ref_y.as_ref(),
                    self.picture.stride(PlanarImageComponent::Y) as usize,
                    width as usize,
                    height as usize,
                );
            };

            let y_plane: &[u16] = if self.picture.stride(PlanarImageComponent::Y) as usize & 1 == 0
            {
                match bytemuck::try_cast_slice(ref_y.as_ref()) {
                    Ok(slice) => slice,
                    Err(_) => {
                        shape_y_plane();
                        _bind_y.as_slice()
                    }
                }
            } else {
                shape_y_plane();
                _bind_y.as_slice()
            };

            if self.picture.pixel_layout() != PixelLayout::I400 {
                let mut u_plane_stride = self.picture.stride(PlanarImageComponent::U) >> 1;

                let ref_u = self.picture.plane(PlanarImageComponent::U);
                let mut _bind_u = vec![];
                let ref_v = self.picture.plane(PlanarImageComponent::V);
                let mut _bind_v = vec![];

                let mut shape_u_plane = || {
                    u_plane_stride = match self.picture.pixel_layout() {
                        PixelLayout::I400 => unreachable!(),
                        PixelLayout::I420 | PixelLayout::I422 => (width + 1) / 2,
                        PixelLayout::I444 => width,
                    };
                    let u_plane_height = match self.picture.pixel_layout() {
                        PixelLayout::I400 => unreachable!(),
                        PixelLayout::I420 => (height + 1) / 2,
                        PixelLayout::I422 | PixelLayout::I444 => height,
                    };
                    _bind_u = reshape_plane(
                        ref_u.as_ref(),
                        self.picture.stride(PlanarImageComponent::U) as usize,
                        u_plane_stride as usize,
                        u_plane_height as usize,
                    );
                };

                let u_plane: &[u16] =
                    if self.picture.stride(PlanarImageComponent::U) as usize & 1 == 0 {
                        match bytemuck::try_cast_slice(ref_u.as_ref()) {
                            Ok(slice) => slice,
                            Err(_) => {
                                shape_u_plane();
                                _bind_u.as_slice()
                            }
                        }
                    } else {
                        shape_u_plane();
                        _bind_u.as_slice()
                    };

                let mut v_plane_stride = self.picture.stride(PlanarImageComponent::V) >> 1;

                let mut shape_v_plane = || {
                    v_plane_stride = match self.picture.pixel_layout() {
                        PixelLayout::I400 => unreachable!(),
                        PixelLayout::I420 | PixelLayout::I422 => (width + 1) / 2,
                        PixelLayout::I444 => width,
                    };
                    let v_plane_height = match self.picture.pixel_layout() {
                        PixelLayout::I400 => unreachable!(),
                        PixelLayout::I420 => (height + 1) / 2,
                        PixelLayout::I422 | PixelLayout::I444 => height,
                    };
                    _bind_v = reshape_plane(
                        ref_v.as_ref(),
                        self.picture.stride(PlanarImageComponent::V) as usize,
                        v_plane_stride as usize,
                        v_plane_height as usize,
                    );
                };

                let v_plane: &[u16] =
                    if self.picture.stride(PlanarImageComponent::V) as usize & 1 == 0 {
                        match bytemuck::try_cast_slice(ref_v.as_ref()) {
                            Ok(slice) => slice,
                            Err(_) => {
                                shape_v_plane();
                                _bind_v.as_slice()
                            }
                        }
                    } else {
                        shape_v_plane();
                        _bind_v.as_slice()
                    };

                let worker = match self.picture.pixel_layout() {
                    PixelLayout::I400 => unreachable!(),
                    PixelLayout::I420 => yuv420_to_rgba,
                    PixelLayout::I422 => yuv422_to_rgba,
                    PixelLayout::I444 => yuv444_to_rgba,
                };

                let image = YuvPlanarImage::new(
                    y_plane,
                    y_plane_stride as usize,
                    u_plane,
                    u_plane_stride as usize,
                    v_plane,
                    v_plane_stride as usize,
                    width as usize,
                    height as usize,
                );

                let res = worker(
                    image,
                    rgba16_buf,
                    self.picture.bit_depth() as u32,
                    yuv_range,
                    color_matrix,
                );

                if let Err(err) = res {
                    return Err(ImageError::Unsupported(
                        UnsupportedError::from_format_and_kind(
                            ImageFormat::Avif.into(),
                            UnsupportedErrorKind::GenericFeature(err),
                        ),
                    ));
                }
            } else {
                let gray_image = YuvGrayImage::new(
                    y_plane,
                    y_plane_stride as usize,
                    width as usize,
                    height as usize,
                );
                let cr = yuv400_to_rgba(
                    gray_image,
                    rgba16_buf,
                    self.picture.bit_depth() as u32,
                    yuv_range,
                    color_matrix,
                );
                if let Err(err) = cr {
                    return Err(ImageError::Unsupported(
                        UnsupportedError::from_format_and_kind(
                            ImageFormat::Avif.into(),
                            UnsupportedErrorKind::GenericFeature(err),
                        ),
                    ));
                }
            }

            // Squashing alpha plane into a picture
            if let Some(picture) = self.alpha_picture {
                if picture.pixel_layout() != PixelLayout::I400 {
                    return Err(ImageError::Unsupported(
                        UnsupportedError::from_format_and_kind(
                            ImageFormat::Avif.into(),
                            UnsupportedErrorKind::GenericFeature(format!(
                                "Alpha must be PixelLayout::I400 but was: {:?}",
                                picture.pixel_layout() // PixelLayout does not implement display
                            )),
                        ),
                    ));
                }
                let ref_a = self.picture.plane(PlanarImageComponent::Y);
                let mut _bind_a = vec![];

                let mut a_plane_stride = self.picture.stride(PlanarImageComponent::Y) >> 1;

                let mut shape_a_plane = || {
                    a_plane_stride = width;
                    _bind_a = reshape_plane(
                        ref_a.as_ref(),
                        picture.stride(PlanarImageComponent::Y) as usize,
                        width as usize,
                        height as usize,
                    );
                };

                let a_plane: &[u16] = if picture.stride(PlanarImageComponent::Y) as usize & 1 == 0 {
                    match bytemuck::try_cast_slice(ref_y.as_ref()) {
                        Ok(slice) => slice,
                        Err(_) => {
                            shape_a_plane();
                            _bind_a.as_slice()
                        }
                    }
                } else {
                    shape_a_plane();
                    _bind_a.as_slice()
                };

                let width = picture.width();
                for (buf, slice) in Iterator::zip(
                    rgba16_buf.chunks_exact_mut(width as usize * 4),
                    a_plane.as_ref().chunks_exact(a_plane_stride as usize),
                ) {
                    for (rgba, a_src) in buf.chunks_exact_mut(4).zip(slice) {
                        rgba[3] = *a_src;
                    }
                }
            }

            // Expand current bit depth to target 16
            let target_expand_bits = 16u32.saturating_sub(self.picture.bit_depth() as u32);
            if target_expand_bits > 0 {
                for rgba in rgba16_buf.chunks_exact_mut(4) {
                    rgba[0] <<= target_expand_bits;
                    rgba[1] <<= target_expand_bits;
                    rgba[2] <<= target_expand_bits;
                    rgba[3] <<= target_expand_bits;
                }
            }
        }

        Ok(())
    }

    fn read_image_boxed(self: Box<Self>, buf: &mut [u8]) -> ImageResult<()> {
        (*self).read_image(buf)
    }
}

/// `get_picture` and `send_pending_data` yield `Again` as a non-fatal error requesting more data is sent to the decoder
/// This ensures that in the case of `Again` all pending data is submitted
/// This should be called after `send_data` (which does not yield `Again` when called the first time)
fn read_until_ready(decoder: &mut dav1d::Decoder) -> ImageResult<dav1d::Picture> {
    loop {
        match decoder.get_picture() {
            Err(dav1d::Error::Again) => match decoder.send_pending_data() {
                Ok(_) => {}
                Err(dav1d::Error::Again) => {}
                Err(e) => return Err(error_map(e)),
            },
            r => return r.map_err(error_map),
        }
    }
}
