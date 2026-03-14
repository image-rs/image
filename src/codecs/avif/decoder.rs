//! Decoding of AVIF images.
use crate::error::{
    DecodingError, ImageFormatHint, LimitError, LimitErrorKind, UnsupportedError,
    UnsupportedErrorKind,
};
use crate::metadata::Orientation;
use crate::{ColorType, ImageDecoder, ImageError, ImageFormat, ImageResult};
///
/// The [AVIF] specification defines an image derivative of the AV1 bitstream, an open video codec.
///
/// [AVIF]: https://aomediacodec.github.io/av1-avif/
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::io::Read;
use std::marker::PhantomData;

use crate::codecs::avif::ycgco::{
    ycgco420_to_rgba10, ycgco420_to_rgba12, ycgco420_to_rgba8, ycgco422_to_rgba10,
    ycgco422_to_rgba12, ycgco422_to_rgba8, ycgco444_to_rgba10, ycgco444_to_rgba12,
    ycgco444_to_rgba8,
};
use crate::codecs::avif::yuv::*;
use mp4parse::{read_avif, ImageMirror, ImageRotation, ParseStrictness};
use rav1d_safe::{ColorRange, Frame, MatrixCoefficients, PixelLayout};

fn error_map<E: Into<Box<dyn Error + Send + Sync>>>(err: E) -> ImageError {
    ImageError::Decoding(DecodingError::new(ImageFormat::Avif.into(), err))
}

/// AVIF Decoder.
///
/// Reads one image into the chosen input.
pub struct AvifDecoder<R> {
    inner: PhantomData<R>,
    picture: Frame,
    alpha_picture: Option<Frame>,
    icc_profile: Option<Vec<u8>>,
    orientation: Orientation,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum AvifDecoderError {
    AlphaPlaneFormat(PixelLayout),
    YuvLayoutOnIdentityMatrix(PixelLayout),
    UnsupportedLayoutAndMatrix(PixelLayout, YuvMatrixStrategy),
}

impl Display for AvifDecoderError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            AvifDecoderError::AlphaPlaneFormat(pixel_layout) => match pixel_layout {
                PixelLayout::I400 => unreachable!("This option must be handled correctly"),
                PixelLayout::I420 => f.write_str("Alpha layout must be 4:0:0, but it was 4:2:0"),
                PixelLayout::I422 => f.write_str("Alpha layout must be 4:0:0, but it was 4:2:2"),
                PixelLayout::I444 => f.write_str("Alpha layout must be 4:0:0, but it was 4:4:4"),
            },
            AvifDecoderError::YuvLayoutOnIdentityMatrix(pixel_layout) => match pixel_layout {
                PixelLayout::I400 => {
                    f.write_str("YUV layout on 'Identity' matrix must be 4:4:4, but it was 4:0:0")
                }
                PixelLayout::I420 => {
                    f.write_str("YUV layout on 'Identity' matrix must be 4:4:4, but it was 4:2:0")
                }
                PixelLayout::I422 => {
                    f.write_str("YUV layout on 'Identity' matrix must be 4:4:4, but it was 4:2:2")
                }
                PixelLayout::I444 => unreachable!("This option must be handled correctly"),
            },
            AvifDecoderError::UnsupportedLayoutAndMatrix(layout, matrix) => f.write_fmt(
                format_args!("YUV layout {layout:?} on matrix {matrix:?} is not supported",),
            ),
        }
    }
}

impl Error for AvifDecoderError {}

impl<R: Read> AvifDecoder<R> {
    /// Create a new decoder that reads its input from `r`.
    pub fn new(mut r: R) -> ImageResult<Self> {
        let ctx = read_avif(&mut r, ParseStrictness::Permissive).map_err(error_map)?;
        let coded = ctx.primary_item_coded_data().unwrap_or_default();

        let mut primary_decoder = rav1d_safe::Decoder::new().map_err(error_map)?;
        let picture = primary_decoder
            .decode(coded)
            .map_err(error_map)?
            .ok_or_else(|| {
                error_map(std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    "no picture decoded from primary item",
                ))
            })?;
        let alpha_item = ctx.alpha_item_coded_data().unwrap_or_default();
        let alpha_picture = if !alpha_item.is_empty() {
            let mut alpha_decoder = rav1d_safe::Decoder::new().map_err(error_map)?;
            Some(
                alpha_decoder
                    .decode(alpha_item)
                    .map_err(error_map)?
                    .ok_or_else(|| {
                        error_map(std::io::Error::new(
                            std::io::ErrorKind::InvalidData,
                            "no picture decoded from alpha item",
                        ))
                    })?,
            )
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
        let rotation = ctx.image_rotation().map_err(error_map)?;
        // mp4parse does not expose a safe wrapper around the pointer :(
        let mirror_ptr = ctx.image_mirror_ptr().map_err(error_map)?;
        let mirror: Option<ImageMirror> = if mirror_ptr.is_null() {
            None
        } else {
            // SAFETY: we have verified above that the pointer is non-null.
            // We trust mp4parse to return a pointer that is not dangling.
            // We immediately copy/move the value, so no issues with lifetimes.
            unsafe { Some(std::ptr::read_unaligned(mirror_ptr)) }
        };

        let orientation = convert_orientation(rotation, mirror);
        Ok(AvifDecoder {
            inner: PhantomData,
            picture,
            alpha_picture,
            icc_profile,
            orientation,
        })
    }
}

fn convert_orientation(rotation: ImageRotation, mirror: Option<ImageMirror>) -> Orientation {
    // Order of operations: rotate then mirror
    match mirror {
        None => match rotation {
            ImageRotation::D0 => Orientation::NoTransforms,
            // AVIF rotations are counter-clockwise (clocksilly)
            ImageRotation::D90 => Orientation::Rotate270,
            ImageRotation::D180 => Orientation::Rotate180,
            ImageRotation::D270 => Orientation::Rotate90,
        },
        Some(ImageMirror::LeftRight) => match rotation {
            ImageRotation::D0 => Orientation::FlipHorizontal,
            ImageRotation::D90 => Orientation::Rotate270FlipH,
            ImageRotation::D180 => Orientation::FlipVertical,
            ImageRotation::D270 => Orientation::Rotate90FlipH,
        },
        Some(ImageMirror::TopBottom) => match rotation {
            ImageRotation::D0 => Orientation::FlipVertical,
            // FlipV is equivalent to Rotate180FlipH, we use that for conversions below
            ImageRotation::D90 => Orientation::Rotate90FlipH,
            ImageRotation::D180 => Orientation::FlipHorizontal,
            ImageRotation::D270 => Orientation::Rotate270FlipH,
        },
    }
}

#[derive(Copy, Clone, Debug, PartialOrd, Eq, PartialEq)]
enum YuvMatrixStrategy {
    KrKb(YuvStandardMatrix),
    CgCo,
    Identity,
}

/// Getting one of prebuilt matrix or fails
fn get_matrix(matrix: MatrixCoefficients) -> Result<YuvMatrixStrategy, ImageError> {
    match matrix {
        MatrixCoefficients::Identity => Ok(YuvMatrixStrategy::Identity),
        MatrixCoefficients::BT709 => Ok(YuvMatrixStrategy::KrKb(YuvStandardMatrix::Bt709)),
        // This is arguable, some applications prefer to go with Bt.709 as default,
        // and some applications prefer Bt.601 as default.
        // For ex. `Chrome` always prefer Bt.709 even for SD content
        // However, nowadays standard should be Bt.709 for HD+ size otherwise Bt.601
        MatrixCoefficients::Unspecified => Ok(YuvMatrixStrategy::KrKb(YuvStandardMatrix::Bt709)),
        MatrixCoefficients::Reserved => Err(ImageError::Unsupported(
            UnsupportedError::from_format_and_kind(
                ImageFormat::Avif.into(),
                UnsupportedErrorKind::GenericFeature(
                    "Using 'Reserved' color matrix is not supported".to_string(),
                ),
            ),
        )),
        MatrixCoefficients::FCC => Ok(YuvMatrixStrategy::KrKb(YuvStandardMatrix::Bt470_6)),
        MatrixCoefficients::BT470BG => Ok(YuvMatrixStrategy::KrKb(YuvStandardMatrix::Bt601)),
        MatrixCoefficients::BT601 => Ok(YuvMatrixStrategy::KrKb(YuvStandardMatrix::Smpte240)),
        MatrixCoefficients::SMPTE240 => Ok(YuvMatrixStrategy::KrKb(YuvStandardMatrix::Smpte240)),
        MatrixCoefficients::YCgCo => Ok(YuvMatrixStrategy::CgCo),
        MatrixCoefficients::BT2020NCL => Ok(YuvMatrixStrategy::KrKb(YuvStandardMatrix::Bt2020)),
        MatrixCoefficients::BT2020CL => {
            // This matrix significantly differs from others because linearize values is required
            // to compute Y instead of Y'.
            // Actually it is almost everywhere is not implemented.
            // Libavif + libheif missing this also so actually AVIF images
            // with CL BT.2020 might be made only by mistake
            Err(ImageError::Unsupported(
                UnsupportedError::from_format_and_kind(
                    ImageFormat::Avif.into(),
                    UnsupportedErrorKind::GenericFeature(
                        "BT2020ConstantLuminance matrix is not supported".to_string(),
                    ),
                ),
            ))
        }
        MatrixCoefficients::SMPTE2085 => Err(ImageError::Unsupported(
            UnsupportedError::from_format_and_kind(
                ImageFormat::Avif.into(),
                UnsupportedErrorKind::GenericFeature("ST2085 matrix is not supported".to_string()),
            ),
        )),
        MatrixCoefficients::ChromaDerivedCL | MatrixCoefficients::ChromaDerivedNCL => Err(
            ImageError::Unsupported(UnsupportedError::from_format_and_kind(
                ImageFormat::Avif.into(),
                UnsupportedErrorKind::GenericFeature(
                    "Chromaticity Derived Luminance matrix is not supported".to_string(),
                ),
            )),
        ),
        MatrixCoefficients::ICtCp => Err(ImageError::Unsupported(
            UnsupportedError::from_format_and_kind(
                ImageFormat::Avif.into(),
                UnsupportedErrorKind::GenericFeature(
                    "ICtCp Derived Luminance matrix is not supported".to_string(),
                ),
            ),
        )),
    }
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

    fn orientation(&mut self) -> ImageResult<Orientation> {
        Ok(self.orientation)
    }

    fn read_image(self, buf: &mut [u8]) -> ImageResult<()> {
        assert_eq!(u64::try_from(buf.len()), Ok(self.total_bytes()));

        let bit_depth = self.picture.bit_depth();

        // Normally this should never happen,
        // if this happens then there is an incorrect implementation somewhere else
        assert!(bit_depth == 8 || bit_depth == 10 || bit_depth == 12);

        let (width, height) = self.dimensions();
        // This is suspicious if this happens, better fail early
        if width == 0 || height == 0 {
            return Err(ImageError::Limits(LimitError::from_kind(
                LimitErrorKind::DimensionError,
            )));
        }

        let color_info = self.picture.color_info();

        let yuv_range = match color_info.color_range {
            ColorRange::Limited => YuvIntensityRange::Tv,
            ColorRange::Full => YuvIntensityRange::Pc,
        };

        let pixel_layout = self.picture.pixel_layout();
        let matrix_strategy = get_matrix(color_info.matrix_coefficients)?;

        // Identity matrix should be possible only on 4:4:4
        if matrix_strategy == YuvMatrixStrategy::Identity && pixel_layout != PixelLayout::I444 {
            return Err(ImageError::Decoding(DecodingError::new(
                ImageFormat::Avif.into(),
                AvifDecoderError::YuvLayoutOnIdentityMatrix(pixel_layout),
            )));
        }

        if matrix_strategy == YuvMatrixStrategy::CgCo && pixel_layout == PixelLayout::I400 {
            return Err(ImageError::Decoding(DecodingError::new(
                ImageFormat::Avif.into(),
                AvifDecoderError::UnsupportedLayoutAndMatrix(pixel_layout, matrix_strategy),
            )));
        }

        if bit_depth == 8 {
            let planes = match self.picture.planes() {
                rav1d_safe::Planes::Depth8(p) => p,
                _ => unreachable!("bit_depth is 8 but planes are not 8-bit"),
            };

            let y_view = planes.y();
            let u_view = planes.u();
            let v_view = planes.v();

            let image = YuvPlanarImage {
                y_plane: y_view.as_slice(),
                y_stride: y_view.stride(),
                u_plane: u_view.as_ref().map_or(&[], |v| v.as_slice()),
                u_stride: u_view.as_ref().map_or(0, |v| v.stride()),
                v_plane: v_view.as_ref().map_or(&[], |v| v.as_slice()),
                v_stride: v_view.as_ref().map_or(0, |v| v.stride()),
                width: width as usize,
                height: height as usize,
            };

            match matrix_strategy {
                YuvMatrixStrategy::KrKb(standard) => {
                    let worker = match pixel_layout {
                        PixelLayout::I400 => yuv400_to_rgba8,
                        PixelLayout::I420 => yuv420_to_rgba8,
                        PixelLayout::I422 => yuv422_to_rgba8,
                        PixelLayout::I444 => yuv444_to_rgba8,
                    };

                    worker(image, buf, yuv_range, standard)?;
                }
                YuvMatrixStrategy::CgCo => {
                    let worker = match pixel_layout {
                        PixelLayout::I400 => unreachable!(),
                        PixelLayout::I420 => ycgco420_to_rgba8,
                        PixelLayout::I422 => ycgco422_to_rgba8,
                        PixelLayout::I444 => ycgco444_to_rgba8,
                    };

                    worker(image, buf, yuv_range)?;
                }
                YuvMatrixStrategy::Identity => {
                    let worker = match pixel_layout {
                        PixelLayout::I400 => unreachable!(),
                        PixelLayout::I420 => unreachable!(),
                        PixelLayout::I422 => unreachable!(),
                        PixelLayout::I444 => gbr_to_rgba8,
                    };

                    worker(image, buf, yuv_range)?;
                }
            }

            // Squashing alpha plane into a picture
            if let Some(ref alpha_pic) = self.alpha_picture {
                if alpha_pic.pixel_layout() != PixelLayout::I400 {
                    return Err(ImageError::Decoding(DecodingError::new(
                        ImageFormat::Avif.into(),
                        AvifDecoderError::AlphaPlaneFormat(alpha_pic.pixel_layout()),
                    )));
                }

                let alpha_planes = match alpha_pic.planes() {
                    rav1d_safe::Planes::Depth8(p) => p,
                    _ => unreachable!("alpha bit_depth mismatch"),
                };
                let alpha_y = alpha_planes.y();
                let stride = alpha_y.stride();

                for (buf, slice) in Iterator::zip(
                    buf.chunks_exact_mut(width as usize * 4),
                    alpha_y.as_slice().chunks_exact(stride),
                ) {
                    for (rgba, a_src) in buf.as_chunks_mut::<4>().0.iter_mut().zip(slice) {
                        rgba[3] = *a_src;
                    }
                }
            }
        } else {
            // 10/12 bit-depth case
            if let Ok(buf) = bytemuck::try_cast_slice_mut(buf) {
                let target_slice: &mut [u16] = buf;
                self.process_16bit_picture(
                    target_slice,
                    yuv_range,
                    matrix_strategy,
                    pixel_layout,
                )?;
            } else {
                // If buffer from Decoder is unaligned
                let mut aligned_store = vec![0u16; buf.len() / 2];
                self.process_16bit_picture(
                    &mut aligned_store,
                    yuv_range,
                    matrix_strategy,
                    pixel_layout,
                )?;
                let buf_chunks = buf.as_chunks_mut::<2>().0.iter_mut();
                for (dst, src) in buf_chunks.zip(aligned_store.iter()) {
                    *dst = src.to_ne_bytes();
                }
            }
        }

        Ok(())
    }

    fn read_image_boxed(self: Box<Self>, buf: &mut [u8]) -> ImageResult<()> {
        (*self).read_image(buf)
    }
}

impl<R: Read> AvifDecoder<R> {
    fn process_16bit_picture(
        &self,
        target: &mut [u16],
        yuv_range: YuvIntensityRange,
        matrix_strategy: YuvMatrixStrategy,
        pixel_layout: PixelLayout,
    ) -> ImageResult<()> {
        let (width, height) = (self.picture.width(), self.picture.height());
        let bit_depth = self.picture.bit_depth();

        let planes = match self.picture.planes() {
            rav1d_safe::Planes::Depth16(p) => p,
            _ => unreachable!("bit_depth > 8 but planes are not 16-bit"),
        };

        let y_view = planes.y();
        let u_view = planes.u();
        let v_view = planes.v();

        let image = YuvPlanarImage {
            y_plane: y_view.as_slice(),
            y_stride: y_view.stride(),
            u_plane: u_view.as_ref().map_or(&[], |v| v.as_slice()),
            u_stride: u_view.as_ref().map_or(0, |v| v.stride()),
            v_plane: v_view.as_ref().map_or(&[], |v| v.as_slice()),
            v_stride: v_view.as_ref().map_or(0, |v| v.stride()),
            width: width as usize,
            height: height as usize,
        };

        match matrix_strategy {
            YuvMatrixStrategy::KrKb(standard) => {
                let worker = match pixel_layout {
                    PixelLayout::I400 => {
                        if bit_depth == 10 {
                            yuv400_to_rgba10
                        } else {
                            yuv400_to_rgba12
                        }
                    }
                    PixelLayout::I420 => {
                        if bit_depth == 10 {
                            yuv420_to_rgba10
                        } else {
                            yuv420_to_rgba12
                        }
                    }
                    PixelLayout::I422 => {
                        if bit_depth == 10 {
                            yuv422_to_rgba10
                        } else {
                            yuv422_to_rgba12
                        }
                    }
                    PixelLayout::I444 => {
                        if bit_depth == 10 {
                            yuv444_to_rgba10
                        } else {
                            yuv444_to_rgba12
                        }
                    }
                };
                worker(image, target, yuv_range, standard)?;
            }
            YuvMatrixStrategy::CgCo => {
                let worker = match pixel_layout {
                    PixelLayout::I400 => unreachable!(),
                    PixelLayout::I420 => {
                        if bit_depth == 10 {
                            ycgco420_to_rgba10
                        } else {
                            ycgco420_to_rgba12
                        }
                    }
                    PixelLayout::I422 => {
                        if bit_depth == 10 {
                            ycgco422_to_rgba10
                        } else {
                            ycgco422_to_rgba12
                        }
                    }
                    PixelLayout::I444 => {
                        if bit_depth == 10 {
                            ycgco444_to_rgba10
                        } else {
                            ycgco444_to_rgba12
                        }
                    }
                };
                worker(image, target, yuv_range)?;
            }
            YuvMatrixStrategy::Identity => {
                let worker = match pixel_layout {
                    PixelLayout::I400 => unreachable!(),
                    PixelLayout::I420 => unreachable!(),
                    PixelLayout::I422 => unreachable!(),
                    PixelLayout::I444 => {
                        if bit_depth == 10 {
                            gbr_to_rgba10
                        } else {
                            gbr_to_rgba12
                        }
                    }
                };
                worker(image, target, yuv_range)?;
            }
        }

        // Squashing alpha plane into a picture
        if let Some(ref alpha_pic) = self.alpha_picture {
            if alpha_pic.pixel_layout() != PixelLayout::I400 {
                return Err(ImageError::Decoding(DecodingError::new(
                    ImageFormat::Avif.into(),
                    AvifDecoderError::AlphaPlaneFormat(alpha_pic.pixel_layout()),
                )));
            }

            let alpha_planes = match alpha_pic.planes() {
                rav1d_safe::Planes::Depth16(p) => p,
                _ => unreachable!("alpha bit_depth mismatch for 16-bit"),
            };
            let alpha_y = alpha_planes.y();
            let a_stride = alpha_y.stride();

            for (buf, slice) in Iterator::zip(
                target.chunks_exact_mut(width as usize * 4),
                alpha_y.as_slice().chunks_exact(a_stride),
            ) {
                for (rgba, a_src) in buf.as_chunks_mut::<4>().0.iter_mut().zip(slice) {
                    rgba[3] = *a_src;
                }
            }
        }

        // Expand current bit depth to target 16
        let target_expand_bits = 16u32 - self.picture.bit_depth() as u32;
        for item in target.iter_mut() {
            *item = (*item).rotate_left(target_expand_bits);
        }

        Ok(())
    }
}
