use crate::error::DecodingError;
use crate::{ImageError, ImageFormat};
use num_traits::AsPrimitive;
use std::fmt::{Display, Formatter};
use std::mem::size_of;

#[derive(Debug, Copy, Clone)]
/// Representation of inversion matrix
struct CbCrInverseTransform<T> {
    y_coef: T,
    cr_coef: T,
    cb_coef: T,
    g_coeff_1: T,
    g_coeff_2: T,
}

impl CbCrInverseTransform<f32> {
    fn to_integers(self, precision: u32) -> CbCrInverseTransform<i32> {
        let precision_scale: i32 = 1i32 << (precision as i32);
        let cr_coef = (self.cr_coef * precision_scale as f32) as i32;
        let cb_coef = (self.cb_coef * precision_scale as f32) as i32;
        let y_coef = (self.y_coef * precision_scale as f32) as i32;
        let g_coef_1 = (self.g_coeff_1 * precision_scale as f32) as i32;
        let g_coef_2 = (self.g_coeff_2 * precision_scale as f32) as i32;
        CbCrInverseTransform::<i32> {
            y_coef,
            cr_coef,
            cb_coef,
            g_coeff_1: g_coef_1,
            g_coeff_2: g_coef_2,
        }
    }
}

#[derive(Copy, Clone, Debug)]
struct ErrorSize {
    expected: usize,
    received: usize,
}

#[derive(Copy, Clone, Debug)]
enum PlaneDefinition {
    Y,
    U,
    V,
}

impl Display for PlaneDefinition {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            PlaneDefinition::Y => f.write_str("Luma"),
            PlaneDefinition::U => f.write_str("U chroma"),
            PlaneDefinition::V => f.write_str("V chroma"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum YuvConversionError {
    YuvPlaneSizeMismatch(PlaneDefinition, ErrorSize),
    RgbDestinationSizeMismatch(ErrorSize),
}

impl Display for YuvConversionError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            YuvConversionError::YuvPlaneSizeMismatch(plane, error_size) => {
                f.write_fmt(format_args!(
                    "For plane {} expected size is {} but was received {}",
                    plane, error_size.received, error_size.expected,
                ))
            }
            YuvConversionError::RgbDestinationSizeMismatch(error_size) => {
                f.write_fmt(format_args!(
                    "For RGB destination expected size is {} but was received {}",
                    error_size.received, error_size.expected,
                ))
            }
        }
    }
}

impl std::error::Error for YuvConversionError {}

#[inline]
fn check_yuv_plane_preconditions<V>(
    plane: &[V],
    plane_definition: PlaneDefinition,
    stride: usize,
    height: usize,
) -> Result<(), ImageError> {
    if plane.len() != stride * height {
        return Err(ImageError::Decoding(DecodingError::new(
            ImageFormat::Avif.into(),
            YuvConversionError::YuvPlaneSizeMismatch(
                plane_definition,
                ErrorSize {
                    expected: stride * height,
                    received: plane.len(),
                },
            ),
        )));
    }
    Ok(())
}

#[inline]
fn check_rgb_preconditions<V>(
    rgb_data: &[V],
    stride: usize,
    height: usize,
) -> Result<(), ImageError> {
    if rgb_data.len() != stride * height {
        return Err(ImageError::Decoding(DecodingError::new(
            ImageFormat::Avif.into(),
            YuvConversionError::RgbDestinationSizeMismatch(ErrorSize {
                expected: stride * height,
                received: rgb_data.len(),
            }),
        )));
    }
    Ok(())
}

/// Transformation YUV to RGB with coefficients as specified in [ITU-R](https://www.itu.int/rec/T-REC-H.273/en)
fn get_inverse_transform(
    range_bgra: u32,
    range_y: u32,
    range_uv: u32,
    kr: f32,
    kb: f32,
    precision: u32,
) -> CbCrInverseTransform<i32> {
    let range_uv = range_bgra as f32 / range_uv as f32;
    let y_coef = range_bgra as f32 / range_y as f32;
    let cr_coeff = (2f32 * (1f32 - kr)) * range_uv;
    let cb_coeff = (2f32 * (1f32 - kb)) * range_uv;
    let kg = 1.0f32 - kr - kb;
    assert_ne!(kg, 0., "1.0f - kr - kg must not be 0");
    let g_coeff_1 = (2f32 * ((1f32 - kr) * kr / kg)) * range_uv;
    let g_coeff_2 = (2f32 * ((1f32 - kb) * kb / kg)) * range_uv;
    let exact_transform = CbCrInverseTransform {
        y_coef,
        cr_coef: cr_coeff,
        cb_coef: cb_coeff,
        g_coeff_1,
        g_coeff_2,
    };
    exact_transform.to_integers(precision)
}

#[derive(Debug, Copy, Clone, PartialOrd, PartialEq)]
/// Declares YUV range TV (limited) or PC (full),
/// more info [ITU-R](https://www.itu.int/rec/T-REC-H.273/en)
pub(crate) enum YuvIntensityRange {
    /// Limited range Y ∈ [16 << (depth - 8), 16 << (depth - 8) + 224 << (depth - 8)],
    /// UV ∈ [-1 << (depth - 1), -1 << (depth - 1) + 1 << (depth - 1)]
    Tv,
    /// Full range Y ∈ [0, 2^bit_depth - 1],
    /// UV ∈ [-1 << (depth - 1), -1 << (depth - 1) + 2^bit_depth - 1]
    Pc,
}

#[derive(Debug, Copy, Clone, PartialOrd, PartialEq)]
struct YuvChromaRange {
    bias_y: u32,
    bias_uv: u32,
    range_y: u32,
    range_uv: u32,
    range: YuvIntensityRange,
}

impl YuvIntensityRange {
    const fn get_yuv_range(self, depth: u32) -> YuvChromaRange {
        match self {
            YuvIntensityRange::Tv => YuvChromaRange {
                bias_y: 16 << (depth - 8),
                bias_uv: 1 << (depth - 1),
                range_y: 219 << (depth - 8),
                range_uv: 224 << (depth - 8),
                range: self,
            },
            YuvIntensityRange::Pc => YuvChromaRange {
                bias_y: 0,
                bias_uv: 1 << (depth - 1),
                range_uv: (1 << depth) - 1,
                range_y: (1 << depth) - 1,
                range: self,
            },
        }
    }
}

#[derive(Debug, Copy, Clone, PartialOrd, PartialEq)]
/// Declares standard prebuilt YUV conversion matrices,
/// check [ITU-R](https://www.itu.int/rec/T-REC-H.273/en) information for more info
pub(crate) enum YuvStandardMatrix {
    Bt601,
    Bt709,
    Bt2020,
    Smpte240,
    Bt470_6,
    Identity,
}

#[derive(Debug, Copy, Clone, PartialOrd, PartialEq)]
struct YuvBias {
    kr: f32,
    kb: f32,
}

impl YuvStandardMatrix {
    const fn get_kr_kb(self) -> YuvBias {
        match self {
            YuvStandardMatrix::Bt601 => YuvBias {
                kr: 0.299f32,
                kb: 0.114f32,
            },
            YuvStandardMatrix::Bt709 => YuvBias {
                kr: 0.2126f32,
                kb: 0.0722f32,
            },
            YuvStandardMatrix::Bt2020 => YuvBias {
                kr: 0.2627f32,
                kb: 0.0593f32,
            },
            YuvStandardMatrix::Smpte240 => YuvBias {
                kr: 0.087f32,
                kb: 0.212f32,
            },
            YuvStandardMatrix::Bt470_6 => YuvBias {
                kr: 0.2220f32,
                kb: 0.0713f32,
            },
            YuvStandardMatrix::Identity => unreachable!(),
        }
    }
}

pub(crate) struct YuvPlanarImage<'a, T> {
    pub(crate) y_plane: &'a [T],
    pub(crate) y_stride: usize,
    pub(crate) u_plane: &'a [T],
    pub(crate) u_stride: usize,
    pub(crate) v_plane: &'a [T],
    pub(crate) v_stride: usize,
    pub(crate) width: usize,
    pub(crate) height: usize,
}

#[inline(always)]
/// Saturating rounding shift right against bit depth
fn qrshr<const PRECISION: i32, const BIT_DEPTH: usize>(val: i32) -> i32 {
    let rounding: i32 = 1 << (PRECISION - 1);
    let max_value: i32 = (1 << BIT_DEPTH) - 1;
    ((val + rounding) >> PRECISION).clamp(0, max_value)
}

/// Converts Yuv 400 planar format 8 bit to Rgba 8 bit
///
/// # Arguments
///
/// * `image`: see [YuvGrayImage]
/// * `rgba`: RGBA image layout
/// * `range`: see [YuvIntensityRange]
/// * `matrix`: see [YuvStandardMatrix]
///
pub(crate) fn yuv400_to_rgba8(
    image: YuvPlanarImage<u8>,
    rgba: &mut [u8],
    range: YuvIntensityRange,
    matrix: YuvStandardMatrix,
) -> Result<(), ImageError> {
    yuv400_to_rgbx_impl::<u8, 4, 8>(image, rgba, range, matrix)
}

/// Converts Yuv 400 planar format 10 bit to Rgba 10 bit
///
/// Stride here is not supported as it can be in passed from FFI.
///
/// # Arguments
///
/// * `image`: see [YuvGrayImage]
/// * `rgba`: RGBA image layout
/// * `range`: see [YuvIntensityRange]
/// * `matrix`: see [YuvStandardMatrix]
///
pub(crate) fn yuv400_to_rgba10(
    image: YuvPlanarImage<u16>,
    rgba: &mut [u16],
    range: YuvIntensityRange,
    matrix: YuvStandardMatrix,
) -> Result<(), ImageError> {
    yuv400_to_rgbx_impl::<u16, 4, 10>(image, rgba, range, matrix)
}

/// Converts Yuv 400 planar format 12 bit to Rgba 12 bit
///
/// Stride here is not supported as it can be in passed from FFI.
///
/// # Arguments
///
/// * `image`: see [YuvGrayImage]
/// * `rgba`: RGBA image layout
/// * `range`: see [YuvIntensityRange]
/// * `matrix`: see [YuvStandardMatrix]
///
pub(crate) fn yuv400_to_rgba12(
    image: YuvPlanarImage<u16>,
    rgba: &mut [u16],
    range: YuvIntensityRange,
    matrix: YuvStandardMatrix,
) -> Result<(), ImageError> {
    yuv400_to_rgbx_impl::<u16, 4, 12>(image, rgba, range, matrix)
}

/// Converts Yuv 400 planar format to Rgba
///
/// Stride here is not supported as it can be in passed from FFI.
///
/// # Arguments
///
/// * `image`: see [YuvGrayImage]
/// * `rgba`: RGBA image layout
/// * `range`: see [YuvIntensityRange]
/// * `matrix`: see [YuvStandardMatrix]
///
#[inline]
fn yuv400_to_rgbx_impl<
    V: Copy + AsPrimitive<i32> + 'static + Sized,
    const CHANNELS: usize,
    const BIT_DEPTH: usize,
>(
    image: YuvPlanarImage<V>,
    rgba: &mut [V],
    range: YuvIntensityRange,
    matrix: YuvStandardMatrix,
) -> Result<(), ImageError>
where
    i32: AsPrimitive<V>,
{
    assert!(
        CHANNELS == 3 || CHANNELS == 4,
        "YUV 4:0:0 -> RGB is implemented only on 3 and 4 channels"
    );
    assert!(
        (8..=16).contains(&BIT_DEPTH),
        "Invalid bit depth is provided"
    );
    assert!(
        if BIT_DEPTH > 8 {
            size_of::<V>() == 2
        } else {
            size_of::<V>() == 1
        },
        "Unsupported bit depth and data type combination"
    );
    assert_ne!(
        matrix,
        YuvStandardMatrix::Identity,
        "Identity matrix cannot be used on 4:0:0"
    );

    let y_plane = image.y_plane;
    let y_stride = image.y_stride;
    let height = image.height;
    let width = image.width;

    check_yuv_plane_preconditions(y_plane, PlaneDefinition::Y, y_stride, height)?;
    check_rgb_preconditions(rgba, width * CHANNELS, height)?;

    let rgba_stride = width * CHANNELS;

    let max_value = (1 << BIT_DEPTH) - 1;

    // If luma plane is in full range it can be just redistributed across the image
    if range == YuvIntensityRange::Pc {
        let y_iter = y_plane.chunks_exact(y_stride);
        let rgb_iter = rgba.chunks_exact_mut(rgba_stride);

        // All branches on generic const will be optimized out.
        for (y_src, rgb) in y_iter.zip(rgb_iter) {
            let rgb_chunks = rgb.chunks_exact_mut(CHANNELS);

            for (y_src, rgb_dst) in y_src.iter().zip(rgb_chunks) {
                let r = *y_src;
                rgb_dst[0] = r;
                rgb_dst[1] = r;
                rgb_dst[2] = r;
                if CHANNELS == 4 {
                    rgb_dst[3] = max_value.as_();
                }
            }
        }
        return Ok(());
    }

    let range = range.get_yuv_range(BIT_DEPTH as u32);
    let kr_kb = matrix.get_kr_kb();
    const PRECISION: i32 = 11;

    let inverse_transform = get_inverse_transform(
        (1 << BIT_DEPTH) - 1,
        range.range_y,
        range.range_uv,
        kr_kb.kr,
        kr_kb.kb,
        PRECISION as u32,
    );
    let y_coef = inverse_transform.y_coef;

    let bias_y = range.bias_y as i32;

    let y_iter = y_plane.chunks_exact(y_stride);
    let rgb_iter = rgba.chunks_exact_mut(rgba_stride);

    // All branches on generic const will be optimized out.
    for (y_src, rgb) in y_iter.zip(rgb_iter) {
        let rgb_chunks = rgb.chunks_exact_mut(CHANNELS);

        for (y_src, rgb_dst) in y_src.iter().zip(rgb_chunks) {
            let y_value = (y_src.as_() - bias_y) * y_coef;

            let r = qrshr::<PRECISION, BIT_DEPTH>(y_value);
            rgb_dst[0] = r.as_();
            rgb_dst[1] = r.as_();
            rgb_dst[2] = r.as_();
            if CHANNELS == 4 {
                rgb_dst[3] = max_value.as_();
            }
        }
    }

    Ok(())
}

/// Converts YUV420 8 bit-depth to Rgba 8 bit
///
/// # Arguments
///
/// * `image`: see [YuvPlanarImage]
/// * `rgb`: RGB image layout
/// * `range`: see [YuvIntensityRange]
/// * `matrix`: see [YuvStandardMatrix]
///
pub(crate) fn yuv420_to_rgba8(
    image: YuvPlanarImage<u8>,
    rgb: &mut [u8],
    range: YuvIntensityRange,
    matrix: YuvStandardMatrix,
) -> Result<(), ImageError> {
    yuv420_to_rgbx::<u8, 4, 8>(image, rgb, range, matrix)
}

/// Converts YUV420 10 bit-depth to Rgba 10 bit-depth
///
/// Stride here is not supported as it can be in passed from FFI.
///
/// # Arguments
///
/// * `image`: see [YuvPlanarImage]
/// * `rgb`: RGB image layout
/// * `range`: see [YuvIntensityRange]
/// * `matrix`: see [YuvStandardMatrix]
///
pub(crate) fn yuv420_to_rgba10(
    image: YuvPlanarImage<u16>,
    rgb: &mut [u16],
    range: YuvIntensityRange,
    matrix: YuvStandardMatrix,
) -> Result<(), ImageError> {
    yuv420_to_rgbx::<u16, 4, 10>(image, rgb, range, matrix)
}

/// Converts YUV420 12 bit-depth to Rgba 12 bit-depth
///
/// Stride here is not supported as it can be in passed from FFI.
///
/// # Arguments
///
/// * `image`: see [YuvPlanarImage]
/// * `rgb`: RGB image layout
/// * `range`: see [YuvIntensityRange]
/// * `matrix`: see [YuvStandardMatrix]
///
pub(crate) fn yuv420_to_rgba12(
    image: YuvPlanarImage<u16>,
    rgb: &mut [u16],
    range: YuvIntensityRange,
    matrix: YuvStandardMatrix,
) -> Result<(), ImageError> {
    yuv420_to_rgbx::<u16, 4, 12>(image, rgb, range, matrix)
}

#[inline]
fn process_halved_chroma_row<
    V: Copy + AsPrimitive<i32> + 'static + Sized,
    const PRECISION: i32,
    const CHANNELS: usize,
    const BIT_DEPTH: usize,
>(
    image: YuvPlanarImage<V>,
    rgba: &mut [V],
    transform: &CbCrInverseTransform<i32>,
    range: &YuvChromaRange,
) where
    i32: AsPrimitive<V>,
{
    let cr_coef = transform.cr_coef;
    let cb_coef = transform.cb_coef;
    let y_coef = transform.y_coef;
    let g_coef_1 = transform.g_coeff_1;
    let g_coef_2 = transform.g_coeff_2;

    let max_value = (1 << BIT_DEPTH) - 1;

    // If the stride is larger than the plane size,
    // it might contain junk data beyond the actual valid region.
    // To avoid processing artifacts when working with odd-sized images,
    // the buffer is reshaped to its actual size,
    // preventing accidental use of invalid values from the trailing region.

    let y_plane = &image.y_plane[0..image.width];
    let chroma_size = (image.width + 1) / 2;
    let u_plane = &image.u_plane[0..chroma_size];
    let v_plane = &image.v_plane[0..chroma_size];
    let rgba = &mut rgba[0..image.width * CHANNELS];

    let bias_y = range.bias_y as i32;
    let bias_uv = range.bias_uv as i32;
    let y_iter = y_plane.chunks_exact(2);
    let rgb_chunks = rgba.chunks_exact_mut(CHANNELS * 2);
    for (((y_src, &u_src), &v_src), rgb_dst) in y_iter.zip(u_plane).zip(v_plane).zip(rgb_chunks) {
        let y_value: i32 = (y_src[0].as_() - bias_y) * y_coef;
        let cb_value: i32 = u_src.as_() - bias_uv;
        let cr_value: i32 = v_src.as_() - bias_uv;

        let r = qrshr::<PRECISION, BIT_DEPTH>(y_value + cr_coef * cr_value);
        let b = qrshr::<PRECISION, BIT_DEPTH>(y_value + cb_coef * cb_value);
        let g = qrshr::<PRECISION, BIT_DEPTH>(y_value - g_coef_1 * cr_value - g_coef_2 * cb_value);

        if CHANNELS == 4 {
            rgb_dst[0] = r.as_();
            rgb_dst[1] = g.as_();
            rgb_dst[2] = b.as_();
            rgb_dst[3] = max_value.as_();
        } else if CHANNELS == 3 {
            rgb_dst[0] = r.as_();
            rgb_dst[1] = g.as_();
            rgb_dst[2] = b.as_();
        } else {
            unreachable!();
        }

        let y_value = (y_src[1].as_() - bias_y) * y_coef;

        let r = qrshr::<PRECISION, BIT_DEPTH>(y_value + cr_coef * cr_value);
        let b = qrshr::<PRECISION, BIT_DEPTH>(y_value + cb_coef * cb_value);
        let g = qrshr::<PRECISION, BIT_DEPTH>(y_value - g_coef_1 * cr_value - g_coef_2 * cb_value);

        if CHANNELS == 4 {
            rgb_dst[4] = r.as_();
            rgb_dst[5] = g.as_();
            rgb_dst[6] = b.as_();
            rgb_dst[7] = max_value.as_();
        } else if CHANNELS == 3 {
            rgb_dst[3] = r.as_();
            rgb_dst[4] = g.as_();
            rgb_dst[5] = b.as_();
        } else {
            unreachable!();
        }
    }

    // Process remainder if width is odd.
    if image.width & 1 != 0 {
        let y_left = y_plane.chunks_exact(2).remainder();
        let rgb_chunks = rgba
            .chunks_exact_mut(CHANNELS * 2)
            .into_remainder()
            .chunks_exact_mut(CHANNELS);
        let u_iter = u_plane.iter().rev();
        let v_iter = v_plane.iter().rev();

        for (((y_src, u_src), v_src), rgb_dst) in
            y_left.iter().zip(u_iter).zip(v_iter).zip(rgb_chunks)
        {
            let y_value = (y_src.as_() - bias_y) * y_coef;
            let cb_value = u_src.as_() - bias_uv;
            let cr_value = v_src.as_() - bias_uv;

            let r = qrshr::<PRECISION, BIT_DEPTH>(y_value + cr_coef * cr_value);
            let b = qrshr::<PRECISION, BIT_DEPTH>(y_value + cb_coef * cb_value);
            let g =
                qrshr::<PRECISION, BIT_DEPTH>(y_value - g_coef_1 * cr_value - g_coef_2 * cb_value);

            if CHANNELS == 4 {
                rgb_dst[0] = r.as_();
                rgb_dst[1] = g.as_();
                rgb_dst[2] = b.as_();
                rgb_dst[3] = max_value.as_();
            } else if CHANNELS == 3 {
                rgb_dst[0] = r.as_();
                rgb_dst[1] = g.as_();
                rgb_dst[2] = b.as_();
            } else {
                unreachable!();
            }
        }
    }
}

/// Converts YUV420 to Rgba
///
/// Stride here is not supported as it can be in passed from FFI.
///
/// # Arguments
///
/// * `image`: see [YuvPlanarImage]
/// * `rgb`: RGB image layout
/// * `range`: see [YuvIntensityRange]
/// * `matrix`: see [YuvStandardMatrix]
///
#[inline]
fn yuv420_to_rgbx<
    V: Copy + AsPrimitive<i32> + 'static + Sized,
    const CHANNELS: usize,
    const BIT_DEPTH: usize,
>(
    image: YuvPlanarImage<V>,
    rgb: &mut [V],
    range: YuvIntensityRange,
    matrix: YuvStandardMatrix,
) -> Result<(), ImageError>
where
    i32: AsPrimitive<V>,
{
    assert!(
        CHANNELS == 3 || CHANNELS == 4,
        "YUV 4:2:0 -> RGB is implemented only on 3 and 4 channels"
    );
    assert!(
        (8..=16).contains(&BIT_DEPTH),
        "Invalid bit depth is provided"
    );
    assert!(
        if BIT_DEPTH > 8 {
            size_of::<V>() == 2
        } else {
            size_of::<V>() == 1
        },
        "Unsupported bit depth and data type combination"
    );
    assert_ne!(
        matrix,
        YuvStandardMatrix::Identity,
        "Identity matrix cannot be used on 4:2:0"
    );
    let y_plane = image.y_plane;
    let u_plane = image.u_plane;
    let v_plane = image.v_plane;
    let y_stride = image.y_stride;
    let u_stride = image.u_stride;
    let v_stride = image.v_stride;
    let chroma_height = (image.height + 1) / 2;

    check_yuv_plane_preconditions(y_plane, PlaneDefinition::Y, y_stride, image.height)?;
    check_yuv_plane_preconditions(u_plane, PlaneDefinition::U, u_stride, chroma_height)?;
    check_yuv_plane_preconditions(v_plane, PlaneDefinition::V, v_stride, chroma_height)?;

    check_rgb_preconditions(rgb, image.width * CHANNELS, image.height)?;

    const PRECISION: i32 = 11;

    let range = range.get_yuv_range(BIT_DEPTH as u32);
    let kr_kb = matrix.get_kr_kb();
    let inverse_transform = get_inverse_transform(
        (1 << BIT_DEPTH) - 1,
        range.range_y,
        range.range_uv,
        kr_kb.kr,
        kr_kb.kb,
        PRECISION as u32,
    );

    let rgb_stride = image.width * CHANNELS;

    let y_iter = y_plane.chunks_exact(y_stride * 2);
    let rgb_iter = rgb.chunks_exact_mut(rgb_stride * 2);
    let u_iter = u_plane.chunks_exact(u_stride);
    let v_iter = v_plane.chunks_exact(v_stride);

    /*
       Sample 4x4 YUV420 planar image
       start_y + 0:  Y00 Y01 Y02 Y03
       start_y + 4:  Y04 Y05 Y06 Y07
       start_y + 8:  Y08 Y09 Y10 Y11
       start_y + 12: Y12 Y13 Y14 Y15
       start_cb + 0: Cb00 Cb01
       start_cb + 2: Cb02 Cb03
       start_cr + 0: Cr00 Cr01
       start_cr + 2: Cr02 Cr03

       For 4 luma components (2x2 on rows and cols) there are 1 chroma Cb/Cr components.
       Luma channel must have always exact size as RGB target layout, but chroma is not.

       We're sectioning an image by pair of rows, then for each pair of luma and RGB row,
       there is one chroma row.

       As chroma is shrunk by factor of 2 then we're processing by pairs of RGB and luma,
       for each RGB and luma pair there is one chroma component.

       If image have odd width then luma channel must be exact, and we're replicating last
       chroma component.

       If image have odd height then luma channel is exact, and we're replicating last chroma rows.
    */

    // All branches on generic const will be optimized out.
    for (((y_src, u_src), v_src), rgb) in y_iter.zip(u_iter).zip(v_iter).zip(rgb_iter) {
        // Since we're processing two rows in one loop we need to re-slice once more
        let y_iter = y_src.chunks_exact(y_stride);
        let rgb_iter = rgb.chunks_exact_mut(rgb_stride);
        for (y_src, rgba) in y_iter.zip(rgb_iter) {
            let image = YuvPlanarImage {
                y_plane: y_src,
                y_stride: 0,
                u_plane: u_src,
                u_stride: 0,
                v_plane: v_src,
                v_stride: 0,
                width: image.width,
                height: image.height,
            };
            process_halved_chroma_row::<V, PRECISION, CHANNELS, BIT_DEPTH>(
                image,
                rgba,
                &inverse_transform,
                &range,
            );
        }
    }

    // Process remainder if height is odd

    let y_iter = y_plane
        .chunks_exact(y_stride * 2)
        .remainder()
        .chunks_exact(y_stride);
    let rgb_iter = rgb.chunks_exact_mut(rgb_stride).rev();
    let u_iter = u_plane.chunks_exact(u_stride).rev();
    let v_iter = v_plane.chunks_exact(v_stride).rev();

    for (((y_src, u_src), v_src), rgba) in y_iter.zip(u_iter).zip(v_iter).zip(rgb_iter) {
        let image = YuvPlanarImage {
            y_plane: y_src,
            y_stride: 0,
            u_plane: u_src,
            u_stride: 0,
            v_plane: v_src,
            v_stride: 0,
            width: image.width,
            height: image.height,
        };
        process_halved_chroma_row::<V, PRECISION, CHANNELS, BIT_DEPTH>(
            image,
            rgba,
            &inverse_transform,
            &range,
        );
    }

    Ok(())
}

/// Converts Yuv 422 8-bit planar format to Rgba 8-bit
///
/// # Arguments
///
/// * `image`: see [YuvPlanarImage]
/// * `rgb`: RGB image layout
/// * `range`: see [YuvIntensityRange]
/// * `matrix`: see [YuvStandardMatrix]
///
pub(crate) fn yuv422_to_rgba8(
    image: YuvPlanarImage<u8>,
    rgb: &mut [u8],
    range: YuvIntensityRange,
    matrix: YuvStandardMatrix,
) -> Result<(), ImageError> {
    yuv422_to_rgbx_impl::<u8, 4, 8>(image, rgb, range, matrix)
}

/// Converts Yuv 422 10-bit planar format to Rgba 10-bit
///
/// Stride here is not supported as it can be in passed from FFI.
///
/// # Arguments
///
/// * `image`: see [YuvPlanarImage]
/// * `rgb`: RGB image layout
/// * `range`: see [YuvIntensityRange]
/// * `matrix`: see [YuvStandardMatrix]
///
pub(crate) fn yuv422_to_rgba10(
    image: YuvPlanarImage<u16>,
    rgb: &mut [u16],
    range: YuvIntensityRange,
    matrix: YuvStandardMatrix,
) -> Result<(), ImageError> {
    yuv422_to_rgbx_impl::<u16, 4, 10>(image, rgb, range, matrix)
}

/// Converts Yuv 422 12-bit planar format to Rgba 12-bit
///
/// Stride here is not supported as it can be in passed from FFI.
///
/// # Arguments
///
/// * `image`: see [YuvPlanarImage]
/// * `rgb`: RGB image layout
/// * `range`: see [YuvIntensityRange]
/// * `matrix`: see [YuvStandardMatrix]
///
pub(crate) fn yuv422_to_rgba12(
    image: YuvPlanarImage<u16>,
    rgb: &mut [u16],
    range: YuvIntensityRange,
    matrix: YuvStandardMatrix,
) -> Result<(), ImageError> {
    yuv422_to_rgbx_impl::<u16, 4, 12>(image, rgb, range, matrix)
}

/// Converts Yuv 422 planar format to Rgba
///
/// Stride here is not supports u16 as it can be in passed from FFI.
///
/// # Arguments
///
/// * `image`: see [YuvPlanarImage]
/// * `rgb`: RGB image layout
/// * `range`: see [YuvIntensityRange]
/// * `matrix`: see [YuvStandardMatrix]
///
fn yuv422_to_rgbx_impl<
    V: Copy + AsPrimitive<i32> + 'static + Sized,
    const CHANNELS: usize,
    const BIT_DEPTH: usize,
>(
    image: YuvPlanarImage<V>,
    rgb: &mut [V],
    range: YuvIntensityRange,
    matrix: YuvStandardMatrix,
) -> Result<(), ImageError>
where
    i32: AsPrimitive<V>,
{
    assert!(
        CHANNELS == 3 || CHANNELS == 4,
        "YUV 4:2:2 -> RGB is implemented only on 3 and 4 channels"
    );
    assert!(
        (8..=16).contains(&BIT_DEPTH),
        "Invalid bit depth is provided"
    );
    assert!(
        if BIT_DEPTH > 8 {
            size_of::<V>() == 2
        } else {
            size_of::<V>() == 1
        },
        "Unsupported bit depth and data type combination"
    );
    assert_ne!(
        matrix,
        YuvStandardMatrix::Identity,
        "Identity matrix cannot be used on 4:2:2"
    );
    let y_plane = image.y_plane;
    let u_plane = image.u_plane;
    let v_plane = image.v_plane;
    let y_stride = image.y_stride;
    let u_stride = image.u_stride;
    let v_stride = image.v_stride;
    let width = image.width;

    check_yuv_plane_preconditions(y_plane, PlaneDefinition::Y, y_stride, image.height)?;
    check_yuv_plane_preconditions(u_plane, PlaneDefinition::U, u_stride, image.height)?;
    check_yuv_plane_preconditions(v_plane, PlaneDefinition::V, v_stride, image.height)?;

    check_rgb_preconditions(rgb, image.width * CHANNELS, image.height)?;

    let range = range.get_yuv_range(BIT_DEPTH as u32);
    let kr_kb = matrix.get_kr_kb();
    const PRECISION: i32 = 11;

    let inverse_transform = get_inverse_transform(
        (1 << BIT_DEPTH) - 1,
        range.range_y,
        range.range_uv,
        kr_kb.kr,
        kr_kb.kb,
        PRECISION as u32,
    );

    /*
       Sample 4x4 YUV422 planar image
       start_y + 0:  Y00 Y01 Y02 Y03
       start_y + 4:  Y04 Y05 Y06 Y07
       start_y + 8:  Y08 Y09 Y10 Y11
       start_y + 12: Y12 Y13 Y14 Y15
       start_cb + 0: Cb00 Cb01
       start_cb + 2: Cb02 Cb03
       start_cb + 4: Cb04 Cb05
       start_cb + 6: Cb06 Cb07
       start_cr + 0: Cr00 Cr01
       start_cr + 2: Cr02 Cr03
       start_cr + 4: Cr04 Cr05
       start_cr + 6: Cr06 Cr07

       For 2 luma components there are 1 chroma Cb/Cr components.
       Luma channel must have always exact size as RGB target layout, but chroma is not.

       As chroma is shrunk by factor of 2 then we're processing by pairs of RGB and luma,
       for each RGB and luma pair there is one chroma component.

       If image have odd width then luma channel must be exact, and we're replicating last
       chroma component.
    */

    let rgb_stride = width * CHANNELS;

    let y_iter = y_plane.chunks_exact(y_stride);
    let rgb_iter = rgb.chunks_exact_mut(rgb_stride);
    let u_iter = u_plane.chunks_exact(u_stride);
    let v_iter = v_plane.chunks_exact(v_stride);

    // All branches on generic const will be optimized out.
    for (((y_src, u_src), v_src), rgba) in y_iter.zip(u_iter).zip(v_iter).zip(rgb_iter) {
        let image = YuvPlanarImage {
            y_plane: y_src,
            y_stride: 0,
            u_plane: u_src,
            u_stride: 0,
            v_plane: v_src,
            v_stride: 0,
            width: image.width,
            height: image.height,
        };
        process_halved_chroma_row::<V, PRECISION, CHANNELS, BIT_DEPTH>(
            image,
            rgba,
            &inverse_transform,
            &range,
        );
    }

    Ok(())
}

/// Converts Yuv 444 planar format 8 bit-depth to Rgba 8 bit
///
/// # Arguments
///
/// * `image`: see [YuvPlanarImage]
/// * `rgba`: RGB image layout
/// * `range`: see [YuvIntensityRange]
/// * `matrix`: see [YuvStandardMatrix]
///
pub(crate) fn yuv444_to_rgba8(
    image: YuvPlanarImage<u8>,
    rgba: &mut [u8],
    range: YuvIntensityRange,
    matrix: YuvStandardMatrix,
) -> Result<(), ImageError> {
    if matrix == YuvStandardMatrix::Identity {
        gbr_to_rgba8(image, rgba, range)
    } else {
        yuv444_to_rgbx_impl::<u8, 4, 8>(image, rgba, range, matrix)
    }
}

/// Converts Yuv 444 planar format 10 bit-depth to Rgba 10 bit
///
/// Stride here is not supports u16 as it can be in passed from FFI.
///
/// # Arguments
///
/// * `image`: see [YuvPlanarImage]
/// * `rgba`: RGB image layout
/// * `range`: see [YuvIntensityRange]
/// * `matrix`: see [YuvStandardMatrix]
///
pub(super) fn yuv444_to_rgba10(
    image: YuvPlanarImage<u16>,
    rgba: &mut [u16],
    range: YuvIntensityRange,
    matrix: YuvStandardMatrix,
) -> Result<(), ImageError> {
    if matrix == YuvStandardMatrix::Identity {
        gbr_to_rgba10(image, rgba, range)
    } else {
        yuv444_to_rgbx_impl::<u16, 4, 10>(image, rgba, range, matrix)
    }
}

/// Converts Yuv 444 planar format 12 bit-depth to Rgba 12 bit
///
/// Stride here is not supports u16 as it can be in passed from FFI.
///
/// # Arguments
///
/// * `image`: see [YuvPlanarImage]
/// * `rgba`: RGB image layout
/// * `range`: see [YuvIntensityRange]
/// * `matrix`: see [YuvStandardMatrix]
///
pub(super) fn yuv444_to_rgba12(
    image: YuvPlanarImage<u16>,
    rgba: &mut [u16],
    range: YuvIntensityRange,
    matrix: YuvStandardMatrix,
) -> Result<(), ImageError> {
    if matrix == YuvStandardMatrix::Identity {
        gbr_to_rgba12(image, rgba, range)
    } else {
        yuv444_to_rgbx_impl::<u16, 4, 12>(image, rgba, range, matrix)
    }
}

/// Converts Yuv 444 planar format to Rgba
///
/// Stride here is not supports u16 as it can be in passed from FFI.
///
/// # Arguments
///
/// * `image`: see [YuvPlanarImage]
/// * `rgba`: RGB image layout
/// * `range`: see [YuvIntensityRange]
/// * `matrix`: see [YuvStandardMatrix]
///
#[inline]
fn yuv444_to_rgbx_impl<
    V: Copy + AsPrimitive<i32> + 'static + Sized,
    const CHANNELS: usize,
    const BIT_DEPTH: usize,
>(
    image: YuvPlanarImage<V>,
    rgba: &mut [V],
    range: YuvIntensityRange,
    matrix: YuvStandardMatrix,
) -> Result<(), ImageError>
where
    i32: AsPrimitive<V>,
{
    assert!(
        CHANNELS == 3 || CHANNELS == 4,
        "YUV 4:4:4 -> RGB is implemented only on 3 and 4 channels"
    );
    assert!(
        (8..=16).contains(&BIT_DEPTH),
        "Invalid bit depth is provided"
    );
    assert!(
        if BIT_DEPTH > 8 {
            size_of::<V>() == 2
        } else {
            size_of::<V>() == 1
        },
        "Unsupported bit depth and data type combination"
    );

    let y_plane = image.y_plane;
    let u_plane = image.u_plane;
    let v_plane = image.v_plane;
    let y_stride = image.y_stride;
    let u_stride = image.u_stride;
    let v_stride = image.v_stride;
    let height = image.height;
    let width = image.width;

    check_yuv_plane_preconditions(y_plane, PlaneDefinition::Y, y_stride, height)?;
    check_yuv_plane_preconditions(u_plane, PlaneDefinition::U, u_stride, height)?;
    check_yuv_plane_preconditions(v_plane, PlaneDefinition::V, v_stride, height)?;

    check_rgb_preconditions(rgba, image.width * CHANNELS, height)?;

    let range = range.get_yuv_range(BIT_DEPTH as u32);
    let kr_kb = matrix.get_kr_kb();
    const PRECISION: i32 = 11;

    let inverse_transform = get_inverse_transform(
        (1 << BIT_DEPTH) - 1,
        range.range_y,
        range.range_uv,
        kr_kb.kr,
        kr_kb.kb,
        PRECISION as u32,
    );
    let cr_coef = inverse_transform.cr_coef;
    let cb_coef = inverse_transform.cb_coef;
    let y_coef = inverse_transform.y_coef;
    let g_coef_1 = inverse_transform.g_coeff_1;
    let g_coef_2 = inverse_transform.g_coeff_2;

    let bias_y = range.bias_y as i32;
    let bias_uv = range.bias_uv as i32;

    let max_value = (1 << BIT_DEPTH) - 1;

    let rgb_stride = width * CHANNELS;

    let y_iter = y_plane.chunks_exact(y_stride);
    let rgb_iter = rgba.chunks_exact_mut(rgb_stride);
    let u_iter = u_plane.chunks_exact(u_stride);
    let v_iter = v_plane.chunks_exact(v_stride);

    // All branches on generic const will be optimized out.
    for (((y_src, u_src), v_src), rgb) in y_iter.zip(u_iter).zip(v_iter).zip(rgb_iter) {
        let rgb_chunks = rgb.chunks_exact_mut(CHANNELS);

        for (((y_src, u_src), v_src), rgb_dst) in y_src.iter().zip(u_src).zip(v_src).zip(rgb_chunks)
        {
            let y_value = (y_src.as_() - bias_y) * y_coef;
            let cb_value = u_src.as_() - bias_uv;
            let cr_value = v_src.as_() - bias_uv;

            let r = qrshr::<PRECISION, BIT_DEPTH>(y_value + cr_coef * cr_value);
            let b = qrshr::<PRECISION, BIT_DEPTH>(y_value + cb_coef * cb_value);
            let g =
                qrshr::<PRECISION, BIT_DEPTH>(y_value - g_coef_1 * cr_value - g_coef_2 * cb_value);

            if CHANNELS == 4 {
                rgb_dst[0] = r.as_();
                rgb_dst[1] = g.as_();
                rgb_dst[2] = b.as_();
                rgb_dst[3] = max_value.as_();
            } else if CHANNELS == 3 {
                rgb_dst[0] = r.as_();
                rgb_dst[1] = g.as_();
                rgb_dst[2] = b.as_();
            } else {
                unreachable!();
            }
        }
    }

    Ok(())
}

/// Converts Gbr 8 bit planar format to Rgba 8 bit-depth
///
/// # Arguments
///
/// * `image`: see [YuvPlanarImage]
/// * `rgb`: RGB image layout
/// * `range`: see [YuvIntensityRange]
///
fn gbr_to_rgba8(
    image: YuvPlanarImage<u8>,
    rgb: &mut [u8],
    range: YuvIntensityRange,
) -> Result<(), ImageError> {
    gbr_to_rgbx_impl::<u8, 4, 8>(image, rgb, range)
}

/// Converts Gbr 10 bit planar format to Rgba 10 bit-depth
///
/// Stride here is not supported as it can be in passed from FFI.
///
/// # Arguments
///
/// * `image`: see [YuvPlanarImage]
/// * `rgba`: RGBx image layout
/// * `range`: see [YuvIntensityRange]
///
fn gbr_to_rgba10(
    image: YuvPlanarImage<u16>,
    rgba: &mut [u16],
    range: YuvIntensityRange,
) -> Result<(), ImageError> {
    gbr_to_rgbx_impl::<u16, 4, 10>(image, rgba, range)
}

/// Converts Gbr 12 bit planar format to Rgba 12 bit-depth
///
/// Stride here is not supported as it can be in passed from FFI.
///
/// # Arguments
///
/// * `image`: see [YuvPlanarImage]
/// * `rgba`: RGBx image layout
/// * `range`: see [YuvIntensityRange]
///
fn gbr_to_rgba12(
    image: YuvPlanarImage<u16>,
    rgba: &mut [u16],
    range: YuvIntensityRange,
) -> Result<(), ImageError> {
    gbr_to_rgbx_impl::<u16, 4, 12>(image, rgba, range)
}

/// Converts Gbr planar format to Rgba
///
/// Stride here is not supported as it can be in passed from FFI.
///
/// # Arguments
///
/// * `image`: see [YuvPlanarImage]
/// * `rgb`: RGB image layout
/// * `range`: see [YuvIntensityRange]
///
#[inline]
fn gbr_to_rgbx_impl<
    V: Copy + AsPrimitive<i32> + 'static + Sized,
    const CHANNELS: usize,
    const BIT_DEPTH: usize,
>(
    image: YuvPlanarImage<V>,
    rgba: &mut [V],
    yuv_range: YuvIntensityRange,
) -> Result<(), ImageError>
where
    i32: AsPrimitive<V>,
{
    assert!(
        CHANNELS == 3 || CHANNELS == 4,
        "GBR -> RGB is implemented only on 3 and 4 channels"
    );
    assert!(
        (8..=16).contains(&BIT_DEPTH),
        "Invalid bit depth is provided"
    );
    assert!(
        if BIT_DEPTH > 8 {
            size_of::<V>() == 2
        } else {
            size_of::<V>() == 1
        },
        "Unsupported bit depth and data type combination"
    );
    let y_plane = image.y_plane;
    let u_plane = image.u_plane;
    let v_plane = image.v_plane;
    let y_stride = image.y_stride;
    let u_stride = image.u_stride;
    let v_stride = image.v_stride;
    let height = image.height;
    let width = image.width;

    check_yuv_plane_preconditions(y_plane, PlaneDefinition::Y, y_stride, height)?;
    check_yuv_plane_preconditions(u_plane, PlaneDefinition::U, u_stride, height)?;
    check_yuv_plane_preconditions(v_plane, PlaneDefinition::V, v_stride, height)?;

    check_rgb_preconditions(rgba, width * CHANNELS, height)?;

    let max_value = (1 << BIT_DEPTH) - 1;

    let rgb_stride = width * CHANNELS;

    let y_iter = y_plane.chunks_exact(y_stride);
    let rgb_iter = rgba.chunks_exact_mut(rgb_stride);
    let u_iter = u_plane.chunks_exact(u_stride);
    let v_iter = v_plane.chunks_exact(v_stride);

    match yuv_range {
        YuvIntensityRange::Tv => {
            const PRECISION: i32 = 11;
            // All channels on identity should use Y range
            let range = yuv_range.get_yuv_range(BIT_DEPTH as u32);
            let range_rgba = (1 << BIT_DEPTH) - 1;
            let y_coef =
                ((range_rgba as f32 / range.range_y as f32) * (1 << PRECISION) as f32) as i32;
            let y_bias = range.bias_y as i32;

            for (((y_src, u_src), v_src), rgb) in y_iter.zip(u_iter).zip(v_iter).zip(rgb_iter) {
                let rgb_chunks = rgb.chunks_exact_mut(CHANNELS);

                for (((&y_src, &u_src), &v_src), rgb_dst) in
                    y_src.iter().zip(u_src).zip(v_src).zip(rgb_chunks)
                {
                    rgb_dst[0] =
                        qrshr::<PRECISION, BIT_DEPTH>((v_src.as_() - y_bias) * y_coef).as_();
                    rgb_dst[1] =
                        qrshr::<PRECISION, BIT_DEPTH>((y_src.as_() - y_bias) * y_coef).as_();
                    rgb_dst[2] =
                        qrshr::<PRECISION, BIT_DEPTH>((u_src.as_() - y_bias) * y_coef).as_();
                    if CHANNELS == 4 {
                        rgb_dst[3] = max_value.as_();
                    }
                }
            }
        }
        YuvIntensityRange::Pc => {
            for (((y_src, u_src), v_src), rgb) in y_iter.zip(u_iter).zip(v_iter).zip(rgb_iter) {
                let rgb_chunks = rgb.chunks_exact_mut(CHANNELS);

                for (((&y_src, &u_src), &v_src), rgb_dst) in
                    y_src.iter().zip(u_src).zip(v_src).zip(rgb_chunks)
                {
                    rgb_dst[0] = v_src;
                    rgb_dst[1] = y_src;
                    rgb_dst[2] = u_src;
                    if CHANNELS == 4 {
                        rgb_dst[3] = max_value.as_();
                    }
                }
            }
        }
    }

    Ok(())
}
