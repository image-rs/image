use num_traits::AsPrimitive;

#[derive(Debug, Copy, Clone)]
struct CbCrInverseTransform<T> {
    pub y_coef: T,
    pub cr_coef: T,
    pub cb_coef: T,
    pub g_coeff_1: T,
    pub g_coeff_2: T,
}

impl<T> CbCrInverseTransform<T> {
    fn new(
        y_coef: T,
        cr_coef: T,
        cb_coef: T,
        g_coeff_1: T,
        g_coeff_2: T,
    ) -> CbCrInverseTransform<T> {
        CbCrInverseTransform {
            y_coef,
            cr_coef,
            cb_coef,
            g_coeff_1,
            g_coeff_2,
        }
    }
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

/// Transformation RGB to YUV with coefficients as specified in [ITU-R](https://www.itu.int/rec/T-REC-H.273/en)
fn get_inverse_transform(
    range_bgra: u32,
    range_y: u32,
    range_uv: u32,
    kr: f32,
    kb: f32,
    precision: u32,
) -> Result<CbCrInverseTransform<i32>, String> {
    let range_uv = range_bgra as f32 / range_uv as f32;
    let y_coef = range_bgra as f32 / range_y as f32;
    let cr_coeff = (2f32 * (1f32 - kr)) * range_uv;
    let cb_coeff = (2f32 * (1f32 - kb)) * range_uv;
    let kg = 1.0f32 - kr - kb;
    if kg == 0f32 {
        return Err("1.0f - kr - kg must not be 0".parse().unwrap());
    }
    let g_coeff_1 = (2f32 * ((1f32 - kr) * kr / kg)) * range_uv;
    let g_coeff_2 = (2f32 * ((1f32 - kb) * kb / kg)) * range_uv;
    let exact_transform =
        CbCrInverseTransform::new(y_coef, cr_coeff, cb_coeff, g_coeff_1, g_coeff_2);
    Ok(exact_transform.to_integers(precision))
}

#[repr(C)]
#[derive(Debug, Copy, Clone, PartialOrd, PartialEq)]
/// Declares YUV range TV (limited) or Full,
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
    pub bias_y: u32,
    pub bias_uv: u32,
    pub range_y: u32,
    pub range_uv: u32,
    pub range: YuvIntensityRange,
}

const fn get_yuv_range(depth: u32, range: YuvIntensityRange) -> YuvChromaRange {
    match range {
        YuvIntensityRange::Tv => YuvChromaRange {
            bias_y: 16 << (depth - 8),
            bias_uv: 1 << (depth - 1),
            range_y: 219 << (depth - 8),
            range_uv: 224 << (depth - 8),
            range,
        },
        YuvIntensityRange::Pc => YuvChromaRange {
            bias_y: 0,
            bias_uv: 1 << (depth - 1),
            range_uv: (1 << depth) - 1,
            range_y: (1 << depth) - 1,
            range,
        },
    }
}

#[repr(C)]
#[derive(Debug, Copy, Clone, PartialOrd, PartialEq)]
/// Declares standard prebuilt YUV conversion matrices,
/// check [ITU-R](https://www.itu.int/rec/T-REC-H.273/en) information for more info
pub(crate) enum YuvStandardMatrix {
    Bt601,
    Bt709,
    Bt2020,
    Smpte240,
    Bt470_6,
}

#[derive(Debug, Copy, Clone, PartialOrd, PartialEq)]
struct YuvBias {
    pub kr: f32,
    pub kb: f32,
}

const fn get_kr_kb(matrix: YuvStandardMatrix) -> YuvBias {
    match matrix {
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
    }
}

pub(crate) struct YuvPlanarImage<'a, T> {
    y_plane: &'a [T],
    y_stride: usize,
    u_plane: &'a [T],
    u_stride: usize,
    v_plane: &'a [T],
    v_stride: usize,
    width: usize,
    height: usize,
}

impl<'a, T> YuvPlanarImage<'a, T> {
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn new(
        y_plane: &'a [T],
        y_stride: usize,
        u_plane: &'a [T],
        u_stride: usize,
        v_plane: &'a [T],
        v_stride: usize,
        width: usize,
        height: usize,
    ) -> Self {
        YuvPlanarImage {
            y_plane,
            y_stride,
            u_plane,
            u_stride,
            v_plane,
            v_stride,
            width,
            height,
        }
    }
}

pub(crate) struct YuvGrayImage<'a, T> {
    y_plane: &'a [T],
    y_stride: usize,
    width: usize,
    height: usize,
}

impl<'a, T> YuvGrayImage<'a, T> {
    pub(crate) fn new(y_plane: &'a [T], y_stride: usize, width: usize, height: usize) -> Self {
        YuvGrayImage {
            y_plane,
            y_stride,
            width,
            height,
        }
    }
}

/// Converts Yuv 400 planar format to Rgba
///
/// This support not tightly packed data and crop image using stride in place.
///
/// # Arguments
///
/// * `y_plane`: Luma plane
/// * `y_stride`: Luma stride
/// * `u_plane`: U chroma plane
/// * `u_stride`: U chroma stride, even odd images is supported this always must match `u_stride * height`
/// * `v_plane`: V chroma plane
/// * `v_stride`: V chroma stride, even odd images is supported this always must match `v_stride * height`
/// * `rgba`: RGBA image layout
/// * `width`: Image width
/// * `height`: Image height
/// * `range`: see [YuvIntensityRange]
/// * `matrix`: see [YuvStandardMatrix]
///
///
pub(crate) fn yuv400_to_rgba<V: Copy + AsPrimitive<i32> + 'static>(
    image: YuvGrayImage<V>,
    rgba: &mut [V],
    bit_depth: u32,
    range: YuvIntensityRange,
    matrix: YuvStandardMatrix,
) -> Result<(), String>
where
    i32: AsPrimitive<V>,
{
    let y_plane = image.y_plane;
    let y_stride = image.y_stride;
    let height = image.height;
    let width = image.width;
    if y_plane.len() != y_stride * height {
        return Err(format!(
            "Luma plane expected {} bytes, got {}",
            y_stride * height,
            y_plane.len()
        ));
    }
    const CHANNELS: usize = 4;
    let rgba_stride = width * CHANNELS;

    // If luma plane is in full range it can be just redistributed across the image
    if range == YuvIntensityRange::Pc {
        let y_iter = y_plane.chunks_exact(y_stride);
        let rgb_iter = rgba.chunks_exact_mut(rgba_stride);

        for (y_src, rgb) in y_iter.zip(rgb_iter) {
            let rgb_chunks = rgb.chunks_exact_mut(CHANNELS);

            for (y_src, rgb_dst) in y_src.iter().zip(rgb_chunks) {
                let r = *y_src;
                rgb_dst[0] = r;
                rgb_dst[1] = r;
                rgb_dst[2] = r;
                rgb_dst[3] = r;
            }
        }
        return Ok(());
    }

    let range = get_yuv_range(bit_depth, range);
    let kr_kb = get_kr_kb(matrix);
    const PRECISION: i32 = 11;
    const ROUNDING: i32 = 1 << (PRECISION - 1);
    let inverse_transform = get_inverse_transform(
        (1 << bit_depth) - 1,
        range.range_y,
        range.range_uv,
        kr_kb.kr,
        kr_kb.kb,
        PRECISION as u32,
    )?;
    let y_coef = inverse_transform.y_coef;

    let bias_y = range.bias_y as i32;

    if rgba.len() != width * height * CHANNELS {
        return Err(format!(
            "RGB image layout expected {} bytes, got {}",
            width * height * CHANNELS,
            rgba.len()
        ));
    }

    let max_value = (1 << bit_depth) - 1;

    let y_iter = y_plane.chunks_exact(y_stride);
    let rgb_iter = rgba.chunks_exact_mut(rgba_stride);

    for (y_src, rgb) in y_iter.zip(rgb_iter) {
        let rgb_chunks = rgb.chunks_exact_mut(CHANNELS);

        for (y_src, rgb_dst) in y_src.iter().zip(rgb_chunks) {
            let y_value = (y_src.as_() - bias_y) * y_coef;

            let r = ((y_value + ROUNDING) >> PRECISION).clamp(0, max_value);
            rgb_dst[0] = r.as_();
            rgb_dst[1] = r.as_();
            rgb_dst[2] = r.as_();
            rgb_dst[3] = max_value.as_();
        }
    }

    Ok(())
}

/// Converts YUV420 to Rgb
///
/// This support not tightly packed data and crop image using stride in place.
/// Stride here is not supports u16 as it can be in passed from FFI.
///
/// # Arguments
///
/// * `image`: see [YuvPlanarImage]
/// * `rgb`: RGB image layout
/// * `range`: see [YuvIntensityRange]
/// * `matrix`: see [YuvStandardMatrix]
///
///
pub(crate) fn yuv420_to_rgba<V: Copy + AsPrimitive<i32> + 'static>(
    image: YuvPlanarImage<V>,
    rgb: &mut [V],
    bit_depth: u32,
    range: YuvIntensityRange,
    matrix: YuvStandardMatrix,
) -> Result<(), String>
where
    i32: AsPrimitive<V>,
{
    let y_plane = image.y_plane;
    let u_plane = image.u_plane;
    let v_plane = image.v_plane;
    let y_stride = image.y_stride;
    let u_stride = image.u_stride;
    let v_stride = image.v_stride;
    let chroma_height = (image.height + 1) / 2;
    if y_plane.len() != y_stride * image.height {
        return Err(format!(
            "Luma plane expected {} bytes, got {}",
            y_stride * image.height,
            y_plane.len()
        ));
    }

    if u_plane.len() != u_stride * chroma_height {
        return Err(format!(
            "U plane expected {} bytes, got {}",
            u_stride * chroma_height,
            u_plane.len()
        ));
    }

    if v_plane.len() != v_stride * chroma_height {
        return Err(format!(
            "V plane expected {} bytes, got {}",
            v_stride * chroma_height,
            v_plane.len()
        ));
    }

    let max_value = (1 << bit_depth) - 1;

    const PRECISION: i32 = 11;
    const ROUNDING: i32 = 1 << (PRECISION - 1);

    let range = get_yuv_range(bit_depth, range);
    let kr_kb = get_kr_kb(matrix);
    let inverse_transform = get_inverse_transform(
        (1 << bit_depth) - 1,
        range.range_y,
        range.range_uv,
        kr_kb.kr,
        kr_kb.kb,
        PRECISION as u32,
    )?;
    let cr_coef = inverse_transform.cr_coef;
    let cb_coef = inverse_transform.cb_coef;
    let y_coef = inverse_transform.y_coef;
    let g_coef_1 = inverse_transform.g_coeff_1;
    let g_coef_2 = inverse_transform.g_coeff_2;

    let bias_y = range.bias_y as i32;
    let bias_uv = range.bias_uv as i32;

    const CHANNELS: usize = 4;

    if rgb.len() != image.width * image.height * CHANNELS {
        return Err(format!(
            "RGB image layout expected {} bytes, got {}",
            image.width * image.height * CHANNELS,
            rgb.len()
        ));
    }

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

    for (((y_src, u_src), v_src), rgb) in y_iter.zip(u_iter).zip(v_iter).zip(rgb_iter) {
        // Since we're processing two rows in one loop we need to re-slice once more
        let y_iter = y_src.chunks_exact(y_stride);
        let rgb_iter = rgb.chunks_exact_mut(rgb_stride);
        for (y_src, rgb) in y_iter.zip(rgb_iter) {
            let y_iter = y_src.chunks_exact(2);
            let rgb_chunks = rgb.chunks_exact_mut(CHANNELS * 2);
            for (((y_src, &u_src), &v_src), rgb_dst) in y_iter.zip(u_src).zip(v_src).zip(rgb_chunks)
            {
                let y_value: i32 = (y_src[0].as_() - bias_y) * y_coef;
                let cb_value: i32 = u_src.as_() - bias_uv;
                let cr_value: i32 = v_src.as_() - bias_uv;

                let r =
                    ((y_value + cr_coef * cr_value + ROUNDING) >> PRECISION).clamp(0, max_value);
                let b =
                    ((y_value + cb_coef * cb_value + ROUNDING) >> PRECISION).clamp(0, max_value);
                let g = ((y_value - g_coef_1 * cr_value - g_coef_2 * cb_value + ROUNDING)
                    >> PRECISION)
                    .clamp(0, max_value);

                rgb_dst[0] = r.as_();
                rgb_dst[1] = g.as_();
                rgb_dst[2] = b.as_();
                rgb_dst[3] = max_value.as_();

                let y_value = (y_src[1].as_() - bias_y) * y_coef;

                let r =
                    ((y_value + cr_coef * cr_value + ROUNDING) >> PRECISION).clamp(0, max_value);
                let b =
                    ((y_value + cb_coef * cb_value + ROUNDING) >> PRECISION).clamp(0, max_value);
                let g = ((y_value - g_coef_1 * cr_value - g_coef_2 * cb_value + ROUNDING)
                    >> PRECISION)
                    .clamp(0, max_value);

                rgb_dst[4] = r.as_();
                rgb_dst[5] = g.as_();
                rgb_dst[6] = b.as_();
                rgb_dst[7] = max_value.as_();
            }

            // Process remainder if width is odd.
            if image.width & 1 != 0 {
                let y_left = y_src.chunks_exact(2).remainder();
                let rgb_chunks = rgb
                    .chunks_exact_mut(CHANNELS * 2)
                    .into_remainder()
                    .chunks_exact_mut(CHANNELS);
                let u_iter = u_src.iter().rev();
                let v_iter = v_src.iter().rev();

                for (((y_src, u_src), v_src), rgb_dst) in
                    y_left.iter().zip(u_iter).zip(v_iter).zip(rgb_chunks)
                {
                    let y_value = (y_src.as_() - bias_y) * y_coef;
                    let cb_value = u_src.as_() - bias_uv;
                    let cr_value = v_src.as_() - bias_uv;

                    let r = ((y_value + cr_coef * cr_value + ROUNDING) >> PRECISION)
                        .clamp(0, max_value);
                    let b = ((y_value + cb_coef * cb_value + ROUNDING) >> PRECISION)
                        .clamp(0, max_value);
                    let g = ((y_value - g_coef_1 * cr_value - g_coef_2 * cb_value + ROUNDING)
                        >> PRECISION)
                        .clamp(0, max_value);

                    rgb_dst[0] = r.as_();
                    rgb_dst[1] = g.as_();
                    rgb_dst[2] = b.as_();
                    rgb_dst[3] = max_value.as_();
                }
            }
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

    for (((y_src, u_src), v_src), rgb) in y_iter.zip(u_iter).zip(v_iter).zip(rgb_iter) {
        let y_iter = y_src.chunks_exact(2);
        let rgb_chunks = rgb.chunks_exact_mut(CHANNELS * 2);
        for (((y_src, u_src), v_src), rgb_dst) in y_iter.zip(u_src).zip(v_src).zip(rgb_chunks) {
            let y_value = (y_src[0].as_() - bias_y) * y_coef;
            let cb_value = u_src.as_() - bias_uv;
            let cr_value = v_src.as_() - bias_uv;

            let r = ((y_value + cr_coef * cr_value + ROUNDING) >> PRECISION).clamp(0, max_value);
            let b = ((y_value + cb_coef * cb_value + ROUNDING) >> PRECISION).clamp(0, max_value);
            let g = ((y_value - g_coef_1 * cr_value - g_coef_2 * cb_value + ROUNDING) >> PRECISION)
                .clamp(0, max_value);

            rgb_dst[0] = r.as_();
            rgb_dst[1] = g.as_();
            rgb_dst[2] = b.as_();
            rgb_dst[3] = max_value.as_();

            let y_value = (y_src[1].as_() - bias_y) * y_coef;

            let r = ((y_value + cr_coef * cr_value + ROUNDING) >> PRECISION).clamp(0, max_value);
            let b = ((y_value + cb_coef * cb_value + ROUNDING) >> PRECISION).clamp(0, max_value);
            let g = ((y_value - g_coef_1 * cr_value - g_coef_2 * cb_value + ROUNDING) >> PRECISION)
                .clamp(0, max_value);

            rgb_dst[4] = r.as_();
            rgb_dst[5] = g.as_();
            rgb_dst[6] = b.as_();
            rgb_dst[7] = max_value.as_();
        }

        let y_left = y_src.chunks_exact(2).remainder();
        let rgb_chunks = rgb
            .chunks_exact_mut(CHANNELS * 2)
            .into_remainder()
            .chunks_exact_mut(CHANNELS);
        let u_iter = u_plane.iter().rev();
        let v_iter = v_plane.iter().rev();

        // Process remainder if width is odd.

        for (((y_src, u_src), v_src), rgb_dst) in
            y_left.iter().zip(u_iter).zip(v_iter).zip(rgb_chunks)
        {
            let y_value = (y_src.as_() - bias_y) * y_coef;
            let cb_value = u_src.as_() - bias_uv;
            let cr_value = v_src.as_() - bias_uv;

            let r = ((y_value + cr_coef * cr_value + ROUNDING) >> PRECISION).clamp(0, max_value);
            let b = ((y_value + cb_coef * cb_value + ROUNDING) >> PRECISION).clamp(0, max_value);
            let g = ((y_value - g_coef_1 * cr_value - g_coef_2 * cb_value + ROUNDING) >> PRECISION)
                .clamp(0, max_value);

            rgb_dst[0] = r.as_();
            rgb_dst[1] = g.as_();
            rgb_dst[2] = b.as_();
            rgb_dst[3] = max_value.as_();
        }
    }

    Ok(())
}

/// Converts Yuv 422 planar format to Rgba
///
/// This support not tightly packed data and crop image using stride in place.
///
/// # Arguments
///
/// * `image`: see [YuvPlanarImage]
/// * `rgb`: RGB image layout
/// * `range`: see [YuvIntensityRange]
/// * `matrix`: see [YuvStandardMatrix]
///
///
pub(crate) fn yuv422_to_rgba<V: Copy + AsPrimitive<i32> + 'static>(
    image: YuvPlanarImage<V>,
    rgb: &mut [V],
    bit_depth: u32,
    range: YuvIntensityRange,
    matrix: YuvStandardMatrix,
) -> Result<(), String>
where
    i32: AsPrimitive<V>,
{
    let y_plane = image.y_plane;
    let u_plane = image.u_plane;
    let v_plane = image.v_plane;
    let y_stride = image.y_stride;
    let u_stride = image.u_stride;
    let v_stride = image.v_stride;
    let height = image.height;
    let width = image.width;
    if y_plane.len() != y_stride * height {
        return Err(format!(
            "Luma plane expected {} bytes, got {}",
            y_stride * height,
            y_plane.len()
        ));
    }

    if u_plane.len() != u_stride * height {
        return Err(format!(
            "U plane expected {} bytes, got {}",
            u_stride * height,
            u_plane.len()
        ));
    }

    if v_plane.len() != v_stride * height {
        return Err(format!(
            "V plane expected {} bytes, got {}",
            v_stride * height,
            v_plane.len()
        ));
    }

    let max_value = (1 << bit_depth) - 1;

    let range = get_yuv_range(bit_depth, range);
    let kr_kb = get_kr_kb(matrix);
    const PRECISION: i32 = 11;
    const ROUNDING: i32 = 1 << (PRECISION - 1);
    let inverse_transform = get_inverse_transform(
        (1 << bit_depth) - 1,
        range.range_y,
        range.range_uv,
        kr_kb.kr,
        kr_kb.kb,
        PRECISION as u32,
    )?;
    let cr_coef = inverse_transform.cr_coef;
    let cb_coef = inverse_transform.cb_coef;
    let y_coef = inverse_transform.y_coef;
    let g_coef_1 = inverse_transform.g_coeff_1;
    let g_coef_2 = inverse_transform.g_coeff_2;

    let bias_y = range.bias_y as i32;
    let bias_uv = range.bias_uv as i32;

    const CHANNELS: usize = 4;

    if rgb.len() != width * height * CHANNELS {
        return Err(format!(
            "RGB image layout expected {} bytes, got {}",
            width * height * CHANNELS,
            rgb.len()
        ));
    }

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

    for (((y_src, u_src), v_src), rgb) in y_iter.zip(u_iter).zip(v_iter).zip(rgb_iter) {
        let y_iter = y_src.chunks_exact(2);
        let rgb_chunks = rgb.chunks_exact_mut(CHANNELS * 2);

        for (((y_src, u_src), v_src), rgb_dst) in y_iter.zip(u_src).zip(v_src).zip(rgb_chunks) {
            let y_value = (y_src[0].as_() - bias_y) * y_coef;
            let cb_value = u_src.as_() - bias_uv;
            let cr_value = v_src.as_() - bias_uv;

            let r = ((y_value + cr_coef * cr_value + ROUNDING) >> PRECISION).clamp(0, max_value);
            let b = ((y_value + cb_coef * cb_value + ROUNDING) >> PRECISION).clamp(0, max_value);
            let g = ((y_value - g_coef_1 * cr_value - g_coef_2 * cb_value + ROUNDING) >> PRECISION)
                .clamp(0, max_value);

            rgb_dst[0] = r.as_();
            rgb_dst[1] = g.as_();
            rgb_dst[2] = b.as_();
            rgb_dst[3] = max_value.as_();

            let y_value = (y_src[1].as_() - bias_y) * y_coef;

            let r = ((y_value + cr_coef * cr_value + ROUNDING) >> PRECISION).clamp(0, max_value);
            let b = ((y_value + cb_coef * cb_value + ROUNDING) >> PRECISION).clamp(0, max_value);
            let g = ((y_value - g_coef_1 * cr_value - g_coef_2 * cb_value + ROUNDING) >> PRECISION)
                .clamp(0, max_value);

            rgb_dst[4] = r.as_();
            rgb_dst[5] = g.as_();
            rgb_dst[6] = b.as_();
            rgb_dst[7] = max_value.as_();
        }

        // Process left pixels for odd images, this should work since luma must be always exact
        if width & 1 != 0 {
            let y_left = y_src.chunks_exact(2).remainder();
            let rgb_chunks = rgb
                .chunks_exact_mut(CHANNELS * 2)
                .into_remainder()
                .chunks_exact_mut(CHANNELS);
            let u_iter = u_src.iter().rev();
            let v_iter = v_src.iter().rev();

            for (((y_src, u_src), v_src), rgb_dst) in
                y_left.iter().zip(u_iter).zip(v_iter).zip(rgb_chunks)
            {
                let y_value = (y_src.as_() - bias_y) * y_coef;
                let cb_value = u_src.as_() - bias_uv;
                let cr_value = v_src.as_() - bias_uv;

                let r =
                    ((y_value + cr_coef * cr_value + ROUNDING) >> PRECISION).clamp(0, max_value);
                let b =
                    ((y_value + cb_coef * cb_value + ROUNDING) >> PRECISION).clamp(0, max_value);
                let g = ((y_value - g_coef_1 * cr_value - g_coef_2 * cb_value + ROUNDING)
                    >> PRECISION)
                    .clamp(0, max_value);

                rgb_dst[0] = r.as_();
                rgb_dst[1] = g.as_();
                rgb_dst[2] = b.as_();
                rgb_dst[3] = max_value.as_();
            }
        }
    }

    Ok(())
}

/// Converts YUV444 to Rgb
///
/// This support not tightly packed data and crop image using stride in place.
///
/// # Arguments
///
/// * `image`: see [YuvPlanarImage]
/// * `rgb`: RGB image layout
/// * `range`: see [YuvIntensityRange]
/// * `matrix`: see [YuvStandardMatrix]
///
///
pub(crate) fn yuv444_to_rgba<V: Copy + AsPrimitive<i32> + 'static>(
    image: YuvPlanarImage<V>,
    rgb: &mut [V],
    bit_depth: u32,
    range: YuvIntensityRange,
    matrix: YuvStandardMatrix,
) -> Result<(), String>
where
    i32: AsPrimitive<V>,
{
    let y_plane = image.y_plane;
    let u_plane = image.u_plane;
    let v_plane = image.v_plane;
    let y_stride = image.y_stride;
    let u_stride = image.u_stride;
    let v_stride = image.v_stride;
    let height = image.height;
    let width = image.width;
    if y_plane.len() != y_stride * height {
        return Err(format!(
            "Luma plane expected {} bytes, got {}",
            y_stride * height,
            y_plane.len()
        ));
    }

    if u_plane.len() != u_stride * height {
        return Err(format!(
            "U plane expected {} bytes, got {}",
            u_stride * height,
            u_plane.len()
        ));
    }

    if v_plane.len() != v_stride * height {
        return Err(format!(
            "V plane expected {} bytes, got {}",
            v_stride * height,
            v_plane.len()
        ));
    }

    let range = get_yuv_range(bit_depth, range);
    let kr_kb = get_kr_kb(matrix);
    const PRECISION: i32 = 11;
    const ROUNDING: i32 = 1 << (PRECISION - 1);
    let inverse_transform = get_inverse_transform(
        (1 << bit_depth) - 1,
        range.range_y,
        range.range_uv,
        kr_kb.kr,
        kr_kb.kb,
        PRECISION as u32,
    )?;
    let cr_coef = inverse_transform.cr_coef;
    let cb_coef = inverse_transform.cb_coef;
    let y_coef = inverse_transform.y_coef;
    let g_coef_1 = inverse_transform.g_coeff_1;
    let g_coef_2 = inverse_transform.g_coeff_2;

    let bias_y = range.bias_y as i32;
    let bias_uv = range.bias_uv as i32;

    const CHANNELS: usize = 4;

    if rgb.len() != width * height * CHANNELS {
        return Err(format!(
            "RGB image layout expected {} bytes, got {}",
            width * height * CHANNELS,
            rgb.len()
        ));
    }

    let max_value = (1 << bit_depth) - 1;

    let rgb_stride = width * CHANNELS;

    let y_iter = y_plane.chunks_exact(y_stride);
    let rgb_iter = rgb.chunks_exact_mut(rgb_stride);
    let u_iter = u_plane.chunks_exact(u_stride);
    let v_iter = v_plane.chunks_exact(v_stride);

    for (((y_src, u_src), v_src), rgb) in y_iter.zip(u_iter).zip(v_iter).zip(rgb_iter) {
        let rgb_chunks = rgb.chunks_exact_mut(CHANNELS);

        for (((y_src, u_src), v_src), rgb_dst) in y_src.iter().zip(u_src).zip(v_src).zip(rgb_chunks)
        {
            let y_value = (y_src.as_() - bias_y) * y_coef;
            let cb_value = u_src.as_() - bias_uv;
            let cr_value = v_src.as_() - bias_uv;

            let r = ((y_value + cr_coef * cr_value + ROUNDING) >> PRECISION).clamp(0, max_value);
            let b = ((y_value + cb_coef * cb_value + ROUNDING) >> PRECISION).clamp(0, max_value);
            let g = ((y_value - g_coef_1 * cr_value - g_coef_2 * cb_value + ROUNDING) >> PRECISION)
                .clamp(0, max_value);

            rgb_dst[0] = r.as_();
            rgb_dst[1] = g.as_();
            rgb_dst[2] = b.as_();
            rgb_dst[3] = max_value.as_();
        }
    }

    Ok(())
}
