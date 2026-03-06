use num_traits::Bounded;

use crate::imageops::filter_1d::{SafeAdd, SafeMul};
use crate::{ImageBuffer, Pixel, Primitive};

/// Approximation of Gaussian blur.
///
/// # Arguments
///
/// * `image_buffer` - source image.
/// * `sigma` - value controls image flattening level.
///
/// This method assumes alpha pre-multiplication for images that contain non-constant alpha.
///
/// This method typically assumes that the input is scene-linear light.
/// If it is not, color distortion may occur.
///
/// Source: Kovesi, P.:  Fast Almost-Gaussian Filtering The Australian Pattern
/// Recognition Society Conference: DICTA 2010. December 2010. Sydney.
#[must_use]
pub fn fast_blur<P: Pixel>(
    input_buffer: &ImageBuffer<P, Vec<P::Subpixel>>,
    sigma: f32,
) -> ImageBuffer<P, Vec<P::Subpixel>>
{
    let (width, height) = input_buffer.dimensions();

    if width == 0 || height == 0 {
        return input_buffer.clone();
    }

    let num_passes = 3;

    let boxes = boxes_for_gauss(sigma, num_passes);
    if boxes.is_empty() {
        return input_buffer.clone();
    }

    let samples = input_buffer.as_flat_samples().samples;

    let destination_size = match (width as usize)
        .safe_mul(height as usize)
        .and_then(|x| x.safe_mul(P::CHANNEL_COUNT as usize))
    {
        Ok(s) => s,
        Err(_) => panic!("Width and height and channels count exceeded pointer size"),
    };

    let first_box = boxes[0];

    let mut transient = vec![P::Subpixel::min_value(); destination_size];
    let mut dst = vec![P::Subpixel::min_value(); destination_size];

    // If destination_size isn't failed this one must not fail either
    let stride = width as usize * P::CHANNEL_COUNT as usize;

    // bound + radius + 1 must fit in a pointer size
    test_radius_size(width as usize, first_box);
    test_radius_size(height as usize, first_box);

    box_blur_horizontal_pass_strategy::<P>(
        samples,
        stride,
        &mut transient,
        stride,
        width,
        first_box,
    );

    box_blur_vertical_pass_strategy::<P>(
        &transient, stride, &mut dst, stride, width, height, first_box,
    );

    for &box_container in boxes.iter().skip(1) {
        // bound + radius + 1 must fit in a pointer size
        test_radius_size(width as usize, box_container);
        test_radius_size(height as usize, box_container);

        box_blur_horizontal_pass_strategy::<P>(
            &dst,
            stride,
            &mut transient,
            stride,
            width,
            box_container,
        );

        box_blur_vertical_pass_strategy::<P>(
            &transient,
            stride,
            &mut dst,
            stride,
            width,
            height,
            box_container,
        );
    }

    let mut buffer = ImageBuffer::from_raw(width, height, dst).unwrap();
    buffer.copy_color_space_from(input_buffer);
    buffer
}

#[inline]
fn test_radius_size(bound: usize, radius: usize) {
    match bound.safe_add(radius) {
        Ok(_) => {}
        Err(_) => panic!("Radius overflowed maximum possible size"),
    }
}

fn boxes_for_gauss(sigma: f32, n: usize) -> Vec<usize> {
    let w_ideal = f32::sqrt((12.0 * sigma.powi(2) / (n as f32)) + 1.0);
    let mut w_l = w_ideal.floor();
    if w_l % 2.0 == 0.0 {
        w_l -= 1.0;
    }
    let w_u = w_l + 2.0;

    let m_ideal = 0.25 * (n as f32) * (w_l + 3.0) - 3.0 * sigma.powi(2) * (w_l + 1.0).recip();

    let m = f32::round(m_ideal) as usize;

    (0..n)
        .map(|i| if i < m { w_l as usize } else { w_u as usize })
        .map(|i| ceil_to_odd(i.saturating_sub(1) / 2))
        .collect::<Vec<_>>()
}

#[inline]
fn ceil_to_odd(x: usize) -> usize {
    if x.is_multiple_of(2) {
        x + 1
    } else {
        x
    }
}

#[inline]
fn box_blur_horizontal_pass_strategy<T: Pixel>(
    src: &[T::Subpixel],
    src_stride: usize,
    dst: &mut [T::Subpixel],
    dst_stride: usize,
    width: u32,
    radius: usize,
) {
    if T::CHANNEL_COUNT == 1 {
        box_blur_horizontal_pass::<T::Subpixel, 1>(src, src_stride, dst, dst_stride, width, radius);
    } else if T::CHANNEL_COUNT == 2 {
        box_blur_horizontal_pass::<T::Subpixel, 2>(src, src_stride, dst, dst_stride, width, radius);
    } else if T::CHANNEL_COUNT == 3 {
        box_blur_horizontal_pass::<T::Subpixel, 3>(src, src_stride, dst, dst_stride, width, radius);
    } else if T::CHANNEL_COUNT == 4 {
        box_blur_horizontal_pass::<T::Subpixel, 4>(src, src_stride, dst, dst_stride, width, radius);
    } else {
        unimplemented!("More than 4 channels is not yet implemented");
    }
}

fn box_blur_vertical_pass_strategy<T: Pixel>(
    src: &[T::Subpixel],
    src_stride: usize,
    dst: &mut [T::Subpixel],
    dst_stride: usize,
    width: u32,
    height: u32,
    radius: usize,
) {
    box_blur_vertical_pass::<T::Subpixel>(
        src,
        src_stride,
        dst,
        dst_stride,
        width,
        height,
        radius,
        T::CHANNEL_COUNT as usize,
    );
}

fn box_blur_horizontal_pass<P: Primitive, const CN: usize>(
    src: &[P],
    src_stride: usize,
    dst: &mut [P],
    dst_stride: usize,
    width: u32,
    radius: usize,
) {
    assert!(width > 0, "Width must be sanitized before this method");
    test_radius_size(width as usize, radius);

    let kernel_size = radius * 2 + 1;
    let weight = P::make_weight(kernel_size);
    let edge_count = (kernel_size / 2) + 1;
    let half_kernel = kernel_size / 2;
    let width_bound = width as usize - 1;

    // Horizontal blurring consists from 4 phases
    // 1 - Fill initial sliding window
    // 2 - Blur dangerous leading zone where clamping is required
    // 3 - Blur *normal* zone where clamping is not required
    // 4 - Blur dangerous trailing zone where clamping is required

    for (dst, src) in dst
        .chunks_exact_mut(dst_stride)
        .zip(src.chunks_exact(src_stride))
    {
        let mut sums = [P::ZERO; CN];

        let chunk0 = &src[..CN];

        // replicate edge
        for c in 0..CN {
            sums[c] = P::scale(chunk0[c].to_acc(), edge_count);
        }

        for x in 1..=half_kernel {
            let px = x.min(width_bound) * CN;
            let chunk = &src[px..px + CN];
            for c in 0..CN {
                sums[c] += chunk[c].to_acc();
            }
        }

        for x in 0..half_kernel.min(width as usize) {
            let next = (x + half_kernel + 1).min(width_bound) * CN;
            let previous = (x as i64 - half_kernel as i64).max(0) as usize * CN;

            let dst_chunk = &mut dst[x * CN..x * CN + CN];
            for c in 0..CN {
                dst_chunk[c] = P::to_store(sums[c], weight);
            }

            let next_chunk = &src[next..next + CN];
            let previous_chunk = &src[previous..previous + CN];
            for c in 0..CN {
                sums[c] += next_chunk[c].to_acc();
                sums[c] -= previous_chunk[c].to_acc();
            }
        }

        let max_x_before_clamping = width_bound.saturating_sub(half_kernel + 1);
        let row_length = src.len();

        let mut last_processed_item = half_kernel;

        if ((half_kernel * 2 + 1) * CN < row_length) && ((max_x_before_clamping * CN) < row_length)
        {
            let data_section = src;
            let advanced_kernel_part = &data_section[(half_kernel * 2 + 1) * CN..];
            let section_length = max_x_before_clamping - half_kernel;
            let dst = &mut dst[half_kernel * CN..(half_kernel * CN + section_length * CN)];

            let dst_chunks = dst.as_chunks_mut::<CN>().0.iter_mut();
            let data_section_chunks = data_section.as_chunks::<CN>().0.iter();
            let advanced_kernel_part_chunks = advanced_kernel_part.as_chunks::<CN>().0.iter();
            for ((dst_chunk, src_previous), src_next) in dst_chunks
                .zip(data_section_chunks)
                .zip(advanced_kernel_part_chunks)
            {
                for c in 0..CN {
                    dst_chunk[c] = P::to_store(sums[c], weight);
                }
                for c in 0..CN {
                    sums[c] += src_next[c].to_acc();
                    sums[c] -= src_previous[c].to_acc();
                }
            }

            last_processed_item = max_x_before_clamping;
        }

        for x in last_processed_item..width as usize {
            let next = (x + half_kernel + 1).min(width_bound) * CN;
            let previous = (x as i64 - half_kernel as i64).max(0) as usize * CN;

            let dst_chunk = &mut dst[x * CN..x * CN + CN];
            for c in 0..CN {
                dst_chunk[c] = P::to_store(sums[c], weight);
            }

            let next_chunk = &src[next..next + CN];
            let previous_chunk = &src[previous..previous + CN];
            for c in 0..CN {
                sums[c] += next_chunk[c].to_acc();
                sums[c] -= previous_chunk[c].to_acc();
            }
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn box_blur_vertical_pass<P: Primitive>(
    src: &[P],
    src_stride: usize,
    dst: &mut [P],
    dst_stride: usize,
    width: u32,
    height: u32,
    radius: usize,
    n: usize,
) {
    assert!(width > 0, "Width must be sanitized before this method");
    assert!(height > 0, "Height must be sanitized before this method");
    test_radius_size(width as usize, radius);

    let kernel_size = radius * 2 + 1;
    let weight = P::make_weight(kernel_size);
    let edge_count = (kernel_size / 2) + 1;
    let half_kernel = kernel_size / 2;
    let height_bound = height as usize - 1;

    let buf_size = width as usize * n;

    // Instead of summing each column separately we use here transient buffer that
    // averages columns in row manner.
    // So, we make the initial buffer at the top edge
    // and then doing blur by averaging the whole row ( which is in buffer )
    // and subtracting and adding next and previous rows in horizontal manner.

    let mut buffer = vec![P::ZERO; buf_size];

    for (x, bf) in buffer.iter_mut().enumerate() {
        let mut w = P::scale(src[x].to_acc(), edge_count);
        for y in 1..=half_kernel {
            let y_src_shift = y.min(height_bound) * src_stride;
            w += src[y_src_shift + x].to_acc();
        }
        *bf = w;
    }

    for (dst_row, y) in dst.chunks_exact_mut(dst_stride).zip(0..height as usize) {
        let next = (y + half_kernel + 1).min(height_bound) * src_stride;
        let previous = (y as i64 - half_kernel as i64).max(0) as usize * src_stride;

        let next_row = &src[next..next + buf_size];
        let previous_row = &src[previous..previous + buf_size];

        for (((src_next, src_previous), buf), dst) in next_row
            .iter()
            .zip(previous_row.iter())
            .zip(buffer.iter_mut())
            .zip(dst_row.iter_mut())
        {
            let acc = *buf;
            *dst = P::to_store(acc, weight);
            *buf = acc + src_next.to_acc() - src_previous.to_acc();
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{DynamicImage, GrayAlphaImage, GrayImage, RgbImage, RgbaImage};
    use std::time::{SystemTime, UNIX_EPOCH};

    struct Rng {
        state: u64,
    }

    impl Rng {
        fn new(seed: u64) -> Self {
            Self { state: seed }
        }
        fn next_u32(&mut self) -> u32 {
            self.state = self.state.wrapping_mul(6364136223846793005).wrapping_add(1);
            (self.state >> 32) as u32
        }

        fn next_u8(&mut self) -> u8 {
            (self.next_u32() % 256) as u8
        }

        fn next_f32_in_range(&mut self, a: f32, b: f32) -> f32 {
            let u = self.next_u32();
            let unit = (u as f32) / (u32::MAX as f32 + 1.0);
            a + (b - a) * unit
        }
    }

    #[test]
    fn test_box_blur() {
        let now = SystemTime::now().duration_since(UNIX_EPOCH).unwrap();
        let mut rng = Rng::new((now.as_millis() & 0xffff_ffff_ffff_ffff) as u64);
        for _ in 0..35 {
            let width = rng.next_u8();
            let height = rng.next_u8();
            let sigma = rng.next_f32_in_range(0., 100.);
            let px = rng.next_u8();
            let cn = rng.next_u8();
            if width == 0 || height == 0 || sigma <= 0. {
                continue;
            }
            match cn % 4 {
                0 => {
                    let vc = vec![px; width as usize * height as usize];
                    let image = DynamicImage::from(
                        GrayImage::from_vec(u32::from(width), u32::from(height), vc).unwrap(),
                    );
                    let res = image.fast_blur(sigma);
                    for clr in res.as_bytes() {
                        assert_eq!(*clr, px);
                    }
                }
                1 => {
                    let vc = vec![px; width as usize * height as usize * 2];
                    let image = DynamicImage::from(
                        GrayAlphaImage::from_vec(u32::from(width), u32::from(height), vc).unwrap(),
                    );
                    let res = image.fast_blur(sigma);
                    for clr in res.as_bytes() {
                        assert_eq!(*clr, px);
                    }
                }
                2 => {
                    let vc = vec![px; width as usize * height as usize * 3];
                    let image = DynamicImage::from(
                        RgbImage::from_vec(u32::from(width), u32::from(height), vc).unwrap(),
                    );
                    let res = image.fast_blur(sigma);
                    for clr in res.as_bytes() {
                        assert_eq!(*clr, px);
                    }
                }
                3 => {
                    let vc = vec![px; width as usize * height as usize * 4];
                    let image = DynamicImage::from(
                        RgbaImage::from_vec(u32::from(width), u32::from(height), vc).unwrap(),
                    );
                    let res = image.fast_blur(sigma);
                    for clr in res.as_bytes() {
                        assert_eq!(*clr, px);
                    }
                }
                _ => {}
            }
        }
    }

}
