use num_traits::clamp;

use crate::{ImageBuffer, Pixel, Primitive};

/// Approximation of Gaussian blur after
/// Kovesi, P.:  Fast Almost-Gaussian Filtering The Australian Pattern
/// Recognition Society Conference: DICTA 2010. December 2010. Sydney.
/// This method assumes alpha pre-multiplication for images that contain non-constant alpha.
#[must_use]
pub fn fast_blur<P: Pixel>(
    image_buffer: &ImageBuffer<P, Vec<P::Subpixel>>,
    sigma: f32,
) -> ImageBuffer<P, Vec<P::Subpixel>> {
    let (width, height) = image_buffer.dimensions();

    if width == 0 || height == 0 {
        return image_buffer.clone();
    }
    let mut samples = image_buffer.as_flat_samples().samples.to_vec();
    let num_passes = 3;

    let boxes = boxes_for_gauss(sigma, num_passes);

    for radius in boxes.iter().take(num_passes) {
        let horizontally_blurred_transposed = horizontal_fast_blur_half::<P::Subpixel>(
            &samples,
            width as usize,
            height as usize,
            (*radius - 1) / 2,
            P::CHANNEL_COUNT as usize,
        );
        samples = horizontal_fast_blur_half::<P::Subpixel>(
            &horizontally_blurred_transposed,
            height as usize,
            width as usize,
            (*radius - 1) / 2,
            P::CHANNEL_COUNT as usize,
        );
    }
    ImageBuffer::from_raw(width, height, samples).unwrap()
}

fn boxes_for_gauss(sigma: f32, n: usize) -> Vec<usize> {
    let w_ideal = f32::sqrt((12.0 * sigma.powi(2) / (n as f32)) + 1.0);
    let mut w_l = w_ideal.floor();
    if w_l % 2.0 == 0.0 {
        w_l -= 1.0;
    };
    let w_u = w_l + 2.0;

    let m_ideal = 0.25 * (n as f32) * (w_l + 3.0) - 3.0 * sigma.powi(2) * (w_l + 1.0).recip();

    let m = f32::round(m_ideal) as usize;

    (0..n)
        .map(|i| if i < m { w_l as usize } else { w_u as usize })
        .collect::<Vec<_>>()
}

fn channel_idx(channel: usize, idx: usize, channel_num: usize) -> usize {
    channel_num * idx + channel
}

fn horizontal_fast_blur_half<P: Primitive>(
    samples: &[P],
    width: usize,
    height: usize,
    r: usize,
    channel_num: usize,
) -> Vec<P> {
    let channel_size = width * height;

    let mut out_samples = vec![P::from(0).unwrap(); channel_size * channel_num];
    let mut vals = vec![0.0; channel_num];

    let min_value = P::DEFAULT_MIN_VALUE.to_f32().unwrap();
    let max_value = P::DEFAULT_MAX_VALUE.to_f32().unwrap();

    for row in 0..height {
        for (channel, value) in vals.iter_mut().enumerate().take(channel_num) {
            *value = ((-(r as isize))..(r + 1) as isize)
                .map(|x| {
                    extended_f(
                        samples,
                        width,
                        height,
                        x,
                        row as isize,
                        channel,
                        channel_num,
                    )
                    .to_f32()
                    .unwrap_or(0.0)
                })
                .sum();
        }

        for column in 0..width {
            for (channel, channel_val) in vals.iter_mut().enumerate() {
                let val = *channel_val / (2.0 * r as f32 + 1.0);
                let val = clamp(val, min_value, max_value);
                let val = P::from(val).unwrap();

                let destination_row = column;
                let destination_column = row;
                let destination_sample_index = channel_idx(
                    channel,
                    destination_column + destination_row * height,
                    channel_num,
                );
                out_samples[destination_sample_index] = val;
                *channel_val = *channel_val
                    - extended_f(
                        samples,
                        width,
                        height,
                        column as isize - r as isize,
                        row as isize,
                        channel,
                        channel_num,
                    )
                    .to_f32()
                    .unwrap_or(0.0)
                    + extended_f(
                        samples,
                        width,
                        height,
                        { column + r + 1 } as isize,
                        row as isize,
                        channel,
                        channel_num,
                    )
                    .to_f32()
                    .unwrap_or(0.0);
            }
        }
    }

    out_samples
}

fn extended_f<P: Primitive>(
    samples: &[P],
    width: usize,
    height: usize,
    x: isize,
    y: isize,
    channel: usize,
    channel_num: usize,
) -> P {
    let x = clamp(x, 0, width as isize - 1) as usize;
    let y = clamp(y, 0, height as isize - 1) as usize;
    samples[channel_idx(channel, y * width + x, channel_num)]
}
