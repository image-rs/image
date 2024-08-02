use std::cmp::{max, min};

use crate::{ImageBuffer, Pixel, Primitive};

/// Approximation of Gaussian blur after
/// Kovesi, P.:  Fast Almost-Gaussian Filtering The Australian Pattern
/// Recognition Society Conference: DICTA 2010. December 2010. Sydney.
pub fn fast_blur<P: Pixel>(
    image_buffer: &ImageBuffer<P, Vec<P::Subpixel>>,
    sigma: f32,
) -> ImageBuffer<P, Vec<P::Subpixel>> {
    let (width, height) = image_buffer.dimensions();
    let mut samples = image_buffer.as_flat_samples().samples.to_vec();
    let num_passes = 3;

    let boxes = boxes_for_gauss(sigma, num_passes);

    for radius in boxes.iter().take(num_passes) {
        let horizontally_blurred_transposed = my_fast_horizontal_blur::<P::Subpixel>(
            &samples,
            width as usize,
            height as usize,
            *radius,
            P::CHANNEL_COUNT as usize,
        );
        samples = my_fast_horizontal_blur::<P::Subpixel>(
            &horizontally_blurred_transposed,
            height as usize,
            width as usize,
            *radius,
            P::CHANNEL_COUNT as usize,
        );
    }
    ImageBuffer::from_raw(width, height, samples).unwrap()
}

fn boxes_for_gauss(sigma: f32, n: usize) -> Vec<usize> {
    let w_ideal = f32::sqrt((12.0 * sigma * sigma / (n as f32)) + 1.0);
    let mut w_l = w_ideal.floor();
    if w_l % 2.0 == 0.0 {
        w_l -= 1.0
    };
    let w_u = w_l + 2.0;

    let m_ideal = (12.0 * sigma * sigma
        - (n as f32) * (w_l) * (w_l)
        - 4.0 * (n as f32) * (w_l)
        - 3.0 * (n as f32))
        / (-4.0 * (w_l) - 4.0);
    let m = f32::round(m_ideal) as usize;

    let mut box_sizes: Vec<usize> = vec![];
    for i in 0..n {
        box_sizes.push(if i < m { w_l as usize } else { w_u as usize });
    }
    box_sizes
}

fn channel_idx(channel: usize, idx: usize, channel_num: usize) -> usize {
    channel_num * idx + channel
}

fn my_fast_horizontal_blur<P: Primitive>(
    samples: &[P],
    width: usize,
    height: usize,
    r: usize,
    channel_num: usize,
) -> Vec<P> {
    let channel_size = width * height;

    let mut out_samples: Vec<P> = vec![P::from(0).unwrap(); channel_size * channel_num];
    let mut vals = vec![0.0; channel_num];

    let min_value = P::DEFAULT_MIN_VALUE.to_f32().unwrap();
    let max_value = P::DEFAULT_MAX_VALUE.to_f32().unwrap();

    for i in 0..height {
        for (channel, value) in vals.iter_mut().enumerate().take(channel_num) {
            *value = ((-(r as isize))..(r + 1) as isize)
                .map(|x| {
                    extended_f(samples, width, height, x, i as isize, channel, channel_num)
                        .to_f32()
                        .unwrap_or(0.0)
                })
                .sum()
        }

        for j in 0..width {
            for channel in 0..channel_num {
                let val = vals[channel] / (2.0 * r as f32 + 1.0);
                let val = if val < min_value {
                    min_value
                } else if val > max_value {
                    max_value
                } else {
                    val
                };
                let val = P::from(val).unwrap();

                out_samples[channel_idx(channel, i + j * height, channel_num)] = val;
                vals[channel] = vals[channel]
                    - extended_f(
                        samples,
                        width,
                        height,
                        j as isize - r as isize,
                        i as isize,
                        channel,
                        channel_num,
                    )
                    .to_f32()
                    .unwrap_or(0.0)
                    + extended_f(
                        samples,
                        width,
                        height,
                        { j + r + 1 } as isize,
                        i as isize,
                        channel,
                        channel_num,
                    )
                    .to_f32()
                    .unwrap_or(0.0)
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
    let x = min(width as isize - 1, max(0, x)) as usize;
    let y = min(height as isize - 1, max(0, y)) as usize;
    samples[channel_idx(channel, y * width + x, channel_num)]
}
