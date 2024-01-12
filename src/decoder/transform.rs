//! Transforming a decompressed, unfiltered row into the final output.

use crate::Info;

#[inline(always)]
fn unpack_bits<F>(input: &[u8], output: &mut [u8], channels: usize, bit_depth: u8, func: F)
where
    F: Fn(u8, &mut [u8]),
{
    // Only [1, 2, 4, 8] are valid bit depths
    assert!(matches!(bit_depth, 1 | 2 | 4 | 8));
    // Check that `input` is capable of producing a buffer as long as `output`:
    // number of shift lookups per bit depth * channels * input length
    assert!((8 / bit_depth as usize * channels).saturating_mul(input.len()) >= output.len());

    let mut buf_chunks = output.chunks_exact_mut(channels);
    let mut iter = input.iter();

    // `shift` iterates through the corresponding bit depth sequence:
    // 1 => &[7, 6, 5, 4, 3, 2, 1, 0],
    // 2 => &[6, 4, 2, 0],
    // 4 => &[4, 0],
    // 8 => &[0],
    //
    // `(0..8).step_by(bit_depth.into()).rev()` doesn't always optimize well so
    // shifts are calculated instead. (2023-08, Rust 1.71)

    if bit_depth == 8 {
        for (&curr, chunk) in iter.zip(&mut buf_chunks) {
            func(curr, chunk);
        }
    } else {
        let mask = ((1u16 << bit_depth) - 1) as u8;

        // These variables are initialized in the loop
        let mut shift = -1;
        let mut curr = 0;

        for chunk in buf_chunks {
            if shift < 0 {
                shift = 8 - bit_depth as i32;
                curr = *iter.next().expect("input for unpack bits is not empty");
            }

            let pixel = (curr >> shift) & mask;
            func(pixel, chunk);

            shift -= bit_depth as i32;
        }
    }
}

pub fn expand_trns_line(input: &[u8], output: &mut [u8], trns: Option<&[u8]>, channels: usize) {
    for (input, output) in input
        .chunks_exact(channels)
        .zip(output.chunks_exact_mut(channels + 1))
    {
        output[..channels].copy_from_slice(input);
        output[channels] = if Some(input) == trns { 0 } else { 0xFF };
    }
}

pub fn expand_trns_line16(input: &[u8], output: &mut [u8], trns: Option<&[u8]>, channels: usize) {
    for (input, output) in input
        .chunks_exact(channels * 2)
        .zip(output.chunks_exact_mut(channels * 2 + 2))
    {
        output[..channels * 2].copy_from_slice(input);
        if Some(input) == trns {
            output[channels * 2] = 0;
            output[channels * 2 + 1] = 0
        } else {
            output[channels * 2] = 0xFF;
            output[channels * 2 + 1] = 0xFF
        };
    }
}

pub fn expand_trns_and_strip_line16(
    input: &[u8],
    output: &mut [u8],
    trns: Option<&[u8]>,
    channels: usize,
) {
    for (input, output) in input
        .chunks_exact(channels * 2)
        .zip(output.chunks_exact_mut(channels + 1))
    {
        for i in 0..channels {
            output[i] = input[i * 2];
        }
        output[channels] = if Some(input) == trns { 0 } else { 0xFF };
    }
}

pub fn expand_paletted(row: &[u8], buffer: &mut [u8], info: &Info, trns: Option<Option<&[u8]>>) {
    let palette = info.palette.as_deref().expect("Caller should verify");
    let black = [0, 0, 0];
    if let Some(trns) = trns {
        let trns = trns.unwrap_or(&[]);
        // > The tRNS chunk shall not contain more alpha values than there are palette
        // entries, but a tRNS chunk may contain fewer values than there are palette
        // entries. In this case, the alpha value for all remaining palette entries is
        // assumed to be 255.
        //
        // It seems, accepted reading is to fully *ignore* an invalid tRNS as if it were
        // completely empty / all pixels are non-transparent.
        let trns = if trns.len() <= palette.len() / 3 {
            trns
        } else {
            &[]
        };

        unpack_bits(row, buffer, 4, info.bit_depth as u8, |i, chunk| {
            let (rgb, a) = (
                palette
                    .get(3 * i as usize..3 * i as usize + 3)
                    .unwrap_or(&black),
                *trns.get(i as usize).unwrap_or(&0xFF),
            );
            chunk[0] = rgb[0];
            chunk[1] = rgb[1];
            chunk[2] = rgb[2];
            chunk[3] = a;
        });
    } else {
        unpack_bits(row, buffer, 3, info.bit_depth as u8, |i, chunk| {
            let rgb = palette
                .get(3 * i as usize..3 * i as usize + 3)
                .unwrap_or(&black);
            chunk[0] = rgb[0];
            chunk[1] = rgb[1];
            chunk[2] = rgb[2];
        })
    }
}

pub fn expand_gray_u8(row: &[u8], buffer: &mut [u8], info: &Info, trns: Option<Option<&[u8]>>) {
    let rescale = true;
    let scaling_factor = if rescale {
        (255) / ((1u16 << info.bit_depth as u8) - 1) as u8
    } else {
        1
    };
    if let Some(trns) = trns {
        unpack_bits(row, buffer, 2, info.bit_depth as u8, |pixel, chunk| {
            chunk[1] = if let Some(trns) = trns {
                if pixel == trns[0] {
                    0
                } else {
                    0xFF
                }
            } else {
                0xFF
            };
            chunk[0] = pixel * scaling_factor
        })
    } else {
        unpack_bits(row, buffer, 1, info.bit_depth as u8, |val, chunk| {
            chunk[0] = val * scaling_factor
        })
    }
}
