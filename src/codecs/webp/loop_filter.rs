//! Does loop filtering on webp lossy images

use crate::utils::{clamp, clamp_simd};

#[inline]
fn c(val: i32) -> i32 {
    clamp(val, -128, 127)
}

#[inline]
fn c_simd(val: [i32; 4]) -> [i32; 4] {
    clamp_simd(val, -128, 127)
}

//unsigned to signed
#[inline]
fn u2s(val: u8) -> i32 {
    i32::from(val) - 128
}

//signed to unsigned
#[inline]
fn s2u(val: i32) -> u8 {
    (c(val) + 128) as u8
}

//signed to unsigned
#[inline]
fn s2u_simd(mut val: [i32; 4]) -> [u8; 4] {
    val.iter_mut().for_each(|i| *i += 128);
    c_simd(val);
    [val[0] as u8, val[1] as u8, val[2] as u8, val[3] as u8]
}

#[inline]
fn diff(val1: u8, val2: u8) -> u8 {
    if val1 > val2 {
        val1 - val2
    } else {
        val2 - val1
    }
}

//15.2
fn common_adjust(use_outer_taps: bool, pixels: &mut [u8], point: usize, stride: usize) -> i32 {
    let p1 = u2s(pixels[point - 2 * stride]);
    let p0 = u2s(pixels[point - stride]);
    let q0 = u2s(pixels[point]);
    let q1 = u2s(pixels[point + stride]);

    //value for the outer 2 pixels
    let outer = if use_outer_taps { c(p1 - q1) } else { 0 };

    let mut a = c(outer + 3 * (q0 - p0));

    let b = (c(a + 3)) >> 3;

    a = (c(a + 4)) >> 3;

    pixels[point] = s2u(q0 - a);
    pixels[point - stride] = s2u(p0 + b);

    a
}

fn simple_threshold(filter_limit: i32, pixels: &[u8], point: usize, stride: usize) -> bool {
    i32::from(diff(pixels[point - stride], pixels[point])) * 2
        + i32::from(diff(pixels[point - 2 * stride], pixels[point + stride])) / 2
        <= filter_limit
}

fn should_filter(
    interior_limit: u8,
    edge_limit: u8,
    pixels: &[u8],
    point: usize,
    stride: usize,
) -> bool {
    simple_threshold(i32::from(edge_limit), pixels, point, stride)
        && diff(pixels[point - 4 * stride], pixels[point - 3 * stride]) <= interior_limit
        && diff(pixels[point - 3 * stride], pixels[point - 2 * stride]) <= interior_limit
        && diff(pixels[point - 2 * stride], pixels[point - stride]) <= interior_limit
        && diff(pixels[point + 3 * stride], pixels[point + 2 * stride]) <= interior_limit
        && diff(pixels[point + 2 * stride], pixels[point + stride]) <= interior_limit
        && diff(pixels[point + stride], pixels[point]) <= interior_limit
}

fn high_edge_variance(threshold: u8, pixels: &[u8], point: usize, stride: usize) -> bool {
    diff(pixels[point - 2 * stride], pixels[point - stride]) > threshold
        || diff(pixels[point + stride], pixels[point]) > threshold
}

//simple filter
//effects 4 pixels on an edge(2 each side)
pub(crate) fn simple_segment(edge_limit: u8, pixels: &mut [u8], point: usize, stride: usize) {
    if simple_threshold(i32::from(edge_limit), pixels, point, stride) {
        common_adjust(true, pixels, point, stride);
    }
}

//normal filter
//works on the 8 pixels on the edges between subblocks inside a macroblock
pub(crate) fn subblock_filter(
    hev_threshold: u8,
    interior_limit: u8,
    edge_limit: u8,
    pixels: &mut [u8],
    point: usize,
    stride: usize,
) {
    if should_filter(interior_limit, edge_limit, pixels, point, stride) {
        let hv = high_edge_variance(hev_threshold, pixels, point, stride);

        let a = (common_adjust(hv, pixels, point, stride) + 1) >> 1;

        if !hv {
            pixels[point + stride] = s2u(u2s(pixels[point + stride]) - a);
            pixels[point - 2 * stride] = s2u(u2s(pixels[point - 2 * stride]) - a);
        }
    }
}

//normal filter
//works on the 8 pixels on the edges between macroblocks
pub(crate) fn macroblock_filter(
    hev_threshold: u8,
    interior_limit: u8,
    edge_limit: u8,
    pixels: &mut [u8],
    point: usize,
    stride: usize,
) {
    let mut spixels = [0i32; 8];
    for i in 0..8 {
        spixels[i] = u2s(pixels[point + i * stride - 4 * stride]);
    }

    if should_filter(interior_limit, edge_limit, pixels, point, stride) {
        if !high_edge_variance(hev_threshold, pixels, point, stride) {
            let w = c(c(spixels[2] - spixels[5]) + 3 * (spixels[4] - spixels[3]));

            let a = c_simd([
                (27 * w + 63) >> 7, // a1
                (18 * w + 63) >> 7, // a2
                (9 * w + 63) >> 7,  // a3
                0,                  // padding to 4 elements in SIMD lane
            ]);

            // Gather the values we're going to use into SIMD registers.
            // The last element is going to be unused, it just has to be 4 to match SIMD lanes
            let mut sub: [i32; 4] = spixels[4..8].try_into().unwrap();
            let mut add: [i32; 4] = spixels[1..5].try_into().unwrap();

            // Run the addition and subtraction operations
            sub.iter_mut().zip(a.iter()).for_each(|(i, a)| *i -= a);
            add.iter_mut().zip(a.iter()).for_each(|(i, a)| *i += a);

            // Convert from i32 to u8, clamping the values
            let add = s2u_simd(add);
            let sub = s2u_simd(sub);

            // Take the values out of SIMD lanes and output them
            pixels[point] = sub[0];
            pixels[point + stride] = sub[1];
            pixels[point + 2 * stride] = sub[2];

            pixels[point - 3 * stride] = add[0];
            pixels[point - 2 * stride] = add[1];
            pixels[point - stride] = add[2];
        } else {
            common_adjust(true, pixels, point, stride);
        }
    }
}
