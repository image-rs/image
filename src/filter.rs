use core::convert::TryInto;

use crate::common::BytesPerPixel;

/// The byte level filter applied to scanlines to prepare them for compression.
///
/// Compression in general benefits from repetitive data. The filter is a content-aware method of
/// compressing the range of occurring byte values to help the compression algorithm. Note that
/// this does not operate on pixels but on raw bytes of a scanline.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum FilterType {
    NoFilter = 0,
    Sub = 1,
    Up = 2,
    Avg = 3,
    Paeth = 4,
}

impl Default for FilterType {
    fn default() -> Self {
        FilterType::Sub
    }
}

impl FilterType {
    /// u8 -> Self. Temporary solution until Rust provides a canonical one.
    pub fn from_u8(n: u8) -> Option<FilterType> {
        match n {
            0 => Some(FilterType::NoFilter),
            1 => Some(FilterType::Sub),
            2 => Some(FilterType::Up),
            3 => Some(FilterType::Avg),
            4 => Some(FilterType::Paeth),
            _ => None,
        }
    }
}

/// The filtering method for preprocessing scanline data before compression.
///
/// Adaptive filtering performs additional computation in an attempt to maximize
/// the compression of the data. [`NonAdaptive`] filtering is the default.
///
/// [`NonAdaptive`]: enum.AdaptiveFilterType.html#variant.NonAdaptive
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum AdaptiveFilterType {
    Adaptive,
    NonAdaptive,
}

impl Default for AdaptiveFilterType {
    fn default() -> Self {
        AdaptiveFilterType::NonAdaptive
    }
}

fn filter_paeth_decode(a: u8, b: u8, c: u8) -> u8 {
    // Decoding seems to optimize better with this algorithm
    let pa = (i16::from(b) - i16::from(c)).abs();
    let pb = (i16::from(a) - i16::from(c)).abs();
    let pc = ((i16::from(a) - i16::from(c)) + (i16::from(b) - i16::from(c))).abs();

    let mut out = a;
    let mut min = pa;

    if pb < min {
        min = pb;
        out = b;
    }
    if pc < min {
        out = c;
    }

    out
}

fn filter_paeth(a: u8, b: u8, c: u8) -> u8 {
    // This is an optimized version of the paeth filter from the PNG specification, proposed by
    // Luca Versari for [FPNGE](https://www.lucaversari.it/FJXL_and_FPNGE.pdf). It operates
    // entirely on unsigned 8-bit quantities, making it more conducive to vectorization.
    //
    //     p = a + b - c
    //     pa = |p - a| = |a + b - c - a| = |b - c| = max(b, c) - min(b, c)
    //     pb = |p - b| = |a + b - c - b| = |a - c| = max(a, c) - min(a, c)
    //     pc = |p - c| = |a + b - c - c| = |(b - c) + (a - c)| = ...
    //
    // Further optimizing the calculation of `pc` a bit tricker. However, notice that:
    //
    //        a > c && b > c
    //    ==> (a - c) > 0 && (b - c) > 0
    //    ==> pc > (a - c) && pc > (b - c)
    //    ==> pc > |a - c| && pc > |b - c|
    //    ==> pc > pb && pc > pa
    //
    // Meaning that if `c` is smaller than `a` and `b`, the value of `pc` is irrelevant. Similar
    // reasoning applies if `c` is larger than the other two inputs. Assuming that `c >= b` and
    // `c <= b` or vice versa:
    //
    //     pc = ||b - c| - |a - c|| =  |pa - pb| = max(pa, pb) - min(pa, pb)
    //
    let pa = b.max(c) - c.min(b);
    let pb = a.max(c) - c.min(a);
    let pc = if (a < c) == (c < b) {
        pa.max(pb) - pa.min(pb)
    } else {
        255
    };

    if pa <= pb && pa <= pc {
        a
    } else if pb <= pc {
        b
    } else {
        c
    }
}

pub(crate) fn unfilter(
    filter: FilterType,
    tbpp: BytesPerPixel,
    previous: &[u8],
    current: &mut [u8],
) {
    use self::FilterType::*;

    // [2023/01 @okaneco] - Notes on optimizing decoding filters
    //
    // Links:
    // [PR]: https://github.com/image-rs/image-png/pull/382
    // [SWAR]: http://aggregate.org/SWAR/over.html
    // [AVG]: http://aggregate.org/MAGIC/#Average%20of%20Integers
    //
    // #382 heavily refactored and optimized the following filters making the
    // implementation nonobvious. These comments function as a summary of that
    // PR with an explanation of the choices made below.
    //
    // #382 originally started with trying to optimize using a technique called
    // SWAR, SIMD Within a Register. SWAR uses regular integer types like `u32`
    // and `u64` as SIMD registers to perform vertical operations in parallel,
    // usually involving bit-twiddling. This allowed each `BytesPerPixel` (bpp)
    // pixel to be decoded in parallel: 3bpp and 4bpp in a `u32`, 6bpp and 8pp
    // in a `u64`. The `Sub` filter looked like the following code block, `Avg`
    // was similar but used a bitwise average method from [AVG]:
    // ```
    // // See "Unpartitioned Operations With Correction Code" from [SWAR]
    // fn swar_add_u32(x: u32, y: u32) -> u32 {
    //     // 7-bit addition so there's no carry over the most significant bit
    //     let n = (x & 0x7f7f7f7f) + (y & 0x7f7f7f7f); // 0x7F = 0b_0111_1111
    //     // 1-bit parity/XOR addition to fill in the missing MSB
    //     n ^ (x ^ y) & 0x80808080                     // 0x80 = 0b_1000_0000
    // }
    //
    // let mut prev =
    //     u32::from_ne_bytes([current[0], current[1], current[2], current[3]]);
    // for chunk in current[4..].chunks_exact_mut(4) {
    //     let cur = u32::from_ne_bytes([chunk[0], chunk[1], chunk[2], chunk[3]]);
    //     let new_chunk = swar_add_u32(cur, prev);
    //     chunk.copy_from_slice(&new_chunk.to_ne_bytes());
    //     prev = new_chunk;
    // }
    // ```
    // While this provided a measurable increase, @fintelia found that this idea
    // could be taken even further by unrolling the chunks component-wise and
    // avoiding unnecessary byte-shuffling by using byte arrays instead of
    // `u32::from|to_ne_bytes`. The bitwise operations were no longer necessary
    // so they were reverted to their obvious arithmetic equivalent. Lastly,
    // `TryInto` was used instead of `copy_from_slice`. The `Sub` code now
    // looked like this (with asserts to remove `0..bpp` bounds checks):
    // ```
    // assert!(len > 3);
    // let mut prev = [current[0], current[1], current[2], current[3]];
    // for chunk in current[4..].chunks_exact_mut(4) {
    //     let new_chunk = [
    //         chunk[0].wrapping_add(prev[0]),
    //         chunk[1].wrapping_add(prev[1]),
    //         chunk[2].wrapping_add(prev[2]),
    //         chunk[3].wrapping_add(prev[3]),
    //     ];
    //     *TryInto::<&mut [u8; 4]>::try_into(chunk).unwrap() = new_chunk;
    //     prev = new_chunk;
    // }
    // ```
    // The compiler was able to optimize the code to be even faster and this
    // method even sped up Paeth filtering! Assertions were experimentally
    // added within loop bodies which produced better instructions but no
    // difference in speed. Finally, the code was refactored to remove manual
    // slicing and start the previous pixel chunks with arrays of `[0; N]`.
    // ```
    // let mut prev = [0; 4];
    // for chunk in current.chunks_exact_mut(4) {
    //     let new_chunk = [
    //         chunk[0].wrapping_add(prev[0]),
    //         chunk[1].wrapping_add(prev[1]),
    //         chunk[2].wrapping_add(prev[2]),
    //         chunk[3].wrapping_add(prev[3]),
    //     ];
    //     *TryInto::<&mut [u8; 4]>::try_into(chunk).unwrap() = new_chunk;
    //     prev = new_chunk;
    // }
    // ```
    // While we're not manually bit-twiddling anymore, a possible takeaway from
    // this is to "think in SWAR" when dealing with small byte arrays. Unrolling
    // array operations and performing them component-wise may unlock previously
    // unavailable optimizations from the compiler, even when using the
    // `chunks_exact` methods for their potential auto-vectorization benefits.
    match filter {
        NoFilter => {}
        Sub => match tbpp {
            BytesPerPixel::One => {
                current.iter_mut().reduce(|&mut prev, curr| {
                    *curr = curr.wrapping_add(prev);
                    curr
                });
            }
            BytesPerPixel::Two => {
                let mut prev = [0; 2];
                for chunk in current.chunks_exact_mut(2) {
                    let new_chunk = [
                        chunk[0].wrapping_add(prev[0]),
                        chunk[1].wrapping_add(prev[1]),
                    ];
                    *TryInto::<&mut [u8; 2]>::try_into(chunk).unwrap() = new_chunk;
                    prev = new_chunk;
                }
            }
            BytesPerPixel::Three => {
                let mut prev = [0; 3];
                for chunk in current.chunks_exact_mut(3) {
                    let new_chunk = [
                        chunk[0].wrapping_add(prev[0]),
                        chunk[1].wrapping_add(prev[1]),
                        chunk[2].wrapping_add(prev[2]),
                    ];
                    *TryInto::<&mut [u8; 3]>::try_into(chunk).unwrap() = new_chunk;
                    prev = new_chunk;
                }
            }
            BytesPerPixel::Four => {
                let mut prev = [0; 4];
                for chunk in current.chunks_exact_mut(4) {
                    let new_chunk = [
                        chunk[0].wrapping_add(prev[0]),
                        chunk[1].wrapping_add(prev[1]),
                        chunk[2].wrapping_add(prev[2]),
                        chunk[3].wrapping_add(prev[3]),
                    ];
                    *TryInto::<&mut [u8; 4]>::try_into(chunk).unwrap() = new_chunk;
                    prev = new_chunk;
                }
            }
            BytesPerPixel::Six => {
                let mut prev = [0; 6];
                for chunk in current.chunks_exact_mut(6) {
                    let new_chunk = [
                        chunk[0].wrapping_add(prev[0]),
                        chunk[1].wrapping_add(prev[1]),
                        chunk[2].wrapping_add(prev[2]),
                        chunk[3].wrapping_add(prev[3]),
                        chunk[4].wrapping_add(prev[4]),
                        chunk[5].wrapping_add(prev[5]),
                    ];
                    *TryInto::<&mut [u8; 6]>::try_into(chunk).unwrap() = new_chunk;
                    prev = new_chunk;
                }
            }
            BytesPerPixel::Eight => {
                let mut prev = [0; 8];
                for chunk in current.chunks_exact_mut(8) {
                    let new_chunk = [
                        chunk[0].wrapping_add(prev[0]),
                        chunk[1].wrapping_add(prev[1]),
                        chunk[2].wrapping_add(prev[2]),
                        chunk[3].wrapping_add(prev[3]),
                        chunk[4].wrapping_add(prev[4]),
                        chunk[5].wrapping_add(prev[5]),
                        chunk[6].wrapping_add(prev[6]),
                        chunk[7].wrapping_add(prev[7]),
                    ];
                    *TryInto::<&mut [u8; 8]>::try_into(chunk).unwrap() = new_chunk;
                    prev = new_chunk;
                }
            }
        },
        Up => {
            for (curr, &above) in current.iter_mut().zip(previous) {
                *curr = curr.wrapping_add(above);
            }
        }
        Avg => match tbpp {
            BytesPerPixel::One => {
                let mut lprev = [0; 1];
                for (chunk, above) in current.chunks_exact_mut(1).zip(previous.chunks_exact(1)) {
                    let new_chunk =
                        [chunk[0].wrapping_add(((above[0] as u16 + lprev[0] as u16) / 2) as u8)];
                    *TryInto::<&mut [u8; 1]>::try_into(chunk).unwrap() = new_chunk;
                    lprev = new_chunk;
                }
            }
            BytesPerPixel::Two => {
                let mut lprev = [0; 2];
                for (chunk, above) in current.chunks_exact_mut(2).zip(previous.chunks_exact(2)) {
                    let new_chunk = [
                        chunk[0].wrapping_add(((above[0] as u16 + lprev[0] as u16) / 2) as u8),
                        chunk[1].wrapping_add(((above[1] as u16 + lprev[1] as u16) / 2) as u8),
                    ];
                    *TryInto::<&mut [u8; 2]>::try_into(chunk).unwrap() = new_chunk;
                    lprev = new_chunk;
                }
            }
            BytesPerPixel::Three => {
                let mut lprev = [0; 3];
                for (chunk, above) in current.chunks_exact_mut(3).zip(previous.chunks_exact(3)) {
                    let new_chunk = [
                        chunk[0].wrapping_add(((above[0] as u16 + lprev[0] as u16) / 2) as u8),
                        chunk[1].wrapping_add(((above[1] as u16 + lprev[1] as u16) / 2) as u8),
                        chunk[2].wrapping_add(((above[2] as u16 + lprev[2] as u16) / 2) as u8),
                    ];
                    *TryInto::<&mut [u8; 3]>::try_into(chunk).unwrap() = new_chunk;
                    lprev = new_chunk;
                }
            }
            BytesPerPixel::Four => {
                let mut lprev = [0; 4];
                for (chunk, above) in current.chunks_exact_mut(4).zip(previous.chunks_exact(4)) {
                    let new_chunk = [
                        chunk[0].wrapping_add(((above[0] as u16 + lprev[0] as u16) / 2) as u8),
                        chunk[1].wrapping_add(((above[1] as u16 + lprev[1] as u16) / 2) as u8),
                        chunk[2].wrapping_add(((above[2] as u16 + lprev[2] as u16) / 2) as u8),
                        chunk[3].wrapping_add(((above[3] as u16 + lprev[3] as u16) / 2) as u8),
                    ];
                    *TryInto::<&mut [u8; 4]>::try_into(chunk).unwrap() = new_chunk;
                    lprev = new_chunk;
                }
            }
            BytesPerPixel::Six => {
                let mut lprev = [0; 6];
                for (chunk, above) in current.chunks_exact_mut(6).zip(previous.chunks_exact(6)) {
                    let new_chunk = [
                        chunk[0].wrapping_add(((above[0] as u16 + lprev[0] as u16) / 2) as u8),
                        chunk[1].wrapping_add(((above[1] as u16 + lprev[1] as u16) / 2) as u8),
                        chunk[2].wrapping_add(((above[2] as u16 + lprev[2] as u16) / 2) as u8),
                        chunk[3].wrapping_add(((above[3] as u16 + lprev[3] as u16) / 2) as u8),
                        chunk[4].wrapping_add(((above[4] as u16 + lprev[4] as u16) / 2) as u8),
                        chunk[5].wrapping_add(((above[5] as u16 + lprev[5] as u16) / 2) as u8),
                    ];
                    *TryInto::<&mut [u8; 6]>::try_into(chunk).unwrap() = new_chunk;
                    lprev = new_chunk;
                }
            }
            BytesPerPixel::Eight => {
                let mut lprev = [0; 8];
                for (chunk, above) in current.chunks_exact_mut(8).zip(previous.chunks_exact(8)) {
                    let new_chunk = [
                        chunk[0].wrapping_add(((above[0] as u16 + lprev[0] as u16) / 2) as u8),
                        chunk[1].wrapping_add(((above[1] as u16 + lprev[1] as u16) / 2) as u8),
                        chunk[2].wrapping_add(((above[2] as u16 + lprev[2] as u16) / 2) as u8),
                        chunk[3].wrapping_add(((above[3] as u16 + lprev[3] as u16) / 2) as u8),
                        chunk[4].wrapping_add(((above[4] as u16 + lprev[4] as u16) / 2) as u8),
                        chunk[5].wrapping_add(((above[5] as u16 + lprev[5] as u16) / 2) as u8),
                        chunk[6].wrapping_add(((above[6] as u16 + lprev[6] as u16) / 2) as u8),
                        chunk[7].wrapping_add(((above[7] as u16 + lprev[7] as u16) / 2) as u8),
                    ];
                    *TryInto::<&mut [u8; 8]>::try_into(chunk).unwrap() = new_chunk;
                    lprev = new_chunk;
                }
            }
        },
        Paeth => {
            // Paeth filter pixels:
            // C B D
            // A X
            match tbpp {
                BytesPerPixel::One => {
                    let mut a_bpp = [0; 1];
                    let mut c_bpp = [0; 1];
                    for (chunk, b_bpp) in current.chunks_exact_mut(1).zip(previous.chunks_exact(1))
                    {
                        let new_chunk = [chunk[0]
                            .wrapping_add(filter_paeth_decode(a_bpp[0], b_bpp[0], c_bpp[0]))];
                        *TryInto::<&mut [u8; 1]>::try_into(chunk).unwrap() = new_chunk;
                        a_bpp = new_chunk;
                        c_bpp = b_bpp.try_into().unwrap();
                    }
                }
                BytesPerPixel::Two => {
                    let mut a_bpp = [0; 2];
                    let mut c_bpp = [0; 2];
                    for (chunk, b_bpp) in current.chunks_exact_mut(2).zip(previous.chunks_exact(2))
                    {
                        let new_chunk = [
                            chunk[0]
                                .wrapping_add(filter_paeth_decode(a_bpp[0], b_bpp[0], c_bpp[0])),
                            chunk[1]
                                .wrapping_add(filter_paeth_decode(a_bpp[1], b_bpp[1], c_bpp[1])),
                        ];
                        *TryInto::<&mut [u8; 2]>::try_into(chunk).unwrap() = new_chunk;
                        a_bpp = new_chunk;
                        c_bpp = b_bpp.try_into().unwrap();
                    }
                }
                BytesPerPixel::Three => {
                    let mut a_bpp = [0; 3];
                    let mut c_bpp = [0; 3];
                    for (chunk, b_bpp) in current.chunks_exact_mut(3).zip(previous.chunks_exact(3))
                    {
                        let new_chunk = [
                            chunk[0]
                                .wrapping_add(filter_paeth_decode(a_bpp[0], b_bpp[0], c_bpp[0])),
                            chunk[1]
                                .wrapping_add(filter_paeth_decode(a_bpp[1], b_bpp[1], c_bpp[1])),
                            chunk[2]
                                .wrapping_add(filter_paeth_decode(a_bpp[2], b_bpp[2], c_bpp[2])),
                        ];
                        *TryInto::<&mut [u8; 3]>::try_into(chunk).unwrap() = new_chunk;
                        a_bpp = new_chunk;
                        c_bpp = b_bpp.try_into().unwrap();
                    }
                }
                BytesPerPixel::Four => {
                    let mut a_bpp = [0; 4];
                    let mut c_bpp = [0; 4];
                    for (chunk, b_bpp) in current.chunks_exact_mut(4).zip(previous.chunks_exact(4))
                    {
                        let new_chunk = [
                            chunk[0]
                                .wrapping_add(filter_paeth_decode(a_bpp[0], b_bpp[0], c_bpp[0])),
                            chunk[1]
                                .wrapping_add(filter_paeth_decode(a_bpp[1], b_bpp[1], c_bpp[1])),
                            chunk[2]
                                .wrapping_add(filter_paeth_decode(a_bpp[2], b_bpp[2], c_bpp[2])),
                            chunk[3]
                                .wrapping_add(filter_paeth_decode(a_bpp[3], b_bpp[3], c_bpp[3])),
                        ];
                        *TryInto::<&mut [u8; 4]>::try_into(chunk).unwrap() = new_chunk;
                        a_bpp = new_chunk;
                        c_bpp = b_bpp.try_into().unwrap();
                    }
                }
                BytesPerPixel::Six => {
                    let mut a_bpp = [0; 6];
                    let mut c_bpp = [0; 6];
                    for (chunk, b_bpp) in current.chunks_exact_mut(6).zip(previous.chunks_exact(6))
                    {
                        let new_chunk = [
                            chunk[0]
                                .wrapping_add(filter_paeth_decode(a_bpp[0], b_bpp[0], c_bpp[0])),
                            chunk[1]
                                .wrapping_add(filter_paeth_decode(a_bpp[1], b_bpp[1], c_bpp[1])),
                            chunk[2]
                                .wrapping_add(filter_paeth_decode(a_bpp[2], b_bpp[2], c_bpp[2])),
                            chunk[3]
                                .wrapping_add(filter_paeth_decode(a_bpp[3], b_bpp[3], c_bpp[3])),
                            chunk[4]
                                .wrapping_add(filter_paeth_decode(a_bpp[4], b_bpp[4], c_bpp[4])),
                            chunk[5]
                                .wrapping_add(filter_paeth_decode(a_bpp[5], b_bpp[5], c_bpp[5])),
                        ];
                        *TryInto::<&mut [u8; 6]>::try_into(chunk).unwrap() = new_chunk;
                        a_bpp = new_chunk;
                        c_bpp = b_bpp.try_into().unwrap();
                    }
                }
                BytesPerPixel::Eight => {
                    let mut a_bpp = [0; 8];
                    let mut c_bpp = [0; 8];
                    for (chunk, b_bpp) in current.chunks_exact_mut(8).zip(previous.chunks_exact(8))
                    {
                        let new_chunk = [
                            chunk[0]
                                .wrapping_add(filter_paeth_decode(a_bpp[0], b_bpp[0], c_bpp[0])),
                            chunk[1]
                                .wrapping_add(filter_paeth_decode(a_bpp[1], b_bpp[1], c_bpp[1])),
                            chunk[2]
                                .wrapping_add(filter_paeth_decode(a_bpp[2], b_bpp[2], c_bpp[2])),
                            chunk[3]
                                .wrapping_add(filter_paeth_decode(a_bpp[3], b_bpp[3], c_bpp[3])),
                            chunk[4]
                                .wrapping_add(filter_paeth_decode(a_bpp[4], b_bpp[4], c_bpp[4])),
                            chunk[5]
                                .wrapping_add(filter_paeth_decode(a_bpp[5], b_bpp[5], c_bpp[5])),
                            chunk[6]
                                .wrapping_add(filter_paeth_decode(a_bpp[6], b_bpp[6], c_bpp[6])),
                            chunk[7]
                                .wrapping_add(filter_paeth_decode(a_bpp[7], b_bpp[7], c_bpp[7])),
                        ];
                        *TryInto::<&mut [u8; 8]>::try_into(chunk).unwrap() = new_chunk;
                        a_bpp = new_chunk;
                        c_bpp = b_bpp.try_into().unwrap();
                    }
                }
            }
        }
    }
}

fn filter_internal(
    method: FilterType,
    bpp: usize,
    len: usize,
    previous: &[u8],
    current: &[u8],
    output: &mut [u8],
) -> FilterType {
    use self::FilterType::*;

    // This value was chosen experimentally based on what acheived the best performance. The
    // Rust compiler does auto-vectorization, and 32-bytes per loop iteration seems to enable
    // the fastest code when doing so.
    const CHUNK_SIZE: usize = 32;

    match method {
        NoFilter => {
            output.copy_from_slice(current);
            NoFilter
        }
        Sub => {
            let mut out_chunks = output[bpp..].chunks_exact_mut(CHUNK_SIZE);
            let mut cur_chunks = current[bpp..].chunks_exact(CHUNK_SIZE);
            let mut prev_chunks = current[..len - bpp].chunks_exact(CHUNK_SIZE);

            for ((out, cur), prev) in (&mut out_chunks).zip(&mut cur_chunks).zip(&mut prev_chunks) {
                for i in 0..CHUNK_SIZE {
                    out[i] = cur[i].wrapping_sub(prev[i]);
                }
            }

            for ((out, cur), &prev) in out_chunks
                .into_remainder()
                .iter_mut()
                .zip(cur_chunks.remainder())
                .zip(prev_chunks.remainder())
            {
                *out = cur.wrapping_sub(prev);
            }

            output[..bpp].copy_from_slice(&current[..bpp]);
            Sub
        }
        Up => {
            let mut out_chunks = output.chunks_exact_mut(CHUNK_SIZE);
            let mut cur_chunks = current.chunks_exact(CHUNK_SIZE);
            let mut prev_chunks = previous.chunks_exact(CHUNK_SIZE);

            for ((out, cur), prev) in (&mut out_chunks).zip(&mut cur_chunks).zip(&mut prev_chunks) {
                for i in 0..CHUNK_SIZE {
                    out[i] = cur[i].wrapping_sub(prev[i]);
                }
            }

            for ((out, cur), &prev) in out_chunks
                .into_remainder()
                .iter_mut()
                .zip(cur_chunks.remainder())
                .zip(prev_chunks.remainder())
            {
                *out = cur.wrapping_sub(prev);
            }
            Up
        }
        Avg => {
            let mut out_chunks = output[bpp..].chunks_exact_mut(CHUNK_SIZE);
            let mut cur_chunks = current[bpp..].chunks_exact(CHUNK_SIZE);
            let mut cur_minus_bpp_chunks = current[..len - bpp].chunks_exact(CHUNK_SIZE);
            let mut prev_chunks = previous[bpp..].chunks_exact(CHUNK_SIZE);

            for (((out, cur), cur_minus_bpp), prev) in (&mut out_chunks)
                .zip(&mut cur_chunks)
                .zip(&mut cur_minus_bpp_chunks)
                .zip(&mut prev_chunks)
            {
                for i in 0..CHUNK_SIZE {
                    // Bitwise average of two integers without overflow and
                    // without converting to a wider bit-width. See:
                    // http://aggregate.org/MAGIC/#Average%20of%20Integers
                    // If this is unrolled by component, consider reverting to
                    // `((cur_minus_bpp[i] as u16 + prev[i] as u16) / 2) as u8`
                    out[i] = cur[i].wrapping_sub(
                        (cur_minus_bpp[i] & prev[i]) + ((cur_minus_bpp[i] ^ prev[i]) >> 1),
                    );
                }
            }

            for (((out, cur), &cur_minus_bpp), &prev) in out_chunks
                .into_remainder()
                .iter_mut()
                .zip(cur_chunks.remainder())
                .zip(cur_minus_bpp_chunks.remainder())
                .zip(prev_chunks.remainder())
            {
                *out = cur.wrapping_sub((cur_minus_bpp & prev) + ((cur_minus_bpp ^ prev) >> 1));
            }

            for i in 0..bpp {
                output[i] = current[i].wrapping_sub(previous[i] / 2);
            }
            Avg
        }
        Paeth => {
            let mut out_chunks = output[bpp..].chunks_exact_mut(CHUNK_SIZE);
            let mut cur_chunks = current[bpp..].chunks_exact(CHUNK_SIZE);
            let mut a_chunks = current[..len - bpp].chunks_exact(CHUNK_SIZE);
            let mut b_chunks = previous[bpp..].chunks_exact(CHUNK_SIZE);
            let mut c_chunks = previous[..len - bpp].chunks_exact(CHUNK_SIZE);

            for ((((out, cur), a), b), c) in (&mut out_chunks)
                .zip(&mut cur_chunks)
                .zip(&mut a_chunks)
                .zip(&mut b_chunks)
                .zip(&mut c_chunks)
            {
                for i in 0..CHUNK_SIZE {
                    out[i] = cur[i].wrapping_sub(filter_paeth(a[i], b[i], c[i]));
                }
            }

            for ((((out, cur), &a), &b), &c) in out_chunks
                .into_remainder()
                .iter_mut()
                .zip(cur_chunks.remainder())
                .zip(a_chunks.remainder())
                .zip(b_chunks.remainder())
                .zip(c_chunks.remainder())
            {
                *out = cur.wrapping_sub(filter_paeth(a, b, c));
            }

            for i in 0..bpp {
                output[i] = current[i].wrapping_sub(filter_paeth(0, previous[i], 0));
            }
            Paeth
        }
    }
}

pub(crate) fn filter(
    method: FilterType,
    adaptive: AdaptiveFilterType,
    bpp: BytesPerPixel,
    previous: &[u8],
    current: &[u8],
    output: &mut [u8],
) -> FilterType {
    use FilterType::*;
    let bpp = bpp.into_usize();
    let len = current.len();

    match adaptive {
        AdaptiveFilterType::NonAdaptive => {
            filter_internal(method, bpp, len, previous, current, output)
        }
        AdaptiveFilterType::Adaptive => {
            let mut min_sum: u64 = u64::MAX;
            let mut filter_choice = FilterType::NoFilter;
            for &filter in [Sub, Up, Avg, Paeth].iter() {
                filter_internal(filter, bpp, len, previous, current, output);
                let sum = sum_buffer(output);
                if sum <= min_sum {
                    min_sum = sum;
                    filter_choice = filter;
                }
            }

            if filter_choice != Paeth {
                filter_internal(filter_choice, bpp, len, previous, current, output);
            }
            filter_choice
        }
    }
}

// Helper function for Adaptive filter buffer summation
fn sum_buffer(buf: &[u8]) -> u64 {
    const CHUNK_SIZE: usize = 32;

    let mut buf_chunks = buf.chunks_exact(CHUNK_SIZE);
    let mut sum = 0_u64;

    for chunk in &mut buf_chunks {
        // At most, `acc` can be `32 * (i8::MIN as u8) = 32 * 128 = 4096`.
        let mut acc = 0;
        for &b in chunk {
            acc += u64::from((b as i8).unsigned_abs());
        }
        sum = sum.saturating_add(acc);
    }

    let mut acc = 0;
    for &b in buf_chunks.remainder() {
        acc += u64::from((b as i8).unsigned_abs());
    }

    sum.saturating_add(acc)
}

#[cfg(test)]
mod test {
    use super::{filter, unfilter, AdaptiveFilterType, BytesPerPixel, FilterType};
    use core::iter;

    #[test]
    fn roundtrip() {
        // A multiple of 8, 6, 4, 3, 2, 1
        const LEN: u8 = 240;
        let previous: Vec<_> = iter::repeat(1).take(LEN.into()).collect();
        let current: Vec<_> = (0..LEN).collect();
        let expected = current.clone();
        let adaptive = AdaptiveFilterType::NonAdaptive;

        let roundtrip = |kind, bpp: BytesPerPixel| {
            let mut output = vec![0; LEN.into()];
            filter(kind, adaptive, bpp, &previous, &current, &mut output);
            unfilter(kind, bpp, &previous, &mut output);
            assert_eq!(
                output, expected,
                "Filtering {:?} with {:?} does not roundtrip",
                bpp, kind
            );
        };

        let filters = [
            FilterType::NoFilter,
            FilterType::Sub,
            FilterType::Up,
            FilterType::Avg,
            FilterType::Paeth,
        ];

        let bpps = [
            BytesPerPixel::One,
            BytesPerPixel::Two,
            BytesPerPixel::Three,
            BytesPerPixel::Four,
            BytesPerPixel::Six,
            BytesPerPixel::Eight,
        ];

        for &filter in filters.iter() {
            for &bpp in bpps.iter() {
                roundtrip(filter, bpp);
            }
        }
    }

    #[test]
    fn roundtrip_ascending_previous_line() {
        // A multiple of 8, 6, 4, 3, 2, 1
        const LEN: u8 = 240;
        let previous: Vec<_> = (0..LEN).collect();
        let current: Vec<_> = (0..LEN).collect();
        let expected = current.clone();
        let adaptive = AdaptiveFilterType::NonAdaptive;

        let roundtrip = |kind, bpp: BytesPerPixel| {
            let mut output = vec![0; LEN.into()];
            filter(kind, adaptive, bpp, &previous, &current, &mut output);
            unfilter(kind, bpp, &previous, &mut output);
            assert_eq!(
                output, expected,
                "Filtering {:?} with {:?} does not roundtrip",
                bpp, kind
            );
        };

        let filters = [
            FilterType::NoFilter,
            FilterType::Sub,
            FilterType::Up,
            FilterType::Avg,
            FilterType::Paeth,
        ];

        let bpps = [
            BytesPerPixel::One,
            BytesPerPixel::Two,
            BytesPerPixel::Three,
            BytesPerPixel::Four,
            BytesPerPixel::Six,
            BytesPerPixel::Eight,
        ];

        for &filter in filters.iter() {
            for &bpp in bpps.iter() {
                roundtrip(filter, bpp);
            }
        }
    }

    #[test]
    // This tests that converting u8 to i8 doesn't overflow when taking the
    // absolute value for adaptive filtering: -128_i8.abs() will panic in debug
    // or produce garbage in release mode. The sum of 0..=255u8 should equal the
    // sum of the absolute values of -128_i8..=127, or abs(-128..=0) + 1..=127.
    fn sum_buffer_test() {
        let sum = (0..=128).sum::<u64>() + (1..=127).sum::<u64>();
        let buf: Vec<u8> = (0_u8..=255).collect();

        assert_eq!(sum, crate::filter::sum_buffer(&buf));
    }
}
