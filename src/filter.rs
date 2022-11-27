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
) -> std::result::Result<(), &'static str> {
    use self::FilterType::*;
    let bpp = tbpp.into_usize();
    let len = current.len();

    fn require_length(slice: &[u8], length: usize) -> Result<&[u8], &'static str> {
        match slice.get(..length) {
            None => Err("Filtering failed: not enough data in previous row"),
            Some(slice) => Ok(slice),
        }
    }

    match filter {
        NoFilter => Ok(()),
        Sub => {
            let current = &mut current[..len];
            for i in bpp..len {
                current[i] = current[i].wrapping_add(current[i - bpp]);
            }
            Ok(())
        }
        Up => {
            let current = &mut current[..len];
            let previous = require_length(previous, len)?;
            for i in 0..len {
                current[i] = current[i].wrapping_add(previous[i]);
            }
            Ok(())
        }
        Avg => {
            let current = &mut current[..len];
            let previous = require_length(previous, len)?;
            if bpp > len {
                return Err("Filtering failed: bytes per pixel is greater than length of row");
            }

            for i in 0..bpp {
                current[i] = current[i].wrapping_add(previous[i] / 2);
            }

            macro_rules! avg_tail {
                ($name:ident, $bpp:expr) => {
                    fn $name(current: &mut [u8], previous: &[u8]) {
                        let len = current.len();
                        let current = &mut current[..len];
                        let previous = &previous[..len];

                        let mut current = current.chunks_exact_mut($bpp);
                        let mut previous = previous.chunks_exact($bpp);

                        let mut lprevious = current.next().unwrap();
                        let _ = previous.next();

                        while let Some(pprevious) = previous.next() {
                            let pcurrent = current.next().unwrap();

                            for i in 0..$bpp {
                                let lprev = lprevious[i];
                                let pprev = pprevious[i];
                                pcurrent[i] = pcurrent[i].wrapping_add(
                                    ((u16::from(lprev) + u16::from(pprev)) / 2) as u8,
                                );
                            }

                            lprevious = pcurrent;
                        }
                    }
                };
            }

            avg_tail!(avg_tail_8, 8);
            avg_tail!(avg_tail_6, 6);
            avg_tail!(avg_tail_4, 4);
            avg_tail!(avg_tail_3, 3);
            avg_tail!(avg_tail_2, 2);
            avg_tail!(avg_tail_1, 1);

            match tbpp {
                BytesPerPixel::Eight => avg_tail_8(current, previous),
                BytesPerPixel::Six => avg_tail_6(current, previous),
                BytesPerPixel::Four => avg_tail_4(current, previous),
                BytesPerPixel::Three => avg_tail_3(current, previous),
                BytesPerPixel::Two => avg_tail_2(current, previous),
                BytesPerPixel::One => avg_tail_1(current, previous),
            }

            Ok(())
        }
        Paeth => {
            let current = &mut current[..len];
            let previous = require_length(previous, len)?;
            if bpp > len {
                return Err("Filtering failed: bytes per pixel is greater than length of row");
            }

            for i in 0..bpp {
                current[i] = current[i].wrapping_add(filter_paeth(0, previous[i], 0));
            }

            let mut current = current.chunks_exact_mut(bpp);
            let mut previous = previous.chunks_exact(bpp);

            let mut lprevious = current.next().unwrap();
            let mut lpprevious = previous.next().unwrap();

            for pprevious in previous {
                let pcurrent = current.next().unwrap();

                for i in 0..bpp {
                    pcurrent[i] = pcurrent[i].wrapping_add(filter_paeth(
                        lprevious[i],
                        pprevious[i],
                        lpprevious[i],
                    ));
                }

                lprevious = pcurrent;
                lpprevious = pprevious;
            }

            Ok(())
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
                .into_iter()
                .zip(cur_chunks.remainder())
                .zip(prev_chunks.remainder())
            {
                *out = cur.wrapping_sub(prev);
            }

            output[..bpp].copy_from_slice(&current[..bpp]);
            Sub
        }
        Up => {
            for i in 0..len {
                output[i] = current[i].wrapping_sub(previous[i]);
            }
            Up
        }
        Avg => {
            for i in (bpp..len).rev() {
                output[i] = current[i].wrapping_sub(
                    ((u16::from(current[i - bpp]) + u16::from(previous[i])) / 2) as u8,
                );
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
                .into_iter()
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
            // Filter the current buffer with each filter type. Sum the absolute
            // values of each filtered buffer treating the bytes as signed
            // integers. Choose the filter with the smallest sum.
            let mut filtered_buffer = vec![0; len];
            filtered_buffer.copy_from_slice(current);
            let mut scratch = vec![0; len];

            // Initialize min_sum with the NoFilter buffer sum
            let mut min_sum: usize = sum_buffer(&filtered_buffer);
            let mut filter_choice = FilterType::NoFilter;

            for &filter in [Sub, Up, Avg, Paeth].iter() {
                filter_internal(filter, bpp, len, previous, current, &mut scratch);
                let sum = sum_buffer(&scratch);
                if sum < min_sum {
                    min_sum = sum;
                    filter_choice = filter;
                    core::mem::swap(&mut filtered_buffer, &mut scratch);
                }
            }

            output.copy_from_slice(&filtered_buffer);

            filter_choice
        }
    }
}

// Helper function for Adaptive filter buffer summation
fn sum_buffer(buf: &[u8]) -> usize {
    buf.iter().fold(0, |acc, &x| {
        acc.saturating_add(i16::from(x as i8).abs() as usize)
    })
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
            unfilter(kind, bpp, &previous, &mut output).expect("Unfilter worked");
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
            unfilter(kind, bpp, &previous, &mut output).expect("Unfilter worked");
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
        let sum = (0..=128).sum::<usize>() + (1..=127).sum::<usize>();
        let buf: Vec<u8> = (0_u8..=255).collect();

        assert_eq!(sum, crate::filter::sum_buffer(&buf));
    }
}
