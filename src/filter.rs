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
    let ia = i16::from(a);
    let ib = i16::from(b);
    let ic = i16::from(c);

    let p = ia + ib - ic;

    let pa = (p - ia).abs();
    let pb = (p - ib).abs();
    let pc = (p - ic).abs();

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

fn sub(bpp: usize, len: usize, current: &mut [u8]) {
    for i in (bpp..len).rev() {
        current[i] = current[i].wrapping_sub(current[i - bpp]);
    }
}

fn up(len: usize, previous: &[u8], current: &mut [u8]) {
    for i in 0..len {
        current[i] = current[i].wrapping_sub(previous[i]);
    }
}

fn avg(bpp: usize, len: usize, previous: &[u8], current: &mut [u8]) {
    for i in (bpp..len).rev() {
        current[i] = current[i]
            .wrapping_sub(((u16::from(current[i - bpp]) + u16::from(previous[i])) / 2) as u8);
    }

    for i in 0..bpp {
        current[i] = current[i].wrapping_sub(previous[i] / 2);
    }
}

fn paeth(bpp: usize, len: usize, previous: &[u8], current: &mut [u8]) {
    for i in (bpp..len).rev() {
        current[i] = current[i].wrapping_sub(filter_paeth(
            current[i - bpp],
            previous[i],
            previous[i - bpp],
        ));
    }

    for i in 0..bpp {
        current[i] = current[i].wrapping_sub(filter_paeth(0, previous[i], 0));
    }
}

// Helper function for Adaptive filter buffer summation
fn sum_buffer(buf: &[u8]) -> usize {
    buf.iter()
        .fold(0, |acc, &x| acc.saturating_add((x as i8).abs() as usize))
}

pub(crate) fn filter(
    method: Option<FilterType>,
    bpp: BytesPerPixel,
    previous: &[u8],
    current: &mut [u8],
) -> FilterType {
    use self::FilterType::*;
    let bpp = bpp.into_usize();
    let len = current.len();

    match method {
        Some(NoFilter) => NoFilter,
        Some(Sub) => {
            sub(bpp, len, current);
            Sub
        }
        Some(Up) => {
            up(len, previous, current);
            Up
        }
        Some(Avg) => {
            avg(bpp, len, previous, current);
            Avg
        }
        Some(Paeth) => {
            paeth(bpp, len, previous, current);
            Paeth
        }
        None => {
            // Filter the current buffer with each filter type. Sum the absolute
            // values of each filtered buffer treating the bytes as signed
            // integers. Choose the filter with the smallest sum.
            let mut filtered_buffer = vec![0; len];
            filtered_buffer.copy_from_slice(&current);
            let mut scratch = vec![0; len];
            scratch.copy_from_slice(&current);
            let mut filter_type = NoFilter;

            // Initialize min_sum with the NoFilter buffer sum
            let mut min_sum: usize = sum_buffer(&filtered_buffer);

            sub(bpp, len, &mut scratch);
            let sum = sum_buffer(&scratch);
            if sum < min_sum {
                min_sum = sum;
                filter_type = Sub;
                core::mem::swap(&mut filtered_buffer, &mut scratch);
            }

            scratch.copy_from_slice(&current);
            up(len, previous, &mut scratch);
            let sum = sum_buffer(&scratch);
            if sum < min_sum {
                min_sum = sum;
                filter_type = Up;
                core::mem::swap(&mut filtered_buffer, &mut scratch);
            }

            scratch.copy_from_slice(&current);
            avg(bpp, len, previous, &mut scratch);
            let sum = sum_buffer(&scratch);
            if sum < min_sum {
                min_sum = sum;
                filter_type = Avg;
                core::mem::swap(&mut filtered_buffer, &mut scratch);
            }

            scratch.copy_from_slice(&current);
            paeth(bpp, len, previous, &mut scratch);
            let sum = sum_buffer(&scratch);
            if sum < min_sum {
                filter_type = Paeth;
                core::mem::swap(&mut filtered_buffer, &mut scratch);
            }

            current.copy_from_slice(&filtered_buffer);

            filter_type
        }
    }
}

#[cfg(test)]
mod test {
    use super::{filter, unfilter, BytesPerPixel, FilterType};
    use core::iter;

    #[test]
    fn roundtrip() {
        // A multiple of 8, 6, 4, 3, 2, 1
        const LEN: u8 = 240;
        let previous: Vec<_> = iter::repeat(1).take(LEN.into()).collect();
        let mut current: Vec<_> = (0..LEN).collect();
        let expected = current.clone();

        let mut roundtrip = |kind, bpp: BytesPerPixel| {
            filter(Some(kind), bpp, &previous, &mut current);
            unfilter(kind, bpp, &previous, &mut current).expect("Unfilter worked");
            assert_eq!(
                current, expected,
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
        let mut current: Vec<_> = (0..LEN).collect();
        let expected = current.clone();

        let mut roundtrip = |kind, bpp: BytesPerPixel| {
            filter(Some(kind), bpp, &previous, &mut current);
            unfilter(kind, bpp, &previous, &mut current).expect("Unfilter worked");
            assert_eq!(
                current, expected,
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
}
