use std::num::Wrapping as w;

enum_from_primitive! {
#[derive(Debug)]
pub enum FilterType {
    NoFilter = 0,
    Sub = 1,
    Up = 2,
    Avg = 3,
    Paeth = 4
}
}

fn filter_paeth(a: u8, b: u8, c: u8) -> u8 {
    let ia = a as i16;
    let ib = b as i16;
    let ic = c as i16;

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

pub fn unfilter(filter: FilterType, bpp: usize, previous: &[u8], current: &mut [u8]) {
    let len = current.len();

    match filter {
        FilterType::NoFilter => (),
        FilterType::Sub => {
            for i in (bpp..len) {
                current[i] = (w(current[i]) + w(current[i - bpp])).0;
            }
        }
        FilterType::Up => {
            for i in (0..len) {
                current[i] = (w(current[i]) + w(previous[i])).0;
            }
        }
        FilterType::Avg => {
            for i in (0..bpp) {
                current[i] = (w(current[i]) + w(previous[i] / 2)).0;
            }

            for i in (bpp..len) {
                current[i] = (w(current[i]) + w(((current[i - bpp] as i16 + previous[i] as i16) / 2) as u8)).0;
            }
        }
        FilterType::Paeth => {
            for i in (0..bpp) {
                current[i] = (w(current[i]) + w(filter_paeth(0, previous[i], 0))).0;
            }

            for i in (bpp..len) {
                current[i] = (w(current[i]) + w(filter_paeth(current[i - bpp], previous[i], previous[i - bpp]))).0;
            }
        }
    }
}

pub fn filter(method: FilterType, bpp: usize, previous: &[u8], current: &mut [u8]) {
    let len  = current.len();

    match method {
        FilterType::NoFilter => (),
        FilterType::Sub      => {
            for i in (bpp..len).rev() {
                current[i] = (w(current[i]) - w(current[i - bpp])).0;
            }
        }
        FilterType::Up       => {
            for i in (0..len) {
                current[i] = (w(current[i]) - w(previous[i])).0;
            }
        }
        FilterType::Avg  => {
            for i in (bpp..len).rev() {
                current[i] = (w(current[i]) - w(((current[i - bpp] as i16 + previous[i] as i16) / 2) as u8)).0;
            }

            for i in (0..bpp) {
                current[i] = (w(current[i]) - w(previous[i] / 2)).0;
            }
        }
        FilterType::Paeth    => {
            for i in (bpp..len).rev() {
                current[i] = (w(current[i]) - w(filter_paeth(current[i - bpp], previous[i], previous[i - bpp]))).0;
            }

            for i in (0..bpp) {
                current[i] = (w(current[i]) - w(filter_paeth(0, previous[i], 0))).0;
            }
        }
    }
}
