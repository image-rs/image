use std::num::SignedInt;

#[derive(FromPrimitive, Debug)]
pub enum FilterType {
    NoFilter = 0,
    Sub = 1,
    Up = 2,
    Avg = 3,
    Paeth = 4
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
                current[i] += current[i - bpp];
            }
        }
        FilterType::Up => {
            for i in (0..len) {
                current[i] += previous[i];
            }
        }
        FilterType::Avg => {
            for i in (0..bpp) {
                current[i] += previous[i] / 2;
            }

            for i in (bpp..len) {
                current[i] += ((current[i - bpp] as i16 + previous[i] as i16) / 2) as u8;
            }
        }
        FilterType::Paeth => {
            for i in (0..bpp) {
                current[i] += filter_paeth(0, previous[i], 0);
            }

            for i in (bpp..len) {
                current[i] += filter_paeth(current[i - bpp], previous[i], previous[i - bpp]);
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
                current[i] = current[i] - current[i - bpp];
            }
        }
        FilterType::Up       => {
            for i in (0..len) {
                current[i] = current[i] - previous[i];
            }
        }
        FilterType::Avg  => {
            for i in (bpp..len).rev() {
                current[i] = current[i] - ((current[i - bpp] as i16 + previous[i] as i16) / 2) as u8;
            }

            for i in (0..bpp) {
                current[i] = current[i] - previous[i] / 2;
            }
        }
        FilterType::Paeth    => {
            for i in (bpp..len).rev() {
                current[i] = current[i] - filter_paeth(current[i - bpp], previous[i], previous[i - bpp]);
            }

            for i in (0..bpp) {
                current[i] = current[i] - filter_paeth(0, previous[i], 0);
            }
        }
    }
}
