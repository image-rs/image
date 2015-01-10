use std::num::SignedInt;

#[derive(FromPrimitive, Show)]
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
            for i in range(bpp, len) {
                current[i] += current[i - bpp];
            }
        }
        FilterType::Up => {
            for i in range(0, len) {
                current[i] += previous[i];
            }
        }
        FilterType::Avg => {
            for i in range(0, bpp) {
                current[i] += previous[i] / 2;
            }

            for i in range(bpp, len) {
                current[i] += ((current[i - bpp] as i16 + previous[i] as i16) / 2) as u8;
            }
        }
        FilterType::Paeth => {
            for i in range(0, bpp) {
                current[i] += filter_paeth(0, previous[i], 0);
            }

            for i in range(bpp, len) {
                current[i] += filter_paeth(current[i - bpp], previous[i], previous[i - bpp]);
            }
        }
    }
}

pub fn filter(method: FilterType, bpp: usize, previous: &[u8], current: &mut [u8]) {
    let len  = current.len();
    let orig: Vec<u8> = range(0, len).map(| i | current[i]).collect();

    match method {
        FilterType::NoFilter => (),
        FilterType::Sub      => {
            for i in range(bpp, len) {
                current[i] = orig[i] - orig[i - bpp];
            }
        }
        FilterType::Up       => {
            for i in range(0, len) {
                current[i] = orig[i] - previous[i];
            }
        }
        FilterType::Avg  => {
            for i in range(0, bpp) {
                current[i] = orig[i] - previous[i] / 2;
            }

            for i in range(bpp, len) {
                current[i] = orig[i] - ((orig[i - bpp] as i16 + previous[i] as i16) / 2) as u8;
            }
        }
        FilterType::Paeth    => {
            for i in range(0, bpp) {
                current[i] = orig[i] - filter_paeth(0, previous[i], 0);
            }

            for i in range(bpp, len) {
                current[i] = orig[i] - filter_paeth(orig[i - bpp], previous[i], previous[i - bpp]);
            }
        }
    }
}
