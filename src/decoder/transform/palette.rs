//! Helpers for taking a slice of indeces (indices into `PLTE` and/or `trNS`
//! entries) and transforming this into RGB or RGBA output.

use super::unpack_bits;
use crate::Info;

pub fn expand_paletted_into_rgb8(row: &[u8], buffer: &mut [u8], info: &Info) {
    let palette = info.palette.as_deref().expect("Caller should verify");
    let black = [0, 0, 0];

    unpack_bits(row, buffer, 3, info.bit_depth as u8, |i, chunk| {
        let rgb = palette
            .get(3 * i as usize..3 * i as usize + 3)
            .unwrap_or(&black);
        chunk[0] = rgb[0];
        chunk[1] = rgb[1];
        chunk[2] = rgb[2];
    })
}

pub fn expand_paletted_into_rgba8(row: &[u8], buffer: &mut [u8], info: &Info) {
    let palette = info.palette.as_deref().expect("Caller should verify");
    let trns = info.trns.as_deref().unwrap_or(&[]);
    let black = [0, 0, 0];

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
}

#[cfg(test)]
mod test {
    use crate::{BitDepth, ColorType, Info, Transformations};

    fn expand_paletted(
        src: &[u8],
        src_bit_depth: u8,
        palette: &[u8],
        trns: Option<&[u8]>,
    ) -> Vec<u8> {
        let info = Info {
            color_type: ColorType::Indexed,
            bit_depth: BitDepth::from_u8(src_bit_depth).unwrap(),
            palette: Some(palette.into()),
            trns: trns.map(Into::into),
            ..Info::default()
        };
        let output_bytes_per_input_sample = match trns {
            None => 3,
            Some(_) => 4,
        };
        let samples_count_per_byte = (8 / src_bit_depth) as usize;
        let samples_count = src.len() * samples_count_per_byte;
        let mut dst = vec![0; samples_count * output_bytes_per_input_sample];
        let transform_fn =
            super::super::create_transform_fn(&info, Transformations::EXPAND).unwrap();
        transform_fn(src, dst.as_mut_slice(), &info);
        dst
    }

    #[test]
    fn test_expand_paletted_rgba_8bit() {
        let actual = expand_paletted(
            &[0, 1, 2, 3], // src
            8,             // src_bit_depth
            &[
                // palette
                0, 1, 2, //    entry #0
                4, 5, 6, //    entry #1
                8, 9, 10, //   entry #2
                12, 13, 14, // entry #3
            ],
            Some(&[3, 7, 11, 15]), // trns
        );
        assert_eq!(actual, (0..16).collect::<Vec<u8>>());
    }

    #[test]
    fn test_expand_paletted_rgb_8bit() {
        let actual = expand_paletted(
            &[0, 1, 2, 3], // src
            8,             // src_bit_depth
            &[
                // palette
                0, 1, 2, //   entry #0
                3, 4, 5, //   entry #1
                6, 7, 8, //   entry #2
                9, 10, 11, // entry #3
            ],
            None, // trns
        );
        assert_eq!(actual, (0..12).collect::<Vec<u8>>());
    }

    #[test]
    fn test_expand_paletted_rgba_4bit() {
        let actual = expand_paletted(
            &[0x01, 0x23], // src
            4,             // src_bit_depth
            &[
                // palette
                0, 1, 2, //    entry #0
                4, 5, 6, //    entry #1
                8, 9, 10, //   entry #2
                12, 13, 14, // entry #3
            ],
            Some(&[3, 7, 11, 15]), // trns
        );
        assert_eq!(actual, (0..16).collect::<Vec<u8>>());
    }

    #[test]
    fn test_expand_paletted_rgb_4bit() {
        let actual = expand_paletted(
            &[0x01, 0x23], // src
            4,             // src_bit_depth
            &[
                // palette
                0, 1, 2, //   entry #0
                3, 4, 5, //   entry #1
                6, 7, 8, //   entry #2
                9, 10, 11, // entry #3
            ],
            None, // trns
        );
        assert_eq!(actual, (0..12).collect::<Vec<u8>>());
    }

    #[test]
    fn test_expand_paletted_rgba_8bit_more_trns_entries_than_palette_entries() {
        let actual = expand_paletted(
            &[0, 1, 2, 3], // src
            8,             // src_bit_depth
            &[
                // palette
                0, 1, 2, //    entry #0
                4, 5, 6, //    entry #1
                8, 9, 10, //   entry #2
                12, 13, 14, // entry #3
            ],
            Some(&[123; 5]), // trns
        );

        // Invalid (too-long) `trns` means that we'll use 0xFF / opaque alpha everywhere.
        assert_eq!(
            actual,
            vec![0, 1, 2, 0xFF, 4, 5, 6, 0xFF, 8, 9, 10, 0xFF, 12, 13, 14, 0xFF],
        );
    }

    #[test]
    fn test_expand_paletted_rgba_8bit_less_trns_entries_than_palette_entries() {
        let actual = expand_paletted(
            &[0, 1, 2, 3], // src
            8,             // src_bit_depth
            &[
                // palette
                0, 1, 2, //    entry #0
                4, 5, 6, //    entry #1
                8, 9, 10, //   entry #2
                12, 13, 14, // entry #3
            ],
            Some(&[3, 7]), // trns
        );

        // Too-short `trns` is treated differently from too-long - only missing entries are
        // replaced with 0XFF / opaque.
        assert_eq!(
            actual,
            vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 0xFF, 12, 13, 14, 0xFF],
        );
    }
}
