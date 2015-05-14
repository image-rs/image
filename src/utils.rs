//! Utility functions
use std::iter::repeat;
use num::range_step;

#[inline(always)]
pub fn unpack_bits<F>(buf: &mut [u8], channels: usize, bit_depth: u8, func: F)
where F: Fn(u8, &mut[u8]) {
    let bits = buf.len()/channels*bit_depth as usize;
    let extra = bits % 8;
    let entries = bits / 8 + match extra {
        0 => 0,
        _ => 1
    };
    let mask = ((1u16 << bit_depth) - 1) as u8;
    let i =
        (0..entries)
        .rev() // Reverse iterator
        .flat_map(|idx|
            // This has to be reversed to
            range_step(0, 8, bit_depth)
            .zip(repeat(idx))
        )
        .skip(extra);
    let channels = channels as isize;
    let j = range_step(buf.len() as isize - channels, -channels, -channels);
    //let j = range_step(0, buf.len(), channels).rev(); // ideal solution;
    for ((shift, i), j) in i.zip(j) {
        let pixel = (buf[i] & (mask << shift)) >> shift;
        func(pixel, &mut buf[j as usize..(j + channels) as usize])
    }
}

fn expand_trns_line(buf: &mut[u8], trns: &[u8], channels: usize) {
    let channels = channels as isize;
    let i = range_step(buf.len() as isize / (channels+1) * channels - channels, -channels, -channels);
    let j = range_step(buf.len() as isize - (channels+1), -(channels+1), -(channels+1));
    let channels = channels as usize;
    for (i, j) in i.zip(j) {
        let i_pixel = i as usize;
        let j_chunk = j as usize;
        if &buf[i_pixel..i_pixel+channels] == trns {
            buf[j_chunk+channels] = 0
        } else {
            buf[j_chunk+channels] = 0xFF
        }
        for k in (0..channels).rev() {
            buf[j_chunk+k] = buf[i_pixel+k];
        }
    }
}