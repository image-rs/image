//! Utility functions
use std::iter::repeat;

#[inline(always)]
pub fn unpack_bits<F>(buf: &mut [u8], channels: usize, bit_depth: u8, func: F)
where F: Fn(u8, &mut[u8]) {
    let bits = buf.len()/channels*bit_depth as usize;
    let extra_bits = bits % 8;
    let entries = bits / 8 + match extra_bits {
        0 => 0,
        _ => 1
    };
    let skip = match extra_bits {
        0 => 0,
        n => (8-n) / bit_depth as usize
    };
    let mask = ((1u16 << bit_depth) - 1) as u8;
    let i =
        (0..entries)
        .rev() // reverse iterator
        .flat_map(|idx|
            // this has to be reversed too
            (0..8).step_by(bit_depth.into())
            .zip(repeat(idx))
        )
        .skip(skip);
    let j = (0..=buf.len() - channels).rev().step_by(channels);
    for ((shift, i), j) in i.zip(j) {
        let pixel = (buf[i] & (mask << shift)) >> shift;
        func(pixel, &mut buf[j as usize..(j + channels) as usize])
    }
}

pub fn expand_trns_line(buf: &mut[u8], trns: &[u8], channels: usize) {
    let i = (0..=buf.len() / (channels+1) * channels - channels).rev().step_by(channels);
    let j = (0..=buf.len() - (channels+1)).rev().step_by(channels+1);
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

pub fn expand_trns_line16(buf: &mut[u8], trns: &[u8], channels: usize) {
    let c2 = 2 * channels;
    let i = (0..=buf.len() / (c2+2) * c2 - c2).rev().step_by(c2);
    let j = (0..=buf.len() - (c2+2)).rev().step_by(c2+2);
    for (i, j) in i.zip(j) {
        let i_pixel = i as usize;
        let j_chunk = j as usize;
        if &buf[i_pixel..i_pixel+c2] == trns {
            buf[j_chunk+c2] = 0;
            buf[j_chunk+c2 + 1] = 0
        } else {
            buf[j_chunk+c2] = 0xFF;
            buf[j_chunk+c2 + 1] = 0xFF
        }
        for k in (0..c2).rev() {
            buf[j_chunk+k] = buf[i_pixel+k];
        }
    }
}


/// This iterator iterates over the different passes of an image Adam7 encoded
/// PNG image
/// The pattern is:
///     16462646
///     77777777
///     56565656
///     77777777
///     36463646
///     77777777
///     56565656
///     77777777
///
#[derive(Clone)]
pub struct Adam7Iterator {
    line: u32,
    lines: u32,
    line_width: u32,
    current_pass: u8,
    width: u32,
    height: u32,
}

impl Adam7Iterator {
    pub fn new(width: u32, height: u32) -> Adam7Iterator {
        let mut this = Adam7Iterator {
            line: 0,
            lines: 0,
            line_width: 0,
            current_pass: 1,
            width: width,
            height: height
        };
        this.init_pass();
        this
    }

    /// Calculates the bounds of the current pass
    fn init_pass(&mut self) {
        let w = self.width as f64;
        let h = self.height as f64;
        let (line_width, lines) = match self.current_pass {
            1 => (w/8.0, h/8.0),
            2 => ((w-4.0)/8.0, h/8.0),
            3 => (w/4.0, (h-4.0)/8.0),
            4 => ((w-2.0)/4.0, h/4.0),
            5 => (w/2.0, (h-2.0)/4.0),
            6 => ((w-1.0)/2.0, h/2.0),
            7 => (w, (h-1.0)/2.0),
            _ => unreachable!()
        };
        self.line_width = line_width.ceil() as u32;
        self.lines = lines.ceil() as u32;
        self.line = 0;
    }
    /// The current pass#.
    pub fn current_pass(&self) -> u8 {
        self.current_pass
    }
}

/// Iterates over the (passes, lines, widths)
impl Iterator for Adam7Iterator {
    type Item = (u8, u32, u32);
    fn next(&mut self) -> Option<(u8, u32, u32)> {
        if self.line < self.lines && self.line_width > 0 {
            let this_line = self.line;
            self.line += 1;
            Some((self.current_pass, this_line, self.line_width))
        } else if self.current_pass < 7 {
            self.current_pass += 1;
            self.init_pass();
            self.next()
        } else {
            None
        }
    }
}

fn subbyte_pixel(pixel_idx: usize, bits_pp: usize, scanline: &[u8]) -> u8 {
    assert!(bits_pp < 8);

    let bit_idx = pixel_idx * bits_pp;
    let byte_idx = bit_idx / 8;
    let rem = bit_idx % 8;

    match bits_pp {
        // evenly divides bytes
        1 => (scanline[byte_idx] >> rem) & 1,
        2 => (scanline[byte_idx] >> rem) & 3,
        4 => (scanline[byte_idx] >> rem) & 15,
        _ => unreachable!(),
    }
}

/// Expands an Adam 7 pass
pub fn expand_pass(
    img: &mut [u8], width: u32, scanline: &[u8],
    pass: u8, line_no: u32, bits_pp: u8) {
    let line_no = line_no as usize;
    let width = width as usize;
    let bits_pp = bits_pp as usize;


    fn inner(img: &mut [u8], scanline: &[u8], bits_pp: usize, pos_fn: impl Fn(usize) -> usize) {
        if bits_pp < 8 {
            for i in (0 .. scanline.len() * 8).step_by(bits_pp) {
                let pos = pos_fn(i);
                img[pos / 8] |= subbyte_pixel(i, bits_pp, scanline) << (pos % 8);
            }

            return;
        }

        for (j, pixel) in scanline.chunks(bits_pp / 8).enumerate() {
            for (offset, val) in pixel.iter().enumerate() {
                let pos = pos_fn(j);
                img[pos + offset] = *val
            }
        }
    }

    match pass {
        1 => inner(img, scanline, bits_pp,  |j|  8*line_no    * width + bits_pp *  j*8     ),
        2 => inner(img, scanline, bits_pp,  |j|  8*line_no    * width + bits_pp * (j*8 + 4)),
        3 => inner(img, scanline, bits_pp,  |j| (8*line_no+4) * width + bits_pp *  j*4     ),
        4 => inner(img, scanline, bits_pp,  |j|  4*line_no    * width + bits_pp * (j*4 + 2)),
        5 => inner(img, scanline, bits_pp,  |j| (4*line_no+2) * width + bits_pp *  j*2     ),
        6 => inner(img, scanline, bits_pp,  |j|  2*line_no    * width + bits_pp * (j*2+1)  ),
        7 => inner(img, scanline, bits_pp,  |j| (2*line_no+1) * width + bits_pp *  j       ),
        _ => {}
    }
}

#[test]
fn test_adam7() {
    /*
        1646
        7777
        5656
        7777
    */
    let it = Adam7Iterator::new(4, 4);
    let passes: Vec<_> = it.collect();
    assert_eq!(&*passes, &[(1, 0, 1), (4, 0, 1), (5, 0, 2), (6, 0, 2), (6, 1, 2), (7, 0, 4), (7, 1, 4)]);
}

#[test]
fn test_subbyte_pixel() {
    let bytes = [0b10101010u8];

    for idx in 0 .. 8 {
        assert_eq!(subbyte_pixel(idx, 1, &bytes), (idx % 2) as u8);
    }

    for idx in 0 .. 4 {
        assert_eq!(subbyte_pixel(idx, 2, &bytes), 0b10);
    }

    for idx in 0 .. 2 {
        assert_eq!(subbyte_pixel(idx, 4, &bytes), 0b1010);
    }
}

#[test]
fn test_expand_pass_subbyte() {
    let mut img = [0u8; 8];
    let width = 8;
    let bits_pp = 1;

    expand_pass(&mut img, width, &[0b1], 1, 0, bits_pp);
    assert_eq!([1u8, 0, 0, 0, 0, 0, 0, 0], img);

    expand_pass(&mut img, width, &[0b1], 2, 0, bits_pp);
    assert_eq!([0b10001u8, 0, 0, 0, 0, 0, 0, 0], img);

    expand_pass(&mut img, width, &[0b11], 3, 0, bits_pp);
    assert_eq!([0b10001u8, 0, 0, 0, 0b10001, 0, 0, 0], img);

    expand_pass(&mut img, width, &[0b11], 4, 0, bits_pp);
    assert_eq!([0b1010101u8, 0, 0, 0, 0b10001, 0, 0, 0], img);

    expand_pass(&mut img, width, &[0b11], 4, 1, bits_pp);
    assert_eq!([0b1010101u8, 0, 0, 0, 0b1010101, 0, 0, 0], img);

    expand_pass(&mut img, width, &[0b1111], 5, 0, bits_pp);
    assert_eq!([0b01010101u8, 0, 0b01010101, 0, 0b01010101, 0, 0, 0], img);

    expand_pass(&mut img, width, &[0b1111], 5, 1, bits_pp);
    assert_eq!([0b01010101u8, 0, 0b01010101, 0, 0b01010101, 0, 0b01010101, 0], img);

    expand_pass(&mut img, width, &[0b1111], 6, 0, bits_pp);
    assert_eq!([0b11111111u8, 0, 0b01010101, 0, 0b01010101, 0, 0b01010101, 0], img);

    expand_pass(&mut img, width, &[0b1111], 6, 1, bits_pp);
    assert_eq!([0b11111111u8, 0, 0b11111111, 0, 0b01010101, 0, 0b01010101, 0], img);

    expand_pass(&mut img, width, &[0b1111], 6, 2, bits_pp);
    assert_eq!([0b11111111u8, 0, 0b11111111, 0, 0b11111111, 0, 0b01010101, 0], img);

    expand_pass(&mut img, width, &[0b1111], 6, 3, bits_pp);
    assert_eq!([0b11111111u8, 0, 0b11111111, 0, 0b11111111, 0, 0b11111111, 0], img);

    expand_pass(&mut img, width, &[0b11111111], 7, 0, bits_pp);
    assert_eq!([0b11111111u8, 0b11111111, 0b11111111, 0, 0b11111111, 0, 0b11111111, 0], img);

    expand_pass(&mut img, width, &[0b11111111], 7, 1, bits_pp);
    assert_eq!([0b11111111u8, 0b11111111, 0b11111111, 0b11111111, 0b11111111, 0, 0b11111111, 0], img);

    expand_pass(&mut img, width, &[0b11111111], 7, 2, bits_pp);
    assert_eq!([0b11111111u8, 0b11111111, 0b11111111, 0b11111111, 0b11111111, 0b11111111, 0b11111111, 0], img);

    expand_pass(&mut img, width, &[0b11111111], 7, 3, bits_pp);
    assert_eq!([0b11111111u8, 0b11111111, 0b11111111, 0b11111111, 0b11111111, 0b11111111, 0b11111111, 0b11111111], img);
}