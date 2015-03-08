use std::ops::{Deref, DerefMut};
use std::collections::HashMap;
use std::collections::hash_map::Entry::{Occupied, Vacant};

use std::io;
use std::io::Result;
use std::num::Int;

use buffer::{ImageBuffer, Pixel};
use color::{Rgb, Rgba};
use utils::lzw;
use utils::bitstream::LsbWriter;
use imageops::ColorMap;

use super::{Extension, Block, DisposalMethod};

use math::nq;

#[derive(Debug, Copy)]
#[allow(unused_qualifications)]
/// The color mode the encoder will use to encode the image.
pub enum ColorMode {
    /// Image will be encoded in multiple frames if more than 256 colors are present
    TrueColor,
    /// Number of colors will be reduced
    Indexed(u8),
}
use self::ColorMode::{TrueColor, Indexed};

/// A GIF encoder.
///
/// Encodes the image either in true color using indexed colors.
/// If the mode is set to TrueColor the image is split into multiple frames
/// when the number of colors including transparent color exceeds 256.
/// Pixels with an alpha value != 1.0 will be set to alpha = 0.
pub struct Encoder<Image> {
    image: Image,
    bg_color: Option<Rgb<u8>>,
    color_mode: ColorMode
}

const TRANSPARENT: Rgba<u8> = Rgba([0, 0, 0, 0]);

impl<Container> Encoder<ImageBuffer<Rgba<u8>, Container>>
where Container: Deref<Target=[u8]> + DerefMut {
    /// Creates a new GIF encoder
    pub fn new(image: ImageBuffer<Rgba<u8>, Container>,
               bg_color: Option<Rgb<u8>>,
               color_mode: ColorMode,
              ) -> Encoder<ImageBuffer<Rgba<u8>, Container>> {
        Encoder {
            image: image,
            bg_color: bg_color,
            color_mode: color_mode
        }
    }

    /// Encodes the image
    pub fn encode<W: Writer>(&mut self, w: &mut W) -> io::Result<()> {
        // Header
        try!(w.write_all(b"GIF89a"));
        // Logical screen descriptor
        let height = self.image.height();
        let width = self.image.width();
        if width > <u16 as Int>::max_value() as u32 ||
           height > <u16 as Int>::max_value() as u32 {
            return Err(io::Error{
                kind: io::InvalidInput,
                desc: "Image dimensions are to large for the gif format.",
                detail: None
            })
        }
        try!(w.write_le_u16(width as u16));
        try!(w.write_le_u16(height as u16));

        let (hist, transparent) = self.histogram();
        let num_colors = hist.len();

        try!(self.write_global_table(w, &hist));

        match self.color_mode {
            // Global color table has been written, just write the image data now
            TrueColor if num_colors <= 256 => {
                // NOTE: copy paste any changes to case: Indexed(n) if n as usize <= num_colors
                try!(self.write_control_ext(w, 0, transparent));
                try!(self.write_descriptor(w, None));
                try!(self.write_image_simple(w, &hist, transparent));
            },
            TrueColor => {
                try!(self.write_true_color(w, hist, transparent));
            },
            Indexed(n) if n as usize <= num_colors && num_colors <= 256 => {
                // NOTE: copy paste from case: TrueColor if num_colors <= 256
                try!(self.write_control_ext(w, 0, transparent));
                try!(self.write_descriptor(w, None));
                try!(self.write_image_simple(w, &hist, transparent));
            }
            Indexed(n) => {
                try!(self.write_indexed_colors(w, n))
            }

        }
        w.write_u8(Block::Trailer as u8)
    }

    /// Returns a histogram of the colors in the image
    fn histogram(&self) -> (Vec<(Rgba<u8>, usize)>, Option<usize>){
        let mut hist: HashMap<_, usize> = HashMap::new();
        // Add bg_color to table
        if let Some(bg_color) = self.bg_color {
            let _ = hist.insert(bg_color.to_rgba(), 0);
        }
        for p in self.image.pixels() {
            let mut p = *p;
            // Replace transparent pixels with black, alpha = 0
            if let Some(bg_color) = self.bg_color {
                p.blend(&bg_color.to_rgba())
            // do not use 255 as a limit there could be rounding error etc.
            // thus take 250 to give it some security margin.
            } else if p[3] < 250 {
                p = TRANSPARENT;
            }
            match hist.entry(p) {
                Occupied(mut entry) => {
                    let val = entry.get_mut();
                    *val = *val + 1;
                },
                Vacant(entry) => {
                    entry.insert(1);
                }
            }
        }
        let mut colors: Vec<(Rgba<u8>, usize)> = hist.into_iter().collect();
        // Sort color map with decreasing color freqs
        colors.sort_by(|a, b| b.1.cmp(&a.1));
        // Add bg_color to table
        let transparent = colors.iter().position(|x| x.0 == TRANSPARENT);
        (colors, transparent)
    }

    /// Write the global color table and the corresponding flags
    fn write_global_table<W: Writer>(&mut self,
                                     w: &mut W,
                                     hist: &[(Rgba<u8>, usize)]) -> io::Result<()>
    {
        let num_colors = hist.len();
        let mut flags = 0;
        flags |= 1 << 4; // sort flag
        let n = flag_n(num_colors);
        flags |= n << 4; // 2^(n+1) colors
        let (global_table, bg_index) = if num_colors <= 256 {
            flags |= 1 << 7; // glocal table flag
            flags |= n;
            let mut bg_index = 0;
            let mut table = Vec::with_capacity(3*(2 << n));
            for (i, &(color, _)) in hist.iter().enumerate() {
                let channels = &color.channels()[..3];
                if let Some(bg_color) = self.bg_color {
                    if channels == bg_color.channels() {
                        bg_index = i;
                    }
                }
                table.push_all(channels)
            }
            // Waste some space as of gif spec
            for _ in 0..((2 << n) - num_colors) {
                table.push_all(&[0, 0, 0]);
            }
            (Some(table), bg_index as u8)
        } else if let Some(bg_color) = self.bg_color {
            flags |= 1 << 7; // glocal table flag
            let mut table = Vec::with_capacity(6);
            table.push_all(&bg_color.channels()[..3]);
            table.push_all(&[0, 0, 0]);
            (Some(table), 0)
        } else {
            (None, 0)
        };
        try!(w.write_u8(flags));
        try!(w.write_u8(bg_index));
        try!(w.write_u8(0)); // aspect ration, disregard
        if let Some(global_table) = global_table {
            try!(w.write_all(&global_table));
        }
        Ok(())
    }

    /// Writes the graphics control extension
    fn write_control_ext<W: Writer>(&mut self,
                                    w: &mut W,
                                    delay: u16,
                                    transparent: Option<usize>) -> io::Result<()>
    {
        try!(w.write_u8(Block::Extension as u8));
        try!(w.write_u8(Extension::Control as u8));
        try!(w.write_u8(4)); // size
        let mut field = 0;
        field |= (DisposalMethod::None as u8) << 2;
        let idx = if let Some(idx) = transparent {
            field |=  1;
            idx as u8
        } else {
            0
        };
        try!(w.write_u8(field));
        try!(w.write_le_u16(delay));
        try!(w.write_u8(idx));
        w.write_u8(0)
    }

    /// Writes the image descriptor
    fn write_descriptor<W: Writer>(&mut self,
                                   w: &mut W,
                                   table_len: Option<usize>) -> io::Result<()>
    {
        try!(w.write_u8(Block::Image as u8));
        try!(w.write_le_u16(0)); // left
        try!(w.write_le_u16(0)); // top
        let height = self.image.height();
        let width = self.image.width();
        try!(w.write_le_u16(width as u16));
        try!(w.write_le_u16(height as u16));
        if let Some(len) = table_len {
            w.write_u8(flag_n(len) | 0x80)
        } else {
            w.write_u8(0)
        }
    }

    /// Writes and compresses the indexed data
    fn write_indices<W: Writer>(&mut self,
                                w: &mut W,
                                indices: &[u8]) -> io::Result<()>
    {
        let code_size = match flag_n(indices.len()) + 1 {
            1 => 2,
            n => n
        };
        let mut encoded_data = Vec::new();
        try!(lzw::encode(indices, LsbWriter::new(&mut encoded_data), code_size));
        try!(w.write_u8(code_size));
        for chunk in encoded_data.chunks(255) {
            try!(w.write_u8((chunk.len()) as u8));
            try!(w.write_all(chunk));
        }
        w.write_u8(0) // block terminator

    }

    /// Writes the image to the file assuming that every pixel is in the color table
    /// If not, the index of the transparent pixel is written
    fn write_image_simple<W: Writer>(&mut self,
                                     w: &mut W,
                                     hist: &[(Rgba<u8>, usize)],
                                     transparent: Option<usize>,
                                    ) -> io::Result<()> 
    {
        let t_idx = match transparent { Some(i) => i as u8, None => 0 };
        let data: Vec<u8> = self.image.pixels().map(|p| {
            if let Some(idx) = hist.iter().position(|x| x.0 == *p) {
                idx as u8
            } else {
                t_idx
            }
        }).collect();
        self.write_indices(w, &data)

    }

    /// Writes the image as a true color image by splitting the colors
    /// over several frames
    fn write_true_color<W: Writer>(&mut self,
                                   w: &mut W,
                                   hist: Vec<(Rgba<u8>, usize)>,
                                   transparent: Option<usize>
                                  ) -> io::Result<()> 
    {
        let mut hist = hist;
        // Remove transparent idx
        if let Some(transparent) = transparent {
            let _ = hist.swap_remove(transparent);
        }
        let transparent = Some(0);
        // Calculating the indices is expensive so just do it once
        let indices: Vec<u32> = self.image.pixels().map(|p| {
            if let Some(idx) = hist.iter().position(|x| x.0 == *p) {
                idx as u32
            } else {
                0
            }
        }).collect();
        let mut chunk_indices = Vec::with_capacity(
            self.image.width() as usize * self.image.height() as usize
        );
        // starting from 1 since we inject the transparent color in every frame
        for (j, chunk) in hist.chunks(255).enumerate() {
            // Now we can reuse the indices for every subimage
            // if the substract j*255 from each an replace it with 0 if it gets negative
            chunk_indices.clear();
            for &idx in indices.iter() {
                let i: i64 = idx as i64 - j as i64*255;
                chunk_indices.push(if i < 0 || i > 255 { 0 } else { i as u8 + 1 })
            }
            // Write local color table
            let num_local_colors = chunk.len() + 1;
            let n = flag_n(num_local_colors);
            try!(self.write_control_ext(w, 0, transparent));
            try!(self.write_descriptor(w, Some(num_local_colors)));
            // Inject transparent color
            try!(w.write_all(&TRANSPARENT.channels()[..3]));
            for &(color, _) in chunk.iter() {
                try!(w.write_all(&color.channels()[..3]));
            }
            // waste some space as of gif spec
            for _ in 0..((2 << n) - num_local_colors) {
                try!(w.write_all(&[0, 0, 0]));
            }
            // write indices
            try!(self.write_indices(w, &chunk_indices));
        }
        Ok(())
    }
    /// Writes the image as a true color image by splitting the colors
    /// over several frames
    fn write_indexed_colors<W: Writer>(&mut self, w: &mut W, n: u8) -> io::Result<()> {
        if n < 64 {
            return  Err(io::Error{
                kind: io::InvalidInput,
                desc: "Unsupported number of colors.",
                detail: Some(
                    format!("{} colors < 64 colors", n))
            })
        }
        let nq = nq::NeuQuant::new(3, 256, self.image.as_slice());
        for pixel in self.image.pixels_mut() {
            nq.map_color(pixel);
        }

        let (hist, transparent) = self.histogram();
        let num_local_colors = hist.len();
        let n = flag_n(num_local_colors);
        try!(self.write_control_ext(w, 0, transparent));
        try!(self.write_descriptor(w, Some(num_local_colors)));
        for &(color, _) in hist.iter() {
            try!(w.write_all(&color.channels()[..3]));
        }
        // waste some space as of gif spec
        for _ in 0..((2 << n) - num_local_colors) {
            try!(w.write_all(&[0, 0, 0]));
        }
        self.write_image_simple(w, &hist, transparent)
    }

    /// Writes the netscape application block to set the number `n` of repetitions
    #[allow(dead_code)]
    fn write_nab<W: Writer>(&mut self, w: &mut W, n: u16) -> io::Result<()> {
        try!(w.write_u8(Block::Extension as u8));
        try!(w.write_u8(Extension::Application as u8));
        try!(w.write_u8(0x0B)); // size
        try!(w.write_all(b"NETSCAPE2.0"));
        try!(w.write_u8(0x03)); // sub-block size
        try!(w.write_u8(0x01)); // sub-block id
        try!(w.write_le_u16(n));
        w.write_u8(0) // terminator
    }
}

// Color table len converted to flag bits
fn flag_n(size: usize) -> u8 {
    match size {
        0  ...2   => 0,
        3  ...4   => 1,
        5  ...8   => 2,
        7  ...16  => 3,
        17 ...32  => 4,
        33 ...64  => 5,
        65 ...128 => 6,
        129...256 => 7,
        _ => 7
    }
}
