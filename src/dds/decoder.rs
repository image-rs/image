use std::io::Read;
use byteorder::{ReadBytesExt, LittleEndian, ByteOrder};

use image::{
    DecodingResult,
    ImageResult,
    ImageDecoder,
    ImageError
};
use color::ColorType;

static LOOKUP_TABLE_5_BIT_TO_8_BIT: [u8; 32] = [0, 8, 16, 25, 33, 41, 49, 58, 66, 74, 82, 90, 99, 107, 115, 123, 132, 140, 148, 156, 165, 173, 181, 189, 197, 206, 214, 222, 230, 239, 247, 255];
static LOOKUP_TABLE_6_BIT_TO_8_BIT: [u8; 64] = [0, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 45, 49, 53, 57, 61, 65, 69, 73, 77, 81, 85, 89, 93, 97, 101, 105, 109, 113, 117, 121, 125, 130, 134, 138, 142, 146, 150, 154, 158, 162, 166, 170, 174, 178, 182, 186, 190, 194, 198, 202, 206, 210, 215, 219, 223, 227, 231, 235, 239, 243, 247, 251, 255];

#[derive(PartialEq, Copy, Clone)]
enum ImageType {
    DXT1,
    DXT2,
    DXT3,
    DXT4,
    DXT5,
}

/// A dds decoder
pub struct DDSDecoder<R> {
    r: R,
    header_loaded: bool,
    image_type: ImageType,
    flags: u32,
    width: u32,
    height: u32,
    depth: u32,
    mip_map_count: u32,
    ddspf: DDSPixelformat,
}

struct DDSPixelformat {
    flags: u32,
    fourcc: [u8; 4],
    rgb_bit_count: u32,
    r_bit_mask: u32,
    g_bit_mask: u32,
    b_bit_mask: u32,
    a_bit_mask: u32,
}

impl<R: Read> DDSDecoder<R> {
    /// Create a new decoder that decodes from the stream ```r```
    pub fn new(r: R) -> Self {
        DDSDecoder {
            r: r,
            header_loaded: false,
            image_type: ImageType::DXT1,
            flags: 0x0,
            width: 0,
            height: 0,
            depth: 0,
            mip_map_count: 0,
            ddspf: DDSPixelformat {
                flags: 0,
                fourcc: [0; 4],
                rgb_bit_count: 0,
                r_bit_mask: 0,
                g_bit_mask: 0,
                b_bit_mask: 0,
                a_bit_mask: 0,
            },
        }
    }

    #[allow(unused_must_use)]
    fn read_header(&mut self) -> ImageResult<()> {
        if !self.header_loaded {
            let mut magic = [0; 4];

            try!(self.r.read_exact(&mut magic));

            if magic != b"DDS "[..] {
                return Err(ImageError::FormatError("DDS magic bytes not found".to_string()));
            }
            let _ = self.r.read_u32::<LittleEndian>()?;//size = 124[0x7C]

            self.flags = self.r.read_u32::<LittleEndian>()?;
            self.height = self.r.read_u32::<LittleEndian>()?;
            self.width = self.r.read_u32::<LittleEndian>()?;
            let _ = self.r.read_u32::<LittleEndian>()?;//pitch_or_linear_size
            self.depth = self.r.read_u32::<LittleEndian>()?;
            self.mip_map_count = self.r.read_u32::<LittleEndian>()?;

            let _ = self.r.read_exact(&mut [0; 4*11]);//reserved1[11]

            //DDSPixelformat struct
            let _ = self.r.read_u32::<LittleEndian>()?;//size = 32[0x20]
            self.ddspf.flags = self.r.read_u32::<LittleEndian>()?;
            self.r.read_exact(&mut self.ddspf.fourcc)?;
            self.ddspf.rgb_bit_count = self.r.read_u32::<LittleEndian>()?;
            self.ddspf.r_bit_mask = self.r.read_u32::<LittleEndian>()?;
            self.ddspf.g_bit_mask = self.r.read_u32::<LittleEndian>()?;
            self.ddspf.b_bit_mask = self.r.read_u32::<LittleEndian>()?;
            self.ddspf.a_bit_mask = self.r.read_u32::<LittleEndian>()?;
            //DDSPixelformat struct end
            
            if let Some(image_type) = self.get_image_type() {
                self.image_type = image_type;
            } else {
                return Err(ImageError::FormatError("Unknown Image Type".to_string()));
            }

            let _ = self.r.read_exact(&mut [0; 4*5]);//dwCaps, dwCaps2, dwCaps3, dwCaps4, reserved2

            self.header_loaded = true;
        }
        Ok(())
    }

    fn read_image_data(&mut self) -> ImageResult<Vec<u8>> {
        match self.image_type {
            ImageType::DXT1 => {
                self.decode_dxt_1()
            },
            ImageType::DXT2 | ImageType::DXT3 => {
                self.decode_dxt_3()
            },
            ImageType::DXT4 | ImageType::DXT5 => {
                self.decode_dxt_5()
            },
        }
    }

    fn decode_dxt_1(&mut self) -> ImageResult<Vec<u8>> {
        let mut pixels: Vec<u8> = vec![0; 4 * self.width as usize * self.height as usize];
        let w: u32 = self.width/4;
        let h: u32 = self.height/4;
        for i in 0..h {
            for j in 0..w {
                let color0 = try!(self.r.read_u16::<LittleEndian>()) as usize;
                let color1 = try!(self.r.read_u16::<LittleEndian>()) as usize;
                for k in 0..4 {
                    let row = try!(self.r.read_u8());
                    let mut t: [u8; 4] = [0, 0, 0, 0];
                    t[0] = row & 0x03;
                    t[1] = (row & 0x0C) >> 2;
                    t[2] = (row & 0x30) >> 4;
                    t[3] = (row & 0xC0) >> 6;
                    let i_base: usize = 4*(4*self.width*i+4*j+self.width*k) as usize;
                    let mut color_offset = 0;
                    for m in 0..4 {
                        for value in self.get_dxt_color(color0, color1, 0xFF, t[m]).into_iter() {
                            pixels[i_base+color_offset] = value;
                            color_offset += 1;
                        }
                    }
                }
            }
        }
        Ok(pixels)
    }

    fn decode_dxt_3(&mut self) -> ImageResult<Vec<u8>> {
        let mut pixels: Vec<u8> = vec![0; 4 * self.width as usize * self.height as usize];
        let mut alpha_table: Vec<u8> = vec![0; 16];
        let w: u32 = self.width/4;
        let h: u32 = self.height/4;
        for i in 0..h {
            for j in 0..w {
                for k in 0..4 as usize {
                    let alpha0: u8 = try!(self.r.read_u8());
                    let alpha1: u8 = try!(self.r.read_u8());
                    alpha_table[4*k+0] = 17 * (alpha0 & 0x0F);
                    alpha_table[4*k+1] = 17 * ((alpha0 & 0xF0)>>4);
                    alpha_table[4*k+2] = 17 * (alpha1 & 0x0F);
                    alpha_table[4*k+3] = 17 * ((alpha1 & 0xF0)>>4);
                }
                let color0 = try!(self.r.read_u16::<LittleEndian>()) as usize;
                let color1 = try!(self.r.read_u16::<LittleEndian>()) as usize;
                for k in 0..4 {
                    let row = try!(self.r.read_u8());
                    let mut t: [u8; 4] = [0, 0, 0, 0];
                    t[0] = row & 0x03;
                    t[1] = (row & 0x0C) >> 2;
                    t[2] = (row & 0x30) >> 4;
                    t[3] = (row & 0xC0) >> 6;
                    let i_base: usize = 4*(4*self.width*i+4*j+self.width*k) as usize;
                    let mut color_offset = 0;
                    for m in 0..4 as usize {
                        for value in self.get_dxt_color(color0, color1, alpha_table[4*(k as usize)+m], t[m]).into_iter() {
                            pixels[i_base+color_offset] = value;
                            color_offset += 1;
                        }
                    }
                }
            }
        }
        Ok(pixels)
    }

    fn decode_dxt_5(&mut self) -> ImageResult<Vec<u8>> {
        let mut pixels: Vec<u8> = vec![0; 4 * self.width as usize * self.height as usize];
        let mut alpha_table: Vec<u8> = vec![0; 16];
        let w: u32 = self.width/4;
        let h: u32 = self.height/4;
        for i in 0..h {
            for j in 0..w {
                let alpha0 = try!(self.r.read_u8());
                let alpha1 = try!(self.r.read_u8());
                let mut buffer: [u8; 6] = [0; 6];
                try!(self.r.read_exact(&mut buffer));
                let alpha_info0: u32 = (buffer[0] as u32) | (buffer[1] as u32) << 8 | (buffer[2] as u32) << 16;
                let alpha_info1: u32 = (buffer[3] as u32) | (buffer[4] as u32) << 8 | (buffer[5] as u32) << 16;
                for k in 0..8 {
                    alpha_table[k] = ((alpha_info0 >> k*3) & 0x07) as u8;
                }
                for k in 0..8 {
                    alpha_table[k+8] = ((alpha_info1 >> k*3) & 0x07) as u8;
                }
                let color0 = try!(self.r.read_u16::<LittleEndian>()) as usize;
                let color1 = try!(self.r.read_u16::<LittleEndian>()) as usize;
                for k in 0..4 {
                    let row = try!(self.r.read_u8());
                    let mut t: [u8; 4] = [0, 0, 0, 0];
                    t[0] = row & 0x03;
                    t[1] = (row & 0x0C) >> 2;
                    t[2] = (row & 0x30) >> 4;
                    t[3] = (row & 0xC0) >> 6;
                    let i_base: usize = 4*(4*self.width*i+4*j+self.width*k) as usize;
                    let mut color_offset = 0;
                    for m in 0..4 as usize {
                        for value in self.get_dxt_color(color0, color1, self.get_dxt_alpha(alpha0, alpha1, alpha_table[4*(k as usize)+m]), t[m]).into_iter() {
                            pixels[i_base+color_offset] = value;
                            color_offset += 1;
                        }
                    }
                }
            }
        }
        Ok(pixels)
    }

    fn get_dxt_color(&self, c0: usize, c1: usize, alpha:u8, t: u8) -> Vec<u8> {
        match t {
            0 => {// 5bit(r) 6bit(g) 5bit(b) a
                vec![LOOKUP_TABLE_5_BIT_TO_8_BIT[(c0 & 0xFC00) >> 11], LOOKUP_TABLE_6_BIT_TO_8_BIT[(c0 & 0x07E0) >> 5], LOOKUP_TABLE_5_BIT_TO_8_BIT[(c0 & 0x001F)], alpha]
            },
            1 => {
                vec![LOOKUP_TABLE_5_BIT_TO_8_BIT[(c1 & 0xFC00) >> 11], LOOKUP_TABLE_6_BIT_TO_8_BIT[(c1 & 0x07E0) >> 5], LOOKUP_TABLE_5_BIT_TO_8_BIT[(c1 & 0x001F)], alpha]
            },
            2 => {
                if (c0 > c1) || self.image_type != ImageType::DXT1 {
                    self.get_dxt_color_2_1(c0, c1, alpha)
                } else {
                    self.get_dxt_color_1_1(c0, c1, alpha)
                }
            },
            3 => {
                if c0 > c1 || self.image_type != ImageType::DXT1 {
                    self.get_dxt_color_2_1(c1, c0, alpha)
                } else {
                    vec![0, 0, 0, 0]
                }
            },
            _ => {vec![0, 0, 0, 0]}
        }
    }

    fn get_dxt_color_2_1(&self, c0: usize, c1: usize, a:u8) -> Vec<u8> {
        ///Returns an rgba color vector with the given alpha value and 2:1(c0, c1) ratio rgb components
        let r: u16 = (2*(LOOKUP_TABLE_5_BIT_TO_8_BIT[(c0 & 0xFC00) >> 11] as u16) + (LOOKUP_TABLE_5_BIT_TO_8_BIT[(c1 & 0xFC00) >> 11] as u16)) / 3;
        let g: u16 = (2*(LOOKUP_TABLE_6_BIT_TO_8_BIT[(c0 & 0x07E0) >> 5] as u16) + (LOOKUP_TABLE_6_BIT_TO_8_BIT[(c1 & 0x07E0) >> 5] as u16)) / 3;
        let b: u16 = (2*(LOOKUP_TABLE_5_BIT_TO_8_BIT[(c0 & 0x001F)] as u16) + (LOOKUP_TABLE_5_BIT_TO_8_BIT[(c1 & 0x001F)] as u16)) / 3;
        vec![r as u8, g as u8, b as u8, a]
    }

    fn get_dxt_color_1_1(&self, c0: usize, c1: usize, a:u8) -> Vec<u8> {
        ///Returns an rgba color vector with the given alpha value and 1:1(c0, c1) ratio rgb components
        let r: u16 = ((LOOKUP_TABLE_5_BIT_TO_8_BIT[(c0 & 0xFC00) >> 11] as u16) + (LOOKUP_TABLE_5_BIT_TO_8_BIT[(c1 & 0xFC00) >> 11] as u16)) / 2;
        let g: u16 = ((LOOKUP_TABLE_6_BIT_TO_8_BIT[(c0 & 0x07E0) >> 5] as u16) + (LOOKUP_TABLE_6_BIT_TO_8_BIT[(c1 & 0x07E0) >> 5] as u16)) / 2;
        let b: u16 = ((LOOKUP_TABLE_5_BIT_TO_8_BIT[c0 & 0x001F] as u16) + (LOOKUP_TABLE_5_BIT_TO_8_BIT[c1 & 0x001F] as u16)) / 2;
        vec![r as u8, g as u8, b as u8, a]
    }

    fn get_dxt_alpha(&self, alpha0: u8, alpha1: u8, t: u8) -> u8 {
        if alpha0 > alpha1 {
            match t {
                0 => {alpha0},
                1 => {alpha1},
                2 => {(6*alpha0+alpha1)/7},
                3 => {(5*alpha0+2*alpha1)/7},
                4 => {(4*alpha0+3*alpha1)/7},
                5 => {(3*alpha0+4*alpha1)/7},
                6 => {(2*alpha0+5*alpha1)/7},
                7 => {(alpha0+6*alpha1)/7},
                _ => 0,
            }
        } else {
            match t {
                0 => {alpha0},
                1 => {alpha1},
                2 => {(4*alpha0+alpha1)/5},
                3 => {(3*alpha0+2*alpha1)/5},
                4 => {(2*alpha0+3*alpha1)/5},
                5 => {(alpha0+4*alpha1)/5},
                6 => 0,
                7 => 255,
                _ => 0,
            }
        }
    }

    fn get_image_type(&self) -> Option<ImageType> {
        if self.ddspf.flags & 0x04 != 0 {//Texture contains compressed RGB data; dwFourCC contains valid data.
            match LittleEndian::read_u32(&self.ddspf.fourcc) {
                0x31545844 => {Some(ImageType::DXT1)},
                0x32545844 => {Some(ImageType::DXT2)},
                0x33545844 => {Some(ImageType::DXT3)},
                0x34545844 => {Some(ImageType::DXT4)},
                0x35545844 => {Some(ImageType::DXT5)},
                _ => {None},
            }
        } else if self.ddspf.flags & 0x40 != 0 {//	Texture contains uncompressed RGB data; dwRGBBitCount and the RGB masks (dwRBitMask, dwGBitMask, dwBBitMask) contain valid data.
            match self.ddspf.rgb_bit_count {// check the different masks to find out what type of uncompressed image we are dealing with
                16 => {None},
                24 => {None},
                32 => {None},
                _ => {None},
            }
        } else {
            None
        }
    }
}
impl<R: Read> ImageDecoder for DDSDecoder<R> {
    fn dimensions(&mut self) -> ImageResult<(u32, u32)> {
        try!(self.read_header());
        Ok((self.width, self.height))
    }

    fn colortype(&mut self) -> ImageResult<ColorType> {
        try!(self.read_header());
        match self.image_type {
            ImageType::DXT1 | ImageType::DXT2 | ImageType::DXT3 | ImageType::DXT4 | ImageType::DXT5 => {
                Ok(ColorType::RGBA(8))
            },
        }
    }

    fn row_len(&mut self) -> ImageResult<usize> {
        unimplemented!();
    }

    fn read_scanline(&mut self, _buf: &mut [u8]) -> ImageResult<u32> {
        unimplemented!();
    }

    fn read_image(&mut self) -> ImageResult<DecodingResult> {
        try!(self.read_header());
        self.read_image_data().map(|v| DecodingResult::U8(v))
    }
}