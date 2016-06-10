use std::io::{Read, Seek, SeekFrom};
use std::iter::{Iterator, repeat, Rev};
use std::slice::ChunksMut;
use byteorder::{ReadBytesExt, LittleEndian};

use image::{
    DecodingResult,
    ImageResult,
    ImageDecoder,
    ImageError
};
use color::ColorType;

const BITMAPCOREHEADER_SIZE: u32 = 12;
const BITMAPINFOHEADER_SIZE: u32 = 40;
const BITMAPV2HEADER_SIZE: u32 = 52;
const BITMAPV3HEADER_SIZE: u32 = 56;
const BITMAPV4HEADER_SIZE: u32 = 108;
const BITMAPV5HEADER_SIZE: u32 = 124;

static LOOKUP_TABLE_4_BIT_TO_8_BIT: [u8; 16] = [0, 17, 34, 51, 68, 85, 102, 119, 136, 153, 170, 187, 204, 221, 238, 255];
static LOOKUP_TABLE_5_BIT_TO_8_BIT: [u8; 32] = [0, 8, 16, 25, 33, 41, 49, 58, 66, 74, 82, 90, 99, 107, 115, 123, 132, 140, 148, 156, 165, 173, 181, 189, 197, 206, 214, 222, 230, 239, 247, 255];
static LOOKUP_TABLE_6_BIT_TO_8_BIT: [u8; 64] = [0, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 45, 49, 53, 57, 61, 65, 69, 73, 77, 81, 85, 89, 93, 97, 101, 105, 109, 113, 117, 121, 125, 130, 134, 138, 142, 146, 150, 154, 158, 162, 166, 170, 174, 178, 182, 186, 190, 194, 198, 202, 206, 210, 215, 219, 223, 227, 231, 235, 239, 243, 247, 251, 255];

const R4_G4_B4_COLOR_MASK: (u32, u32, u32) = (0xF00, 0xF0, 0xF);
const R5_G5_B5_COLOR_MASK: (u32, u32, u32) = (0x7c00, 0x03e0, 0x1f);
const R5_G6_B5_COLOR_MASK: (u32, u32, u32) = (0xf800, 0x07e0, 0x1f);
const R8_G8_B8_COLOR_MASK: (u32, u32, u32) = (0xff000000, 0xff0000, 0xff00);

const RLE_ESCAPE: u8 = 0;
const RLE_ESCAPE_EOL: u8 = 0;
const RLE_ESCAPE_EOF: u8 = 1;
const RLE_ESCAPE_DELTA: u8 = 2;

#[derive(PartialEq, Copy, Clone)]
enum ImageType {
    RGB,
    RLE8,
    RLE4,
    Bitfields,
}

#[derive(PartialEq)]
enum BMPHeaderType {
    CoreHeader,
    InfoHeader,
    V2Header,
    V3Header,
    V4Header,
    V5Header,
}

#[derive(PartialEq)]
enum Format16Bit {
    Format444,
    Format555,
    Format565
}

#[derive(PartialEq)]
enum FormatFullBytes {
    FormatRGB24,
    FormatRGB32,
    FormatRGBA32,
    Format888
}

enum Chunker<'a> {
    FromTop(ChunksMut<'a, u8>),
    FromBottom(Rev<ChunksMut<'a, u8>>),
}

pub struct RowIterator<'a> {
    chunks: Chunker<'a>
}

impl<'a> Iterator for RowIterator<'a> {
    type Item = &'a mut [u8];

    #[inline(always)]
    fn next(&mut self) -> Option<&'a mut [u8]> {
        match self.chunks {
            Chunker::FromTop(ref mut chunks) => chunks.next(),
            Chunker::FromBottom(ref mut chunks) => chunks.next()
        }
    }
}

fn set_8bit_pixel_run<'a, T: Iterator<Item=&'a u8>>(pixel_iter: &mut ChunksMut<u8>,
                                                    palette: &Vec<(u8, u8, u8)>,
                                                    indices: T, n_pixels: usize) -> bool {
    for idx in indices.take(n_pixels) {
        if let Some(pixel) = pixel_iter.next() {
            let (r, g, b) = palette[*idx as usize];
            pixel[0] = r;
            pixel[1] = g;
            pixel[2] = b;
        } else {
            return false;
        }
    }
    true
}

fn set_4bit_pixel_run<'a, T: Iterator<Item=&'a u8>>(pixel_iter: &mut ChunksMut<u8>,
                                                    palette: &Vec<(u8, u8, u8)>,
                                                    indices: T, mut n_pixels: usize) -> bool {
    for idx in indices {
        macro_rules! set_pixel {
            ($i:expr) => (
                if n_pixels == 0 {
                    break;
                }
                if let Some(pixel) = pixel_iter.next() {
                    let (r, g, b) = palette[$i as usize];
                    pixel[0] = r;
                    pixel[1] = g;
                    pixel[2] = b;
                } else {
                    return false;
                }
                n_pixels -= 1;
            )
        }
        set_pixel!(idx >> 4);
        set_pixel!(idx & 0xf);
    }
    true
}

fn set_1bit_pixel_run<'a, T: Iterator<Item=&'a u8>>(pixel_iter: &mut ChunksMut<u8>,
                                                    palette: &Vec<(u8, u8, u8)>,
                                                    indices: T) {
    for idx in indices {
        let mut bit = 0x80;
        loop {
            if let Some(pixel) = pixel_iter.next() {
                let (r, g, b) = palette[((idx & bit) != 0) as usize];
                pixel[0] = r;
                pixel[1] = g;
                pixel[2] = b;
            } else {
                return
            }

            bit = bit >> 1;
            if bit == 0 {
                break;
            }
        }
    }
}

/// A bmp decoder
pub struct BMPDecoder<R> {
    r: R,

    bmp_header_type: BMPHeaderType,

    width: i32,
    height: i32,
    data_offset: u64,
    top_down: bool,
    no_file_header: bool,
    add_alpha_channel: bool,
    has_loaded_metadata: bool,
    image_type: ImageType,

    bit_count: u16,
    colors_used: u32,
    palette: Option<Vec<(u8, u8, u8)>>,
    bitfields: Option<(u32, u32, u32)>,
}

enum RLEInsn {
    EndOfFile,
    EndOfRow,
    Delta(u8, u8),
    Absolute(u8, Vec<u8>),
    PixelRun(u8, u8),
}

struct RLEInsnIterator<'a, R: 'a + Read> {
    r: &'a mut R,
    image_type: ImageType,
}

impl<'a, R: Read> Iterator for RLEInsnIterator<'a, R> {
    type Item = RLEInsn;

    fn next(&mut self) -> Option<RLEInsn> {
        let control_byte = match self.r.read_u8() {
            Ok(b) => b,
            Err(_) => return None
        };

        match control_byte {
            RLE_ESCAPE => {
                let op = match self.r.read_u8() {
                    Ok(b) => b,
                    Err(_) => return None
                };

                match op {
                    RLE_ESCAPE_EOL => Some(RLEInsn::EndOfRow),
                    RLE_ESCAPE_EOF => Some(RLEInsn::EndOfFile),
                    RLE_ESCAPE_DELTA => {
                        let xdelta = match self.r.read_u8() {
                            Ok(n) => n,
                            Err(_) => return None
                        };
                        let ydelta = match self.r.read_u8() {
                            Ok(n) => n,
                            Err(_) => return None
                        };
                        Some(RLEInsn::Delta(xdelta, ydelta))
                    },
                    _ => {
                        let mut length = op as usize;
                        if self.image_type == ImageType::RLE4 {
                            length = (length + 1) / 2;
                        }
                        length += length & 1;
                        let mut buffer = vec![0; length];
                        match self.r.read_exact(&mut buffer) {
                            Ok(()) => Some(RLEInsn::Absolute(op, buffer)),
                            Err(_) => None
                        }
                    }
                }
            },
            _ => {
                match self.r.read_u8() {
                    Ok(palette_index) => {
                        Some(RLEInsn::PixelRun(control_byte, palette_index))
                    },
                    Err(_) => None
                }
            }
        }
    }
}

impl<R: Read + Seek> BMPDecoder<R> {
    /// Create a new decoder that decodes from the stream ```r```
    pub fn new(r: R) -> BMPDecoder<R> {
        BMPDecoder {
            r: r,

            bmp_header_type: BMPHeaderType::InfoHeader,

            width: 0,
            height: 0,
            data_offset: 0,
            top_down: false,
            no_file_header: false,
            add_alpha_channel: false,
            has_loaded_metadata: false,
            image_type: ImageType::RGB,

            bit_count: 0,
            colors_used: 0,
            palette: None,
            bitfields: None,
        }
    }

    #[cfg(feature = "ico")]
    #[doc(hidden)]
    pub fn reader(&mut self) -> &mut R {
        &mut self.r
    }

    fn read_file_header(&mut self) -> ImageResult<()> {
        if self.no_file_header {
            return Ok(())
        }
        let mut signature = [0; 2];
        try!(self.r.read_exact(&mut signature));

        if signature != b"BM"[..] {
            return Err(ImageError::FormatError("BMP signature not found".to_string()));
        }

        // The next 8 bytes represent file size, followed the 4 reserved bytes
        // We're not interesting these values
        try!(self.r.seek(SeekFrom::Current(8)));

        self.data_offset = try!(self.r.read_u32::<LittleEndian>()) as u64;

        Ok(())
    }

    fn read_bitmap_core_header(&mut self) ->ImageResult<()> {
        self.width  = try!(self.r.read_u16::<LittleEndian>()) as i32;
        self.height = try!(self.r.read_u16::<LittleEndian>()) as i32;

        // Don't care about number of planes
        try!(self.r.seek(SeekFrom::Current(2)));

        self.bit_count = try!(self.r.read_u16::<LittleEndian>());
        match self.bit_count {
            1 | 4 | 8 | 24 => (),
            _ => return Err(ImageError::FormatError("Invalid bit count".to_string())),
        }

        Ok(())
    }

    fn read_bitmap_info_header(&mut self) -> ImageResult<()> {
        self.width  = try!(self.r.read_i32::<LittleEndian>());
        self.height = try!(self.r.read_i32::<LittleEndian>());

        if self.width < 0 {
            return Err(ImageError::FormatError("Negative width".to_string()));
        }

        if self.height == i32::min_value() {
            return Err(ImageError::FormatError("Invalid height".to_string()));
        }

        if self.height < 0 {
            self.height *= -1;
            self.top_down = true;
        }

        // Don't care about number of planes
        try!(self.r.seek(SeekFrom::Current(2)));

        self.bit_count = try!(self.r.read_u16::<LittleEndian>());
        match self.bit_count {
            1 | 4 | 8 | 16 | 24 | 32 => (),
            _ => return Err(ImageError::FormatError("Invalid bit count".to_string())),
        }

        let image_type_u32 = try!(self.r.read_u32::<LittleEndian>());
        match image_type_u32 {
            0 => self.image_type = ImageType::RGB,
            1 => self.image_type = ImageType::RLE8,
            2 => self.image_type = ImageType::RLE4,
            3 => self.image_type = ImageType::Bitfields,
            _  => return Err(ImageError::UnsupportedError("Unsupported image type".to_string())),
        }

        // The next 12 bytes represent data array size in bytes,
        // followed the horizontal and vertical printing resolutions
        // We will calculate the pixel array size using width & height of image
        // We're not interesting the horz or vert printing resolutions
        try!(self.r.seek(SeekFrom::Current(12)));

        self.colors_used = try!(self.r.read_u32::<LittleEndian>());

        // The next 4 bytes represent number of "important" colors
        // We're not interested in this value, so we'll skip it
        try!(self.r.seek(SeekFrom::Current(4)));

        Ok(())
    }


    fn read_bitmasks(&mut self) -> ImageResult<()> {
        let r_mask = try!(self.r.read_u32::<LittleEndian>());
        let g_mask = try!(self.r.read_u32::<LittleEndian>());
        let b_mask = try!(self.r.read_u32::<LittleEndian>());
        self.bitfields = Some((r_mask, g_mask, b_mask));
        Ok(())
    }

    fn read_metadata(&mut self) -> ImageResult<()> {
        if !self.has_loaded_metadata {
            try!(self.read_file_header());
            let bmp_header_size  = try!(self.r.read_u32::<LittleEndian>());

            self.bmp_header_type = match bmp_header_size {
                BITMAPCOREHEADER_SIZE => BMPHeaderType::CoreHeader,
                BITMAPINFOHEADER_SIZE => BMPHeaderType::InfoHeader,
                BITMAPV2HEADER_SIZE => BMPHeaderType::V2Header,
                BITMAPV3HEADER_SIZE => BMPHeaderType::V3Header,
                BITMAPV4HEADER_SIZE => BMPHeaderType::V4Header,
                BITMAPV5HEADER_SIZE => BMPHeaderType::V5Header,
                _ => return Err(ImageError::UnsupportedError("Unsupported Bitmap Header".to_string()))
            };

            match self.bmp_header_type {
                BMPHeaderType::CoreHeader => {
                    try!(self.read_bitmap_core_header());
                },
                BMPHeaderType::InfoHeader | BMPHeaderType::V2Header | BMPHeaderType::V3Header | BMPHeaderType::V4Header | BMPHeaderType::V5Header => {
                    try!(self.read_bitmap_info_header());
                }
            };

            if self.image_type == ImageType::Bitfields {
                match self.bit_count {
                    16 | 32 => {
                        if self.bmp_header_type ==  BMPHeaderType::CoreHeader {
                            return Err(ImageError::FormatError("Cannot use bitfield mode with BITMAPCOREHEADER BMP".to_string()));
                        }

                        try!(self.read_bitmasks());

                        // Skip past alpha mask
                        if self.bmp_header_type != BMPHeaderType::InfoHeader && self.bmp_header_type != BMPHeaderType::V2Header {
                            try!(self.r.seek(SeekFrom::Current(1)));
                        }
                    },
                    _ => return Err(ImageError::FormatError("Invalid bit count for bitfield BMP".to_string())),
                }
            };

            match self.image_type {
                ImageType::RGB => {
                    match self.bit_count {
                        1 | 4 | 8  => try!(self.read_palette()),
                        16 | 24 | 32 => (),
                        _ => return Err(ImageError::UnsupportedError(format!("Unsupported bit count: {}", self.bit_count ))),
                    };
                },
                ImageType::RLE8 => {
                    match self.bit_count {
                        8 => try!(self.read_palette()),
                        _ => return Err(ImageError::UnsupportedError(format!("Unsupported bit count: {}", self.bit_count))),
                    };
                },
                ImageType::RLE4 => {
                    match self.bit_count {
                        4 => try!(self.read_palette()),
                        _ => return Err(ImageError::UnsupportedError(format!("Unsupported bit count: {}", self.bit_count))),
                    };
                },
                _ => { }
            };

            if self.no_file_header {
                // Use the offset of the end of metadata instead of reading a BMP file header.
                self.data_offset = try!(self.r.seek(SeekFrom::Current(0)));
            }

            self.has_loaded_metadata = true;
        }
        Ok(())
    }

    #[cfg(feature = "ico")]
    #[doc(hidden)]
    pub fn read_metadata_in_ico_format(&mut self) -> ImageResult<()> {
        self.no_file_header = true;
        self.add_alpha_channel = true;
        try!(self.read_metadata());

        // The height field in an ICO file is doubled to account for the AND mask
        // (whether or not an AND mask is actually present).
        self.height = self.height / 2;
        Ok(())
    }

    fn get_palette_size(&mut self) -> ImageResult<usize> {
        match self.colors_used {
            0 => match self.bit_count {
                8 | 4 | 1 => Ok(1 << self.bit_count),
                _ => Err(ImageError::FormatError("Invalid bit count for palletized BMP".to_string()))
            },
            _ => {
                if self.colors_used > 1 << self.bit_count {
                    return Err(ImageError::FormatError(format!(
                        "Palette size {} exceeds maximum size for BMP with bit count of {}",
                        self.colors_used, self.bit_count
                    )))
                }
                Ok(self.colors_used as usize)
            }
        }
    }

    fn bytes_per_color(&self) -> usize {
        match self.bmp_header_type {
            BMPHeaderType::CoreHeader => 3,
            _ => 4
        }
    }

    fn read_palette(&mut self) -> ImageResult<()> {
        const MAX_PALETTE_SIZE: usize = 256; // Palette indices are u8.

        let bytes_per_color = self.bytes_per_color();
        let palette_size = try!(self.get_palette_size());
        let length = palette_size * bytes_per_color;
        let max_length = MAX_PALETTE_SIZE * bytes_per_color;
        let mut buf = Vec::with_capacity(max_length);

        buf.resize(length, 0);
        try!(self.r.by_ref().read_exact(&mut buf));

        // Allocate 256 entries even if palette_size is smaller, to prevent corrupt files from
        // causing an out-of-bounds array access.
        if length < max_length {
            buf.resize(max_length, 0);
        }

        let p: Vec<(u8, u8, u8)> = (0..MAX_PALETTE_SIZE).map(|i| {
            let b = buf[bytes_per_color * i];
            let g = buf[bytes_per_color * i + 1];
            let r = buf[bytes_per_color * i + 2];
            (r, g, b)
        }).collect();

        self.palette = Some(p);

        Ok(())
    }

    fn num_channels(&self) -> usize {
        if self.add_alpha_channel { 4 } else { 3 }
    }

    fn create_pixel_data(&self) -> Vec<u8> {
        vec![0xFF; self.num_channels() * self.width as usize * self.height as usize]
    }

    fn rows<'a>(&self, pixel_data: &'a mut Vec<u8>) -> RowIterator<'a> {
        let stride = self.width as usize * self.num_channels();
        if self.top_down {
            RowIterator{ chunks: Chunker::FromTop(pixel_data.chunks_mut(stride)) }
        } else {
            RowIterator{ chunks: Chunker::FromBottom(pixel_data.chunks_mut(stride).rev()) }
        }
    }

    fn read_palettized_pixel_data(&mut self) -> ImageResult<Vec<u8>> {
        let mut pixel_data = self.create_pixel_data();
        let num_channels = self.num_channels();
        let row_byte_length = ((self.bit_count as u32 * self.width as u32 + 31) / 32 * 4) as usize;
        let mut indices = vec![0; row_byte_length];
        let palette = self.palette.as_ref().unwrap();

        try!(self.r.seek(SeekFrom::Start(self.data_offset)));
        for row in self.rows(&mut pixel_data) {
            try!(self.r.by_ref().read_exact(&mut indices));
            let mut pixel_iter = row.chunks_mut(num_channels);
            match self.bit_count {
                1 => { set_1bit_pixel_run(&mut pixel_iter, &palette, indices.iter()); },
                4 => { set_4bit_pixel_run(&mut pixel_iter, &palette, indices.iter(), self.width as usize); },
                8 => { set_8bit_pixel_run(&mut pixel_iter, &palette, indices.iter(), self.width as usize); },
                _ => panic!(),
            }
        }

        Ok(pixel_data)
    }

    fn read_16_bit_pixel_data(&mut self, format: Format16Bit) -> ImageResult<Vec<u8>> {
        let mut pixel_data = self.create_pixel_data();
        let num_channels = self.num_channels();
        let row_padding = self.width % 2 * 2;

        try!(self.r.seek(SeekFrom::Start(self.data_offset)));
        for row in self.rows(&mut pixel_data) {
            for pixel in row.chunks_mut(num_channels) {
                let data = try!(self.r.read_u16::<LittleEndian>());

                let b = match format {
                    Format16Bit::Format444 => LOOKUP_TABLE_4_BIT_TO_8_BIT[(data & 0b1111) as usize],
                    Format16Bit::Format555 => LOOKUP_TABLE_5_BIT_TO_8_BIT[(data & 0b11111) as usize],
                    Format16Bit::Format565 => LOOKUP_TABLE_5_BIT_TO_8_BIT[(data & 0b11111) as usize]
                };

                let g = match format {
                    Format16Bit::Format444 => LOOKUP_TABLE_4_BIT_TO_8_BIT[(data >> 4 & 0b1111) as usize],
                    Format16Bit::Format555 => LOOKUP_TABLE_5_BIT_TO_8_BIT[(data >> 5 & 0b11111) as usize],
                    Format16Bit::Format565 => LOOKUP_TABLE_6_BIT_TO_8_BIT[(data >> 5 & 0b111111) as usize]
                };

                let r = match format {
                    Format16Bit::Format444 => LOOKUP_TABLE_4_BIT_TO_8_BIT[(data >> 8 & 0b1111) as usize],
                    Format16Bit::Format555 => LOOKUP_TABLE_5_BIT_TO_8_BIT[(data >> 10 & 0b11111) as usize],
                    Format16Bit::Format565 => LOOKUP_TABLE_5_BIT_TO_8_BIT[(data >> 11 & 0b11111) as usize]
                };

                pixel[0] = r;
                pixel[1] = g;
                pixel[2] = b;
            }
            // Seek past row padding
            try!(self.r.seek(SeekFrom::Current(row_padding as i64)));
        }

        Ok(pixel_data)
    }

    fn read_full_byte_pixel_data(&mut self, format: FormatFullBytes) -> ImageResult<Vec<u8>> {
        let mut pixel_data = self.create_pixel_data();
        let num_channels = self.num_channels();
        let row_padding = match format {
            FormatFullBytes::FormatRGB24 => (4 - (self.width as i64 * 3) % 4) % 4,
            _ => 0
        };

        try!(self.r.seek(SeekFrom::Start(self.data_offset)));
        for row in self.rows(&mut pixel_data) {
            for pixel in row.chunks_mut(num_channels) {

                if format == FormatFullBytes::Format888 {
                    try!(self.r.seek(SeekFrom::Current(1)));
                }

                let b = try!(self.r.read_u8());
                let g = try!(self.r.read_u8());
                let r = try!(self.r.read_u8());

                if format == FormatFullBytes::FormatRGB32 {
                    try!(self.r.seek(SeekFrom::Current(1)));
                }

                pixel[0] = r;
                pixel[1] = g;
                pixel[2] = b;

                if format == FormatFullBytes::FormatRGBA32 {
                    let a = try!(self.r.read_u8());
                    pixel[3] = a;
                }
            }
            // Seek past row padding
            try!(self.r.seek(SeekFrom::Current(row_padding)));
        }

        Ok(pixel_data)
    }

    fn read_rle_data(&mut self, image_type: ImageType) -> ImageResult<Vec<u8>> {
        let mut pixel_data = self.create_pixel_data();
        let num_channels = self.num_channels();

        try!(self.r.seek(SeekFrom::Start(self.data_offset)));
        // Scope the borrowing of pixel_data by the row iterator.
        {
            // Handling deltas in the RLE scheme means that we need to manually
            // iterate through rows and pixels.  Even if we didn't have to handle
            // deltas, we have to ensure that a single runlength doesn't straddle
            // two rows.
            let mut row_iter = self.rows(&mut pixel_data);
            let mut insns_iter = RLEInsnIterator{ r: &mut self.r, image_type: image_type };
            let p = self.palette.as_ref().unwrap();

            'row_loop: while let Some(row) = row_iter.next() {
                let mut pixel_iter = row.chunks_mut(num_channels);

                'rle_loop: loop {
                    if let Some(insn) = insns_iter.next() {
                        match insn {
                            RLEInsn::EndOfFile => {
                                break 'row_loop;
                            },
                            RLEInsn::EndOfRow => {
                                break 'rle_loop;
                            },
                            RLEInsn::Delta(x_delta, y_delta) => {
                                for _ in 0..x_delta {
                                    if let None = pixel_iter.next() {
                                        // We can't go any further in this row.
                                        break;
                                    }
                                }

                                if y_delta > 0 {
                                    for _ in 1..y_delta {
                                        if let None = row_iter.next() {
                                            // We've reached the end of the image.
                                            break 'row_loop;
                                        }
                                    }
                                }
                            },
                            RLEInsn::Absolute(length, indices) => {
                                // Absolute mode cannot span rows, so if we run
                                // out of pixels to process, we should stop
                                // processing the image.
                                match image_type {
                                    ImageType::RLE8 => {
                                        if !set_8bit_pixel_run(&mut pixel_iter,
                                                               &p,
                                                               indices.iter(),
                                                               length as usize) {
                                            break 'row_loop;
                                        }
                                    },
                                    ImageType::RLE4 => {
                                        if !set_4bit_pixel_run(&mut pixel_iter,
                                                               &p,
                                                               indices.iter(),
                                                               length as usize) {
                                            break 'row_loop;
                                        }
                                    },
                                    _ => panic!(),
                                }
                            },
                            RLEInsn::PixelRun(n_pixels, palette_index) => {
                                // A pixel run isn't allowed to span rows, but we
                                // simply continue on to the next row if we run
                                // out of pixels to set.
                                match image_type {
                                    ImageType::RLE8 => {
                                        if !set_8bit_pixel_run(&mut pixel_iter,
                                                               &p,
                                                               repeat(&palette_index),
                                                               n_pixels as usize) {
                                            break 'rle_loop;
                                        }
                                    },
                                    ImageType::RLE4 => {
                                        if !set_4bit_pixel_run(&mut pixel_iter,
                                                               &p,
                                                               repeat(&palette_index),
                                                               n_pixels as usize) {
                                            break 'rle_loop;
                                        }
                                    },
                                    _ => panic!()
                                }
                            }
                        }
                    } else {
                        // We ran out of data while we still had rows to fill in.
                        return Err(ImageError::FormatError("Not enough RLE data".to_string()))
                    }
                }
            }

        }
        Ok(pixel_data)
    }

    fn read_image_data(&mut self) -> ImageResult<Vec<u8>> {
        match self.image_type {
            ImageType::RGB => {
                match self.bit_count {
                    1 | 4 | 8 => {
                        return self.read_palettized_pixel_data();
                    },
                    16 => return self.read_16_bit_pixel_data(Format16Bit::Format555),
                    24 => return self.read_full_byte_pixel_data(FormatFullBytes::FormatRGB24),
                    32 => return if self.add_alpha_channel {
                        self.read_full_byte_pixel_data(FormatFullBytes::FormatRGBA32)
                    } else {
                        self.read_full_byte_pixel_data(FormatFullBytes::FormatRGB32)
                    },
                    _ => return Err(ImageError::FormatError("Invalid bit count for RGB bitmap".to_string()))
                }
            },
            ImageType::RLE8 => {
                match self.bit_count {
                    8 => {
                        return self.read_rle_data(ImageType::RLE8);
                    },
                    _ => return Err(ImageError::FormatError("Invalid bit count for RLE8 bitmap".to_string()))
                }
            },
            ImageType::RLE4 => {
                match self.bit_count {
                    4 => {
                        return self.read_rle_data(ImageType::RLE4);
                    },
                    _ => return Err(ImageError::FormatError("Invalid bit count for RLE4 bitmap".to_string()))
                }
            },
            ImageType::Bitfields => {
                match self.bit_count{
                    16 => {
                        match self.bitfields {
                            Some(R4_G4_B4_COLOR_MASK) => {
                                return self.read_16_bit_pixel_data(Format16Bit::Format444)
                            },
                            Some(R5_G5_B5_COLOR_MASK) => {
                                return self.read_16_bit_pixel_data(Format16Bit::Format555)
                            },
                            Some(R5_G6_B5_COLOR_MASK) => {
                                return self.read_16_bit_pixel_data(Format16Bit::Format565)
                            },
                            _ => return Err(ImageError::UnsupportedError("Unsupported 16-bit bitfield".to_string()))
                        }
                    },
                    32 => {
                        match self.bitfields {
                            Some(R8_G8_B8_COLOR_MASK) => {
                                return self.read_full_byte_pixel_data(FormatFullBytes::Format888)
                            },
                            _ => return Err(ImageError::UnsupportedError("Unsupported 32-bit bitfield".to_string()))
                        }
                    },
                    _ => return Err(ImageError::FormatError("Invalid bit count for bitfield bitmap".to_string())),
                }
            },
        }
    }
}

impl<R: Read + Seek> ImageDecoder for BMPDecoder<R> {
    fn dimensions(&mut self) -> ImageResult<(u32, u32)> {
        try!(self.read_metadata());
        Ok((self.width as u32, self.height as u32))
    }

    fn colortype(&mut self) -> ImageResult<ColorType> {
        if self.add_alpha_channel {
            Ok(ColorType::RGBA(8))
        } else {
            Ok(ColorType::RGB(8))
        }
    }

    fn row_len(&mut self) -> ImageResult<usize> {
        try!(self.read_metadata());
        Ok(3 * self.width as usize)
    }

    fn read_scanline(&mut self, _buf: &mut [u8]) -> ImageResult<u32> {
        unimplemented!();
    }

    fn read_image(&mut self) -> ImageResult<DecodingResult> {
        try!(self.read_metadata());
        self.read_image_data().map(|v| DecodingResult::U8(v) )
    }
}

#[cfg(test)]
mod bench {
    use super::BMPDecoder;
    use image::ImageDecoder;
    use std::{fs, io, path};
    use std::io::Read;
    use test;

    const IMAGE_DIR: [&'static str; 5] = [".", "tests", "images", "bmp", "images"];
    fn bench_read_image(b: &mut test::Bencher, filename: &str) {
        let mut path: path::PathBuf = IMAGE_DIR.iter().collect();
        path.push(filename);
        let mut fin = fs::File::open(path).unwrap();
        let mut buf = Vec::new();
        fin.read_to_end(&mut buf).unwrap();
        b.iter(|| {
            let r = io::Cursor::new(&buf);
            let mut d = BMPDecoder::new(r);
            d.read_image().unwrap();
        })
    }

    #[bench]
    fn bench_read_1bit(b: &mut test::Bencher) {
        bench_read_image(b, "Core_1_Bit.bmp");
    }

    #[bench]
    fn bench_read_4bit(b: &mut test::Bencher) {
        bench_read_image(b, "Core_4_Bit.bmp");
    }

    #[bench]
    fn bench_read_8bit(b: &mut test::Bencher) {
        bench_read_image(b, "Core_8_Bit.bmp");
    }

    #[bench]
    fn bench_read_4rle(b: &mut test::Bencher) {
        bench_read_image(b, "pal4rle.bmp");
    }

    #[bench]
    fn bench_read_8rle(b: &mut test::Bencher) {
        bench_read_image(b, "pal8rle.bmp");
    }
}
