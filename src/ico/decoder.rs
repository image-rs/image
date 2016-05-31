use byteorder::{ReadBytesExt, LittleEndian};
use std::io::{Read, Seek, SeekFrom};

use color::ColorType;
use image::{DecodingResult, ImageResult, ImageDecoder, ImageError};

use bmp::BMPDecoder;
use png::PNGDecoder;
use self::InnerDecoder::*;

// http://www.w3.org/TR/PNG-Structure.html
// The first eight bytes of a PNG file always contain the following (decimal) values:
const PNG_SIGNATURE: [u8; 8] = [137, 80, 78, 71, 13, 10, 26, 10];

/// An ico decoder
pub struct ICODecoder<R: Read> {
    selected_entry: DirEntry,
    inner_decoder: InnerDecoder<R>,
}

enum InnerDecoder<R: Read> {
    BMP(BMPDecoder<R>),
    PNG(PNGDecoder<R>)
}

#[derive(Clone, Copy, Default)]
struct DirEntry {
    width: u8,
    height: u8,
    color_count: u8,
    reserved: u8,

    num_color_planes: u16,
    bits_per_pixel: u16,

    image_length: u32,
    image_offset: u32,
}

impl<R: Read + Seek> ICODecoder<R> {
    /// Create a new decoder that decodes from the stream ```r```
    pub fn new(mut r: R) -> ImageResult<ICODecoder<R>> {
        let entries = try!(read_entries(&mut r));
        let entry = try!(best_entry(entries));
        let decoder = try!(entry.decoder(r));

        Ok(ICODecoder {
            selected_entry: entry,
            inner_decoder: decoder,
        })
    }
}

fn read_entries<R: Read>(r: &mut R) -> ImageResult<Vec<DirEntry>> {
    let _reserved = try!(r.read_u16::<LittleEndian>());
    let _type = try!(r.read_u16::<LittleEndian>());
    let count = try!(r.read_u16::<LittleEndian>());
    (0..count).map(|_| read_entry(r)).collect()
}

fn read_entry<R: Read>(r: &mut R) -> ImageResult<DirEntry> {
    let mut entry = DirEntry::default();

    entry.width = try!(r.read_u8());
    entry.height = try!(r.read_u8());
    entry.color_count = try!(r.read_u8());
    entry.reserved = try!(r.read_u8());

    entry.num_color_planes = try!(r.read_u16::<LittleEndian>());
    entry.bits_per_pixel = try!(r.read_u16::<LittleEndian>());

    entry.image_length = try!(r.read_u32::<LittleEndian>());
    entry.image_offset = try!(r.read_u32::<LittleEndian>());

    Ok(entry)
}

/// Find the entry with the highest (color depth, size).
fn best_entry(mut entries: Vec<DirEntry>) -> ImageResult<DirEntry> {
    let mut best = try!(entries.pop().ok_or(ImageError::ImageEnd));
    let mut best_score = (best.bits_per_pixel, best.real_width() as u32 * best.real_height() as u32);

    for entry in entries.into_iter() {
        let score = (entry.bits_per_pixel, entry.real_width() as u32 * entry.real_height() as u32);
        if score > best_score {
            best = entry;
            best_score = score;
        }
    }
    Ok(best)
}


impl DirEntry {
    fn real_width(&self) -> u16 {
        match self.width {
            0 => 256,
            w => w as u16
        }
    }

    fn real_height(&self) -> u16 {
        match self.height {
            0 => 256,
            h => h as u16
        }
    }

    fn seek_to_start<R: Read + Seek>(&self, r: &mut R) -> ImageResult<()> {
        try!(r.seek(SeekFrom::Start(self.image_offset as u64)));
        Ok(())
    }

    fn is_png<R: Read + Seek>(&self, r: &mut R) -> ImageResult<bool> {
        try!(self.seek_to_start(r));

        // Read the first 8 bytes to sniff the image.
        let mut signature = [0u8; 8];
        try!(r.read_exact(&mut signature));

        Ok(signature == PNG_SIGNATURE)
    }

    fn decoder<R: Read + Seek>(&self, mut r: R) -> ImageResult<InnerDecoder<R>> {
        let is_png = try!(self.is_png(&mut r));
        try!(self.seek_to_start(&mut r));

        if is_png {
            Ok(PNG(PNGDecoder::new(r)))
        } else {
            let mut decoder = BMPDecoder::new(r);
            try!(decoder.read_metadata_in_ico_format());
            Ok(BMP(decoder))
        }
    }
}

impl<R: Read + Seek> ImageDecoder for ICODecoder<R> {
    fn dimensions(&mut self) -> ImageResult<(u32, u32)> {
        match self.inner_decoder {
            BMP(ref mut decoder) => decoder.dimensions(),
            PNG(ref mut decoder) => decoder.dimensions()
        }
    }

    fn colortype(&mut self) -> ImageResult<ColorType> {
        match self.inner_decoder {
            BMP(ref mut decoder) => decoder.colortype(),
            PNG(ref mut decoder) => decoder.colortype()
        }
    }

    fn row_len(&mut self) -> ImageResult<usize> {
        match self.inner_decoder {
            BMP(ref mut decoder) => decoder.row_len(),
            PNG(ref mut decoder) => decoder.row_len()
        }
    }

    fn read_scanline(&mut self, buf: &mut [u8]) -> ImageResult<u32> {
        match self.inner_decoder {
            BMP(ref mut decoder) => decoder.read_scanline(buf),
            PNG(ref mut decoder) => decoder.read_scanline(buf)
        }
    }

    fn read_image(&mut self) -> ImageResult<DecodingResult> {
        match self.inner_decoder {
            PNG(ref mut decoder) => {
                decoder.read_image()
            }
            BMP(ref mut decoder) => {
                let (width, height) = try!(decoder.dimensions());

                // The ICO decoder needs an alpha chanel to apply the AND mask.
                if try!(decoder.colortype()) != ColorType::RGBA(8) {
                    return Err(ImageError::UnsupportedError("Unsupported color type".to_string()))
                }
                let mut pixel_data = match try!(decoder.read_image()) {
                    DecodingResult::U8(v) => v,
                    _ => unreachable!()
                };

                // If there's an AND mask following the image, read and apply it.
                let r = decoder.reader();
                let mask_start = try!(r.seek(SeekFrom::Current(0)));
                let mask_end = (self.selected_entry.image_offset + self.selected_entry.image_length) as u64;
                let mask_length = mask_end - mask_start;

                if mask_length > 0 {
                    // A mask row contains 1 bit per pixel, padded to 4 bytes.
                    let mask_row_bytes = ((width + 31) / 32) * 4;
                    let expected_length = (mask_row_bytes * height) as u64;
                    if mask_length < expected_length {
                        return Err(ImageError::ImageEnd)
                    }

                    for y in 0..height {
                        let mut x = 0;
                        for _ in 0..mask_row_bytes {
                            // Apply the bits of each byte until we reach the end of the row.
                            let mask_byte = try!(r.read_u8());
                            for bit in (0..8).rev() {
                                if x >= width { break }
                                if mask_byte & (1 << bit) != 0 {
                                    // Set alpha channel to transparent.
                                    pixel_data[((height - y - 1) * width + x) as usize * 4 + 3] = 0;
                                }
                                x += 1;
                            }
                        }
                    }
                }
                Ok(DecodingResult::U8(pixel_data))
            }
        }
    }
}
