use byteorder::{LittleEndian, WriteBytesExt};
use std::io::{self, Write};

use crate::color::ColorType;
use crate::error::ImageResult;
use crate::image::ImageEncoder;

use crate::codecs::png::PngEncoder;

// Enum value indicating an ICO image (as opposed to a CUR image):
const ICO_IMAGE_TYPE: u16 = 1;
// The length of an ICO file ICONDIR structure, in bytes:
const ICO_ICONDIR_SIZE: u32 = 6;
// The length of an ICO file DIRENTRY structure, in bytes:
const ICO_DIRENTRY_SIZE: u32 = 16;

/// ICO encoder
pub struct IcoEncoder<W: Write> {
    w: W,
}

impl<W: Write> IcoEncoder<W> {
    /// Create a new encoder that writes its output to ```w```.
    pub fn new(w: W) -> IcoEncoder<W> {
        IcoEncoder { w }
    }

    /// Encodes the image ```image``` that has dimensions ```width``` and
    /// ```height``` and ```ColorType``` ```c```.  The dimensions of the image
    /// must be between 1 and 256 (inclusive) or an error will be returned.
    pub fn encode(
        mut self,
        data: &[u8],
        width: u32,
        height: u32,
        color: ColorType,
    ) -> ImageResult<()> {
        let mut image_data: Vec<u8> = Vec::new();
        PngEncoder::new(&mut image_data).encode(data, width, height, color)?;

        write_icondir(&mut self.w, 1)?;
        write_direntry(
            &mut self.w,
            width,
            height,
            color,
            ICO_ICONDIR_SIZE + ICO_DIRENTRY_SIZE,
            image_data.len() as u32,
        )?;
        self.w.write_all(&image_data)?;
        Ok(())
    }
}

impl<W: Write> ImageEncoder for IcoEncoder<W> {
    fn write_image(
        self,
        buf: &[u8],
        width: u32,
        height: u32,
        color_type: ColorType,
    ) -> ImageResult<()> {
        self.encode(buf, width, height, color_type)
    }
}

fn write_icondir<W: Write>(w: &mut W, num_images: u16) -> io::Result<()> {
    // Reserved field (must be zero):
    w.write_u16::<LittleEndian>(0)?;
    // Image type (ICO or CUR):
    w.write_u16::<LittleEndian>(ICO_IMAGE_TYPE)?;
    // Number of images in the file:
    w.write_u16::<LittleEndian>(num_images)?;
    Ok(())
}

fn write_direntry<W: Write>(
    w: &mut W,
    width: u32,
    height: u32,
    color: ColorType,
    data_start: u32,
    data_size: u32,
) -> io::Result<()> {
    // Image dimensions:
    write_width_or_height(w, width)?;
    write_width_or_height(w, height)?;
    // Number of colors in palette (or zero for no palette):
    w.write_u8(0)?;
    // Reserved field (must be zero):
    w.write_u8(0)?;
    // Color planes:
    w.write_u16::<LittleEndian>(0)?;
    // Bits per pixel:
    w.write_u16::<LittleEndian>(color.bits_per_pixel())?;
    // Image data size, in bytes:
    w.write_u32::<LittleEndian>(data_size)?;
    // Image data offset, in bytes:
    w.write_u32::<LittleEndian>(data_start)?;
    Ok(())
}

/// Encode a width/height value as a single byte, where 0 means 256.
fn write_width_or_height<W: Write>(w: &mut W, value: u32) -> io::Result<()> {
    if value < 1 || value > 256 {
        // TODO: this is not very idiomatic yet. Should return an EncodingError and be checked
        // prior to encoding.
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "Invalid ICO dimensions (width and \
             height must be between 1 and 256)",
        ));
    }
    w.write_u8(if value < 256 { value as u8 } else { 0 })
}
