use byteorder::{LittleEndian, WriteBytesExt};
use std::io::{self, Write};

use crate::color::ColorType;
use crate::error::{ImageError, ImageResult, ParameterError, ParameterErrorKind};
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
    ///
    /// Expects data to be big endian.
    #[deprecated = "Use `IcoEncoder::write_image` instead. Beware that `write_image` has a different endianness convention"]
    pub fn encode(
        mut self,
        data: &[u8],
        width: u32,
        height: u32,
        color: ColorType,
    ) -> ImageResult<()> {
        let mut image_data: Vec<u8> = Vec::new();
        #[allow(deprecated)]
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

    /// Takes some already encoded PNG or BMP images and encodes them into an ICO.
    ///
    /// `images` is a list of images, usually ordered by dimension.
    /// The items of the tuple composing the list are as follows:
    ///
    /// * `encoded_image`: a PNG or BMP image
    /// * `width`: the width of the `encoded_image`
    /// * `height`: the height of the `encoded_image`
    /// * `color_type`: the [`ColorType`] of the `encoded_image`
    ///
    /// The dimensions of the image must be between 1 and 256 (inclusive) or
    /// an error will be returned.
    /// `images` must have a lenght between 1 and 65535 (inclusive).
    pub fn write_pre_encoded_images(
        mut self,
        images: &[(&[u8], u32, u32, ColorType)],
    ) -> ImageResult<()> {
        if !(1..=usize::from(u16::MAX)).contains(&images.len()) {
            return Err(ImageError::Parameter(ParameterError::from_kind(
                ParameterErrorKind::Generic(format!(
                    "the number of images must be `1..=u16::MAX`, instead {} images were provided",
                    images.len(),
                )),
            )));
        }
        let num_images = images.len() as u16;

        let mut offset = ICO_ICONDIR_SIZE;
        write_icondir(&mut self.w, num_images)?;
        for &(encoded_image, width, height, color_type) in images {
            offset += ICO_DIRENTRY_SIZE;

            write_direntry(
                &mut self.w,
                width,
                height,
                color_type,
                offset,
                encoded_image.len() as u32,
            )?;

            offset += encoded_image.len() as u32;
        }
        for &(encoded_image, _width, _height, _color_type) in images {
            self.w.write_all(encoded_image)?;
        }
        Ok(())
    }
}

impl<W: Write> ImageEncoder for IcoEncoder<W> {
    /// Write an ICO image with the specified width, height, and color type.
    ///
    /// For color types with 16-bit per channel or larger, the contents of `buf` should be in
    /// native endian.
    ///
    /// WARNING: In image 0.23.14 and earlier this method erroneously expected buf to be in big endian.
    fn write_image(
        self,
        buf: &[u8],
        width: u32,
        height: u32,
        color_type: ColorType,
    ) -> ImageResult<()> {
        let mut image_data: Vec<u8> = Vec::new();
        PngEncoder::new(&mut image_data).write_image(buf, width, height, color_type)?;

        let images = [(image_data.as_slice(), width, height, color_type)];
        self.write_pre_encoded_images(&images)
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
