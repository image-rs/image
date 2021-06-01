//! Decoding of OpenEXR (.exr) Images
//!
//! OpenEXR is an image format that is widely used, especially in VFX,
//! because it supports lossless and lossy compression for float data.
//!
//! # Related Links
//! * <https://www.openexr.com/documentation.html> - The OpenEXR reference.

// # ROADMAP
// - [x] ImageDecoder
// - [x] ImageEncoder
// - [ ] GenericImageView?
// - [ ] GenericImage?
// - [x] Progress

// # MAYBE SOON
// - [ ] ImageDecoderExt::read_rect_with_progress
// - [ ] Layers -> Animation?


extern crate exr;
use exr::prelude::*;

use crate::{ImageDecoder, ImageResult, ColorType, Progress, image, ImageError, ImageFormat};
use std::io::{Write, Seek, BufRead, SeekFrom, Cursor};
use crate::error::{DecodingError, ImageFormatHint};
use self::exr::meta::header::Header;

/// An OpenEXR decoder
#[derive(Debug)]
pub struct ExrDecoder<R> {
    exr_reader: exr::block::reader::Reader<R>,

    // select a header that is rgb and not deep
    header_index: usize,
}

impl<R: BufRead + Seek> ExrDecoder<R> {

    /// Create a decoder. Consumes the first few bytes of the source to extract image dimensions.
    pub fn read(mut source: R) -> ImageResult<Self> {

        // read meta data, then wait for further instructions, keeping the file open and ready
        let exr_reader = exr::block::read(source, false)?;

        let header_index = exr_reader.headers().into_iter()
            .position(|header|{
                let has_rgb = ["R","G","B"].iter().all( // alpha will be optional
                    // check if r/g/b exists in the channels
                    |required| header.channels.list.iter()
                        .find(|chan| chan.name.eq(required)).is_some() // TODO eq_lowercase only if exrs supports it
                );

                !header.deep && has_rgb
            })
            .ok_or_else(|| ImageError::Decoding(DecodingError::new(
                ImageFormatHint::Exact(ImageFormat::Exr),
                "image does not contain non-deep rgb channels"
            )))?;

        Ok(Self { exr_reader, header_index })
    }
}


impl<'a, R: 'a + BufRead + Seek> ImageDecoder<'a> for ExrDecoder<R> {
    type Reader = Cursor<Vec<u8>>;

    fn dimensions(&self) -> (u32, u32) {
        let size = self.exr_reader.headers()[self.header_index].layer_size;
        (size.width() as u32, size.height() as u32)
    }

    // TODO fn original_color_type(&self) -> ExtendedColorType {}

    fn color_type(&self) -> ColorType {
        // TODO adapt to actual exr color type
        // let _channels = &self.exr_reader.headers()[self.header_index].channels;
        ColorType::Rgba32F
    }

    /// Use `read_image` if possible,
    /// as this method creates a whole new buffer to contain the entire image.
    fn into_reader(self) -> ImageResult<Self::Reader> {
        Ok(Cursor::new(image::decoder_to_vec(self)?)) // TODO no vec?
    }

    fn scanline_bytes(&self) -> u64 {
        // we cannot always read individual scan lines for every file,
        // as the tiles or lines in the file could be in random or reversed order.
        // therefore we currently read all lines at once
        // Todo: optimize for specific exr.line_order?
       self.total_bytes()
    }

    fn read_image_with_progress<F: Fn(Progress)>(self, target: &mut [u8], progress_callback: F) -> ImageResult<()> {
        let blocks_in_header = self.exr_reader.headers()[self.header_index].chunk_count as u64;

        let result = exr::prelude::read()
            .no_deep_data().largest_resolution_level()
            .rgba_channels(
                |size, _channels|
                    (size, vec![0_f32; size.area()*4])
                ,

                |(size, buffer), position, (r,g,b,a_or_1): (f32,f32,f32,f32)| {
                    // TODO white point chromaticities + srgb/linear conversion?
                    let first_f32_index = position.flat_index_for_size(*size);
                    buffer[first_f32_index*4 .. (first_f32_index + 1)*4].copy_from_slice(&[r, g, b, a_or_1]);
                }
            )
            .first_valid_layer() // TODO select exact layer by self.header_index?
            .all_attributes()
            .on_progress(move |progress| {
                let mut prog = progress_callback;
                prog(Progress::new((progress*blocks_in_header as f64) as u64, blocks_in_header)); // TODO precision errors?
            })
            .from_chunks(self.exr_reader)?;

        // TODO this copy is strictly not necessary, but the exr api is a little too simple for reading into a borrowed target slice
        target.copy_from_slice(bytemuck::cast_slice(result.layer_data.channel_data.pixels.1.as_slice()));

        // TODO keep meta data?
        Ok(())
    }
}



pub fn write_image(mut seekable_write: impl Write + Seek, bytes: &[u8], width: u32, height: u32, color_type: ColorType) -> ImageResult<()> {
    let width = width as usize;
    let height = height as usize;

    let pixels: &[f32] = bytemuck::try_cast_slice(bytes).expect("image byte buffer must be aligned to f32");

    match color_type {
        ColorType::Rgb16 => {
            exr::prelude::Image
                ::from_channels(
                    (width, height),
                    SpecificChannels::rgb(|pixel: Vec2<usize>| {
                        let pixel_index = 3 * pixel.flat_index_for_size(Vec2(width, height));
                        (pixels[pixel_index], pixels[pixel_index+1], pixels[pixel_index+2])
                    })
                )
                .write()
                // .on_progress(|progress| todo!())
                .to_buffered(&mut seekable_write)?;
        }

        ColorType::Rgba16 => {
            exr::prelude::Image
                ::from_channels(
                    (width, height),
                    SpecificChannels::rgba(|pixel: Vec2<usize>| {
                        let pixel_index = 4 * pixel.flat_index_for_size(Vec2(width, height));
                        (pixels[pixel_index], pixels[pixel_index+1], pixels[pixel_index+2], pixels[pixel_index+3])
                    })
                )
                .write()
                // .on_progress(|progress| todo!())
                .to_buffered(&mut seekable_write)?;
        }

        _ => todo!()
    }

    Ok(())
}


impl From<exr::error::Error> for ImageError {
    fn from(exr_error: Error) -> Self {
        ImageError::Decoding(DecodingError::new(
            ImageFormatHint::Exact(ImageFormat::Exr),
            exr_error
        ))
    }
}


