//! Decoding of OpenEXR (.exr) Images
//!
//! OpenEXR is an image format that is widely used, especially in VFX,
//! because it supports lossless and lossy compression for float data.
//!
//! This decoder only supports RGB and RGBA images.
//! If an image does not contain alpha information,
//! it is defaulted to `1.0`, which means no transparency.
//!
//! # Related Links
//! * <https://www.openexr.com/documentation.html> - The OpenEXR reference.

// ROADMAP: See https://github.com/image-rs/image/pull/1475

extern crate exr;
use exr::prelude::*;

use crate::{ImageDecoder, ImageResult, ColorType, Progress, ImageError, ImageFormat, ImageBuffer, Rgba, Rgb, ImageEncoder, ExtendedColorType};
use std::io::{Write, Seek, BufRead, Cursor, BufReader, BufWriter};
use crate::error::{DecodingError, ImageFormatHint, LimitError, LimitErrorKind, EncodingError};
use crate::image::decoder_to_vec;
use std::path::Path;

/// An image buffer for 32-bit float RGB pixels,
/// where the backing container is a flattened vector of floats.
pub type RgbF32Buffer = ImageBuffer<Rgb<f32>, Vec<f32>>;

/// An image buffer for 32-bit float RGBA pixels,
/// where the backing container is a flattened vector of floats.
pub type RgbaF32Buffer = ImageBuffer<Rgba<f32>, Vec<f32>>;


// TODO this could be a generic function that works for any format
/// Read the file from the specified path into an `RgbaF32Buffer`.
// TODO progress? load rect?
pub fn read_as_rgba_image_from_file(path: impl AsRef<Path>) -> ImageResult<RgbaF32Buffer> {
    read_as_rgba_image(BufReader::new(std::fs::File::open(path)?))
}

// TODO this could be a generic function that works for any format
/// Read an `RgbaF32Buffer`.
/// Assumes the reader is buffered. In most cases,
/// you should wrap your reader in a `BufReader` for best performance.
// TODO progress? load rect?
pub fn read_as_rgba_image(read: impl BufRead + Seek) -> ImageResult<RgbaF32Buffer> {
    ExrDecoder::read(read)?.read_as_rgba_image()
}

// TODO this could be a generic function that works for any format
/// Read the file from the specified path into an `RgbF32Buffer`.
// TODO progress? load rect?
pub fn read_as_rgb_image_from_file(path: impl AsRef<Path>) -> ImageResult<RgbF32Buffer> {
    read_as_rgb_image(BufReader::new(std::fs::File::open(path)?))
}

// TODO this could be a generic function that works for any format
/// Read an `RgbF32Buffer`.
/// Assumes the reader is buffered. In most cases,
/// you should wrap your reader in a `BufReader` for best performance.
// TODO progress? load rect?
pub fn read_as_rgb_image(read: impl BufRead + Seek) -> ImageResult<RgbF32Buffer> {
    ExrDecoder::read(read)?.read_as_rgb_image()
}


// TODO this could be a generic function that works for any format
/// Write an `RgbF32Buffer` to the specified file path, replacing any existing file.
// TODO progress? load rect?
pub fn write_rgb_image_to_file(path: impl AsRef<Path>, image: &RgbF32Buffer) -> ImageResult<()> {
    write_rgb_image(BufWriter::new(std::fs::File::create(path)?), image)
}

// TODO this could be a generic function that works for any format
/// Write an `RgbF32Buffer`.
/// Assumes the writer is buffered. In most cases,
/// you should wrap your writer in a `BufWriter` for best performance.
// TODO progress? load rect?
pub fn write_rgb_image(write: impl Write/* + Seek*/, image: &RgbF32Buffer) -> ImageResult<()> {
    write_buffer(
        write,
        bytemuck::cast_slice(image.as_raw().as_slice()),
        image.width(), image.height(),
        ColorType::Rgb32F
    )
}

// TODO this could be a generic function that works for any format
/// Write an `RgbaF32Buffer` to the specified file path, replacing any existing file.
// TODO progress? load rect?
pub fn write_rgba_image_to_file(path: impl AsRef<Path>, image: &RgbaF32Buffer) -> ImageResult<()> {
    write_rgba_image(BufWriter::new(std::fs::File::create(path)?), image)
}

// TODO this could be a generic function that works for any format
/// Write an `RgbaF32Buffer`.
/// Assumes the writer is buffered. In most cases,
/// you should wrap your writer in a `BufWriter` for best performance.
// TODO progress? load rect?
pub fn write_rgba_image(write: impl Write/* + Seek*/, image: &RgbaF32Buffer) -> ImageResult<()> {
    write_buffer(
        write,
        bytemuck::cast_slice(image.as_raw().as_slice()),
        image.width(), image.height(),
        ColorType::Rgba32F
    )
}


/// An OpenEXR decoder. Immediately reads the meta data from the file.
#[derive(Debug)]
pub struct ExrDecoder<R> {
    exr_reader: exr::block::reader::Reader<R>,

    // select a header that is rgb and not deep
    header_index: usize,

    // decode either rgb or rgba.
    // by default, only activated if image contains alpha.
    // can be changed to include or discard alpha channels.
    alpha_requested: bool,

    // whether the original file contains alpha
    alpha_present_in_file: bool,
}


impl<R: BufRead + Seek> ExrDecoder<R> {

    /// Create a decoder. Consumes the first few bytes of the source to extract image dimensions.
    /// Assumes the reader is buffered. In most cases,
    /// you should wrap your reader in a `BufReader` for best performance.
    pub fn read(source: R) -> ImageResult<Self> {

        // read meta data, then wait for further instructions, keeping the file open and ready
        let exr_reader = exr::block::read(source, false).map_err(to_image_err)?;

        let header_index = exr_reader.headers().into_iter()
            .position(|header|{
                let has_rgb = ["R","G","B"].iter().all( // alpha will be optional
                    // check if r/g/b exists in the channels
                    // TODO binary search, channels are sorted
                    |required| header.channels.list.iter()
                        .find(|chan| chan.name.eq(required)).is_some() // TODO eq_lowercase only if exrs supports it
                );

                // we currently dont support deep images, or images with other color spaces than rgb
                !header.deep && has_rgb
            })
            .ok_or_else(|| ImageError::Decoding(DecodingError::new(
                ImageFormatHint::Exact(ImageFormat::Exr),
                "image does not contain non-deep rgb channels"
            )))?;

        // TODO binary search, channels are sorted
        let has_alpha = exr_reader.headers()[header_index]
            .channels.list
            //.binary_search_by_key(b"A", |chan| chan.name.bytes())
            .iter()
            .find(|chan| chan.name.eq("A")) // TODO eq_lowercase only if exrs supports it
            .is_some();

        Ok(Self { exr_reader, header_index, alpha_requested: has_alpha, alpha_present_in_file: has_alpha })
    }

    // do not leak exrs-specific meta data into public api, just do it for this module
    fn selected_exr_header(&self) -> &exr::meta::header::Header {
        &self.exr_reader.meta_data().headers[self.header_index]
    }

    // TODO does using Rgba<f32> with Vec<f32> automagically un-flatten the vector??
    /// Read this OpenEXR file as an `RgbaF32Buffer`,
    /// including the alpha channel. A default value of `1.0` is used
    /// if there is no alpha channel in the image.
    /// Any calls to `color_type` or `total_bytes`
    /// might not agree with the result of this function.
    // TODO progress? load rect?
    pub fn read_as_rgba_image(mut self) -> ImageResult<RgbaF32Buffer> {
        let (width, height) = self.dimensions();

        self.alpha_requested = true;
        let buffer: Vec<f32> = decoder_to_vec(self)?;

        ImageBuffer::from_raw(width, height, buffer)

            // this should be the only reason for the "from raw" call to fail,
            // even though such a large allocation would probably cause an error much earlier
            .ok_or_else(|| ImageError::Limits(LimitError::from_kind(LimitErrorKind::InsufficientMemory)))
    }

    // TODO does using Rgba<f32> with Vec<f32> automagically un-flatten the vector??
    /// Read this OpenEXR file as an `RgbF32Buffer`, discarding alpha.
    /// Any calls to `color_type` or `total_bytes`
    /// might not agree with the result of this function.
    // TODO progress? load rect?
    pub fn read_as_rgb_image(mut self) -> ImageResult<RgbF32Buffer> {
        let (width, height) = self.dimensions();

        self.alpha_requested = false;
        let buffer: Vec<f32> = decoder_to_vec(self)?;

        ImageBuffer::from_raw(width, height, buffer)

            // this should be the only reason for the "from raw" call to fail,
            // even though such a large allocation would probably cause an error much earlier
            .ok_or_else(|| ImageError::Limits(LimitError::from_kind(LimitErrorKind::InsufficientMemory)))
    }
}


impl<'a, R: 'a + BufRead + Seek> ImageDecoder<'a> for ExrDecoder<R> {
    type Reader = Cursor<Vec<u8>>;

    fn dimensions(&self) -> (u32, u32) {
        let size = self.selected_exr_header().layer_size;
        (size.width() as u32, size.height() as u32)
    }

    fn original_color_type(&self) -> ExtendedColorType {
        if self.alpha_present_in_file { ExtendedColorType::Rgba32F } else { ExtendedColorType::Rgb32F }
    }

    fn color_type(&self) -> ColorType {
        // TODO adapt to actual exr color type
        // let _channels = &self.exr_reader.headers()[self.header_index].channels;
        if self.alpha_requested { ColorType::Rgba32F } else { ColorType::Rgb32F }
    }

    /// Use `read_image` if possible,
    /// as this method creates a whole new buffer to contain the entire image.
    fn into_reader(self) -> ImageResult<Self::Reader> {
        Ok(Cursor::new(decoder_to_vec(self)?)) // TODO no vec?
    }

    fn scanline_bytes(&self) -> u64 {
        // we cannot always read individual scan lines for every file,
        // as the tiles or lines in the file could be in random or reversed order.
        // therefore we currently read all lines at once
        // Todo: optimize for specific exr.line_order?
       self.total_bytes()
    }

    // reads with or without alpha, depending on `self.should_include_alpha`
    fn read_image_with_progress<F: Fn(Progress)>(self, target: &mut [u8], progress_callback: F) -> ImageResult<()> {
        let blocks_in_header = self.selected_exr_header().chunk_count as u64;
        let channel_count = if self.alpha_requested { 4 } else { 3 };

        let result = read()
            .no_deep_data().largest_resolution_level()
            .rgba_channels(
                |size, _channels| {
                    // TODO debug_assert_eq!(self.header_index, header_index);
                    (size, vec![0_f32; size.area() * channel_count])
                },

                move |(size, buffer), position, (r,g,b,a_or_1): (f32,f32,f32,f32)| {
                    let first_f32_index = position.flat_index_for_size(*size);

                    buffer[first_f32_index * channel_count .. (first_f32_index + 1) * channel_count]
                        .copy_from_slice(&[r, g, b, a_or_1][0 .. channel_count]);

                    // TODO white point chromaticities + srgb/linear conversion?
                }
            )
            .first_valid_layer() // TODO select exact layer by self.header_index?
            .all_attributes()
            .on_progress(|progress| {
                progress_callback(Progress::new(
                    (progress*blocks_in_header as f64) as u64, blocks_in_header) // TODO precision errors?
                );
            })
            .from_chunks(self.exr_reader).map_err(to_image_err)?;

        // TODO this copy is strictly not necessary, but the exr api is a little too simple for reading into a borrowed target slice
        target.copy_from_slice(bytemuck::cast_slice(result.layer_data.channel_data.pixels.1.as_slice()));
        Ok(())
    }
}




/// Write a raw byte buffer of pixels,
/// panicking if the buffer is not aligned to `f32`
/// or if it has an invalid length.
///
/// Assumes the writer is buffered. In most cases,
/// you should wrap your writer in a `BufWriter` for best performance.
pub fn write_buffer(
    mut buffered_write: impl Write/* + Seek*/, bytes: &[u8],
    width: u32, height: u32, color_type: ColorType
) -> ImageResult<()>
{
    let width = width as usize;
    let height = height as usize;

    let mut seekable_write = Cursor::new(Vec::with_capacity(width*height*3*4)); // TODO remove

    match color_type {
        ColorType::Rgb32F => {
            let pixels: &[f32] = bytemuck::try_cast_slice(bytes)
                .expect("image byte buffer must be aligned to f32");

            debug_assert_eq!(pixels.len(), width * height * 3, "invalid dimensions for byte buffer length");

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
                .to_buffered(&mut seekable_write).map_err(to_image_err)?; // TODO BufWrite::new()?
        }

        ColorType::Rgba32F => {
            let pixels: &[f32] = bytemuck::try_cast_slice(bytes)
                .expect("image byte buffer must be aligned to f32");

            debug_assert_eq!(pixels.len(), width * height * 4, "invalid dimensions for byte buffer length");

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
                .to_buffered(&mut seekable_write).map_err(to_image_err)?; // TODO BufWrite::new()?
        }

        unsupported_color_type => return Err(ImageError::Encoding(EncodingError::new(
            ImageFormatHint::Exact(ImageFormat::Exr),
            format!("color type {:?} not yet supported", unsupported_color_type)
        )))
    }

    buffered_write.write_all(seekable_write.into_inner().as_slice())?;
    Ok(())
}


// TODO is this struct and trait actually used anywhere?
#[derive(Debug)]
pub struct Encoder<W> (W);
impl<W> Encoder<W> { pub fn new(write: W) -> Self {Self(write)} }
impl<W> ImageEncoder for Encoder<W> where W: Write /*+ Seek*/ {
    fn write_image(self, buf: &[u8], width: u32, height: u32, color_type: ColorType) -> ImageResult<()> {
        write_buffer(self.0, buf, width, height, color_type)
    }
}

fn to_image_err(exr_error: Error) -> ImageError {
    ImageError::Decoding(DecodingError::new(
        ImageFormatHint::Exact(ImageFormat::Exr),
        exr_error.to_string()
    ))
}



#[cfg(test)]
mod test {
    use super::*;
    use std::path::PathBuf;

    const BASE_PATH: &[&str] = &[".", "tests", "images", "exr"];

    #[test]
    fn compare_exr_hdr() {
        assert!(cfg!(feature = "hdr"), "to run all the openexr tests, activate the hdr feature flag");

        #[cfg(feature = "hdr")]
        {
            let folder = BASE_PATH.iter().collect::<PathBuf>();
            let reference_path = folder.clone().join("overexposed gradient.hdr");
            let exr_path = folder.clone().join("overexposed gradient - data window equals display window.exr");

            let hdr: Vec<Rgb<f32>> = crate::codecs::hdr::HdrDecoder::new(
                std::io::BufReader::new(std::fs::File::open(&reference_path).unwrap())
            ).unwrap().read_image_hdr().unwrap();

            let exr_pixels: RgbF32Buffer = read_as_rgb_image_from_file(exr_path).unwrap();

            // TODO let hdr = ImageBuffer::from_raw(___)
            assert_eq!(exr_pixels.dimensions().0 * exr_pixels.dimensions().1, hdr.len() as u32);

            for (expected, found) in hdr.iter().zip(exr_pixels.pixels()){
                for (expected, found) in expected.0.iter().zip(found.0.iter()) {
                    // the large tolerance seems to be caused by
                    // the RGBE u8x4 pixel quantization of the hdr image format
                    assert!((expected - found).abs() < 0.1, "expected {}, found {}", expected, found);
                }
            }
        }
    }

    #[test]
    fn roundtrip_rgba() {
        let mut next_random = vec![ 1.0, 0.0, -1.0, -3.14, 27.0, 11.0, 31.0 ].into_iter().cycle();
        let mut next_random = move || next_random.next().unwrap();

        let generated_image: RgbaF32Buffer = ImageBuffer::from_fn(9, 31, |_x, _y|{
            Rgba([next_random(), next_random(), next_random(), next_random()])
        });

        let mut bytes = vec![];
        write_rgba_image(Cursor::new(&mut bytes), &generated_image).unwrap();
        let decoded_image = read_as_rgba_image(Cursor::new(bytes)).unwrap();

        debug_assert_eq!(generated_image, decoded_image);
    }

    #[test]
    fn roundtrip_rgb() {
        let mut next_random = vec![ 1.0, 0.0, -1.0, -3.14, 27.0, 11.0, 31.0 ].into_iter().cycle();
        let mut next_random = move || next_random.next().unwrap();

        let generated_image: RgbF32Buffer = ImageBuffer::from_fn(9, 31, |_x, _y|{
            Rgb([next_random(), next_random(), next_random()])
        });

        let mut bytes = vec![];
        write_rgb_image(Cursor::new(&mut bytes), &generated_image).unwrap();
        let decoded_image = read_as_rgb_image(Cursor::new(bytes)).unwrap();

        debug_assert_eq!(generated_image, decoded_image);
    }

    #[test]
    fn compare_rgba_rgb() {
        let exr_path = BASE_PATH.iter().collect::<PathBuf>()
            .join("overexposed gradient - data window equals display window.exr");

        let rgb: RgbF32Buffer = read_as_rgb_image_from_file(&exr_path).unwrap();
        let rgba: RgbaF32Buffer = read_as_rgba_image_from_file(&exr_path).unwrap();

        assert_eq!(rgba.dimensions(), rgb.dimensions());

        for (Rgb(rgb), Rgba(rgba)) in rgb.pixels().zip(rgba.pixels()) {
            assert_eq!(rgb, &rgba[..3]);
        }
    }
}
