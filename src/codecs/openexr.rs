//! Decoding of OpenEXR (.exr) Images
//!
//! OpenEXR is an image format that is widely used, especially in VFX,
//! because it supports lossless and lossy compression for float data.
//!
//! This decoder only supports RGB and RGBA images.
//! If an image does not contain alpha information,
//! it is defaulted to `1.0` (no transparency).
//!
//! # Related Links
//! * <https://www.openexr.com/documentation.html> - The OpenEXR reference.
//!
//!
//! Current limitations (July 2021):
//!     - only pixel type `Rgba32F` and `Rgba16F` are supported
//!     - only non-deep rgb/rgba files supported, no conversion from/to YCbCr or similar
//!     - only the first non-deep rgb layer is used
//!     - only the largest mip map level is used
//!     - pixels outside display window are lost
//!     - meta data is lost
//!     - dwaa/dwab compressed images not supported yet by the exr library
//!     - (chroma) subsampling not supported yet by the exr library

extern crate exr;
use exr::prelude::*;

use crate::{ImageDecoder, ImageResult, ColorType, Progress, ImageError, ImageFormat, ImageBuffer, Rgba, Rgb, ImageEncoder, ExtendedColorType};
use std::io::{Write, Seek, BufRead, Cursor, BufReader};
use crate::error::{DecodingError, ImageFormatHint, LimitError, LimitErrorKind, EncodingError};
use crate::image::decoder_to_vec;
use std::path::Path;
use crate::buffer_::{Rgb32FImage, Rgba32FImage};
use std::convert::TryInto;



/// An OpenEXR decoder. Immediately reads the meta data from the file.
#[derive(Debug)]
pub struct OpenExrDecoder<R> {
    exr_reader: exr::block::reader::Reader<R>,

    // select a header that is rgb and not deep
    header_index: usize,

    // decode either rgb or rgba.
    // can be specified to include or discard alpha channels.
    // if none, the alpha channel will only be allocated where the file contains data for it.
    alpha_preference: Option<bool>,

    alpha_present_in_file: bool,
}


impl<R: BufRead + Seek> OpenExrDecoder<R> {


    /// Create a decoder. Consumes the first few bytes of the source to extract image dimensions.
    /// Assumes the reader is buffered. In most cases,
    /// you should wrap your reader in a `BufReader` for best performance.
    /// Loads an alpha channel if the file has alpha samples.
    /// Use `with_alpha_preference` if you want to load or not load alpha unconditionally.
    pub fn new(source: R) -> ImageResult<Self> {
        Self::with_alpha_preference(source, None)
    }

    /// Create a decoder. Consumes the first few bytes of the source to extract image dimensions.
    /// Assumes the reader is buffered. In most cases,
    /// you should wrap your reader in a `BufReader` for best performance.
    /// If alpha preference is specified, an alpha channel will
    /// always be present or always be not present in the returned image.
    /// If alpha preference is none, the alpha channel will only be returned if it is found in the file.
    pub fn with_alpha_preference(source: R, alpha_preference: Option<bool>) -> ImageResult<Self> {

        // read meta data, then wait for further instructions, keeping the file open and ready
        let exr_reader = exr::block::read(source, false).map_err(to_image_err)?;

        let header_index = exr_reader.headers().into_iter()
            .position(|header|{
                let has_rgb = ["R","G","B"].iter().all( // alpha will be optional
                    // check if r/g/b exists in the channels
                    |required| header.channels.list.iter()
                        .any(|chan| chan.name.eq(required)) // TODO use search("A") and eq_lowercase later when exrs supports it
                );

                // we currently dont support deep images, or images with other color spaces than rgb
                !header.deep && has_rgb
            })
            .ok_or_else(|| ImageError::Decoding(DecodingError::new(
                ImageFormatHint::Exact(ImageFormat::OpenExr),
                "image does not contain non-deep rgb channels"
            )))?;

        let has_alpha = exr_reader.headers()[header_index]
            .channels.list
            .iter()
            .any(|chan| chan.name.eq("A")); // TODO use search("A") and eq_lowercase later when exrs supports it

        Ok(Self {
            alpha_preference,
            exr_reader, header_index,
            alpha_present_in_file: has_alpha
        })
    }

    // does not leak exrs-specific meta data into public api, just does it for this module
    fn selected_exr_header(&self) -> &exr::meta::header::Header {
        &self.exr_reader.meta_data().headers[self.header_index]
    }
}


impl<'a, R: 'a + BufRead + Seek> ImageDecoder<'a> for OpenExrDecoder<R> {
    type Reader = Cursor<Vec<u8>>;

    fn dimensions(&self) -> (u32, u32) {
        let size = self.selected_exr_header().shared_attributes.display_window.size;
        (size.width() as u32, size.height() as u32)
    }

    fn color_type(&self) -> ColorType {
        let returns_alpha = self.alpha_preference.unwrap_or(self.alpha_present_in_file);
        if returns_alpha { ColorType::Rgba32F } else { ColorType::Rgb32F }
    }

    fn original_color_type(&self) -> ExtendedColorType {
        if self.alpha_present_in_file { ExtendedColorType::Rgba32F } else { ExtendedColorType::Rgb32F }
    }

    /// Use `read_image` instead if possible,
    /// as this method creates a whole new buffer just to contain the entire image.
    fn into_reader(self) -> ImageResult<Self::Reader> {
        Ok(Cursor::new(decoder_to_vec(self)?))
    }

    fn scanline_bytes(&self) -> u64 {
        // we cannot always read individual scan lines for every file,
        // as the tiles or lines in the file could be in random or reversed order.
        // therefore we currently read all lines at once
        // Todo: optimize for specific exr.line_order?
       self.total_bytes()
    }

    // reads with or without alpha, depending on `self.alpha_preference` and `self.alpha_present_in_file`
    fn read_image_with_progress<F: Fn(Progress)>(self, unaligned_bytes: &mut [u8], progress_callback: F) -> ImageResult<()> {
        let blocks_in_header = self.selected_exr_header().chunk_count as u64;
        let channel_count = self.color_type().channel_count() as usize;

        let display_window = self.selected_exr_header().shared_attributes.display_window;
        let data_window_offset = self.selected_exr_header().own_attributes.layer_position - display_window.position;

        {   // check whether the buffer is large enough for the dimensions of the file
            let (width, height) = self.dimensions();
            let bytes_per_pixel = self.color_type().bytes_per_pixel() as usize;
            let expected_byte_count = (width as usize).checked_mul(height as usize)
                .and_then(|size| size.checked_mul(bytes_per_pixel));

            // if the width and height does not match the length of the bytes, the arguments are invalid
            let has_invalid_size_or_overflowed = expected_byte_count
                .map(|expected_byte_count | unaligned_bytes.len() != expected_byte_count)

                // otherwise, size calculation overflowed, is bigger than memory,
                // therefore data is too small, so it is invalid.
                .unwrap_or(true);

            if has_invalid_size_or_overflowed {
                panic!("byte buffer not large enough for the specified dimensions and f32 pixels");
            }
        }

        let result = read()
            .no_deep_data().largest_resolution_level()
            .rgba_channels(
                move |_size, _channels| {
                    vec![0_f32; display_window.size.area() * channel_count]
                },

                move |buffer, index_in_data_window, (r,g,b,a_or_1): (f32,f32,f32,f32)| {
                    let index_in_display_window = index_in_data_window.to_i32() + data_window_offset;

                    // only keep pixels inside the data window
                    // TODO filter chunks based on this
                    if index_in_display_window.x() >= 0 && index_in_display_window.y() >= 0
                        && index_in_display_window.x() < display_window.size.width() as i32
                        && index_in_display_window.y() < display_window.size.height() as i32
                    {
                        let index_in_display_window = index_in_display_window.to_usize("index bug").unwrap();
                        let first_f32_index = index_in_display_window.flat_index_for_size(display_window.size);

                        buffer[first_f32_index * channel_count .. (first_f32_index + 1) * channel_count]
                            .copy_from_slice(&[r, g, b, a_or_1][0 .. channel_count]);

                        // TODO white point chromaticities + srgb/linear conversion?
                    }
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

        // this cast is safe and works with any alignment, as bytes are copied, and not f32 values.
        // note: buffer slice length is checked in the beginning of this function and will be correct at this point
        unaligned_bytes.copy_from_slice(bytemuck::cast_slice(result.layer_data.channel_data.pixels.as_slice()));
        Ok(())
    }
}




/// Write a raw byte buffer of pixels,
/// returning an Error if the buffer is not aligned to `f32`
/// or if it has an invalid length.
///
/// Assumes the writer is buffered. In most cases,
/// you should wrap your writer in a `BufWriter` for best performance.
// private. access via `OpenExrEncoder`
fn write_buffer(
    mut buffered_write: impl Write/* + Seek*/, unaligned_bytes: &[u8],
    width: u32, height: u32, color_type: ColorType
) -> ImageResult<()>
{
    let width = width as usize;
    let height = height as usize;

    {   // check whether the buffer is large enough for the specified dimensions
        let expected_byte_count = width.checked_mul(height)
            .and_then(|size| size.checked_mul(color_type.bytes_per_pixel() as usize));

        // if the width and height does not match the length of the bytes, the arguments are invalid
        let has_invalid_size_or_overflowed = expected_byte_count
            .map(|expected_byte_count | unaligned_bytes.len() < expected_byte_count)

            // otherwise, size calculation overflowed, is bigger than memory,
            // therefore data is too small, so it is invalid.
            .unwrap_or(true);

        if has_invalid_size_or_overflowed {
            return Err(ImageError::Encoding(EncodingError::new(
                ImageFormatHint::Exact(ImageFormat::OpenExr),
                "byte buffer not large enough for the specified dimensions and f32 pixels"
            )));
        }
    }

    let mut seekable_write = Cursor::new(Vec::with_capacity(width*height*3*4)); // TODO remove

    // bytes might be unaligned so we cannot cast the whole thing, instead lookup each f32 individually
    let lookup_f32 = move |f32_index: usize| {
        let f32_bytes_slice = &unaligned_bytes[f32_index * 4 .. (f32_index + 1) * 4];
        f32::from_ne_bytes(f32_bytes_slice.try_into().expect("indexing error"))
    };

    match color_type {
        ColorType::Rgb32F => {
            exr::prelude::Image // TODO compression method zip??
                ::from_channels(
                    (width, height),
                    SpecificChannels::rgb(|pixel: Vec2<usize>| {
                        let pixel_index = 3 * pixel.flat_index_for_size(Vec2(width, height));
                        (lookup_f32(pixel_index), lookup_f32(pixel_index+1), lookup_f32(pixel_index+2))
                    })
                )
                .write()
                // .on_progress(|progress| todo!())
                .to_buffered(&mut seekable_write).map_err(to_image_err)?;
        }

        ColorType::Rgba32F => {
            exr::prelude::Image // TODO compression method zip??
                ::from_channels(
                    (width, height),
                    SpecificChannels::rgba(|pixel: Vec2<usize>| {
                        let pixel_index = 4 * pixel.flat_index_for_size(Vec2(width, height));
                        (
                            lookup_f32(pixel_index), lookup_f32(pixel_index+1),
                            lookup_f32(pixel_index+2), lookup_f32(pixel_index+3)
                        )
                    })
                )
                .write()
                // .on_progress(|progress| todo!())
                .to_buffered(&mut seekable_write).map_err(to_image_err)?;
        }

        // TODO other color types and channel types
        unsupported_color_type => return Err(ImageError::Encoding(EncodingError::new(
            ImageFormatHint::Exact(ImageFormat::OpenExr),
            format!("color type {:?} not yet supported", unsupported_color_type)
        )))
    }

    buffered_write.write_all(seekable_write.into_inner().as_slice())?;
    Ok(())
}


// TODO is this struct and trait actually used anywhere?
/// A thin wrapper that implements `ImageEncoder` for OpenEXR images. Will behave like `image::codecs::openexr::write_buffer`.
#[derive(Debug)]
pub struct OpenExrEncoder<W> (W);

impl<W> OpenExrEncoder<W> {

    /// Create an `ImageEncoder`. Does not write anything yet. Writing later will behave like `image::codecs::openexr::write_buffer`.
    // use constructor, not public field, for future backwards-compatibility
    pub fn new(write: W) -> Self { Self(write) }
}

impl<W> ImageEncoder for OpenExrEncoder<W> where W: Write /*+ Seek*/ {

    /// Writes the complete image.
    ///
    /// Returns an Error if the buffer is not aligned to `f32`
    /// or if it has an invalid length.
    ///
    /// Assumes the writer is buffered. In most cases,
    /// you should wrap your writer in a `BufWriter` for best performance.
    fn write_image(self, buf: &[u8], width: u32, height: u32, color_type: ColorType) -> ImageResult<()> {
        write_buffer(self.0, buf, width, height, color_type)
    }
}

fn to_image_err(exr_error: Error) -> ImageError {
    ImageError::Decoding(DecodingError::new(
        ImageFormatHint::Exact(ImageFormat::OpenExr),
        exr_error.to_string()
    ))
}



#[cfg(test)]
mod test {
    use super::*;
    use std::path::PathBuf;

    const BASE_PATH: &[&str] = &[".", "tests", "images", "exr"];



    /// Write an `Rgb32FImage`.
    /// Assumes the writer is buffered. In most cases,
    /// you should wrap your writer in a `BufWriter` for best performance.
    fn write_rgb_image(write: impl Write/* + Seek*/, image: &Rgb32FImage) -> ImageResult<()> {
        write_buffer(
            write,
            bytemuck::cast_slice(image.as_raw().as_slice()),
            image.width(), image.height(),
            ColorType::Rgb32F
        )
    }

    /// Write an `Rgba32FImage`.
    /// Assumes the writer is buffered. In most cases,
    /// you should wrap your writer in a `BufWriter` for best performance.
    fn write_rgba_image(write: impl Write/* + Seek*/, image: &Rgba32FImage) -> ImageResult<()> {
        write_buffer(
            write,
            bytemuck::cast_slice(image.as_raw().as_slice()),
            image.width(), image.height(),
            ColorType::Rgba32F
        )
    }

    /// Read the file from the specified path into an `Rgba32FImage`.
    fn read_as_rgba_image_from_file(path: impl AsRef<Path>) -> ImageResult<Rgba32FImage> {
        read_as_rgba_image(BufReader::new(std::fs::File::open(path)?))
    }

    /// Read the file from the specified path into an `Rgb32FImage`.
    fn read_as_rgb_image_from_file(path: impl AsRef<Path>) -> ImageResult<Rgb32FImage> {
        read_as_rgb_image(BufReader::new(std::fs::File::open(path)?))
    }

    /// Read the file from the specified path into an `Rgb32FImage`.
    fn read_as_rgb_image(read: impl BufRead + Seek) -> ImageResult<Rgb32FImage> {
        let decoder = OpenExrDecoder::with_alpha_preference(read, Some(false))?;
        let (width, height) = decoder.dimensions();
        let buffer: Vec<f32> = decoder_to_vec(decoder)?;

        ImageBuffer::from_raw(width, height, buffer)

            // this should be the only reason for the "from raw" call to fail,
            // even though such a large allocation would probably cause an error much earlier
            .ok_or_else(|| ImageError::Limits(LimitError::from_kind(LimitErrorKind::InsufficientMemory)))
    }

    /// Read the file from the specified path into an `Rgba32FImage`.
    fn read_as_rgba_image(read: impl BufRead + Seek) -> ImageResult<Rgba32FImage> {
        let decoder = OpenExrDecoder::with_alpha_preference(read, Some(true))?;
        let (width, height) = decoder.dimensions();
        let buffer: Vec<f32> = decoder_to_vec(decoder)?;

        ImageBuffer::from_raw(width, height, buffer)

            // this should be the only reason for the "from raw" call to fail,
            // even though such a large allocation would probably cause an error much earlier
            .ok_or_else(|| ImageError::Limits(LimitError::from_kind(LimitErrorKind::InsufficientMemory)))
    }

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

            let exr_pixels: Rgb32FImage = read_as_rgb_image_from_file(exr_path).unwrap();
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

        let generated_image: Rgba32FImage = ImageBuffer::from_fn(9, 31, |_x, _y|{
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

        let generated_image: Rgb32FImage = ImageBuffer::from_fn(9, 31, |_x, _y|{
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

        let rgb: Rgb32FImage = read_as_rgb_image_from_file(&exr_path).unwrap();
        let rgba: Rgba32FImage = read_as_rgba_image_from_file(&exr_path).unwrap();

        assert_eq!(rgba.dimensions(), rgb.dimensions());

        for (Rgb(rgb), Rgba(rgba)) in rgb.pixels().zip(rgba.pixels()) {
            assert_eq!(rgb, &rgba[..3]);
        }
    }

    #[test]
    fn compare_cropped() {
        // like in photoshop, exr images may have layers placed anywhere in a canvas.
        // we don't want to load the pixels from the layer, but we want to load the pixels from the canvas.
        // a layer might be smaller than the canvas, in that case the canvas should be transparent black
        // where no layer was covering it. a layer might also be larger than the canvas,
        // these pixels should be discarded.
        //
        // in this test we want to make sure that an
        // auto-cropped image will be reproduced to the original.

        let exr_path = BASE_PATH.iter().collect::<PathBuf>();
        let original = exr_path.clone().join("cropping - uncropped original.exr");
        let cropped = exr_path.clone().join("cropping - data window differs display window.exr");

        // smoke-check that the exr files are actually not the same
        {
            let original_exr = read_first_flat_layer_from_file(&original).unwrap();
            let cropped_exr = read_first_flat_layer_from_file(&cropped).unwrap();
            assert_eq!(original_exr.attributes.display_window, cropped_exr.attributes.display_window);
            assert_ne!(original_exr.layer_data.attributes.layer_position, cropped_exr.layer_data.attributes.layer_position);
            assert_ne!(original_exr.layer_data.size, cropped_exr.layer_data.size);
        }

        // check that they result in the same image
        let original: Rgba32FImage = read_as_rgba_image_from_file(&original).unwrap();
        let cropped: Rgba32FImage = read_as_rgba_image_from_file(&cropped).unwrap();
        assert_eq!(original.dimensions(), cropped.dimensions());

        // the following is not a simple assert_eq, as in case of an error,
        // the whole image would be printed to the console, which takes forever
        assert!(original.pixels().zip(cropped.pixels()).all(|(a,b)| a == b));
    }
}
