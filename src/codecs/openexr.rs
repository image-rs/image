//! Decoding of OpenEXR (.exr) Images
//!
//! OpenEXR is an image format that is widely used, especially in VFX,
//! because it supports lossless and lossy compression for float data.
//!
//! This decoder supports RGB and RGBA images, as well as single-channel
//! luma (`Y`) images, which are decoded as `Luma<f32>`.
//! If an RGB image does not contain alpha information,
//! it is defaulted to `1.0` (no transparency).
//! When a file contains both RGB and luma channels, the RGB channels are preferred.
//! Luma images that additionally carry an alpha channel (`Y` + `A`) are not yet
//! supported and are rejected with an unsupported-color error, rather than
//! silently dropping the alpha plane.
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
use exr::prelude::*;

use crate::error::{
    DecodingError, ImageFormatHint, ParameterError, ParameterErrorKind, UnsupportedError,
    UnsupportedErrorKind,
};
use crate::io::{DecodedImageAttributes, DecoderPreparedImage};
use crate::{
    ColorType, ExtendedColorType, ImageDecoder, ImageEncoder, ImageError, ImageFormat, ImageResult,
};

use std::io::{BufRead, Seek, Write};

/// The color decision made while parsing the selected EXR header.
///
/// This is the "parse, don't validate" result of inspecting the channels of the
/// chosen header exactly once. Instead of storing loose booleans and re-deriving
/// the channel logic in every consumer (which risks the header being interpreted
/// one way during `prepare_image` and a different way during `read_image`), we
/// compute this enum a single time in [`OpenExrDecoder::with_alpha_preference`]
/// and let the rest of the decoder consume the already-made decision.
///
/// The index of each matched channel within the header is retained, so that
/// future work supporting non-standard channel names or alternate layouts can
/// build on the parsed positions instead of scanning the channel list again.
#[derive(Debug, Clone, Copy)]
enum ExrColorLayout {
    /// Red, green and blue channels, without alpha. Decoded as `Rgb32F`.
    Rgb { r: usize, g: usize, b: usize },

    /// Red, green, blue and an alpha channel. Decoded as `Rgba32F`.
    Rgba {
        r: usize,
        g: usize,
        b: usize,
        a: usize,
    },

    /// A single luminance (`Y`) channel, as recommended by the OpenEXR
    /// specification. Decoded as `L32F`; any alpha preference is ignored.
    Luma { y: usize },
}

impl ExrColorLayout {
    /// Whether the selected header is a pure luma (`Y`) image.
    fn is_luma(self) -> bool {
        matches!(self, ExrColorLayout::Luma { .. })
    }

    /// Whether the file actually stores an alpha channel for this layout.
    /// Note this is independent of the caller's alpha *preference*.
    fn file_contains_alpha(self) -> bool {
        matches!(self, ExrColorLayout::Rgba { .. })
    }

    /// The color type the file's pixels are stored as, ignoring alpha
    /// preference. Used to report the `original_color_type` of the source.
    fn original_color_type(self) -> ExtendedColorType {
        match self {
            ExrColorLayout::Luma { .. } => ExtendedColorType::L32F,
            ExrColorLayout::Rgb { .. } => ExtendedColorType::Rgb32F,
            ExrColorLayout::Rgba { .. } => ExtendedColorType::Rgba32F,
        }
    }
}

/// An OpenEXR decoder. Immediately reads the meta data from the file.
#[derive(Debug)]
pub struct OpenExrDecoder<R> {
    exr_reader: Option<exr::block::reader::Reader<R>>,

    // select a header that is rgb or luma and not deep
    header_index: usize,

    // the color layout decided once while parsing the selected header.
    // consumers read this decision instead of re-inspecting channels.
    color_layout: ExrColorLayout,

    // decode either rgb or rgba.
    // can be specified to include or discard alpha channels.
    // if none, the alpha channel will only be allocated where the file contains data for it.
    // ignored for luma images.
    alpha_preference: Option<bool>,
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

        use exr::meta::header::Header;

        // look up a channel by exact name, retaining its index within the header
        let channel_index =
            |header: &Header, name: &str| header.channels.find_index_of_channel(&Text::from(name));

        // classify a non-deep header as RGB or RGBA, retaining the channel indices.
        // returns `None` for deep headers or headers that lack a full R+G+B set.
        let rgb_layout = |header: &Header| -> Option<ExrColorLayout> {
            if header.deep {
                return None;
            }

            let r = channel_index(header, "R")?;
            let g = channel_index(header, "G")?;
            let b = channel_index(header, "B")?;

            Some(match channel_index(header, "A") {
                Some(a) => ExrColorLayout::Rgba { r, g, b, a },
                None => ExrColorLayout::Rgb { r, g, b },
            })
        };

        // parse the color layout exactly once, preferring an RGB(A) header over a
        // pure luma header (an image may contain both R+G+B and Y channels).
        let headers = exr_reader.headers();

        let (header_index, color_layout) = if let Some(selection) = headers
            .iter()
            .enumerate()
            .find_map(|(index, header)| rgb_layout(header).map(|layout| (index, layout)))
        {
            selection
        } else if let Some((header_index, y)) = headers.iter().enumerate().find_map(|(index, h)| {
            (!h.deep)
                .then(|| channel_index(h, "Y"))
                .flatten()
                .map(|y| (index, y))
        }) {
            // A luma image that also carries an alpha channel (`Y` + `A`) is not
            // supported yet. Rather than silently dropping the alpha plane - which
            // would become a behavior change once `LumaA<f32>` decoding is added -
            // reject it explicitly so that support can be added additively.
            if channel_index(&headers[header_index], "A").is_some() {
                return Err(ImageError::Unsupported(
                    UnsupportedError::from_format_and_kind(
                        ImageFormat::OpenExr.into(),
                        UnsupportedErrorKind::Color(ExtendedColorType::La32F),
                    ),
                ));
            }

            (header_index, ExrColorLayout::Luma { y })
        } else {
            return Err(ImageError::Decoding(DecodingError::new(
                ImageFormatHint::Exact(ImageFormat::OpenExr),
                "image does not contain non-deep rgb or luma channels",
            )));
        };

        Ok(Self {
            alpha_preference,
            exr_reader: Some(exr_reader),
            header_index,
            color_layout,
        })
    }
}

impl<R: BufRead + Seek> ImageDecoder for OpenExrDecoder<R> {
    fn prepare_image(&mut self) -> ImageResult<DecoderPreparedImage> {
        let (width, height) = match &self.exr_reader {
            Some(exr) => {
                let header = &exr.meta_data().headers[self.header_index];
                let size = header.shared_attributes.display_window.size;
                (size.width() as u32, size.height() as u32)
            }
            // We have already ended..
            None => {
                return Err(ImageError::Parameter(ParameterError::from_kind(
                    ParameterErrorKind::NoMoreData,
                )))
            }
        };

        let color = if self.color_layout.is_luma() {
            ColorType::L32F
        } else {
            // for rgb(a) images the caller may override whether alpha is kept;
            // otherwise fall back to whatever the file itself stores.
            let returns_alpha = self
                .alpha_preference
                .unwrap_or(self.color_layout.file_contains_alpha());
            if returns_alpha {
                ColorType::Rgba32F
            } else {
                ColorType::Rgb32F
            }
        };

        // We may have discarded the alpha channel.
        Ok(DecoderPreparedImage::new(width, height, color))
    }

    // reads luma, rgb or rgba according to `self.color_layout` and, for rgb(a), `self.alpha_preference`
    fn read_image(&mut self, unaligned_bytes: &mut [u8]) -> ImageResult<DecodedImageAttributes> {
        let layout = self.prepare_image()?;
        let (width, height) = layout.layout.dimensions();

        // the color the file actually stores, independent of any alpha preference
        let original = self.color_layout.original_color_type();

        let reader = self.exr_reader.take().ok_or_else(|| {
            ImageError::Parameter(ParameterError::from_kind(ParameterErrorKind::NoMoreData))
        })?;

        let _blocks_in_header = reader.headers()[self.header_index].chunk_count as u64;
        let channel_count = layout.layout.color.channel_count() as usize;

        let display_window = reader.headers()[self.header_index]
            .shared_attributes
            .display_window;

        let data_window_offset = reader.headers()[self.header_index]
            .own_attributes
            .layer_position
            - display_window.position;

        {
            // check whether the buffer is large enough for the dimensions of the file
            let bytes_per_pixel = usize::from(layout.layout.color.bytes_per_pixel());
            let expected_byte_count = (width as usize)
                .checked_mul(height as usize)
                .and_then(|size| size.checked_mul(bytes_per_pixel));

            // if the width and height does not match the length of the bytes, the arguments are invalid
            let has_invalid_size_or_overflowed = expected_byte_count
                .map(|expected_byte_count| unaligned_bytes.len() != expected_byte_count)
                // otherwise, size calculation overflowed, is bigger than memory,
                // therefore data is too small, so it is invalid.
                .unwrap_or(true);

            assert!(
                !has_invalid_size_or_overflowed,
                "byte buffer not large enough for the specified dimensions and f32 pixels"
            );
        }

        // maps an index from the data window into a flat index in the display window,
        // returning `None` for pixels that lie outside the display window.
        let display_window_index = move |index_in_data_window: Vec2<usize>| {
            let index_in_display_window = index_in_data_window.to_i32() + data_window_offset;

            // only keep pixels inside the data window
            // TODO filter chunks based on this
            if index_in_display_window.x() >= 0
                && index_in_display_window.y() >= 0
                && index_in_display_window.x() < display_window.size.width() as i32
                && index_in_display_window.y() < display_window.size.height() as i32
            {
                let index_in_display_window =
                    index_in_display_window.to_usize("index bug").unwrap();
                Some(index_in_display_window.flat_index_for_size(display_window.size))
            } else {
                None
            }
        };

        // Look up a parsed channel index and return the name the exr reader selects
        // by. Decoding reads exactly the channels chosen during parsing, rather than
        // re-hardcoding channel names here (which would risk interpreting the header
        // differently than the parser did).
        let channel_name = |index: usize| {
            reader.headers()[self.header_index].channels.list[index]
                .name
                .clone()
        };

        let float_pixels: Vec<f32> = if let ExrColorLayout::Luma { y } = self.color_layout {
            // single `Y` channel, as recommended by the OpenEXR specification
            let y_name = channel_name(y);

            let result = read()
                .no_deep_data()
                .largest_resolution_level()
                .specific_channels()
                .required(y_name)
                .collect_pixels(
                    move |_size, _channels| vec![0_f32; display_window.size.area()],
                    move |buffer, index_in_data_window, (luma,): (f32,)| {
                        if let Some(first_f32_index) = display_window_index(index_in_data_window) {
                            buffer[first_f32_index] = luma;
                        }
                    },
                )
                .first_valid_layer() // TODO select exact layer by self.header_index?
                .all_attributes()
                .from_chunks(reader)
                .map_err(to_image_err)?;

            result.layer_data.channel_data.pixels
        } else {
            // rgb or rgba: read red, green, blue and an optional alpha channel,
            // driven by the names resolved from the parsed layout. This mirrors the
            // reader's built-in `rgba_channels`. For an `Rgb` layout (no stored alpha)
            // we probe the conventional `A` name, which defaults to `1.0` when absent.
            let (r, g, b) = match self.color_layout {
                ExrColorLayout::Rgb { r, g, b } | ExrColorLayout::Rgba { r, g, b, .. } => (r, g, b),
                ExrColorLayout::Luma { .. } => unreachable!("luma is handled above"),
            };
            let a_name = match self.color_layout {
                ExrColorLayout::Rgba { a, .. } => channel_name(a),
                _ => Text::from("A"),
            };
            let (r_name, g_name, b_name) = (channel_name(r), channel_name(g), channel_name(b));

            let result = read()
                .no_deep_data()
                .largest_resolution_level()
                .specific_channels()
                .required(r_name)
                .required(g_name)
                .required(b_name)
                .optional(a_name, 1.0_f32)
                .collect_pixels(
                    move |_size, _channels| vec![0_f32; display_window.size.area() * channel_count],
                    move |buffer, index_in_data_window, (r, g, b, a_or_1): (f32, f32, f32, f32)| {
                        if let Some(first_f32_index) = display_window_index(index_in_data_window) {
                            buffer[first_f32_index * channel_count
                                ..(first_f32_index + 1) * channel_count]
                                .copy_from_slice(&[r, g, b, a_or_1][0..channel_count]);

                            // TODO white point chromaticities + srgb/linear conversion?
                        }
                    },
                )
                .first_valid_layer() // TODO select exact layer by self.header_index?
                .all_attributes()
                .from_chunks(reader)
                .map_err(to_image_err)?;

            result.layer_data.channel_data.pixels
        };

        // TODO this copy is strictly not necessary, but the exr api is a little too simple for reading into a borrowed target slice

        // this cast is safe and works with any alignment, as bytes are copied, and not f32 values.
        // note: buffer slice length is checked in the beginning of this function and will be correct at this point
        unaligned_bytes.copy_from_slice(bytemuck::cast_slice(float_pixels.as_slice()));

        Ok(DecodedImageAttributes {
            original_color_type: Some(original),
            ..DecodedImageAttributes::default()
        })
    }
}

/// Write a raw byte buffer of pixels,
/// returning an Error if it has an invalid length.
///
/// Assumes the writer is buffered. In most cases,
/// you should wrap your writer in a `BufWriter` for best performance.
// private. access via `OpenExrEncoder`
fn write_buffer(
    mut buffered_write: impl Write + Seek,
    unaligned_bytes: &[u8],
    width: u32,
    height: u32,
    color_type: ExtendedColorType,
) -> ImageResult<()> {
    let width = width as usize;
    let height = height as usize;
    let bytes_per_pixel = color_type.bits_per_pixel() as usize / 8;

    match color_type {
        ExtendedColorType::Rgb32F => {
            Image // TODO compression method zip??
                ::from_channels(
                (width, height),
                SpecificChannels::rgb(|pixel: Vec2<usize>| {
                    let pixel_index = pixel.flat_index_for_size(Vec2(width, height));
                    let start_byte = pixel_index * bytes_per_pixel;

                    let [r, g, b]: [f32; 3] = bytemuck::pod_read_unaligned(
                        &unaligned_bytes[start_byte..start_byte + bytes_per_pixel],
                    );

                    (r, g, b)
                }),
            )
            .write()
            // .on_progress(|progress| todo!())
            .to_buffered(&mut buffered_write)
            .map_err(to_image_err)?;
        }

        ExtendedColorType::Rgba32F => {
            Image // TODO compression method zip??
                ::from_channels(
                (width, height),
                SpecificChannels::rgba(|pixel: Vec2<usize>| {
                    let pixel_index = pixel.flat_index_for_size(Vec2(width, height));
                    let start_byte = pixel_index * bytes_per_pixel;

                    let [r, g, b, a]: [f32; 4] = bytemuck::pod_read_unaligned(
                        &unaligned_bytes[start_byte..start_byte + bytes_per_pixel],
                    );

                    (r, g, b, a)
                }),
            )
            .write()
            // .on_progress(|progress| todo!())
            .to_buffered(&mut buffered_write)
            .map_err(to_image_err)?;
        }

        ExtendedColorType::L32F => {
            // luma is stored in a single `Y` channel, as recommended by the OpenEXR specification
            Image // TODO compression method zip??
                ::from_channels(
                (width, height),
                SpecificChannels::build()
                    .with_channel::<f32>("Y")
                    .with_pixel_fn(|pixel: Vec2<usize>| {
                        let pixel_index = pixel.flat_index_for_size(Vec2(width, height));
                        let start_byte = pixel_index * bytes_per_pixel;

                        let [luma]: [f32; 1] = bytemuck::pod_read_unaligned(
                            &unaligned_bytes[start_byte..start_byte + bytes_per_pixel],
                        );

                        (luma,)
                    }),
            )
            .write()
            // .on_progress(|progress| todo!())
            .to_buffered(&mut buffered_write)
            .map_err(to_image_err)?;
        }

        // TODO other color types and channel types
        unsupported_color_type => {
            return Err(ImageError::Unsupported(
                UnsupportedError::from_format_and_kind(
                    ImageFormat::OpenExr.into(),
                    UnsupportedErrorKind::Color(unsupported_color_type),
                ),
            ))
        }
    }

    Ok(())
}

// TODO is this struct and trait actually used anywhere?
/// A thin wrapper that implements `ImageEncoder` for OpenEXR images. Will behave like `image::codecs::openexr::write_buffer`.
#[derive(Debug)]
pub struct OpenExrEncoder<W>(W);

impl<W> OpenExrEncoder<W> {
    /// Create an `ImageEncoder`. Does not write anything yet. Writing later will behave like `image::codecs::openexr::write_buffer`.
    // use constructor, not public field, for future backwards-compatibility
    pub fn new(write: W) -> Self {
        Self(write)
    }
}

impl<W> ImageEncoder for OpenExrEncoder<W>
where
    W: Write + Seek,
{
    /// Writes the complete image.
    ///
    /// Assumes the writer is buffered. In most cases, you should wrap your writer in a `BufWriter`
    /// for best performance.
    #[track_caller]
    fn write_image(
        self,
        buf: &[u8],
        width: u32,
        height: u32,
        color_type: ExtendedColorType,
    ) -> ImageResult<()> {
        let expected_buffer_len = color_type.buffer_size(width, height);
        assert_eq!(
            expected_buffer_len,
            buf.len() as u64,
            "Invalid buffer length: expected {expected_buffer_len} got {} for {width}x{height} image",
            buf.len(),
        );

        write_buffer(self.0, buf, width, height, color_type)
    }
}

fn to_image_err(exr_error: Error) -> ImageError {
    ImageError::Decoding(DecodingError::new(
        ImageFormatHint::Exact(ImageFormat::OpenExr),
        exr_error.to_string(),
    ))
}

#[cfg(test)]
mod test {
    use super::*;

    use std::fs::File;
    use std::io::{BufReader, Cursor};
    use std::path::{Path, PathBuf};

    use crate::error::{LimitError, LimitErrorKind};
    use crate::images::buffer::{Rgb32FImage, Rgba32FImage};
    use crate::io::free_functions::decoder_to_vec;
    use crate::{DynamicImage, ImageBuffer, Luma, Rgb, Rgba};

    type Luma32FImage = ImageBuffer<Luma<f32>, Vec<f32>>;

    const BASE_PATH: &[&str] = &[".", "tests", "images", "exr"];

    /// Write an `Rgb32FImage`.
    /// Assumes the writer is buffered. In most cases,
    /// you should wrap your writer in a `BufWriter` for best performance.
    fn write_rgb_image(write: impl Write + Seek, image: &Rgb32FImage) -> ImageResult<()> {
        write_buffer(
            write,
            bytemuck::cast_slice(image.subpixels()),
            image.width(),
            image.height(),
            ExtendedColorType::Rgb32F,
        )
    }

    /// Write an `Rgba32FImage`.
    /// Assumes the writer is buffered. In most cases,
    /// you should wrap your writer in a `BufWriter` for best performance.
    fn write_rgba_image(write: impl Write + Seek, image: &Rgba32FImage) -> ImageResult<()> {
        write_buffer(
            write,
            bytemuck::cast_slice(image.subpixels()),
            image.width(),
            image.height(),
            ExtendedColorType::Rgba32F,
        )
    }

    /// Write a `Luma32FImage`.
    /// Assumes the writer is buffered. In most cases,
    /// you should wrap your writer in a `BufWriter` for best performance.
    fn write_luma_image(write: impl Write + Seek, image: &Luma32FImage) -> ImageResult<()> {
        write_buffer(
            write,
            bytemuck::cast_slice(image.as_raw()),
            image.width(),
            image.height(),
            ExtendedColorType::L32F,
        )
    }

    /// Read the file into a `Luma32FImage`.
    fn read_as_luma_image(read: impl BufRead + Seek) -> ImageResult<Luma32FImage> {
        let mut decoder = OpenExrDecoder::new(read)?;
        let (width, height) = decoder.prepare_image()?.layout.dimensions();
        let (buffer, _): (Vec<f32>, _) = decoder_to_vec(&mut decoder)?;

        ImageBuffer::from_raw(width, height, buffer).ok_or_else(|| {
            ImageError::Limits(LimitError::from_kind(LimitErrorKind::InsufficientMemory))
        })
    }

    /// Read the file from the specified path into an `Rgba32FImage`.
    fn read_as_rgba_image_from_file(path: impl AsRef<Path>) -> ImageResult<Rgba32FImage> {
        read_as_rgba_image(BufReader::new(File::open(path)?))
    }

    /// Read the file from the specified path into an `Rgb32FImage`.
    fn read_as_rgb_image_from_file(path: impl AsRef<Path>) -> ImageResult<Rgb32FImage> {
        read_as_rgb_image(BufReader::new(File::open(path)?))
    }

    /// Read the file from the specified path into an `Rgb32FImage`.
    fn read_as_rgb_image(read: impl BufRead + Seek) -> ImageResult<Rgb32FImage> {
        let mut decoder = OpenExrDecoder::with_alpha_preference(read, Some(false))?;
        let (width, height) = decoder.prepare_image()?.layout.dimensions();
        let (buffer, _): (Vec<f32>, _) = decoder_to_vec(&mut decoder)?;

        ImageBuffer::from_raw(width, height, buffer)
            // this should be the only reason for the "from raw" call to fail,
            // even though such a large allocation would probably cause an error much earlier
            .ok_or_else(|| {
                ImageError::Limits(LimitError::from_kind(LimitErrorKind::InsufficientMemory))
            })
    }

    /// Read the file from the specified path into an `Rgba32FImage`.
    fn read_as_rgba_image(read: impl BufRead + Seek) -> ImageResult<Rgba32FImage> {
        let mut decoder = OpenExrDecoder::with_alpha_preference(read, Some(true))?;
        let (width, height) = decoder.prepare_image()?.layout.dimensions();
        let (buffer, _): (Vec<f32>, _) = decoder_to_vec(&mut decoder)?;

        ImageBuffer::from_raw(width, height, buffer)
            // this should be the only reason for the "from raw" call to fail,
            // even though such a large allocation would probably cause an error much earlier
            .ok_or_else(|| {
                ImageError::Limits(LimitError::from_kind(LimitErrorKind::InsufficientMemory))
            })
    }

    #[test]
    fn compare_exr_hdr() {
        if cfg!(not(feature = "hdr")) {
            eprintln!("warning: to run all the openexr tests, activate the hdr feature flag");
        }

        #[cfg(feature = "hdr")]
        {
            use crate::codecs::hdr::HdrDecoder;

            let folder = BASE_PATH.iter().collect::<PathBuf>();
            let reference_path = folder.join("overexposed gradient.hdr");
            let exr_path =
                folder.join("overexposed gradient - data window equals display window.exr");

            let hdr_decoder =
                HdrDecoder::new(BufReader::new(File::open(reference_path).unwrap())).unwrap();
            let hdr: Rgb32FImage = match DynamicImage::from_decoder(hdr_decoder).unwrap() {
                DynamicImage::ImageRgb32F(image) => image,
                _ => panic!("expected rgb32f image"),
            };

            let exr_pixels: Rgb32FImage = read_as_rgb_image_from_file(exr_path).unwrap();
            assert_eq!(exr_pixels.dimensions(), hdr.dimensions());

            for (expected, found) in hdr.pixels().iter().zip(exr_pixels.pixels().iter()) {
                for (expected, found) in expected.0.iter().zip(found.0.iter()) {
                    // the large tolerance seems to be caused by
                    // the RGBE u8x4 pixel quantization of the hdr image format
                    assert!(
                        (expected - found).abs() < 0.1,
                        "expected {expected}, found {found}"
                    );
                }
            }
        }
    }

    #[test]
    fn roundtrip_rgba() {
        let mut next_random = vec![1.0, 0.0, -1.0, -3.15, 27.0, 11.0, 31.0]
            .into_iter()
            .cycle();
        let mut next_random = move || next_random.next().unwrap();

        let generated_image: Rgba32FImage = ImageBuffer::from_fn(9, 31, |_x, _y| {
            Rgba([next_random(), next_random(), next_random(), next_random()])
        });

        let mut bytes = vec![];
        write_rgba_image(Cursor::new(&mut bytes), &generated_image).unwrap();
        let decoded_image = read_as_rgba_image(Cursor::new(bytes)).unwrap();

        debug_assert_eq!(generated_image, decoded_image);
    }

    #[test]
    fn roundtrip_rgb() {
        let mut next_random = vec![1.0, 0.0, -1.0, -3.15, 27.0, 11.0, 31.0]
            .into_iter()
            .cycle();
        let mut next_random = move || next_random.next().unwrap();

        let generated_image: Rgb32FImage = ImageBuffer::from_fn(9, 31, |_x, _y| {
            Rgb([next_random(), next_random(), next_random()])
        });

        let mut bytes = vec![];
        write_rgb_image(Cursor::new(&mut bytes), &generated_image).unwrap();
        let decoded_image = read_as_rgb_image(Cursor::new(bytes)).unwrap();

        debug_assert_eq!(generated_image, decoded_image);
    }

    #[test]
    fn roundtrip_luma() {
        let mut next_random = vec![1.0, 0.0, -1.0, -3.15, 27.0, 11.0, 31.0]
            .into_iter()
            .cycle();
        let mut next_random = move || next_random.next().unwrap();

        let generated_image: Luma32FImage =
            ImageBuffer::from_fn(9, 31, |_x, _y| Luma([next_random()]));

        let mut bytes = vec![];
        write_luma_image(Cursor::new(&mut bytes), &generated_image).unwrap();
        let decoded_image = read_as_luma_image(Cursor::new(bytes)).unwrap();

        debug_assert_eq!(generated_image, decoded_image);
    }

    /// Write an EXR containing a luma (`Y`) channel plus an alpha (`A`) channel,
    /// a layout this decoder does not (yet) support.
    fn write_luma_alpha_exr(write: impl Write + Seek) {
        Image::from_channels(
            (2, 3),
            SpecificChannels::build()
                .with_channel::<f32>("Y")
                .with_channel::<f32>("A")
                .with_pixel_fn(|_pixel: Vec2<usize>| (0.5_f32, 1.0_f32)),
        )
        .write()
        .to_buffered(write)
        .unwrap();
    }

    #[test]
    fn luma_alpha_is_unsupported() {
        // A `Y` + `A` image must be rejected explicitly rather than silently
        // dropping the alpha plane, so that adding `LumaA<f32>` support later is an
        // additive change instead of a behavior change.
        let mut bytes = vec![];
        write_luma_alpha_exr(Cursor::new(&mut bytes));

        let error =
            OpenExrDecoder::new(Cursor::new(bytes)).expect_err("a Y+A image must not decode");

        assert!(
            matches!(
                error,
                ImageError::Unsupported(ref e)
                    if matches!(
                        e.kind(),
                        UnsupportedErrorKind::Color(ExtendedColorType::La32F)
                    )
            ),
            "expected an unsupported-color error for `LumaA`, got {error:?}"
        );
    }

    #[test]
    fn compare_rgba_rgb() {
        let exr_path = BASE_PATH
            .iter()
            .collect::<PathBuf>()
            .join("overexposed gradient - data window equals display window.exr");

        let rgb: Rgb32FImage = read_as_rgb_image_from_file(&exr_path).unwrap();
        let rgba: Rgba32FImage = read_as_rgba_image_from_file(&exr_path).unwrap();

        assert_eq!(rgba.dimensions(), rgb.dimensions());

        for (Rgb(rgb), Rgba(rgba)) in rgb.pixels().iter().zip(rgba.pixels().iter()) {
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
        let original = exr_path.join("cropping - uncropped original.exr");
        let cropped = exr_path.join("cropping - data window differs display window.exr");

        // smoke-check that the exr files are actually not the same
        {
            let original_exr = read_first_flat_layer_from_file(&original).unwrap();
            let cropped_exr = read_first_flat_layer_from_file(&cropped).unwrap();
            assert_eq!(
                original_exr.attributes.display_window,
                cropped_exr.attributes.display_window
            );
            assert_ne!(
                original_exr.layer_data.attributes.layer_position,
                cropped_exr.layer_data.attributes.layer_position
            );
            assert_ne!(original_exr.layer_data.size, cropped_exr.layer_data.size);
        }

        // check that they result in the same image
        let original: Rgba32FImage = read_as_rgba_image_from_file(&original).unwrap();
        let cropped: Rgba32FImage = read_as_rgba_image_from_file(&cropped).unwrap();
        assert_eq!(original.dimensions(), cropped.dimensions());

        // the following is not a simple assert_eq, as in case of an error,
        // the whole image would be printed to the console, which takes forever
        assert!(original.pixels() == cropped.pixels());
    }
}
