//! Decoding and Encoding of PNG Images
//!
//! PNG (Portable Network Graphics) is an image format that supports lossless compression.
//!
//! # Related Links
//! * <http://www.w3.org/TR/PNG/> - The PNG Specification
//!

use std::convert::TryFrom;
use std::io::{self, Read, Write};

use num_rational::Ratio;
use png::{BlendOp, DisposeOp};

use crate::{DynamicImage, GenericImage, ImageBuffer, Luma, LumaA, RgbaImage, Rgb, Rgba};
use crate::animation::{Delay, Frame, Frames};
use crate::color::{Blend, ColorType, ExtendedColorType};
use crate::error::{
    DecodingError, ImageError, ImageResult, LimitError, LimitErrorKind, ParameterError, ParameterErrorKind, UnsupportedError, UnsupportedErrorKind
};
use crate::image::{AnimationDecoder, ImageDecoder, ImageEncoder, ImageFormat};

/// PNG Reader
///
/// This reader will try to read the png one row at a time,
/// however for interlaced png files this is not possible and
/// these are therefore read at once.
pub struct PNGReader<R: Read> {
    reader: png::Reader<R>,
    buffer: Vec<u8>,
    index: usize,
}

impl<R: Read> PNGReader<R> {
    fn new(mut reader: png::Reader<R>) -> ImageResult<PNGReader<R>> {
        let len = reader.output_buffer_size();
        // Since interlaced images do not come in
        // scanline order it is almost impossible to
        // read them in a streaming fashion, however
        // this shouldn't be a too big of a problem
        // as most interlaced images should fit in memory.
        let buffer = if reader.info().interlaced {
            let mut buffer = vec![0; len];
            reader.next_frame(&mut buffer).map_err(ImageError::from_png)?;
            buffer
        } else {
            Vec::new()
        };

        Ok(PNGReader {
            reader,
            buffer,
            index: 0,
        })
    }
}

impl<R: Read> Read for PNGReader<R> {
    fn read(&mut self, mut buf: &mut [u8]) -> io::Result<usize> {
        // io::Write::write for slice cannot fail
        let readed = buf.write(&self.buffer[self.index..]).unwrap();

        let mut bytes = readed;
        self.index += readed;

        while self.index >= self.buffer.len() {
            match self.reader.next_row()? {
                Some(row) => {
                    // Faster to copy directly to external buffer
                    let readed  = buf.write(row).unwrap();
                    bytes += readed;

                    self.buffer = (&row[readed..]).to_owned();
                    self.index = 0;
                }
                None => return Ok(bytes)
            }
        }

        Ok(bytes)
    }

    fn read_to_end(&mut self, buf: &mut Vec<u8>) -> io::Result<usize> {
        let mut bytes = self.buffer.len();
        buf.extend_from_slice(&self.buffer);
        self.buffer = Vec::new();
        self.index = 0;

        while let Some(row) = self.reader.next_row()? {
            buf.extend_from_slice(row);
            bytes += row.len();
        }

        Ok(bytes)
    }
}

/// PNG decoder
pub struct PngDecoder<R: Read> {
    color_type: ColorType,
    reader: png::Reader<R>,
}

impl<R: Read> PngDecoder<R> {
    /// Creates a new decoder that decodes from the stream ```r```
    pub fn new(r: R) -> ImageResult<PngDecoder<R>> {
        let limits = png::Limits {
            bytes: usize::max_value(),
        };
        let mut decoder = png::Decoder::new_with_limits(r, limits);
        // By default the PNG decoder will scale 16 bpc to 8 bpc, so custom
        // transformations must be set. EXPAND preserves the default behavior
        // expanding bpc < 8 to 8 bpc.
        decoder.set_transformations(png::Transformations::EXPAND);
        let (_, mut reader) = decoder.read_info().map_err(ImageError::from_png)?;
        let (color_type, bits) = reader.output_color_type();
        let color_type = match (color_type, bits) {
            (png::ColorType::Grayscale, png::BitDepth::Eight) => ColorType::L8,
            (png::ColorType::Grayscale, png::BitDepth::Sixteen) => ColorType::L16,
            (png::ColorType::GrayscaleAlpha, png::BitDepth::Eight) => ColorType::La8,
            (png::ColorType::GrayscaleAlpha, png::BitDepth::Sixteen) => ColorType::La16,
            (png::ColorType::RGB, png::BitDepth::Eight) => ColorType::Rgb8,
            (png::ColorType::RGB, png::BitDepth::Sixteen) => ColorType::Rgb16,
            (png::ColorType::RGBA, png::BitDepth::Eight) => ColorType::Rgba8,
            (png::ColorType::RGBA, png::BitDepth::Sixteen) => ColorType::Rgba16,

            (png::ColorType::Grayscale, png::BitDepth::One) =>
                return Err(unsupported_color(ExtendedColorType::L1)),
            (png::ColorType::GrayscaleAlpha, png::BitDepth::One) =>
                return Err(unsupported_color(ExtendedColorType::La1)),
            (png::ColorType::RGB, png::BitDepth::One) =>
                return Err(unsupported_color(ExtendedColorType::Rgb1)),
            (png::ColorType::RGBA, png::BitDepth::One) =>
                return Err(unsupported_color(ExtendedColorType::Rgba1)),

            (png::ColorType::Grayscale, png::BitDepth::Two) =>
                return Err(unsupported_color(ExtendedColorType::L2)),
            (png::ColorType::GrayscaleAlpha, png::BitDepth::Two) =>
                return Err(unsupported_color(ExtendedColorType::La2)),
            (png::ColorType::RGB, png::BitDepth::Two) =>
                return Err(unsupported_color(ExtendedColorType::Rgb2)),
            (png::ColorType::RGBA, png::BitDepth::Two) =>
                return Err(unsupported_color(ExtendedColorType::Rgba2)),

            (png::ColorType::Grayscale, png::BitDepth::Four) =>
                return Err(unsupported_color(ExtendedColorType::L4)),
            (png::ColorType::GrayscaleAlpha, png::BitDepth::Four) =>
                return Err(unsupported_color(ExtendedColorType::La4)),
            (png::ColorType::RGB, png::BitDepth::Four) =>
                return Err(unsupported_color(ExtendedColorType::Rgb4)),
            (png::ColorType::RGBA, png::BitDepth::Four) =>
                return Err(unsupported_color(ExtendedColorType::Rgba4)),

            (png::ColorType::Indexed, bits) =>
                return Err(unsupported_color(ExtendedColorType::Unknown(bits as u8))),
        };

        Ok(PngDecoder { color_type, reader })
    }

    /// Turn this into an iterator over the animation frames.
    ///
    /// Reading the complete animation requires more memory than reading the data from the IDAT
    /// frame–multiple frame buffers need to be reserved at the same time. We further do not
    /// support compositing 16-bit colors. In any case this would be lossy as the interface of
    /// animation decoders does not support 16-bit colors.
    ///
    /// If something is not supported or a limit is violated then the decoding step that requires
    /// them will fail and an error will be returned instead of the frame. No further frames will
    /// be returned.
    pub fn apng(self) -> ApngDecoder<R> {
        ApngDecoder::new(self)
    }

    /// Returns if the image contains an animation.
    ///
    /// Note that the file itself decides if the default image is considered to be part of the
    /// animation. When it is not the common interpretation is to use it as a thumbnail.
    ///
    /// If a non-animated image is converted into an `ApngDecoder` then its iterator is empty.
    pub fn is_apng(&self) -> bool {
        self.reader.info().animation_control.is_some()
    }
}

fn unsupported_color(ect: ExtendedColorType) -> ImageError {
    ImageError::Unsupported(UnsupportedError::from_format_and_kind(
        ImageFormat::Png.into(),
        UnsupportedErrorKind::Color(ect),
    ))
}

impl<'a, R: 'a + Read> ImageDecoder<'a> for PngDecoder<R> {
    type Reader = PNGReader<R>;

    fn dimensions(&self) -> (u32, u32) {
        self.reader.info().size()
    }

    fn color_type(&self) -> ColorType {
        self.color_type
    }

    fn into_reader(self) -> ImageResult<Self::Reader> {
        PNGReader::new(self.reader)
    }

    fn read_image(mut self, buf: &mut [u8]) -> ImageResult<()> {
        use byteorder::{BigEndian, ByteOrder, NativeEndian};

        assert_eq!(u64::try_from(buf.len()), Ok(self.total_bytes()));
        self.reader.next_frame(buf).map_err(ImageError::from_png)?;
        // PNG images are big endian. For 16 bit per channel and larger types,
        // the buffer may need to be reordered to native endianness per the
        // contract of `read_image`.
        // TODO: assumes equal channel bit depth.
        let bpc = self.color_type().bytes_per_pixel() / self.color_type().channel_count();
        match bpc {
            1 => (),  // No reodering necessary for u8
            2 => buf.chunks_mut(2).for_each(|c| {
                let v = BigEndian::read_u16(c);
                NativeEndian::write_u16(c, v)
            }),
            _ => unreachable!(),
        }
        Ok(())
    }

    fn scanline_bytes(&self) -> u64 {
        let width = self.reader.info().width;
        self.reader.output_line_size(width) as u64
    }
}

/// An [`AnimationDecoder`] adapter of [`PngDecoder`].
///
/// See [`PngDecoder::apng`] for more information.
///
/// [`AnimationDecoder`]: ../trait.AnimationDecoder.html
/// [`PngDecoder`]: struct.PngDecoder.html
/// [`PngDecoder::apng`]: struct.PngDecoder.html#method.apng
pub struct ApngDecoder<R: Read> {
    inner: PngDecoder<R>,
    /// The current output buffer.
    current: RgbaImage,
    /// The previous output buffer, used for dispose op previous.
    previous: RgbaImage,
    /// The dispose op of the current frame.
    dispose: DisposeOp,
    /// The number of image still expected to be able to load.
    remaining: u32,
    /// The next (first) image is the thumbnail.
    has_thumbnail: bool,
}

impl<R: Read> ApngDecoder<R> {
    fn new(inner: PngDecoder<R>) -> Self {
        let (width, height) = inner.dimensions();
        let info = inner.reader.info();
        let remaining = match info.animation_control() {
            // The expected number of fcTL in the remaining image.
            Some(actl) => actl.num_frames,
            None => 0,
        };
        // If the IDAT has no fcTL then it is not part of the animation counted by
        // num_frames. All following fdAT chunks must be preceded by an fcTL
        let has_thumbnail = info.frame_control.is_none();
        ApngDecoder {
            inner,
            // TODO: should we delay this allocation? At least if we support limits we should.
            current: RgbaImage::new(width, height),
            previous: RgbaImage::new(width, height),
            dispose: DisposeOp::Background,
            remaining,
            has_thumbnail,
        }
    }

    // TODO: thumbnail(&mut self) -> Option<impl ImageDecoder<'_>>

    /// Decode one subframe and overlay it on the canvas.
    fn mix_next_frame(&mut self) -> Result<Option<&RgbaImage>, ImageError> {
        // Remove this image from remaining.
        self.remaining = match self.remaining.checked_sub(1) {
            None => return Ok(None),
            Some(next) => next,
        };

        // Shorten ourselves to 0 in case of error.
        let remaining = self.remaining;
        self.remaining = 0;

        // Skip the thumbnail that is not part of the animation.
        if self.has_thumbnail {
            self.has_thumbnail = false;
            let mut buffer = vec![0; self.inner.reader.output_buffer_size()];
            self.inner.reader.next_frame(&mut buffer).map_err(ImageError::from_png)?;
        }

        self.animatable_color_type()?;

        // Dispose of the previous frame.
        match self.dispose {
            DisposeOp::None => {
                self.previous.clone_from(&self.current);
            }
            DisposeOp::Background => {
                self.previous.clone_from(&self.current);
                self.current.pixels_mut().for_each(|pixel| *pixel = Rgba([0, 0, 0, 0]));
            }
            DisposeOp::Previous => {
                self.current.clone_from(&self.previous);
            }
        }

        // Read next frame data.
        let mut buffer = vec![0; self.inner.reader.output_buffer_size()];
        self.inner.reader.next_frame(&mut buffer).map_err(ImageError::from_png)?;
        let info = self.inner.reader.info();

        // Find out how to interpret the decoded frame.
        let (width, height, px, py, blend);
        match info.frame_control() {
            None => {
                width = info.width;
                height = info.height;
                px = 0;
                py = 0;
                blend = BlendOp::Source;
            }
            Some(fc) => {
                width = fc.width;
                height = fc.height;
                px = fc.x_offset;
                py = fc.y_offset;
                blend = fc.blend_op;
                self.dispose = fc.dispose_op;
            }
        };

        // Turn the data into an rgba image proper.
        let source = match self.inner.color_type {
            ColorType::L8 => {
                let image = ImageBuffer::<Luma<_>, _>::from_raw(width, height, buffer).unwrap();
                DynamicImage::ImageLuma8(image).into_rgba()
            }
            ColorType::La8 => {
                let image = ImageBuffer::<LumaA<_>, _>::from_raw(width, height, buffer).unwrap();
                DynamicImage::ImageLumaA8(image).into_rgba()
            }
            ColorType::Rgb8 => {
                let image = ImageBuffer::<Rgb<_>, _>::from_raw(width, height, buffer).unwrap();
                DynamicImage::ImageRgb8(image).into_rgba()
            }
            ColorType::Rgba8 => {
                ImageBuffer::<Rgba<_>, _>::from_raw(width, height, buffer).unwrap()
            }
            ColorType::L16 | ColorType::Rgb16 | ColorType::La16 | ColorType::Rgba16 => {
                // TODO: to enable remove restriction in `animatable_color_type` method.
                unreachable!("16-bit apng not yet support")
            }
            _ => unreachable!("Invalid png color"),
        };

        match blend {
            BlendOp::Source => {
                self.current.copy_from(&source, px, py)
                    .expect("Invalid png image not detected in png");
            }
            BlendOp::Over => {
                // TODO: investigate speed, speed-ups, and bounds-checks.
                for (x, y, p) in source.enumerate_pixels() {
                    self.current.get_pixel_mut(x + px, y + py).blend(p);
                }
            }
        }

        // Ok, we can proceed with actually remaining images.
        self.remaining = remaining;
        // Return composited output buffer.
        Ok(Some(&self.current))
    }

    fn animatable_color_type(&self) -> Result<(), ImageError> {
        match self.inner.color_type {
            ColorType::L8 | ColorType::Rgb8 | ColorType::La8 | ColorType::Rgba8 => Ok(()),
            // TODO: do not handle multi-byte colors. Remember to implement it in `mix_next_frame`.
            ColorType::L16 | ColorType::Rgb16 | ColorType::La16 | ColorType::Rgba16  => {
                return Err(unsupported_color(self.inner.color_type.into()))
            },
            _ => unreachable!("{:?} not a valid png color", self.inner.color_type),
        }
    }
}

impl<'a, R: Read + 'a> AnimationDecoder<'a> for ApngDecoder<R> {
    fn into_frames(self) -> Frames<'a> {
        struct FrameIterator<R: Read>(ApngDecoder<R>);

        impl<R: Read> Iterator for FrameIterator<R> {
            type Item = ImageResult<Frame>;

            fn next(&mut self) -> Option<Self::Item> {
                let image = match self.0.mix_next_frame() {
                    Ok(Some(image)) => image.clone(),
                    Ok(None) => return None,
                    Err(err) => return Some(Err(err)),
                };

                let info = self.0.inner.reader.info();
                let fc = info.frame_control().unwrap();
                // PNG delays are rations in seconds.
                let num = u32::from(fc.delay_num) * 1_000u32;
                let denom = match fc.delay_den {
                    // The standard dictates to replace by 100 when the denominator is 0.
                    0 => 100,
                    d => u32::from(d),
                };
                let delay = Delay::from_ratio(Ratio::new(num, denom));
                Some(Ok(Frame::from_parts(image, 0, 0, delay)))
            }
        }

        Frames::new(Box::new(FrameIterator(self)))
    }
}

/// PNG encoder
pub struct PNGEncoder<W: Write> {
    w: W,
    compression: CompressionType,
    filter: FilterType,
}

/// Compression level of a PNG encoder. The default setting is `Fast`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum CompressionType {
    /// Default compression level
    Default,
    /// Fast, minimal compression
    Fast,
    /// High compression level
    Best,
    /// Huffman coding compression
    Huffman,
    /// Run-length encoding compression
    Rle,

    #[doc(hidden)]
    __NonExhaustive(crate::utils::NonExhaustiveMarker),
}

/// Filter algorithms used to process image data to improve compression.
///
/// The default filter is `Sub` though this default may change in the future, most notable if an
/// adaptive encoding option is implemented.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum FilterType {
    /// No processing done, best used for low bit depth greyscale or data with a
    /// low color count
    NoFilter,
    /// Filters based on previous pixel in the same scanline
    Sub,
    /// Filters based on the scanline above
    Up,
    /// Filters based on the average of left and right neighbor pixels
    Avg,
    /// Algorithm that takes into account the left, upper left, and above pixels
    Paeth,

    #[doc(hidden)]
    __NonExhaustive(crate::utils::NonExhaustiveMarker),
}

impl<W: Write> PNGEncoder<W> {
    /// Create a new encoder that writes its output to ```w```
    pub fn new(w: W) -> PNGEncoder<W> {
        PNGEncoder {
            w,
            compression: CompressionType::Fast,
            filter: FilterType::Sub,
        }
    }

    /// Create a new encoder that writes its output to `w` with `CompressionType` `compression` and
    /// `FilterType` `filter`.
    ///
    /// It is best to view the options as a _hint_ to the implementation on the smallest or fastest
    /// option for encoding a particular image. That is, using options that map directly to a PNG
    /// image parameter will use this parameter where possible. But variants that have no direct
    /// mapping may be interpreted differently in minor versions. The exact output is expressly
    /// __not__ part the SemVer stability guarantee.
    ///
    /// Note that it is not optimal to use a single filter type. It is likely that the library used
    /// will at some point gain the ability to use adaptive filtering methods per pixel row (or
    /// even interlaced row). We might make it the new default variant in which case choosing a
    /// particular filter method likely produces larger images. Be sure to check the release notes
    /// once in a while.
    pub fn new_with_quality(w: W, compression: CompressionType, filter: FilterType) -> PNGEncoder<W> {
        PNGEncoder {
            w,
            compression,
            filter,
        }
    }

    /// Encodes the image `data` that has dimensions `width` and `height` and `ColorType` `c`.
    pub fn encode(self, data: &[u8], width: u32, height: u32, color: ColorType) -> ImageResult<()> {
        let (ct, bits) = match color {
            ColorType::L8 => (png::ColorType::Grayscale, png::BitDepth::Eight),
            ColorType::L16 => (png::ColorType::Grayscale,png::BitDepth::Sixteen),
            ColorType::La8 => (png::ColorType::GrayscaleAlpha, png::BitDepth::Eight),
            ColorType::La16 => (png::ColorType::GrayscaleAlpha,png::BitDepth::Sixteen),
            ColorType::Rgb8 => (png::ColorType::RGB, png::BitDepth::Eight),
            ColorType::Rgb16 => (png::ColorType::RGB,png::BitDepth::Sixteen),
            ColorType::Rgba8 => (png::ColorType::RGBA, png::BitDepth::Eight),
            ColorType::Rgba16 => (png::ColorType::RGBA,png::BitDepth::Sixteen),
            _ => return Err(ImageError::Unsupported(UnsupportedError::from_format_and_kind(
                ImageFormat::Png.into(),
                UnsupportedErrorKind::Color(color.into()),
            ))),
        };
        let comp = match self.compression {
            CompressionType::Default => png::Compression::Default,
            CompressionType::Fast => png::Compression::Fast,
            CompressionType::Best => png::Compression::Best,
            CompressionType::Huffman => png::Compression::Huffman,
            CompressionType::Rle => png::Compression::Rle,
            CompressionType::__NonExhaustive(marker) => match marker._private {},
        };
        let filt = match self.filter {
            FilterType::NoFilter => png::FilterType::NoFilter,
            FilterType::Sub => png::FilterType::Sub,
            FilterType::Up => png::FilterType::Up,
            FilterType::Avg => png::FilterType::Avg,
            FilterType::Paeth => png::FilterType::Paeth,
            FilterType::__NonExhaustive(marker) => match marker._private {},
        };

        let mut encoder = png::Encoder::new(self.w, width, height);
        encoder.set_color(ct);
        encoder.set_depth(bits);
        encoder.set_compression(comp);
        encoder.set_filter(filt);
        let mut writer = encoder.write_header().map_err(|e| ImageError::IoError(e.into()))?;
        writer.write_image_data(data).map_err(|e| ImageError::IoError(e.into()))
    }
}

impl<W: Write> ImageEncoder for PNGEncoder<W> {
    fn write_image(
        self,
        buf: &[u8],
        width: u32,
        height: u32,
        color_type: ColorType,
    ) -> ImageResult<()> {
        use byteorder::{BigEndian, ByteOrder, NativeEndian};

        // PNG images are big endian. For 16 bit per channel and larger types,
        // the buffer may need to be reordered to big endian per the
        // contract of `write_image`.
        // TODO: assumes equal channel bit depth.
        let bpc = color_type.bytes_per_pixel() / color_type.channel_count();
        match bpc {
            1 => self.encode(buf, width, height, color_type),  // No reodering necessary for u8
            2 => {
                // Because the buffer is immutable and the PNG encoder does not
                // yet take Write/Read traits, create a temporary buffer for
                // big endian reordering.
                let mut reordered = vec![0; buf.len()];
                buf.chunks(2)
                    .zip(reordered.chunks_mut(2))
                    .for_each(|(b, r)| BigEndian::write_u16(r, NativeEndian::read_u16(b)));
                self.encode(&reordered, width, height, color_type)
            },
            _ => unreachable!(),
        }
    }
}

impl ImageError {
    fn from_png(err: png::DecodingError) -> ImageError {
        use png::DecodingError::*;
        match err {
            IoError(err) => ImageError::IoError(err),
            err @ Format(_) => ImageError::Decoding(DecodingError::new(
                ImageFormat::Png.into(),
                err,
            )),
            LimitsExceeded => ImageError::Limits(LimitError::from_kind(
                LimitErrorKind::InsufficientMemory,
            )),
            // Other is used when the buffer to `Reader::next_frame` is too small.
            Other(message) => ImageError::Parameter(ParameterError::from_kind(
                ParameterErrorKind::Generic(message.into_owned())
            )),
            err @ InvalidSignature
            | err @ CrcMismatch { .. }
            | err @ CorruptFlateStream => {
                ImageError::Decoding(DecodingError::new(
                    ImageFormat::Png.into(),
                    err,
                ))
            }
        }
    }
}

impl Default for CompressionType {
    fn default() -> Self {
        CompressionType::Fast
    }
}

impl Default for FilterType {
    fn default() -> Self {
        FilterType::Sub
    }
}

#[cfg(test)]
mod tests {
    use crate::image::ImageDecoder;
    use std::io::Read;
    use super::*;

    #[test]
    fn ensure_no_decoder_off_by_one() {
        let dec = PngDecoder::new(std::fs::File::open("tests/images/png/bugfixes/debug_triangle_corners_widescreen.png").unwrap())
            .expect("Unable to read PNG file (does it exist?)");

        assert_eq![(2000, 1000), dec.dimensions()];

        assert_eq![
            ColorType::Rgb8,
            dec.color_type(),
            "Image MUST have the Rgb8 format"
        ];

        let correct_bytes = dec
            .into_reader()
            .expect("Unable to read file")
            .bytes()
            .map(|x| x.expect("Unable to read byte"))
            .collect::<Vec<u8>>();

        assert_eq![6_000_000, correct_bytes.len()];
    }

    #[test]
    fn underlying_error() {
        use std::error::Error;

        let mut not_png = std::fs::read("tests/images/png/bugfixes/debug_triangle_corners_widescreen.png").unwrap();
        not_png[0] = 0;

        let error = PngDecoder::new(&not_png[..]).err().unwrap();
        let _ = error
            .source()
            .unwrap()
            .downcast_ref::<png::DecodingError>()
            .expect("Caused by a png error");
    }
}
