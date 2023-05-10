use crate::Primitive;
use num_traits::identities::Zero;
#[cfg(test)]
use std::borrow::Cow;
use std::convert::TryFrom;
use std::io::{self, BufRead, Cursor, Read, Seek};
use std::iter::Iterator;
use std::marker::PhantomData;
use std::num::{ParseFloatError, ParseIntError};
use std::path::Path;
use std::{error, fmt, mem};

use crate::color::{ColorType, Rgb};
use crate::error::{
    DecodingError, ImageError, ImageFormatHint, ImageResult, ParameterError, ParameterErrorKind,
    UnsupportedError, UnsupportedErrorKind,
};
use crate::image::{self, ImageDecoder, ImageDecoderRect, ImageFormat, Progress};

/// Errors that can occur during decoding and parsing of a HDR image
#[derive(Debug, Clone, PartialEq, Eq)]
enum DecoderError {
    /// HDR's "#?RADIANCE" signature wrong or missing
    RadianceHdrSignatureInvalid,
    /// EOF before end of header
    TruncatedHeader,
    /// EOF instead of image dimensions
    TruncatedDimensions,

    /// A value couldn't be parsed
    UnparsableF32(LineType, ParseFloatError),
    /// A value couldn't be parsed
    UnparsableU32(LineType, ParseIntError),
    /// Not enough numbers in line
    LineTooShort(LineType),

    /// COLORCORR contains too many numbers in strict mode
    ExtraneousColorcorrNumbers,

    /// Dimensions line had too few elements
    DimensionsLineTooShort(usize, usize),
    /// Dimensions line had too many elements
    DimensionsLineTooLong(usize),

    /// The length of a scanline (1) wasn't a match for the specified length (2)
    WrongScanlineLength(usize, usize),
    /// First pixel of a scanline is a run length marker
    FirstPixelRlMarker,
}

impl fmt::Display for DecoderError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DecoderError::RadianceHdrSignatureInvalid => {
                f.write_str("Radiance HDR signature not found")
            }
            DecoderError::TruncatedHeader => f.write_str("EOF in header"),
            DecoderError::TruncatedDimensions => f.write_str("EOF in dimensions line"),
            DecoderError::UnparsableF32(line, pe) => {
                f.write_fmt(format_args!("Cannot parse {} value as f32: {}", line, pe))
            }
            DecoderError::UnparsableU32(line, pe) => {
                f.write_fmt(format_args!("Cannot parse {} value as u32: {}", line, pe))
            }
            DecoderError::LineTooShort(line) => {
                f.write_fmt(format_args!("Not enough numbers in {}", line))
            }
            DecoderError::ExtraneousColorcorrNumbers => f.write_str("Extra numbers in COLORCORR"),
            DecoderError::DimensionsLineTooShort(elements, expected) => f.write_fmt(format_args!(
                "Dimensions line too short: have {} elements, expected {}",
                elements, expected
            )),
            DecoderError::DimensionsLineTooLong(expected) => f.write_fmt(format_args!(
                "Dimensions line too long, expected {} elements",
                expected
            )),
            DecoderError::WrongScanlineLength(len, expected) => f.write_fmt(format_args!(
                "Wrong length of decoded scanline: got {}, expected {}",
                len, expected
            )),
            DecoderError::FirstPixelRlMarker => {
                f.write_str("First pixel of a scanline shouldn't be run length marker")
            }
        }
    }
}

impl From<DecoderError> for ImageError {
    fn from(e: DecoderError) -> ImageError {
        ImageError::Decoding(DecodingError::new(ImageFormat::Hdr.into(), e))
    }
}

impl error::Error for DecoderError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match self {
            DecoderError::UnparsableF32(_, err) => Some(err),
            DecoderError::UnparsableU32(_, err) => Some(err),
            _ => None,
        }
    }
}

/// Lines which contain parsable data that can fail
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
enum LineType {
    Exposure,
    Pixaspect,
    Colorcorr,
    DimensionsHeight,
    DimensionsWidth,
}

impl fmt::Display for LineType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            LineType::Exposure => "EXPOSURE",
            LineType::Pixaspect => "PIXASPECT",
            LineType::Colorcorr => "COLORCORR",
            LineType::DimensionsHeight => "height dimension",
            LineType::DimensionsWidth => "width dimension",
        })
    }
}

/// Adapter to conform to ```ImageDecoder``` trait
#[derive(Debug)]
pub struct HdrAdapter<R: Read> {
    inner: Option<HdrDecoder<R>>,
    // data: Option<Vec<u8>>,
    meta: HdrMetadata,
}

impl<R: BufRead> HdrAdapter<R> {
    /// Creates adapter
    pub fn new(r: R) -> ImageResult<HdrAdapter<R>> {
        let decoder = HdrDecoder::new(r)?;
        let meta = decoder.metadata();
        Ok(HdrAdapter {
            inner: Some(decoder),
            meta,
        })
    }

    /// Allows reading old Radiance HDR images
    pub fn new_nonstrict(r: R) -> ImageResult<HdrAdapter<R>> {
        let decoder = HdrDecoder::with_strictness(r, false)?;
        let meta = decoder.metadata();
        Ok(HdrAdapter {
            inner: Some(decoder),
            meta,
        })
    }

    /// Read the actual data of the image, and store it in Self::data.
    fn read_image_data(&mut self, buf: &mut [u8]) -> ImageResult<()> {
        assert_eq!(u64::try_from(buf.len()), Ok(self.total_bytes()));
        match self.inner.take() {
            Some(decoder) => {
                let img: Vec<Rgb<u8>> = decoder.read_image_ldr()?;
                for (i, Rgb(data)) in img.into_iter().enumerate() {
                    buf[(i * 3)..][..3].copy_from_slice(&data);
                }

                Ok(())
            }
            None => Err(ImageError::Parameter(ParameterError::from_kind(
                ParameterErrorKind::NoMoreData,
            ))),
        }
    }
}

/// Wrapper struct around a `Cursor<Vec<u8>>`
pub struct HdrReader<R>(Cursor<Vec<u8>>, PhantomData<R>);
impl<R> Read for HdrReader<R> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.0.read(buf)
    }
    fn read_to_end(&mut self, buf: &mut Vec<u8>) -> io::Result<usize> {
        if self.0.position() == 0 && buf.is_empty() {
            mem::swap(buf, self.0.get_mut());
            Ok(buf.len())
        } else {
            self.0.read_to_end(buf)
        }
    }
}

impl<'a, R: 'a + BufRead> ImageDecoder<'a> for HdrAdapter<R> {
    type Reader = HdrReader<R>;

    fn dimensions(&self) -> (u32, u32) {
        (self.meta.width, self.meta.height)
    }

    fn color_type(&self) -> ColorType {
        ColorType::Rgb8
    }

    fn into_reader(self) -> ImageResult<Self::Reader> {
        Ok(HdrReader(
            Cursor::new(image::decoder_to_vec(self)?),
            PhantomData,
        ))
    }

    fn read_image(mut self, buf: &mut [u8]) -> ImageResult<()> {
        self.read_image_data(buf)
    }
}

impl<'a, R: 'a + BufRead + Seek> ImageDecoderRect<'a> for HdrAdapter<R> {
    fn read_rect_with_progress<F: Fn(Progress)>(
        &mut self,
        x: u32,
        y: u32,
        width: u32,
        height: u32,
        buf: &mut [u8],
        progress_callback: F,
    ) -> ImageResult<()> {
        image::load_rect(
            x,
            y,
            width,
            height,
            buf,
            progress_callback,
            self,
            |_, _| unreachable!(),
            |s, buf| s.read_image_data(buf),
        )
    }
}

/// Radiance HDR file signature
pub const SIGNATURE: &[u8] = b"#?RADIANCE";
const SIGNATURE_LENGTH: usize = 10;

/// An Radiance HDR decoder
#[derive(Debug)]
pub struct HdrDecoder<R> {
    r: R,
    width: u32,
    height: u32,
    meta: HdrMetadata,
}

/// Refer to [wikipedia](https://en.wikipedia.org/wiki/RGBE_image_format)
#[repr(C)]
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Rgbe8Pixel {
    /// Color components
    pub c: [u8; 3],
    /// Exponent
    pub e: u8,
}

/// Creates ```Rgbe8Pixel``` from components
pub fn rgbe8(r: u8, g: u8, b: u8, e: u8) -> Rgbe8Pixel {
    Rgbe8Pixel { c: [r, g, b], e }
}

impl Rgbe8Pixel {
    /// Converts ```Rgbe8Pixel``` into ```Rgb<f32>``` linearly
    #[inline]
    pub fn to_hdr(self) -> Rgb<f32> {
        if self.e == 0 {
            Rgb([0.0, 0.0, 0.0])
        } else {
            //            let exp = f32::ldexp(1., self.e as isize - (128 + 8)); // unstable
            let exp = f32::exp2(<f32 as From<_>>::from(self.e) - (128.0 + 8.0));
            Rgb([
                exp * <f32 as From<_>>::from(self.c[0]),
                exp * <f32 as From<_>>::from(self.c[1]),
                exp * <f32 as From<_>>::from(self.c[2]),
            ])
        }
    }

    /// Converts ```Rgbe8Pixel``` into ```Rgb<T>``` with scale=1 and gamma=2.2
    ///
    /// color_ldr = (color_hdr*scale)<sup>gamma</sup>
    ///
    /// # Panic
    ///
    /// Panics when ```T::max_value()``` cannot be represented as f32.
    #[inline]
    pub fn to_ldr<T: Primitive + Zero>(self) -> Rgb<T> {
        self.to_ldr_scale_gamma(1.0, 2.2)
    }

    /// Converts Rgbe8Pixel into Rgb<T> using provided scale and gamma
    ///
    /// color_ldr = (color_hdr*scale)<sup>gamma</sup>
    ///
    /// # Panic
    ///
    /// Panics when T::max_value() cannot be represented as f32.
    /// Panics when scale or gamma is NaN
    #[inline]
    pub fn to_ldr_scale_gamma<T: Primitive + Zero>(self, scale: f32, gamma: f32) -> Rgb<T> {
        let Rgb(data) = self.to_hdr();
        let (r, g, b) = (data[0], data[1], data[2]);
        #[inline]
        fn sg<T: Primitive + Zero>(v: f32, scale: f32, gamma: f32) -> T {
            let t_max = T::max_value();
            // Disassembly shows that t_max_f32 is compiled into constant
            let t_max_f32: f32 = num_traits::NumCast::from(t_max)
                .expect("to_ldr_scale_gamma: maximum value of type is not representable as f32");
            let fv = f32::powf(v * scale, gamma) * t_max_f32 + 0.5;
            if fv < 0.0 {
                T::zero()
            } else if fv > t_max_f32 {
                t_max
            } else {
                num_traits::NumCast::from(fv)
                    .expect("to_ldr_scale_gamma: cannot convert f32 to target type. NaN?")
            }
        }
        Rgb([
            sg(r, scale, gamma),
            sg(g, scale, gamma),
            sg(b, scale, gamma),
        ])
    }
}

impl<R: BufRead> HdrDecoder<R> {
    /// Reads Radiance HDR image header from stream ```r```
    /// if the header is valid, creates HdrDecoder
    /// strict mode is enabled
    pub fn new(reader: R) -> ImageResult<HdrDecoder<R>> {
        HdrDecoder::with_strictness(reader, true)
    }

    /// Reads Radiance HDR image header from stream ```reader```,
    /// if the header is valid, creates ```HdrDecoder```.
    ///
    /// strict enables strict mode
    ///
    /// Warning! Reading wrong file in non-strict mode
    ///   could consume file size worth of memory in the process.
    pub fn with_strictness(mut reader: R, strict: bool) -> ImageResult<HdrDecoder<R>> {
        let mut attributes = HdrMetadata::new();

        {
            // scope to make borrowck happy
            let r = &mut reader;
            if strict {
                let mut signature = [0; SIGNATURE_LENGTH];
                r.read_exact(&mut signature)?;
                if signature != SIGNATURE {
                    return Err(DecoderError::RadianceHdrSignatureInvalid.into());
                } // no else
                  // skip signature line ending
                read_line_u8(r)?;
            } else {
                // Old Radiance HDR files (*.pic) don't use signature
                // Let them be parsed in non-strict mode
            }
            // read header data until empty line
            loop {
                match read_line_u8(r)? {
                    None => {
                        // EOF before end of header
                        return Err(DecoderError::TruncatedHeader.into());
                    }
                    Some(line) => {
                        if line.is_empty() {
                            // end of header
                            break;
                        } else if line[0] == b'#' {
                            // line[0] will not panic, line.len() == 0 is false here
                            // skip comments
                            continue;
                        } // no else
                          // process attribute line
                        let line = String::from_utf8_lossy(&line[..]);
                        attributes.update_header_info(&line, strict)?;
                    } // <= Some(line)
                } // match read_line_u8()
            } // loop
        } // scope to end borrow of reader
          // parse dimensions
        let (width, height) = match read_line_u8(&mut reader)? {
            None => {
                // EOF instead of image dimensions
                return Err(DecoderError::TruncatedDimensions.into());
            }
            Some(dimensions) => {
                let dimensions = String::from_utf8_lossy(&dimensions[..]);
                parse_dimensions_line(&dimensions, strict)?
            }
        };

        // color type is always rgb8
        if crate::utils::check_dimension_overflow(width, height, ColorType::Rgb8.bytes_per_pixel())
        {
            return Err(ImageError::Unsupported(
                UnsupportedError::from_format_and_kind(
                    ImageFormat::Hdr.into(),
                    UnsupportedErrorKind::GenericFeature(format!(
                        "Image dimensions ({}x{}) are too large",
                        width, height
                    )),
                ),
            ));
        }

        Ok(HdrDecoder {
            r: reader,

            width,
            height,
            meta: HdrMetadata {
                width,
                height,
                ..attributes
            },
        })
    } // end with_strictness

    /// Returns file metadata. Refer to ```HdrMetadata``` for details.
    pub fn metadata(&self) -> HdrMetadata {
        self.meta.clone()
    }

    /// Consumes decoder and returns a vector of RGBE8 pixels
    pub fn read_image_native(mut self) -> ImageResult<Vec<Rgbe8Pixel>> {
        // Don't read anything if image is empty
        if self.width == 0 || self.height == 0 {
            return Ok(vec![]);
        }
        // expression self.width > 0 && self.height > 0 is true from now to the end of this method
        let pixel_count = self.width as usize * self.height as usize;
        let mut ret = vec![Default::default(); pixel_count];
        for chunk in ret.chunks_mut(self.width as usize) {
            read_scanline(&mut self.r, chunk)?;
        }
        Ok(ret)
    }

    /// Consumes decoder and returns a vector of transformed pixels
    pub fn read_image_transform<T: Send, F: Send + Sync + Fn(Rgbe8Pixel) -> T>(
        mut self,
        f: F,
        output_slice: &mut [T],
    ) -> ImageResult<()> {
        assert_eq!(
            output_slice.len(),
            self.width as usize * self.height as usize
        );

        // Don't read anything if image is empty
        if self.width == 0 || self.height == 0 {
            return Ok(());
        }

        let chunks_iter = output_slice.chunks_mut(self.width as usize);

        let mut buf = vec![Default::default(); self.width as usize];
        for chunk in chunks_iter {
            // read_scanline overwrites the entire buffer or returns an Err,
            // so not resetting the buffer here is ok.
            read_scanline(&mut self.r, &mut buf[..])?;
            for (dst, &pix) in chunk.iter_mut().zip(buf.iter()) {
                *dst = f(pix);
            }
        }
        Ok(())
    }

    /// Consumes decoder and returns a vector of Rgb<u8> pixels.
    /// scale = 1, gamma = 2.2
    pub fn read_image_ldr(self) -> ImageResult<Vec<Rgb<u8>>> {
        let mut ret = vec![Rgb([0, 0, 0]); self.width as usize * self.height as usize];
        self.read_image_transform(|pix| pix.to_ldr(), &mut ret[..])?;
        Ok(ret)
    }

    /// Consumes decoder and returns a vector of Rgb<f32> pixels.
    ///
    pub fn read_image_hdr(self) -> ImageResult<Vec<Rgb<f32>>> {
        let mut ret = vec![Rgb([0.0, 0.0, 0.0]); self.width as usize * self.height as usize];
        self.read_image_transform(|pix| pix.to_hdr(), &mut ret[..])?;
        Ok(ret)
    }
}

impl<R: Read> IntoIterator for HdrDecoder<R> {
    type Item = ImageResult<Rgbe8Pixel>;
    type IntoIter = HdrImageDecoderIterator<R>;

    fn into_iter(self) -> Self::IntoIter {
        HdrImageDecoderIterator {
            r: self.r,
            scanline_cnt: self.height as usize,
            buf: vec![Default::default(); self.width as usize],
            col: 0,
            scanline: 0,
            trouble: true, // make first call to `next()` read scanline
            error_encountered: false,
        }
    }
}

/// Scanline buffered pixel by pixel iterator
pub struct HdrImageDecoderIterator<R: Read> {
    r: R,
    scanline_cnt: usize,
    buf: Vec<Rgbe8Pixel>, // scanline buffer
    col: usize,           // current position in scanline
    scanline: usize,      // current scanline
    trouble: bool,        // optimization, true indicates that we need to check something
    error_encountered: bool,
}

impl<R: Read> HdrImageDecoderIterator<R> {
    // Advances counter to the next pixel
    #[inline]
    fn advance(&mut self) {
        self.col += 1;
        if self.col == self.buf.len() {
            self.col = 0;
            self.scanline += 1;
            self.trouble = true;
        }
    }
}

impl<R: Read> Iterator for HdrImageDecoderIterator<R> {
    type Item = ImageResult<Rgbe8Pixel>;

    fn next(&mut self) -> Option<Self::Item> {
        if !self.trouble {
            let ret = self.buf[self.col];
            self.advance();
            Some(Ok(ret))
        } else {
            // some condition is pending
            if self.buf.is_empty() || self.scanline == self.scanline_cnt {
                // No more pixels
                return None;
            } // no else
            if self.error_encountered {
                self.advance();
                // Error was encountered. Keep producing errors.
                // ImageError can't implement Clone, so just dump some error
                return Some(Err(ImageError::Parameter(ParameterError::from_kind(
                    ParameterErrorKind::FailedAlready,
                ))));
            } // no else
            if self.col == 0 {
                // fill scanline buffer
                match read_scanline(&mut self.r, &mut self.buf[..]) {
                    Ok(_) => {
                        // no action required
                    }
                    Err(err) => {
                        self.advance();
                        self.error_encountered = true;
                        self.trouble = true;
                        return Some(Err(err));
                    }
                }
            } // no else
            self.trouble = false;
            let ret = self.buf[0];
            self.advance();
            Some(Ok(ret))
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let total_cnt = self.buf.len() * self.scanline_cnt;
        let cur_cnt = self.buf.len() * self.scanline + self.col;
        let remaining = total_cnt - cur_cnt;
        (remaining, Some(remaining))
    }
}

impl<R: Read> ExactSizeIterator for HdrImageDecoderIterator<R> {}

// Precondition: buf.len() > 0
fn read_scanline<R: Read>(r: &mut R, buf: &mut [Rgbe8Pixel]) -> ImageResult<()> {
    assert!(!buf.is_empty());
    let width = buf.len();
    // first 4 bytes in scanline allow to determine compression method
    let fb = read_rgbe(r)?;
    if fb.c[0] == 2 && fb.c[1] == 2 && fb.c[2] < 128 {
        // denormalized pixel value (2,2,<128,_) indicates new per component RLE method
        // decode_component guarantees that offset is within 0 .. width
        // therefore we can skip bounds checking here, but we will not
        decode_component(r, width, |offset, value| buf[offset].c[0] = value)?;
        decode_component(r, width, |offset, value| buf[offset].c[1] = value)?;
        decode_component(r, width, |offset, value| buf[offset].c[2] = value)?;
        decode_component(r, width, |offset, value| buf[offset].e = value)?;
    } else {
        // old RLE method (it was considered old around 1991, should it be here?)
        decode_old_rle(r, fb, buf)?;
    }
    Ok(())
}

#[inline(always)]
fn read_byte<R: Read>(r: &mut R) -> io::Result<u8> {
    let mut buf = [0u8];
    r.read_exact(&mut buf[..])?;
    Ok(buf[0])
}

// Guarantees that first parameter of set_component will be within pos .. pos+width
#[inline]
fn decode_component<R: Read, S: FnMut(usize, u8)>(
    r: &mut R,
    width: usize,
    mut set_component: S,
) -> ImageResult<()> {
    let mut buf = [0; 128];
    let mut pos = 0;
    while pos < width {
        // increment position by a number of decompressed values
        pos += {
            let rl = read_byte(r)?;
            if rl <= 128 {
                // sanity check
                if pos + rl as usize > width {
                    return Err(DecoderError::WrongScanlineLength(pos + rl as usize, width).into());
                }
                // read values
                r.read_exact(&mut buf[0..rl as usize])?;
                for (offset, &value) in buf[0..rl as usize].iter().enumerate() {
                    set_component(pos + offset, value);
                }
                rl as usize
            } else {
                // run
                let rl = rl - 128;
                // sanity check
                if pos + rl as usize > width {
                    return Err(DecoderError::WrongScanlineLength(pos + rl as usize, width).into());
                }
                // fill with same value
                let value = read_byte(r)?;
                for offset in 0..rl as usize {
                    set_component(pos + offset, value);
                }
                rl as usize
            }
        };
    }
    if pos != width {
        return Err(DecoderError::WrongScanlineLength(pos, width).into());
    }
    Ok(())
}

// Decodes scanline, places it into buf
// Precondition: buf.len() > 0
// fb - first 4 bytes of scanline
fn decode_old_rle<R: Read>(r: &mut R, fb: Rgbe8Pixel, buf: &mut [Rgbe8Pixel]) -> ImageResult<()> {
    assert!(!buf.is_empty());
    let width = buf.len();
    // convenience function.
    // returns run length if pixel is a run length marker
    #[inline]
    fn rl_marker(pix: Rgbe8Pixel) -> Option<usize> {
        if pix.c == [1, 1, 1] {
            Some(pix.e as usize)
        } else {
            None
        }
    }
    // first pixel in scanline should not be run length marker
    // it is error if it is
    if rl_marker(fb).is_some() {
        return Err(DecoderError::FirstPixelRlMarker.into());
    }
    buf[0] = fb; // set first pixel of scanline

    let mut x_off = 1; // current offset from beginning of a scanline
    let mut rl_mult = 1; // current run length multiplier
    let mut prev_pixel = fb;
    while x_off < width {
        let pix = read_rgbe(r)?;
        // it's harder to forget to increase x_off if I write this this way.
        x_off += {
            if let Some(rl) = rl_marker(pix) {
                // rl_mult takes care of consecutive RL markers
                let rl = rl * rl_mult;
                rl_mult *= 256;
                if x_off + rl <= width {
                    // do run
                    for b in &mut buf[x_off..x_off + rl] {
                        *b = prev_pixel;
                    }
                } else {
                    return Err(DecoderError::WrongScanlineLength(x_off + rl, width).into());
                };
                rl // value to increase x_off by
            } else {
                rl_mult = 1; // chain of consecutive RL markers is broken
                prev_pixel = pix;
                buf[x_off] = pix;
                1 // value to increase x_off by
            }
        };
    }
    if x_off != width {
        return Err(DecoderError::WrongScanlineLength(x_off, width).into());
    }
    Ok(())
}

fn read_rgbe<R: Read>(r: &mut R) -> io::Result<Rgbe8Pixel> {
    let mut buf = [0u8; 4];
    r.read_exact(&mut buf[..])?;
    Ok(Rgbe8Pixel {
        c: [buf[0], buf[1], buf[2]],
        e: buf[3],
    })
}

/// Metadata for Radiance HDR image
#[derive(Debug, Clone)]
pub struct HdrMetadata {
    /// Width of decoded image. It could be either scanline length,
    /// or scanline count, depending on image orientation.
    pub width: u32,
    /// Height of decoded image. It depends on orientation too.
    pub height: u32,
    /// Orientation matrix. For standard orientation it is ((1,0),(0,1)) - left to right, top to bottom.
    /// First pair tells how resulting pixel coordinates change along a scanline.
    /// Second pair tells how they change from one scanline to the next.
    pub orientation: ((i8, i8), (i8, i8)),
    /// Divide color values by exposure to get to get physical radiance in
    /// watts/steradian/m<sup>2</sup>
    ///
    /// Image may not contain physical data, even if this field is set.
    pub exposure: Option<f32>,
    /// Divide color values by corresponding tuple member (r, g, b) to get to get physical radiance
    /// in watts/steradian/m<sup>2</sup>
    ///
    /// Image may not contain physical data, even if this field is set.
    pub color_correction: Option<(f32, f32, f32)>,
    /// Pixel height divided by pixel width
    pub pixel_aspect_ratio: Option<f32>,
    /// All lines contained in image header are put here. Ordering of lines is preserved.
    /// Lines in the form "key=value" are represented as ("key", "value").
    /// All other lines are ("", "line")
    pub custom_attributes: Vec<(String, String)>,
}

impl HdrMetadata {
    fn new() -> HdrMetadata {
        HdrMetadata {
            width: 0,
            height: 0,
            orientation: ((1, 0), (0, 1)),
            exposure: None,
            color_correction: None,
            pixel_aspect_ratio: None,
            custom_attributes: vec![],
        }
    }

    // Updates header info, in strict mode returns error for malformed lines (no '=' separator)
    // unknown attributes are skipped
    fn update_header_info(&mut self, line: &str, strict: bool) -> ImageResult<()> {
        // split line at first '='
        // old Radiance HDR files (*.pic) feature tabs in key, so                vvv trim
        let maybe_key_value = split_at_first(line, "=").map(|(key, value)| (key.trim(), value));
        // save all header lines in custom_attributes
        match maybe_key_value {
            Some((key, val)) => self
                .custom_attributes
                .push((key.to_owned(), val.to_owned())),
            None => self.custom_attributes.push(("".into(), line.to_owned())),
        }
        // parse known attributes
        match maybe_key_value {
            Some(("FORMAT", val)) => {
                if val.trim() != "32-bit_rle_rgbe" {
                    // XYZE isn't supported yet
                    return Err(ImageError::Unsupported(
                        UnsupportedError::from_format_and_kind(
                            ImageFormat::Hdr.into(),
                            UnsupportedErrorKind::Format(ImageFormatHint::Name(limit_string_len(
                                val, 20,
                            ))),
                        ),
                    ));
                }
            }
            Some(("EXPOSURE", val)) => {
                match val.trim().parse::<f32>() {
                    Ok(v) => {
                        self.exposure = Some(self.exposure.unwrap_or(1.0) * v); // all encountered exposure values should be multiplied
                    }
                    Err(parse_error) => {
                        if strict {
                            return Err(DecoderError::UnparsableF32(
                                LineType::Exposure,
                                parse_error,
                            )
                            .into());
                        } // no else, skip this line in non-strict mode
                    }
                };
            }
            Some(("PIXASPECT", val)) => {
                match val.trim().parse::<f32>() {
                    Ok(v) => {
                        self.pixel_aspect_ratio = Some(self.pixel_aspect_ratio.unwrap_or(1.0) * v);
                        // all encountered exposure values should be multiplied
                    }
                    Err(parse_error) => {
                        if strict {
                            return Err(DecoderError::UnparsableF32(
                                LineType::Pixaspect,
                                parse_error,
                            )
                            .into());
                        } // no else, skip this line in non-strict mode
                    }
                };
            }
            Some(("COLORCORR", val)) => {
                let mut rgbcorr = [1.0, 1.0, 1.0];
                match parse_space_separated_f32(val, &mut rgbcorr, LineType::Colorcorr) {
                    Ok(extra_numbers) => {
                        if strict && extra_numbers {
                            return Err(DecoderError::ExtraneousColorcorrNumbers.into());
                        } // no else, just ignore extra numbers
                        let (rc, gc, bc) = self.color_correction.unwrap_or((1.0, 1.0, 1.0));
                        self.color_correction =
                            Some((rc * rgbcorr[0], gc * rgbcorr[1], bc * rgbcorr[2]));
                    }
                    Err(err) => {
                        if strict {
                            return Err(err);
                        } // no else, skip malformed line in non-strict mode
                    }
                }
            }
            None => {
                // old Radiance HDR files (*.pic) contain commands in a header
                // just skip them
            }
            _ => {
                // skip unknown attribute
            }
        } // match attributes
        Ok(())
    }
}

fn parse_space_separated_f32(line: &str, vals: &mut [f32], line_tp: LineType) -> ImageResult<bool> {
    let mut nums = line.split_whitespace();
    for val in vals.iter_mut() {
        if let Some(num) = nums.next() {
            match num.parse::<f32>() {
                Ok(v) => *val = v,
                Err(err) => return Err(DecoderError::UnparsableF32(line_tp, err).into()),
            }
        } else {
            // not enough numbers in line
            return Err(DecoderError::LineTooShort(line_tp).into());
        }
    }
    Ok(nums.next().is_some())
}

// Parses dimension line "-Y height +X width"
// returns (width, height) or error
fn parse_dimensions_line(line: &str, strict: bool) -> ImageResult<(u32, u32)> {
    const DIMENSIONS_COUNT: usize = 4;

    let mut dim_parts = line.split_whitespace();
    let c1_tag = dim_parts
        .next()
        .ok_or(DecoderError::DimensionsLineTooShort(0, DIMENSIONS_COUNT))?;
    let c1_str = dim_parts
        .next()
        .ok_or(DecoderError::DimensionsLineTooShort(1, DIMENSIONS_COUNT))?;
    let c2_tag = dim_parts
        .next()
        .ok_or(DecoderError::DimensionsLineTooShort(2, DIMENSIONS_COUNT))?;
    let c2_str = dim_parts
        .next()
        .ok_or(DecoderError::DimensionsLineTooShort(3, DIMENSIONS_COUNT))?;
    if strict && dim_parts.next().is_some() {
        // extra data in dimensions line
        return Err(DecoderError::DimensionsLineTooLong(DIMENSIONS_COUNT).into());
    } // no else
      // dimensions line is in the form "-Y 10 +X 20"
      // There are 8 possible orientations: +Y +X, +X -Y and so on
    match (c1_tag, c2_tag) {
        ("-Y", "+X") => {
            // Common orientation (left-right, top-down)
            // c1_str is height, c2_str is width
            let height = c1_str
                .parse::<u32>()
                .map_err(|pe| DecoderError::UnparsableU32(LineType::DimensionsHeight, pe))?;
            let width = c2_str
                .parse::<u32>()
                .map_err(|pe| DecoderError::UnparsableU32(LineType::DimensionsWidth, pe))?;
            Ok((width, height))
        }
        _ => Err(ImageError::Unsupported(
            UnsupportedError::from_format_and_kind(
                ImageFormat::Hdr.into(),
                UnsupportedErrorKind::GenericFeature(format!(
                    "Orientation {} {}",
                    limit_string_len(c1_tag, 4),
                    limit_string_len(c2_tag, 4)
                )),
            ),
        )),
    } // final expression. Returns value
}

// Returns string with no more than len+3 characters
fn limit_string_len(s: &str, len: usize) -> String {
    let s_char_len = s.chars().count();
    if s_char_len > len {
        s.chars().take(len).chain("...".chars()).collect()
    } else {
        s.into()
    }
}

// Splits string into (before separator, after separator) tuple
// or None if separator isn't found
fn split_at_first<'a>(s: &'a str, separator: &str) -> Option<(&'a str, &'a str)> {
    match s.find(separator) {
        None | Some(0) => None,
        Some(p) if p >= s.len() - separator.len() => None,
        Some(p) => Some((&s[..p], &s[(p + separator.len())..])),
    }
}

#[test]
fn split_at_first_test() {
    assert_eq!(split_at_first(&Cow::Owned("".into()), "="), None);
    assert_eq!(split_at_first(&Cow::Owned("=".into()), "="), None);
    assert_eq!(split_at_first(&Cow::Owned("= ".into()), "="), None);
    assert_eq!(
        split_at_first(&Cow::Owned(" = ".into()), "="),
        Some((" ", " "))
    );
    assert_eq!(
        split_at_first(&Cow::Owned("EXPOSURE= ".into()), "="),
        Some(("EXPOSURE", " "))
    );
    assert_eq!(
        split_at_first(&Cow::Owned("EXPOSURE= =".into()), "="),
        Some(("EXPOSURE", " ="))
    );
    assert_eq!(
        split_at_first(&Cow::Owned("EXPOSURE== =".into()), "=="),
        Some(("EXPOSURE", " ="))
    );
    assert_eq!(split_at_first(&Cow::Owned("EXPOSURE".into()), ""), None);
}

// Reads input until b"\n" or EOF
// Returns vector of read bytes NOT including end of line characters
//   or return None to indicate end of file
fn read_line_u8<R: BufRead>(r: &mut R) -> ::std::io::Result<Option<Vec<u8>>> {
    let mut ret = Vec::with_capacity(16);
    match r.read_until(b'\n', &mut ret) {
        Ok(0) => Ok(None),
        Ok(_) => {
            if let Some(&b'\n') = ret[..].last() {
                let _ = ret.pop();
            }
            Ok(Some(ret))
        }
        Err(err) => Err(err),
    }
}

#[test]
fn read_line_u8_test() {
    let buf: Vec<_> = (&b"One\nTwo\nThree\nFour\n\n\n"[..]).into();
    let input = &mut ::std::io::Cursor::new(buf);
    assert_eq!(&read_line_u8(input).unwrap().unwrap()[..], &b"One"[..]);
    assert_eq!(&read_line_u8(input).unwrap().unwrap()[..], &b"Two"[..]);
    assert_eq!(&read_line_u8(input).unwrap().unwrap()[..], &b"Three"[..]);
    assert_eq!(&read_line_u8(input).unwrap().unwrap()[..], &b"Four"[..]);
    assert_eq!(&read_line_u8(input).unwrap().unwrap()[..], &b""[..]);
    assert_eq!(&read_line_u8(input).unwrap().unwrap()[..], &b""[..]);
    assert_eq!(read_line_u8(input).unwrap(), None);
}

/// Helper function for reading raw 3-channel f32 images
pub fn read_raw_file<P: AsRef<Path>>(path: P) -> ::std::io::Result<Vec<Rgb<f32>>> {
    use byteorder::{LittleEndian as LE, ReadBytesExt};
    use std::fs::File;
    use std::io::BufReader;

    let mut r = BufReader::new(File::open(path)?);
    let w = r.read_u32::<LE>()? as usize;
    let h = r.read_u32::<LE>()? as usize;
    let c = r.read_u32::<LE>()? as usize;
    assert_eq!(c, 3);
    let cnt = w * h;
    let mut ret = Vec::with_capacity(cnt);
    for _ in 0..cnt {
        let cr = r.read_f32::<LE>()?;
        let cg = r.read_f32::<LE>()?;
        let cb = r.read_f32::<LE>()?;
        ret.push(Rgb([cr, cg, cb]));
    }
    Ok(ret)
}

#[cfg(test)]
mod test {
    use super::*;
    use std::io::Cursor;

    #[test]
    fn dimension_overflow() {
        let data = b"#?RADIANCE\nFORMAT=32-bit_rle_rgbe\n\n -Y 4294967295 +X 4294967295";

        assert!(HdrAdapter::new(Cursor::new(data)).is_err());
        assert!(HdrAdapter::new_nonstrict(Cursor::new(data)).is_err());
    }
}
