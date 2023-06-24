use byteorder::{LittleEndian, ReadBytesExt};
use std::convert::TryFrom;
use std::io::{self, Cursor, Error, Read};
use std::marker::PhantomData;
use std::{error, fmt, mem};

use crate::error::{DecodingError, ImageError, ImageResult, ParameterError, ParameterErrorKind};
use crate::image::{ImageDecoder, ImageFormat};
use crate::{color, AnimationDecoder, Frames, Rgba};

use super::lossless::{LosslessDecoder, LosslessFrame};
use super::vp8::{Frame as VP8Frame, Vp8Decoder};

use super::extended::{read_extended_header, ExtendedImage};

/// All errors that can occur when attempting to parse a WEBP container
#[derive(Debug, Clone, Copy)]
pub(crate) enum DecoderError {
    /// RIFF's "RIFF" signature not found or invalid
    RiffSignatureInvalid([u8; 4]),
    /// WebP's "WEBP" signature not found or invalid
    WebpSignatureInvalid([u8; 4]),
    /// Chunk Header was incorrect or invalid in its usage
    ChunkHeaderInvalid([u8; 4]),
}

impl fmt::Display for DecoderError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        struct SignatureWriter([u8; 4]);
        impl fmt::Display for SignatureWriter {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(
                    f,
                    "[{:#04X?}, {:#04X?}, {:#04X?}, {:#04X?}]",
                    self.0[0], self.0[1], self.0[2], self.0[3]
                )
            }
        }

        match self {
            DecoderError::RiffSignatureInvalid(riff) => f.write_fmt(format_args!(
                "Invalid RIFF signature: {}",
                SignatureWriter(*riff)
            )),
            DecoderError::WebpSignatureInvalid(webp) => f.write_fmt(format_args!(
                "Invalid WebP signature: {}",
                SignatureWriter(*webp)
            )),
            DecoderError::ChunkHeaderInvalid(header) => f.write_fmt(format_args!(
                "Invalid Chunk header: {}",
                SignatureWriter(*header)
            )),
        }
    }
}

impl From<DecoderError> for ImageError {
    fn from(e: DecoderError) -> ImageError {
        ImageError::Decoding(DecodingError::new(ImageFormat::WebP.into(), e))
    }
}

impl error::Error for DecoderError {}

/// All possible RIFF chunks in a WebP image file
#[allow(clippy::upper_case_acronyms)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum WebPRiffChunk {
    RIFF,
    WEBP,
    VP8,
    VP8L,
    VP8X,
    ANIM,
    ANMF,
    ALPH,
    ICCP,
    EXIF,
    XMP,
}

impl WebPRiffChunk {
    pub(crate) fn from_fourcc(chunk_fourcc: [u8; 4]) -> ImageResult<Self> {
        match &chunk_fourcc {
            b"RIFF" => Ok(Self::RIFF),
            b"WEBP" => Ok(Self::WEBP),
            b"VP8 " => Ok(Self::VP8),
            b"VP8L" => Ok(Self::VP8L),
            b"VP8X" => Ok(Self::VP8X),
            b"ANIM" => Ok(Self::ANIM),
            b"ANMF" => Ok(Self::ANMF),
            b"ALPH" => Ok(Self::ALPH),
            b"ICCP" => Ok(Self::ICCP),
            b"EXIF" => Ok(Self::EXIF),
            b"XMP " => Ok(Self::XMP),
            _ => Err(DecoderError::ChunkHeaderInvalid(chunk_fourcc).into()),
        }
    }

    pub(crate) fn to_fourcc(&self) -> [u8; 4] {
        match self {
            Self::RIFF => *b"RIFF",
            Self::WEBP => *b"WEBP",
            Self::VP8 => *b"VP8 ",
            Self::VP8L => *b"VP8L",
            Self::VP8X => *b"VP8X",
            Self::ANIM => *b"ANIM",
            Self::ANMF => *b"ANMF",
            Self::ALPH => *b"ALPH",
            Self::ICCP => *b"ICCP",
            Self::EXIF => *b"EXIF",
            Self::XMP => *b"XMP ",
        }
    }
}

enum WebPImage {
    Lossy(VP8Frame),
    Lossless(LosslessFrame),
    Extended(ExtendedImage),
}

/// WebP Image format decoder. Currently only supports lossy RGB images or lossless RGBA images.
pub struct WebPDecoder<R> {
    r: R,
    image: WebPImage,
}

impl<R: Read> WebPDecoder<R> {
    /// Create a new WebPDecoder from the Reader ```r```.
    /// This function takes ownership of the Reader.
    pub fn new(r: R) -> ImageResult<WebPDecoder<R>> {
        let image = WebPImage::Lossy(Default::default());

        let mut decoder = WebPDecoder { r, image };
        decoder.read_data()?;
        Ok(decoder)
    }

    //reads the 12 bytes of the WebP file header
    fn read_riff_header(&mut self) -> ImageResult<u32> {
        let mut riff = [0; 4];
        self.r.read_exact(&mut riff)?;
        if &riff != b"RIFF" {
            return Err(DecoderError::RiffSignatureInvalid(riff).into());
        }

        let size = self.r.read_u32::<LittleEndian>()?;

        let mut webp = [0; 4];
        self.r.read_exact(&mut webp)?;
        if &webp != b"WEBP" {
            return Err(DecoderError::WebpSignatureInvalid(webp).into());
        }

        Ok(size)
    }

    //reads the chunk header, decodes the frame and returns the inner decoder
    fn read_frame(&mut self) -> ImageResult<WebPImage> {
        let chunk = read_chunk(&mut self.r)?;

        match chunk {
            Some((cursor, WebPRiffChunk::VP8)) => {
                let mut vp8_decoder = Vp8Decoder::new(cursor);
                let frame = vp8_decoder.decode_frame()?;

                Ok(WebPImage::Lossy(frame.clone()))
            }
            Some((cursor, WebPRiffChunk::VP8L)) => {
                let mut lossless_decoder = LosslessDecoder::new(cursor);
                let frame = lossless_decoder.decode_frame()?;

                Ok(WebPImage::Lossless(frame.clone()))
            }
            Some((mut cursor, WebPRiffChunk::VP8X)) => {
                let info = read_extended_header(&mut cursor)?;

                let image = ExtendedImage::read_extended_chunks(&mut self.r, info)?;

                Ok(WebPImage::Extended(image))
            }
            None => Err(ImageError::IoError(Error::from(
                io::ErrorKind::UnexpectedEof,
            ))),
            Some((_, chunk)) => Err(DecoderError::ChunkHeaderInvalid(chunk.to_fourcc()).into()),
        }
    }

    fn read_data(&mut self) -> ImageResult<()> {
        let _size = self.read_riff_header()?;

        let image = self.read_frame()?;

        self.image = image;

        Ok(())
    }

    /// Returns true if the image as described by the bitstream is animated.
    pub fn has_animation(&self) -> bool {
        match &self.image {
            WebPImage::Lossy(_) => false,
            WebPImage::Lossless(_) => false,
            WebPImage::Extended(extended) => extended.has_animation(),
        }
    }

    /// Sets the background color if the image is an extended and animated webp.
    pub fn set_background_color(&mut self, color: Rgba<u8>) -> ImageResult<()> {
        match &mut self.image {
            WebPImage::Extended(image) => image.set_background_color(color),
            _ => Err(ImageError::Parameter(ParameterError::from_kind(
                ParameterErrorKind::Generic(
                    "Background color can only be set on animated webp".to_owned(),
                ),
            ))),
        }
    }
}

pub(crate) fn read_len_cursor<R>(r: &mut R) -> ImageResult<Cursor<Vec<u8>>>
where
    R: Read,
{
    let unpadded_len = u64::from(r.read_u32::<LittleEndian>()?);

    // RIFF chunks containing an uneven number of bytes append
    // an extra 0x00 at the end of the chunk
    //
    // The addition cannot overflow since we have a u64 that was created from a u32
    let len = unpadded_len + (unpadded_len % 2);

    let mut framedata = Vec::new();
    r.by_ref().take(len).read_to_end(&mut framedata)?;

    //remove padding byte
    if unpadded_len % 2 == 1 {
        framedata.pop();
    }

    Ok(io::Cursor::new(framedata))
}

/// Reads a chunk header FourCC
/// Returns None if and only if we hit end of file reading the four character code of the chunk
/// The inner error is `Err` if and only if the chunk header FourCC is present but unknown
pub(crate) fn read_fourcc<R: Read>(r: &mut R) -> ImageResult<Option<ImageResult<WebPRiffChunk>>> {
    let mut chunk_fourcc = [0; 4];
    let result = r.read_exact(&mut chunk_fourcc);

    match result {
        Ok(()) => {}
        Err(err) => {
            if err.kind() == io::ErrorKind::UnexpectedEof {
                return Ok(None);
            } else {
                return Err(err.into());
            }
        }
    }

    let chunk = WebPRiffChunk::from_fourcc(chunk_fourcc);
    Ok(Some(chunk))
}

/// Reads a chunk
/// Returns an error if the chunk header is not a valid webp header or some other reading error
/// Returns None if and only if we hit end of file reading the four character code of the chunk
pub(crate) fn read_chunk<R>(r: &mut R) -> ImageResult<Option<(Cursor<Vec<u8>>, WebPRiffChunk)>>
where
    R: Read,
{
    if let Some(chunk) = read_fourcc(r)? {
        let chunk = chunk?;
        let cursor = read_len_cursor(r)?;
        Ok(Some((cursor, chunk)))
    } else {
        Ok(None)
    }
}

/// Wrapper struct around a `Cursor<Vec<u8>>`
pub struct WebpReader<R>(Cursor<Vec<u8>>, PhantomData<R>);
impl<R> Read for WebpReader<R> {
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

impl<'a, R: 'a + Read> ImageDecoder<'a> for WebPDecoder<R> {
    type Reader = WebpReader<R>;

    fn dimensions(&self) -> (u32, u32) {
        match &self.image {
            WebPImage::Lossy(vp8_frame) => {
                (u32::from(vp8_frame.width), u32::from(vp8_frame.height))
            }
            WebPImage::Lossless(lossless_frame) => (
                u32::from(lossless_frame.width),
                u32::from(lossless_frame.height),
            ),
            WebPImage::Extended(extended) => extended.dimensions(),
        }
    }

    fn color_type(&self) -> color::ColorType {
        match &self.image {
            WebPImage::Lossy(_) => color::ColorType::Rgb8,
            WebPImage::Lossless(_) => color::ColorType::Rgba8,
            WebPImage::Extended(extended) => extended.color_type(),
        }
    }

    fn into_reader(self) -> ImageResult<Self::Reader> {
        match &self.image {
            WebPImage::Lossy(vp8_frame) => {
                let mut data = vec![0; vp8_frame.get_buf_size()];
                vp8_frame.fill_rgb(data.as_mut_slice());
                Ok(WebpReader(Cursor::new(data), PhantomData))
            }
            WebPImage::Lossless(lossless_frame) => {
                let mut data = vec![0; lossless_frame.get_buf_size()];
                lossless_frame.fill_rgba(data.as_mut_slice());
                Ok(WebpReader(Cursor::new(data), PhantomData))
            }
            WebPImage::Extended(extended) => {
                let mut data = vec![0; extended.get_buf_size()];
                extended.fill_buf(data.as_mut_slice());
                Ok(WebpReader(Cursor::new(data), PhantomData))
            }
        }
    }

    fn read_image(self, buf: &mut [u8]) -> ImageResult<()> {
        assert_eq!(u64::try_from(buf.len()), Ok(self.total_bytes()));

        match &self.image {
            WebPImage::Lossy(vp8_frame) => {
                vp8_frame.fill_rgb(buf);
            }
            WebPImage::Lossless(lossless_frame) => {
                lossless_frame.fill_rgba(buf);
            }
            WebPImage::Extended(extended) => {
                extended.fill_buf(buf);
            }
        }
        Ok(())
    }

    fn icc_profile(&mut self) -> Option<Vec<u8>> {
        if let WebPImage::Extended(extended) = &self.image {
            extended.icc_profile()
        } else {
            None
        }
    }
}

impl<'a, R: 'a + Read> AnimationDecoder<'a> for WebPDecoder<R> {
    fn into_frames(self) -> Frames<'a> {
        match self.image {
            WebPImage::Lossy(_) | WebPImage::Lossless(_) => {
                Frames::new(Box::new(std::iter::empty()))
            }
            WebPImage::Extended(extended_image) => extended_image.into_frames(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn add_with_overflow_size() {
        let bytes = vec![
            0x52, 0x49, 0x46, 0x46, 0xaf, 0x37, 0x80, 0x47, 0x57, 0x45, 0x42, 0x50, 0x6c, 0x64,
            0x00, 0x00, 0xff, 0xff, 0xff, 0xff, 0xfb, 0x7e, 0x73, 0x00, 0x06, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x05, 0x00, 0x00, 0x00, 0x65, 0x65, 0x65, 0x65, 0x65, 0x65,
            0x40, 0xfb, 0xff, 0xff, 0x65, 0x65, 0x65, 0x65, 0x65, 0x65, 0x65, 0x65, 0x65, 0x65,
            0x00, 0x00, 0x00, 0x00, 0x62, 0x00, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x49,
            0x49, 0x54, 0x55, 0x50, 0x4c, 0x54, 0x59, 0x50, 0x45, 0x33, 0x37, 0x44, 0x4d, 0x46,
        ];

        let data = std::io::Cursor::new(bytes);

        let _ = WebPDecoder::new(data);
    }
}
