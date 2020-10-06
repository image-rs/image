use byteorder::{LittleEndian, ReadBytesExt};
use std::convert::TryFrom;
use std::default::Default;
use std::{error, fmt, mem};
use std::io::{self, Cursor, Read};
use std::marker::PhantomData;

use crate::error::{DecodingError, ImageError, ImageResult, UnsupportedError, UnsupportedErrorKind};
use crate::image::{ImageDecoder, ImageFormat};

use crate::color;

use super::vp8::Frame;
use super::vp8::Vp8Decoder;

/// All errors that can occur when attempting to parse a WEBP container
#[derive(Debug, Clone, Copy)]
enum DecoderError {
    /// RIFF's "RIFF" signature not found or invalid
    RiffSignatureInvalid([u8; 4]),
    /// WebP's "WEBP" signature not found or invalid
    WebpSignatureInvalid([u8; 4]),
}

impl fmt::Display for DecoderError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        struct SignatureWriter([u8; 4]);
        impl fmt::Display for SignatureWriter {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "[{:#04X?}, {:#04X?}, {:#04X?}, {:#04X?}]", self.0[0], self.0[1], self.0[2], self.0[3])
            }
        }

        match self {
            DecoderError::RiffSignatureInvalid(riff) =>
                f.write_fmt(format_args!("Invalid RIFF signature: {}", SignatureWriter(*riff))),
            DecoderError::WebpSignatureInvalid(webp) =>
                f.write_fmt(format_args!("Invalid WebP signature: {}", SignatureWriter(*webp))),
        }
    }
}

impl From<DecoderError> for ImageError {
    fn from(e: DecoderError) -> ImageError {
        ImageError::Decoding(DecodingError::new(ImageFormat::WebP.into(), e))
    }
}

impl error::Error for DecoderError {}

/// WebP Image format decoder. Currently only supportes the luma channel (meaning that decoded
/// images will be grayscale).
pub struct WebPDecoder<R> {
    r: R,
    frame: Frame,
    have_frame: bool,
}

impl<R: Read> WebPDecoder<R> {
    /// Create a new WebPDecoder from the Reader ```r```.
    /// This function takes ownership of the Reader.
    pub fn new(r: R) -> ImageResult<WebPDecoder<R>> {
        let f: Frame = Default::default();

        let mut decoder = WebPDecoder {
            r,
            have_frame: false,
            frame: f,
        };
        decoder.read_metadata()?;
        Ok(decoder)
    }

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

    fn read_vp8_header(&mut self) -> ImageResult<u32> {
        loop {
            let mut chunk = [0; 4];
            self.r.read_exact(&mut chunk)?;

            match &chunk {
                b"VP8 " => {
                    let len = self.r.read_u32::<LittleEndian>()?;
                    return Ok(len);
                }
                b"ALPH" | b"VP8L" | b"ANIM" | b"ANMF" => {
                    // Alpha, Lossless and Animation isn't supported
                    return Err(ImageError::Unsupported(UnsupportedError::from_format_and_kind(
                        ImageFormat::WebP.into(),
                        UnsupportedErrorKind::GenericFeature(chunk.iter().map(|&b| b as char).collect()),
                    )));
                }
                _ => {
                    let mut len = self.r.read_u32::<LittleEndian>()?;
                    if len % 2 != 0 {
                        // RIFF chunks containing an uneven number of bytes append
                        // an extra 0x00 at the end of the chunk
                        len += 1;
                    }
                    io::copy(&mut self.r.by_ref().take(len as u64), &mut io::sink())?;
                }
            }
        }
    }

    fn read_frame(&mut self, len: u32) -> ImageResult<()> {
        let mut framedata = Vec::new();
        self.r.by_ref().take(len as u64).read_to_end(&mut framedata)?;
        let m = io::Cursor::new(framedata);

        let mut v = Vp8Decoder::new(m);
        let frame = v.decode_frame()?;

        self.frame = frame.clone();

        Ok(())
    }

    fn read_metadata(&mut self) -> ImageResult<()> {
        if !self.have_frame {
            self.read_riff_header()?;
            let len = self.read_vp8_header()?;
            self.read_frame(len)?;

            self.have_frame = true;
        }

        Ok(())
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
        (u32::from(self.frame.width), u32::from(self.frame.height))
    }

    fn color_type(&self) -> color::ColorType {
        color::ColorType::L8
    }

    fn into_reader(self) -> ImageResult<Self::Reader> {
        Ok(WebpReader(Cursor::new(self.frame.ybuf), PhantomData))
    }

    fn read_image(self, buf: &mut [u8]) -> ImageResult<()> {
        assert_eq!(u64::try_from(buf.len()), Ok(self.total_bytes()));
        buf.copy_from_slice(&self.frame.ybuf);
        Ok(())
    }
}
