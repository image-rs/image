use byteorder::{LittleEndian, ReadBytesExt};
use std::convert::TryFrom;
use std::{error, fmt, mem};
use std::io::{self, Cursor, Read};
use std::marker::PhantomData;

use crate::error::{DecodingError, ImageError, ImageResult, UnsupportedError, UnsupportedErrorKind};
use crate::image::{ImageDecoder, ImageFormat};

use crate::color;

use super::lossless::LosslessDecoder;
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

enum InnerDecoder<R: Read> {
    Lossy(Vp8Decoder<R>),
    Lossless(LosslessDecoder<R>),
}

/// WebP Image format decoder. Currently only supports lossy RGB images.
pub struct WebPDecoder<R: Read> {
    inner_decoder: InnerDecoder<R>,
}

impl<R: Read> WebPDecoder<R> {
    /// Create a new WebPDecoder from the Reader ```r```.
    /// This function takes ownership of the Reader.
    pub fn new(mut r: R) -> ImageResult<WebPDecoder<R>> {
        
        let _size = WebPDecoder::read_file_header(&mut r)?;

        let inner_decoder = WebPDecoder::read_frame(r)?;
        //let len = r.read_u32::<LittleEndian>()?;
        
        let decoder = WebPDecoder {
            inner_decoder,
        };
        //decoder.read_metadata()?;
        Ok(decoder)
    }

    //reads the 12 bytes of the WebP file header
    fn read_file_header(r: &mut R) -> ImageResult<u32> {
        let mut riff = [0; 4];
        r.read_exact(&mut riff)?;
        if &riff != b"RIFF" {
            return Err(DecoderError::RiffSignatureInvalid(riff).into());
        }

        let size = r.read_u32::<LittleEndian>()?;

        let mut webp = [0; 4];
        r.read_exact(&mut webp)?;
        if &webp != b"WEBP" {
            return Err(DecoderError::WebpSignatureInvalid(webp).into());
        }

        Ok(size)
    }

    //reads the chunk header, decodes the frame and returns the inner decoder
    fn read_frame(mut r: R) -> ImageResult<InnerDecoder<R>> {
        let mut chunk = [0; 4];
        r.read_exact(&mut chunk)?;

        match &chunk {
            b"VP8 " => {
                let _len = r.read_u32::<LittleEndian>()?;
                let mut vp8_decoder = Vp8Decoder::new(r);
                vp8_decoder.decode_frame()?;

                return Ok(InnerDecoder::Lossy(vp8_decoder));
            }
            b"VP8L" => {
                let mut lossless_decoder = LosslessDecoder::new(r);
                
                lossless_decoder.decode_frame()?;
                
                return Ok(InnerDecoder::Lossless(lossless_decoder));
            }
            //b"ALPH" | b"ANIM" | b"ANMF" => {
            _ => {
                // Alpha and Animation isn't supported
                return Err(ImageError::Unsupported(UnsupportedError::from_format_and_kind(
                    ImageFormat::WebP.into(),
                    UnsupportedErrorKind::GenericFeature(chunk.iter().map(|&b| b as char).collect()),
                )));
            }
        }
    }

    /* fn read_length(&mut self) -> ImageResult<u32> {
        let len = self.r.read_u32::<LittleEndian>()?;
        return Ok(len);
    } */

    /* fn read_vp8_frame(&mut self, len: u32) -> ImageResult<()> {
        let mut framedata = Vec::new();
        self.r.by_ref().take(len as u64).read_to_end(&mut framedata)?;
        let m = io::Cursor::new(framedata);

        let mut v = Vp8Decoder::new(m);
        let frame = v.decode_frame()?;

        self.frame = frame.clone();

        Ok(())
    } */
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
        match self.inner_decoder {
            InnerDecoder::Lossy(ref vp8_decoder) => vp8_decoder.dimensions(),
            InnerDecoder::Lossless(ref lossless_decoder) => lossless_decoder.dimensions(),
        }
    }

    fn color_type(&self) -> color::ColorType {
        match self.inner_decoder {
            InnerDecoder::Lossy(_) => color::ColorType::Rgb8,
            InnerDecoder::Lossless(_) => color::ColorType::Rgba8,
        }
        
    }

    fn into_reader(self) -> ImageResult<Self::Reader> {
        match self.inner_decoder {
            InnerDecoder::Lossy(ref vp8_decoder) => {
                let mut data = vec![0; vp8_decoder.get_buf_size()];
                vp8_decoder.fill_rgb(data.as_mut_slice());
                Ok(WebpReader(Cursor::new(data), PhantomData))
            }
            InnerDecoder::Lossless(ref lossless_decoder) => {
                let mut data = vec![0; lossless_decoder.get_buf_size()];
                lossless_decoder.fill_rgba(data.as_mut_slice());
                Ok(WebpReader(Cursor::new(data), PhantomData))
            }
        }
    }

    fn read_image(self, buf: &mut [u8]) -> ImageResult<()> {
        assert_eq!(u64::try_from(buf.len()), Ok(self.total_bytes()));

        match self.inner_decoder {
            InnerDecoder::Lossy(ref vp8_decoder) => {
                vp8_decoder.fill_rgb(buf);
            }
            InnerDecoder::Lossless(ref lossless_decoder) => {
                lossless_decoder.fill_rgba(buf);
            }
        }
        Ok(())
    }
}
