use byteorder::{LittleEndian, ReadBytesExt};
use std::convert::TryFrom;
use std::io::{self, Cursor, Read};
use std::marker::PhantomData;
use std::{error, fmt, mem};

use crate::error::{
    DecodingError, ImageError, ImageResult, UnsupportedError, UnsupportedErrorKind,
};
use crate::image::{ImageDecoder, ImageFormat};

use crate::color;

use super::lossless::LosslessDecoder;
use super::lossless::LosslessFrame;
use super::vp8::Frame as VP8Frame;
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
        }
    }
}

impl From<DecoderError> for ImageError {
    fn from(e: DecoderError) -> ImageError {
        ImageError::Decoding(DecodingError::new(ImageFormat::WebP.into(), e))
    }
}

impl error::Error for DecoderError {}

enum Frame {
    Lossy(VP8Frame),
    Lossless(LosslessFrame),
}

/// WebP Image format decoder. Currently only supports lossy RGB images or lossless RGBA images.
pub struct WebPDecoder<R> {
    r: R,
    frame: Frame,
}

impl<R: Read> WebPDecoder<R> {
    /// Create a new WebPDecoder from the Reader ```r```.
    /// This function takes ownership of the Reader.
    pub fn new(r: R) -> ImageResult<WebPDecoder<R>> {
        let frame = Frame::Lossy(Default::default());

        let mut decoder = WebPDecoder { r, frame };
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
    fn read_frame(&mut self) -> ImageResult<Frame> {
        loop {
            let mut chunk = [0; 4];
            self.r.read_exact(&mut chunk)?;

            match &chunk {
                b"VP8 " => {
                    let m = read_len_cursor(&mut self.r)?;

                    let mut vp8_decoder = Vp8Decoder::new(m);
                    let frame = vp8_decoder.decode_frame()?;

                    return Ok(Frame::Lossy(frame.clone()));
                }
                b"VP8L" => {
                    let m = read_len_cursor(&mut self.r)?;

                    let mut lossless_decoder = LosslessDecoder::new(m);
                    let frame = lossless_decoder.decode_frame()?;

                    return Ok(Frame::Lossless(frame.clone()));
                }
                b"ALPH" | b"ANIM" | b"ANMF" => {
                    // Alpha and Animation isn't supported
                    return Err(ImageError::Unsupported(
                        UnsupportedError::from_format_and_kind(
                            ImageFormat::WebP.into(),
                            UnsupportedErrorKind::GenericFeature(
                                chunk.iter().map(|&b| b as char).collect(),
                            ),
                        ),
                    ));
                }
                _ => {
                    let mut len = u64::from(self.r.read_u32::<LittleEndian>()?);

                    if len % 2 != 0 {
                        // RIFF chunks containing an uneven number of bytes append
                        // an extra 0x00 at the end of the chunk
                        //
                        // The addition cannot overflow since we have a u64 that was created from a u32
                        len += 1;
                    }

                    io::copy(&mut self.r.by_ref().take(len), &mut io::sink())?;
                }
            }
        }
    }

    fn read_data(&mut self) -> ImageResult<()> {
        let _size = self.read_riff_header()?;

        let frame = self.read_frame()?;

        self.frame = frame;

        Ok(())
    }
}

fn read_len_cursor<R>(r: &mut R) -> ImageResult<Cursor<Vec<u8>>>
where
    R: Read,
{
    let len = r.read_u32::<LittleEndian>()?;

    let mut framedata = Vec::new();
    r.by_ref().take(len as u64).read_to_end(&mut framedata)?;
    Ok(io::Cursor::new(framedata))
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
        match &self.frame {
            Frame::Lossy(vp8_frame) => (u32::from(vp8_frame.width), u32::from(vp8_frame.height)),
            Frame::Lossless(lossless_frame) => (
                u32::from(lossless_frame.width),
                u32::from(lossless_frame.height),
            ),
        }
    }

    fn color_type(&self) -> color::ColorType {
        match &self.frame {
            Frame::Lossy(_) => color::ColorType::Rgb8,
            Frame::Lossless(_) => color::ColorType::Rgba8,
        }
    }

    fn into_reader(self) -> ImageResult<Self::Reader> {
        match &self.frame {
            Frame::Lossy(vp8_frame) => {
                let mut data = vec![0; vp8_frame.get_buf_size()];
                vp8_frame.fill_rgb(data.as_mut_slice());
                Ok(WebpReader(Cursor::new(data), PhantomData))
            }
            Frame::Lossless(lossless_frame) => {
                let mut data = vec![0; lossless_frame.get_buf_size()];
                lossless_frame.fill_rgba(data.as_mut_slice());
                Ok(WebpReader(Cursor::new(data), PhantomData))
            }
        }
    }

    fn read_image(self, buf: &mut [u8]) -> ImageResult<()> {
        assert_eq!(u64::try_from(buf.len()), Ok(self.total_bytes()));

        match &self.frame {
            Frame::Lossy(vp8_frame) => {
                vp8_frame.fill_rgb(buf);
            }
            Frame::Lossless(lossless_frame) => {
                lossless_frame.fill_rgba(buf);
            }
        }
        Ok(())
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
