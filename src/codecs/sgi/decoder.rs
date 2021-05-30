use crate::{
    color::{ColorType, ExtendedColorType},
    error::{DecodingError, ImageError, ImageResult, UnsupportedError, UnsupportedErrorKind},
    image::{ImageDecoder, ImageFormat, ImageReadBuffer},
};
use byteorder::{BigEndian, ReadBytesExt};
use std::io::{Read, Seek, SeekFrom};

#[derive(Debug, Clone, Copy)]
enum DecoderError {
    MagicMissing(u16),
    BpcInvalid(u8),
    BpcUnsupported,
    TooManyChannels(u16),
    DimensionUnsupported(u16),
}

impl std::error::Error for DecoderError {}

impl std::fmt::Display for DecoderError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DecoderError::MagicMissing(magic) => {
                f.write_fmt(format_args!("Expected magic 474, got {}", magic))
            }
            DecoderError::BpcInvalid(bpc) => {
                f.write_fmt(format_args!("BPC must be either 1 or 2, got {}", bpc))
            }
            DecoderError::BpcUnsupported => {
                f.write_fmt(format_args!("Two byte channels are not supported"))
            }
            DecoderError::TooManyChannels(channels) => {
                f.write_fmt(format_args!("While SGI supports more than 4 channels, there is no color format that may represent them so the max is 4, got {}", channels))
            }
            DecoderError::DimensionUnsupported(channels) => {
                f.write_fmt(format_args!("While SGI supports images of a single scanline, and/or a single channel, I'm not going to bother implementing it, this image has {} dimenions", channels))
            }
        }
    }
}

impl From<DecoderError> for ImageError {
    fn from(e: DecoderError) -> ImageError {
        ImageError::Decoding(DecodingError::new(ImageFormat::Sgi.into(), e))
    }
}

struct SgiHeader {
    storage_format: StorageFormat,
    bpc: u32,
    dimensions: u32,
    xsize: u32,
    ysize: u32,
    zsize: u32,
    pixmin: u32,
    pixmax: u32,
    imagename: String,
    colormap: u32,
}

pub struct SgiDecoder<R> {
    r: R,
    header: SgiHeader,
}

#[derive(PartialEq)]
enum StorageFormat {
    Verbatim,
    Rle,
}

impl<R: Read + Seek> SgiDecoder<R> {
    pub fn new(mut r: R) -> ImageResult<SgiDecoder<R>> {
        let decoder = SgiDecoder {
            header: Self::read_header(&mut r)?,
            r,
        };
        Ok(decoder)
    }

    fn read_header(r: &mut R) -> ImageResult<SgiHeader> {
        let magic: u16 = r.read_u16::<BigEndian>()?;
        if magic != 474 {
            return Err(DecoderError::MagicMissing(magic).into());
        }
        let storage_format = r.read_u8()?;
        let bpc = r.read_u8()?;
        if bpc == 2 {
            return Err(DecoderError::BpcUnsupported.into());
        }
        if bpc > 2 {
            return Err(DecoderError::BpcInvalid(bpc).into());
        }
        let dimensions = r.read_u16::<BigEndian>()?;
        if dimensions != 3 {
            return Err(DecoderError::DimensionUnsupported(dimensions).into());
        }

        let xsize = r.read_u16::<BigEndian>()?;
        let ysize = r.read_u16::<BigEndian>()?;
        let zsize = r.read_u16::<BigEndian>()?;
        if zsize > 4 {
            return Err(DecoderError::TooManyChannels(zsize).into());
        }
        let pixmin = r.read_u32::<BigEndian>()?;
        let pixmax = r.read_u32::<BigEndian>()?;
        let _ignored = r.read_u32::<BigEndian>()?;
        let mut i = 0;
        let mut imagename = String::new();
        loop {
            let c = r.read_u8()?;
            i = i + 1;
            if c == 0 {
                break;
            }
            imagename.push(c as char);
        }
        let mut bytes = vec![0; 80 - i];
        r.read_exact(&mut bytes)?;
        let colormap = r.read_u32::<BigEndian>()?;
        let mut _ignored = [0u8; 404usize];
        r.read_exact(&mut _ignored)?;
        Ok(SgiHeader {
            storage_format: if storage_format == 1 {
                StorageFormat::Rle
            } else {
                StorageFormat::Verbatim
            },
            bpc: bpc.into(),
            dimensions: dimensions.into(),
            xsize: xsize.into(),
            ysize: ysize.into(),
            zsize: zsize.into(),
            pixmin,
            pixmax,
            imagename,
            colormap,
        })
    }

    fn read_scanline(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        if self.header.storage_format == StorageFormat::Verbatim {
            let stride = self.header.zsize;
            let size = self.header.xsize * self.header.ysize;
            for i in 0..stride {
                for t in 0..size {
                    buf[(t * stride + i) as usize] = self.r.read_u8()?;
                }
            }
            Ok((size * stride) as usize)
        } else {
            let tablen = self.header.ysize * self.header.zsize;
            let mut starttab = vec![0u32; tablen as usize];
            let mut lentab = vec![0u32; tablen as usize];
            self.r.read_u32_into::<BigEndian>(&mut starttab)?;
            self.r.read_u32_into::<BigEndian>(&mut lentab)?;
            for z in 0..self.header.zsize {
                for y in 0..self.header.ysize {
                    let offset = (z * self.header.ysize + y) as usize;
                    let start = starttab[offset];
                    let len = lentab[offset];
                    self.r.seek(SeekFrom::Start(start as u64))?;
                    let mut i = 0;
                    let mut xoffset = 0u32;
                    loop {
                        let l = self.r.read_u8()? as u32;
                        i = i + 1;
                        if (l & 0x7f) == 0 {
                            break;
                        }
                        if l & 0x80 == 0x80 {
                            for x in 0..(l & 0x7f) {
                                buf[((self.header.ysize - 1 - y)
                                    * self.header.xsize
                                    * self.header.zsize
                                    + (xoffset + x) * self.header.zsize
                                    + z) as usize] = self.r.read_u8()?;
                                i = i + 1;
                            }
                            xoffset = xoffset + (l & 0x7f);
                        } else {
                            let c = self.r.read_u8()?;
                            i = i + 1;
                            for x in 0..l as u32 {
                                buf[((self.header.ysize - 1 - y)
                                    * self.header.xsize
                                    * self.header.zsize
                                    + (xoffset + x) * self.header.zsize
                                    + z) as usize] = c;
                            }
                            xoffset = xoffset + (l & 0x7f) as u32;
                        }
                    }
                }
            }
            Ok((self.header.xsize * self.header.ysize * self.header.zsize) as usize)
        }
    }
}

impl<'a, R: 'a + Read + Seek> ImageDecoder<'a> for SgiDecoder<R> {
    type Reader = SgiReader<R>;

    fn dimensions(&self) -> (u32, u32) {
        (self.header.xsize.into(), self.header.ysize.into())
    }
    fn color_type(&self) -> ColorType {
        match self.header.zsize {
            1 => match self.header.bpc {
                2 => ColorType::L16,
                _ => ColorType::L8,
            },
            2 => match self.header.bpc {
                2 => ColorType::La16,
                _ => ColorType::La8,
            },
            3 => match self.header.bpc {
                2 => ColorType::Rgb16,
                _ => ColorType::Rgb8,
            },
            4 => match self.header.bpc {
                2 => ColorType::Rgba16,
                _ => ColorType::Rgba8,
            },
            _ => ColorType::L8,
        }
    }

    fn into_reader(self) -> ImageResult<Self::Reader> {
        Ok(SgiReader {
            buffer: ImageReadBuffer::new(self.scanline_bytes(), self.total_bytes()),
            decoder: self,
        })
    }
}

pub struct SgiReader<R> {
    buffer: ImageReadBuffer,
    decoder: SgiDecoder<R>,
}

impl<R: Read + Seek> Read for SgiReader<R> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        let decoder = &mut self.decoder;
        self.buffer.read(buf, |buf| decoder.read_scanline(buf))
    }
}

#[cfg(test)]
mod tests {
    use crate::color::ColorType;
    use crate::image::{ImageDecoder, ImageFormat};
    use crate::io::Reader;
    use crate::sgi::SgiDecoder;
    use std::io::Read;
    #[test]
    fn test_header() {
        let reader = std::fs::File::open(std::path::PathBuf::from("./tests/images/sgi/cat.sgi"))
            .expect("Couldnt find test image");
        let decoder = SgiDecoder::new(reader).expect("Failed to read header");
        assert_eq!(decoder.dimensions(), (320, 240), "Dimensions invalid");
        assert_eq!(decoder.color_type(), ColorType::Rgb8, "Color type invalid");
    }
}
