extern crate crc32fast;
extern crate deflate;

use std::borrow::Cow;
use std::error;
use std::fmt;
use std::io::{self, Read, Write};
use std::mem;
use std::result;

use crc32fast::Hasher as Crc32;

use crate::chunk;
use crate::common::{BitDepth, BytesPerPixel, ColorType, Compression, Info};
use crate::filter::{filter, FilterType};
use crate::traits::WriteBytesExt;

pub type Result<T> = result::Result<T, EncodingError>;

#[derive(Debug)]
pub enum EncodingError {
    IoError(io::Error),
    Format(Cow<'static, str>),
}

impl error::Error for EncodingError {
    fn cause(&self) -> Option<&(dyn error::Error + 'static)> {
        match self {
            EncodingError::IoError(err) => Some(err),
            _ => None,
        }
    }
}

impl fmt::Display for EncodingError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> result::Result<(), fmt::Error> {
        use self::EncodingError::*;
        match self {
            IoError(err) => write!(fmt, "{}", err),
            Format(desc) => write!(fmt, "{}", desc),
        }
    }
}

impl From<io::Error> for EncodingError {
    fn from(err: io::Error) -> EncodingError {
        EncodingError::IoError(err)
    }
}
impl From<EncodingError> for io::Error {
    fn from(err: EncodingError) -> io::Error {
        io::Error::new(io::ErrorKind::Other, err.to_string())
    }
}

/// PNG Encoder
pub struct Encoder<W: Write> {
    w: W,
    info: Info,
}

impl<W: Write> Encoder<W> {
    pub fn new(w: W, width: u32, height: u32) -> Encoder<W> {
        let mut info = Info::default();
        info.width = width;
        info.height = height;
        Encoder { w, info }
    }

    pub fn set_palette(&mut self, palette: Vec<u8>) {
        self.info.palette = Some(palette);
    }

    pub fn set_trns(&mut self, trns: Vec<u8>) {
        self.info.trns = Some(trns);
    }

    pub fn write_header(self) -> Result<Writer<W>> {
        Writer::new(self.w, self.info).init()
    }

    /// Set the color of the encoded image.
    ///
    /// These correspond to the color types in the png IHDR data that will be written. The length
    /// of the image data that is later supplied must match the color type, otherwise an error will
    /// be emitted.
    pub fn set_color(&mut self, color: ColorType) {
        self.info.color_type = color;
    }

    /// Set the indicated depth of the image data.
    pub fn set_depth(&mut self, depth: BitDepth) {
        self.info.bit_depth = depth;
    }

    /// Set compression parameters.
    ///
    /// Accepts a `Compression` or any type that can transform into a `Compression`. Notably `deflate::Compression` and
    /// `deflate::CompressionOptions` which "just work".
    pub fn set_compression<C: Into<Compression>>(&mut self, compression: C) {
        self.info.compression = compression.into();
    }

    /// Set the used filter type.
    ///
    /// The default filter is [`FilterType::Sub`] which provides a basic prediction algorithm for
    /// sample values based on the previous. For a potentially better compression ratio, at the
    /// cost of more complex processing, try out [`FilterType::Paeth`].
    ///
    /// [`FilterType::Sub`]: enum.FilterType.html#variant.Sub
    /// [`FilterType::Paeth`]: enum.FilterType.html#variant.Paeth
    pub fn set_filter(&mut self, filter: FilterType) {
        self.info.filter = filter;
    }
}

/// PNG writer
pub struct Writer<W: Write> {
    w: W,
    info: Info,
}

const DEFAULT_BUFFER_LENGTH: usize = 4 * 1024;

fn write_chunk<W: Write>(mut w: W, name: [u8; 4], data: &[u8]) -> Result<()> {
    w.write_be(data.len() as u32)?;
    w.write_all(&name)?;
    w.write_all(data)?;
    let mut crc = Crc32::new();
    crc.update(&name);
    crc.update(data);
    w.write_be(crc.finalize())?;
    Ok(())
}

impl<W: Write> Writer<W> {
    fn new(w: W, info: Info) -> Writer<W> {
        Writer { w, info }
    }

    fn init(mut self) -> Result<Self> {
        if self.info.width == 0 {
            return Err(EncodingError::Format("Zero width not allowed".into()));
        }

        if self.info.height == 0 {
            return Err(EncodingError::Format("Zero height not allowed".into()));
        }

        // TODO: this could yield the typified BytesPerPixel.
        if self
            .info
            .color_type
            .is_combination_invalid(self.info.bit_depth)
        {
            return Err(EncodingError::Format(
                format!(
                    "Invalid combination of bit-depth '{:?}' and color-type '{:?}'",
                    self.info.bit_depth, self.info.color_type
                )
                .into(),
            ));
        }

        self.w.write_all(&[137, 80, 78, 71, 13, 10, 26, 10])?;
        let mut data = [0; 13];
        (&mut data[..]).write_be(self.info.width)?;
        (&mut data[4..]).write_be(self.info.height)?;
        data[8] = self.info.bit_depth as u8;
        data[9] = self.info.color_type as u8;
        data[12] = if self.info.interlaced { 1 } else { 0 };
        self.write_chunk(chunk::IHDR, &data)?;

        if let Some(p) = &self.info.palette {
            write_chunk(&mut self.w, chunk::PLTE, p)?;
        };

        if let Some(t) = &self.info.trns {
            write_chunk(&mut self.w, chunk::tRNS, t)?;
        }

        Ok(self)
    }

    pub fn write_chunk(&mut self, name: [u8; 4], data: &[u8]) -> Result<()> {
        write_chunk(&mut self.w, name, data)
    }

    /// Writes the image data.
    pub fn write_image_data(&mut self, data: &[u8]) -> Result<()> {
        const MAX_CHUNK_LEN: u32 = (1u32 << 31) - 1;

        if self.info.color_type == ColorType::Indexed && self.info.palette.is_none() {
            return Err(EncodingError::Format(
                "can't write indexed image without palette".into(),
            ));
        }

        let bpp = self.info.bpp_in_prediction();
        let in_len = self.info.raw_row_length() - 1;
        let prev = vec![0; in_len];
        let mut prev = prev.as_slice();
        let mut current = vec![0; in_len];
        let data_size = in_len * self.info.height as usize;
        if data_size != data.len() {
            let message = format!("wrong data size, expected {} got {}", data_size, data.len());
            return Err(EncodingError::Format(message.into()));
        }
        let mut zlib = deflate::write::ZlibEncoder::new(Vec::new(), self.info.compression.clone());
        let filter_method = self.info.filter;
        for line in data.chunks(in_len) {
            current.copy_from_slice(&line);
            zlib.write_all(&[filter_method as u8])?;
            filter(filter_method, bpp, &prev, &mut current);
            zlib.write_all(&current)?;
            prev = line;
        }
        let zlib_encoded = zlib.finish()?;
        for chunk in zlib_encoded.chunks(MAX_CHUNK_LEN as usize) {
            self.write_chunk(chunk::IDAT, &chunk)?;
        }
        Ok(())
    }

    /// Create an stream writer.
    ///
    /// This allows you create images that do not fit in memory. The default
    /// chunk size is 4K, use `stream_writer_with_size` to set another chuck
    /// size.
    ///
    /// This borrows the writer. This preserves it which allows manually
    /// appending additional chunks after the image data has been written
    pub fn stream_writer(&mut self) -> StreamWriter<W> {
        self.stream_writer_with_size(DEFAULT_BUFFER_LENGTH)
    }

    /// Create a stream writer with custom buffer size.
    ///
    /// See [`stream_writer`].
    ///
    /// [`stream_writer`]: #fn.stream_writer
    pub fn stream_writer_with_size(&mut self, size: usize) -> StreamWriter<W> {
        StreamWriter::new(ChunkOutput::Borrowed(self), size)
    }

    /// Turn this into a stream writer for image data.
    ///
    /// This allows you create images that do not fit in memory. The default
    /// chunk size is 4K, use `stream_writer_with_size` to set another chuck
    /// size.
    pub fn into_stream_writer(self) -> StreamWriter<'static, W> {
        self.into_stream_writer_with_size(DEFAULT_BUFFER_LENGTH)
    }

    /// Turn this into a stream writer with custom buffer size.
    ///
    /// See [`into_stream_writer`].
    ///
    /// [`into_stream_writer`]: #fn.into_stream_writer
    pub fn into_stream_writer_with_size(self, size: usize) -> StreamWriter<'static, W> {
        StreamWriter::new(ChunkOutput::Owned(self), size)
    }
}

impl<W: Write> Drop for Writer<W> {
    fn drop(&mut self) {
        let _ = self.write_chunk(chunk::IEND, &[]);
    }
}

struct ChunkWriter<'a, W: Write> {
    writer: ChunkOutput<'a, W>,
    buffer: Vec<u8>,
    index: usize,
}

enum ChunkOutput<'a, W: Write> {
    Borrowed(&'a mut Writer<W>),
    Owned(Writer<W>),
}

impl<'a, W: Write> ChunkWriter<'a, W> {
    fn new(writer: ChunkOutput<'a, W>, buf_len: usize) -> ChunkWriter<'a, W> {
        ChunkWriter {
            writer,
            buffer: vec![0; buf_len],
            index: 0,
        }
    }
}

impl<'a, W: Write> AsMut<Writer<W>> for ChunkOutput<'a, W> {
    fn as_mut(&mut self) -> &mut Writer<W> {
        match self {
            ChunkOutput::Borrowed(writer) => writer,
            ChunkOutput::Owned(writer) => writer,
        }
    }
}

impl<'a, W: Write> Write for ChunkWriter<'a, W> {
    fn write(&mut self, mut buf: &[u8]) -> io::Result<usize> {
        let written = buf.read(&mut self.buffer[self.index..])?;
        self.index += written;

        if self.index + 1 >= self.buffer.len() {
            self.writer
                .as_mut()
                .write_chunk(chunk::IDAT, &self.buffer)?;
            self.index = 0;
        }

        Ok(written)
    }

    fn flush(&mut self) -> io::Result<()> {
        if self.index > 0 {
            self.writer
                .as_mut()
                .write_chunk(chunk::IDAT, &self.buffer[..=self.index])?;
        }
        self.index = 0;
        Ok(())
    }
}

impl<'a, W: Write> Drop for ChunkWriter<'a, W> {
    fn drop(&mut self) {
        let _ = self.flush();
    }
}

/// Streaming png writer
///
/// This may silently fail in the destructor, so it is a good idea to call
/// [`finish`](#method.finish) or [`flush`](https://doc.rust-lang.org/stable/std/io/trait.Write.html#tymethod.flush) before dropping.
pub struct StreamWriter<'a, W: Write> {
    writer: deflate::write::ZlibEncoder<ChunkWriter<'a, W>>,
    prev_buf: Vec<u8>,
    curr_buf: Vec<u8>,
    index: usize,
    bpp: BytesPerPixel,
    filter: FilterType,
}

impl<'a, W: Write> StreamWriter<'a, W> {
    fn new(mut writer: ChunkOutput<'a, W>, buf_len: usize) -> StreamWriter<'a, W> {
        let bpp = writer.as_mut().info.bpp_in_prediction();
        let in_len = writer.as_mut().info.raw_row_length() - 1;
        let filter = writer.as_mut().info.filter;
        let prev_buf = vec![0; in_len];
        let curr_buf = vec![0; in_len];

        let compression = writer.as_mut().info.compression.clone();
        let chunk_writer = ChunkWriter::new(writer, buf_len);
        let zlib = deflate::write::ZlibEncoder::new(chunk_writer, compression);

        StreamWriter {
            writer: zlib,
            index: 0,
            prev_buf,
            curr_buf,
            bpp,
            filter,
        }
    }

    pub fn finish(mut self) -> Result<()> {
        // TODO: call `writer.finish` somehow?
        self.flush()?;
        Ok(())
    }
}

impl<'a, W: Write> Write for StreamWriter<'a, W> {
    fn write(&mut self, mut buf: &[u8]) -> io::Result<usize> {
        let written = buf.read(&mut self.curr_buf[self.index..])?;
        self.index += written;

        if self.index >= self.curr_buf.len() {
            self.writer.write_all(&[self.filter as u8])?;
            filter(self.filter, self.bpp, &self.prev_buf, &mut self.curr_buf);
            self.writer.write_all(&self.curr_buf)?;
            mem::swap(&mut self.prev_buf, &mut self.curr_buf);
            self.index = 0;
        }

        Ok(written)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.writer.flush()?;
        if self.index > 0 {
            let message = format!("wrong data size, got {} bytes too many", self.index);
            return Err(EncodingError::Format(message.into()).into());
        }
        Ok(())
    }
}

impl<'a, W: Write> Drop for StreamWriter<'a, W> {
    fn drop(&mut self) {
        let _ = self.flush();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    extern crate glob;

    use rand::{thread_rng, Rng};
    use std::fs::File;
    use std::io::Write;
    use std::{cmp, io};

    #[test]
    fn roundtrip() {
        // More loops = more random testing, but also more test wait time
        for _ in 0..10 {
            for path in glob::glob("tests/pngsuite/*.png")
                .unwrap()
                .map(|r| r.unwrap())
            {
                if path.file_name().unwrap().to_str().unwrap().starts_with("x") {
                    // x* files are expected to fail to decode
                    continue;
                }
                eprintln!("{}", path.display());
                // Decode image
                let decoder = crate::Decoder::new(File::open(path).unwrap());
                let (info, mut reader) = decoder.read_info().unwrap();
                if info.line_size != 32 {
                    // TODO encoding only works with line size 32?
                    continue;
                }
                let mut buf = vec![0; info.buffer_size()];
                eprintln!("{:?}", info);
                reader.next_frame(&mut buf).unwrap();
                // Encode decoded image
                let mut out = Vec::new();
                {
                    let mut wrapper = RandomChunkWriter {
                        rng: thread_rng(),
                        w: &mut out,
                    };

                    let mut encoder = Encoder::new(&mut wrapper, info.width, info.height)
                        .write_header()
                        .unwrap();
                    encoder.write_image_data(&buf).unwrap();
                }
                // Decode encoded decoded image
                let decoder = crate::Decoder::new(&*out);
                let (info, mut reader) = decoder.read_info().unwrap();
                let mut buf2 = vec![0; info.buffer_size()];
                reader.next_frame(&mut buf2).unwrap();
                // check if the encoded image is ok:
                assert_eq!(buf, buf2);
            }
        }
    }

    #[test]
    fn roundtrip_stream() {
        // More loops = more random testing, but also more test wait time
        for _ in 0..10 {
            for path in glob::glob("tests/pngsuite/*.png")
                .unwrap()
                .map(|r| r.unwrap())
            {
                if path.file_name().unwrap().to_str().unwrap().starts_with("x") {
                    // x* files are expected to fail to decode
                    continue;
                }
                // Decode image
                let decoder = crate::Decoder::new(File::open(path).unwrap());
                let (info, mut reader) = decoder.read_info().unwrap();
                if info.line_size != 32 {
                    // TODO encoding only works with line size 32?
                    continue;
                }
                let mut buf = vec![0; info.buffer_size()];
                reader.next_frame(&mut buf).unwrap();
                // Encode decoded image
                let mut out = Vec::new();
                {
                    let mut wrapper = RandomChunkWriter {
                        rng: thread_rng(),
                        w: &mut out,
                    };

                    let mut encoder = Encoder::new(&mut wrapper, info.width, info.height)
                        .write_header()
                        .unwrap();
                    let mut stream_writer = encoder.stream_writer();

                    let mut outer_wrapper = RandomChunkWriter {
                        rng: thread_rng(),
                        w: &mut stream_writer,
                    };

                    outer_wrapper.write_all(&buf).unwrap();
                }
                // Decode encoded decoded image
                let decoder = crate::Decoder::new(&*out);
                let (info, mut reader) = decoder.read_info().unwrap();
                let mut buf2 = vec![0; info.buffer_size()];
                reader.next_frame(&mut buf2).unwrap();
                // check if the encoded image is ok:
                assert_eq!(buf, buf2);
            }
        }
    }

    #[test]
    fn image_palette() -> Result<()> {
        let samples = 3;
        for bit_depth in vec![1u8, 2, 4, 8] {
            // Do a reference decoding, choose a fitting palette image from pngsuite
            let path = format!("tests/pngsuite/basn3p0{}.png", bit_depth);
            let decoder = crate::Decoder::new(File::open(&path).unwrap());
            let (info, mut reader) = decoder.read_info().unwrap();

            let palette: Vec<u8> = reader.info().palette.clone().unwrap();
            let mut decoded_pixels = vec![0; info.buffer_size()];
            assert_eq!(
                info.width as usize * info.height as usize * samples,
                decoded_pixels.len()
            );
            reader.next_frame(&mut decoded_pixels).unwrap();

            let pixels_per_byte = 8 / usize::from(bit_depth);
            let mut indexed_data = vec![0; decoded_pixels.len() / samples];
            {
                // Retransform the image into palette bits.
                let mut indexes = vec![];
                for color in decoded_pixels.chunks(samples) {
                    let j = palette
                        .chunks(samples)
                        .position(|pcolor| color == pcolor)
                        .unwrap();
                    indexes.push(j as u8);
                }

                let idx_per_byte = indexes.chunks(pixels_per_byte);
                indexed_data.truncate(idx_per_byte.len());
                for (pixels, byte) in idx_per_byte.zip(&mut indexed_data) {
                    let mut shift = 8;
                    for idx in pixels {
                        shift -= bit_depth;
                        *byte = *byte | idx << shift;
                    }
                }
            };

            let mut out = Vec::new();
            {
                let mut encoder = Encoder::new(&mut out, info.width, info.height);
                encoder.set_depth(BitDepth::from_u8(bit_depth).unwrap());
                encoder.set_color(ColorType::Indexed);
                encoder.set_palette(palette.clone());

                let mut writer = encoder.write_header().unwrap();
                writer.write_image_data(&indexed_data).unwrap();
            }

            // Decode re-encoded image
            let decoder = crate::Decoder::new(&*out);
            let (info, mut reader) = decoder.read_info().unwrap();
            let mut redecoded = vec![0; info.buffer_size()];
            reader.next_frame(&mut redecoded).unwrap();
            // check if the encoded image is ok:
            assert_eq!(decoded_pixels, redecoded);
        }
        Ok(())
    }

    #[test]
    fn expect_error_on_wrong_image_len() -> Result<()> {
        use std::io::Cursor;

        let width = 10;
        let height = 10;

        let output = vec![0u8; 1024];
        let writer = Cursor::new(output);
        let mut encoder = Encoder::new(writer, width as u32, height as u32);
        encoder.set_depth(BitDepth::Eight);
        encoder.set_color(ColorType::RGB);
        let mut png_writer = encoder.write_header()?;

        let correct_image_size = width * height * 3;
        let image = vec![0u8; correct_image_size + 1];
        let result = png_writer.write_image_data(image.as_ref());
        assert!(result.is_err());

        Ok(())
    }

    #[test]
    fn expect_error_on_empty_image() -> Result<()> {
        use std::io::Cursor;

        let output = vec![0u8; 1024];
        let mut writer = Cursor::new(output);

        let encoder = Encoder::new(&mut writer, 0, 0);
        assert!(encoder.write_header().is_err());

        let encoder = Encoder::new(&mut writer, 100, 0);
        assert!(encoder.write_header().is_err());

        let encoder = Encoder::new(&mut writer, 0, 100);
        assert!(encoder.write_header().is_err());

        Ok(())
    }

    #[test]
    fn expect_error_on_invalid_bit_depth_color_type_combination() -> Result<()> {
        use std::io::Cursor;

        let output = vec![0u8; 1024];
        let mut writer = Cursor::new(output);

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::One);
        encoder.set_color(ColorType::RGB);
        assert!(encoder.write_header().is_err());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::One);
        encoder.set_color(ColorType::GrayscaleAlpha);
        assert!(encoder.write_header().is_err());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::One);
        encoder.set_color(ColorType::RGBA);
        assert!(encoder.write_header().is_err());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Two);
        encoder.set_color(ColorType::RGB);
        assert!(encoder.write_header().is_err());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Two);
        encoder.set_color(ColorType::GrayscaleAlpha);
        assert!(encoder.write_header().is_err());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Two);
        encoder.set_color(ColorType::RGBA);
        assert!(encoder.write_header().is_err());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Four);
        encoder.set_color(ColorType::RGB);
        assert!(encoder.write_header().is_err());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Four);
        encoder.set_color(ColorType::GrayscaleAlpha);
        assert!(encoder.write_header().is_err());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Four);
        encoder.set_color(ColorType::RGBA);
        assert!(encoder.write_header().is_err());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Sixteen);
        encoder.set_color(ColorType::Indexed);
        assert!(encoder.write_header().is_err());

        Ok(())
    }

    #[test]
    fn can_write_header_with_valid_bit_depth_color_type_combination() -> Result<()> {
        use std::io::Cursor;

        let output = vec![0u8; 1024];
        let mut writer = Cursor::new(output);

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::One);
        encoder.set_color(ColorType::Grayscale);
        assert!(encoder.write_header().is_ok());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::One);
        encoder.set_color(ColorType::Indexed);
        assert!(encoder.write_header().is_ok());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Two);
        encoder.set_color(ColorType::Grayscale);
        assert!(encoder.write_header().is_ok());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Two);
        encoder.set_color(ColorType::Indexed);
        assert!(encoder.write_header().is_ok());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Four);
        encoder.set_color(ColorType::Grayscale);
        assert!(encoder.write_header().is_ok());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Four);
        encoder.set_color(ColorType::Indexed);
        assert!(encoder.write_header().is_ok());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Eight);
        encoder.set_color(ColorType::Grayscale);
        assert!(encoder.write_header().is_ok());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Eight);
        encoder.set_color(ColorType::RGB);
        assert!(encoder.write_header().is_ok());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Eight);
        encoder.set_color(ColorType::Indexed);
        assert!(encoder.write_header().is_ok());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Eight);
        encoder.set_color(ColorType::GrayscaleAlpha);
        assert!(encoder.write_header().is_ok());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Eight);
        encoder.set_color(ColorType::RGBA);
        assert!(encoder.write_header().is_ok());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Sixteen);
        encoder.set_color(ColorType::Grayscale);
        assert!(encoder.write_header().is_ok());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Sixteen);
        encoder.set_color(ColorType::RGB);
        assert!(encoder.write_header().is_ok());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Sixteen);
        encoder.set_color(ColorType::GrayscaleAlpha);
        assert!(encoder.write_header().is_ok());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Sixteen);
        encoder.set_color(ColorType::RGBA);
        assert!(encoder.write_header().is_ok());

        Ok(())
    }

    #[test]
    fn all_filters_roundtrip() -> io::Result<()> {
        let pixel: Vec<_> = (0..48).collect();

        let roundtrip = |filter: FilterType| -> io::Result<()> {
            let mut buffer = vec![];
            let mut encoder = Encoder::new(&mut buffer, 4, 4);
            encoder.set_depth(BitDepth::Eight);
            encoder.set_color(ColorType::RGB);
            encoder.set_filter(filter);
            encoder.write_header()?.write_image_data(&pixel)?;

            let decoder = crate::Decoder::new(io::Cursor::new(buffer));
            let (info, mut reader) = decoder.read_info()?;
            assert_eq!(info.width, 4);
            assert_eq!(info.height, 4);
            let mut dest = vec![0; pixel.len()];
            reader.next_frame(&mut dest)?;
            assert_eq!(dest, pixel, "Deviation with filter type {:?}", filter);

            Ok(())
        };

        roundtrip(FilterType::NoFilter)?;
        roundtrip(FilterType::Sub)?;
        roundtrip(FilterType::Up)?;
        roundtrip(FilterType::Avg)?;
        roundtrip(FilterType::Paeth)?;

        Ok(())
    }

    /// A Writer that only writes a few bytes at a time
    struct RandomChunkWriter<'a, R: Rng, W: Write + 'a> {
        rng: R,
        w: &'a mut W,
    }

    impl<'a, R: Rng, W: Write + 'a> Write for RandomChunkWriter<'a, R, W> {
        fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
            // choose a random length to write
            let len = cmp::min(self.rng.gen_range(1, 50), buf.len());

            self.w.write(&buf[0..len])
        }

        fn flush(&mut self) -> io::Result<()> {
            self.w.flush()
        }
    }
}
