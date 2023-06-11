mod stream;
mod zlib;

pub use self::stream::{DecodeOptions, Decoded, DecodingError, StreamingDecoder};
use self::stream::{FormatErrorInner, CHUNCK_BUFFER_SIZE};

use std::io::{BufRead, BufReader, Read};
use std::mem;
use std::ops::Range;

use crate::chunk;
use crate::common::{
    BitDepth, BytesPerPixel, ColorType, Info, ParameterErrorKind, Transformations,
};
use crate::filter::{unfilter, FilterType};
use crate::utils;

/*
pub enum InterlaceHandling {
    /// Outputs the raw rows
    RawRows,
    /// Fill missing the pixels from the existing ones
    Rectangle,
    /// Only fill the needed pixels
    Sparkle
}
*/

/// Output info.
///
/// This describes one particular frame of the image that was written into the output buffer.
#[derive(Debug, PartialEq, Eq)]
pub struct OutputInfo {
    /// The pixel width of this frame.
    pub width: u32,
    /// The pixel height of this frame.
    pub height: u32,
    /// The chosen output color type.
    pub color_type: ColorType,
    /// The chosen output bit depth.
    pub bit_depth: BitDepth,
    /// The byte count of each scan line in the image.
    pub line_size: usize,
}

impl OutputInfo {
    /// Returns the size needed to hold a decoded frame
    /// If the output buffer was larger then bytes after this count should be ignored. They may
    /// still have been changed.
    pub fn buffer_size(&self) -> usize {
        self.line_size * self.height as usize
    }
}

#[derive(Clone, Copy, Debug)]
/// Limits on the resources the `Decoder` is allowed too use
pub struct Limits {
    /// maximum number of bytes the decoder is allowed to allocate, default is 64Mib
    pub bytes: usize,
}

impl Default for Limits {
    fn default() -> Limits {
        Limits {
            bytes: 1024 * 1024 * 64,
        }
    }
}

/// PNG Decoder
pub struct Decoder<R: Read> {
    read_decoder: ReadDecoder<R>,
    /// Output transformations
    transform: Transformations,
    /// Limits on resources the Decoder is allowed to use
    limits: Limits,
}

/// A row of data with interlace information attached.
#[derive(Clone, Copy, Debug)]
pub struct InterlacedRow<'data> {
    data: &'data [u8],
    interlace: InterlaceInfo,
}

impl<'data> InterlacedRow<'data> {
    pub fn data(&self) -> &'data [u8] {
        self.data
    }

    pub fn interlace(&self) -> InterlaceInfo {
        self.interlace
    }
}

/// PNG (2003) specifies two interlace modes, but reserves future extensions.
#[derive(Clone, Copy, Debug)]
pub enum InterlaceInfo {
    /// the null method means no interlacing
    Null,
    /// Adam7 derives its name from doing 7 passes over the image, only decoding a subset of all pixels in each pass.
    /// The following table shows pictorially what parts of each 8x8 area of the image is found in each pass:
    ///
    /// 1 6 4 6 2 6 4 6
    /// 7 7 7 7 7 7 7 7
    /// 5 6 5 6 5 6 5 6
    /// 7 7 7 7 7 7 7 7
    /// 3 6 4 6 3 6 4 6
    /// 7 7 7 7 7 7 7 7
    /// 5 6 5 6 5 6 5 6
    /// 7 7 7 7 7 7 7 7
    Adam7 { pass: u8, line: u32, width: u32 },
}

/// A row of data without interlace information.
#[derive(Clone, Copy, Debug)]
pub struct Row<'data> {
    data: &'data [u8],
}

impl<'data> Row<'data> {
    pub fn data(&self) -> &'data [u8] {
        self.data
    }
}

impl<R: Read> Decoder<R> {
    /// Create a new decoder configuration with default limits.
    pub fn new(r: R) -> Decoder<R> {
        Decoder::new_with_limits(r, Limits::default())
    }

    /// Create a new decoder configuration with custom limits.
    pub fn new_with_limits(r: R, limits: Limits) -> Decoder<R> {
        Decoder {
            read_decoder: ReadDecoder {
                reader: BufReader::with_capacity(CHUNCK_BUFFER_SIZE, r),
                decoder: StreamingDecoder::new(),
                at_eof: false,
            },
            transform: Transformations::IDENTITY,
            limits,
        }
    }

    /// Create a new decoder configuration with custom `DecodeOptions`.
    pub fn new_with_options(r: R, decode_options: DecodeOptions) -> Decoder<R> {
        Decoder {
            read_decoder: ReadDecoder {
                reader: BufReader::with_capacity(CHUNCK_BUFFER_SIZE, r),
                decoder: StreamingDecoder::new_with_options(decode_options),
                at_eof: false,
            },
            transform: Transformations::IDENTITY,
            limits: Limits::default(),
        }
    }

    /// Limit resource usage.
    ///
    /// Note that your allocations, e.g. when reading into a pre-allocated buffer, are __NOT__
    /// considered part of the limits. Nevertheless, required intermediate buffers such as for
    /// singular lines is checked against the limit.
    ///
    /// Note that this is a best-effort basis.
    ///
    /// ```
    /// use std::fs::File;
    /// use png::{Decoder, Limits};
    /// // This image is 32×32, 1bit per pixel. The reader buffers one row which requires 4 bytes.
    /// let mut limits = Limits::default();
    /// limits.bytes = 3;
    /// let mut decoder = Decoder::new_with_limits(File::open("tests/pngsuite/basi0g01.png").unwrap(), limits);
    /// assert!(decoder.read_info().is_err());
    ///
    /// // This image is 32x32 pixels, so the decoder will allocate less than 10Kib
    /// let mut limits = Limits::default();
    /// limits.bytes = 10*1024;
    /// let mut decoder = Decoder::new_with_limits(File::open("tests/pngsuite/basi0g01.png").unwrap(), limits);
    /// assert!(decoder.read_info().is_ok());
    /// ```
    pub fn set_limits(&mut self, limits: Limits) {
        self.limits = limits;
    }

    /// Read the PNG header and return the information contained within.
    ///
    /// Most image metadata will not be read until `read_info` is called, so those fields will be
    /// None or empty.
    pub fn read_header_info(&mut self) -> Result<&Info, DecodingError> {
        let mut buf = Vec::new();
        while self.read_decoder.info().is_none() {
            buf.clear();
            if self.read_decoder.decode_next(&mut buf)?.is_none() {
                return Err(DecodingError::Format(
                    FormatErrorInner::UnexpectedEof.into(),
                ));
            }
        }
        Ok(self.read_decoder.info().unwrap())
    }

    /// Reads all meta data until the first IDAT chunk
    pub fn read_info(mut self) -> Result<Reader<R>, DecodingError> {
        self.read_header_info()?;

        let mut reader = Reader {
            decoder: self.read_decoder,
            bpp: BytesPerPixel::One,
            subframe: SubframeInfo::not_yet_init(),
            fctl_read: 0,
            next_frame: SubframeIdx::Initial,
            prev: Vec::new(),
            current: Vec::new(),
            scan_start: 0,
            transform: self.transform,
            scratch_buffer: Vec::new(),
            limits: self.limits,
        };

        // Check if the decoding buffer of a single raw line has a valid size.
        if reader.info().checked_raw_row_length().is_none() {
            return Err(DecodingError::LimitsExceeded);
        }

        // Check if the output buffer has a valid size.
        let (width, height) = reader.info().size();
        let (color, depth) = reader.output_color_type();
        let rowlen = color
            .checked_raw_row_length(depth, width)
            .ok_or(DecodingError::LimitsExceeded)?
            - 1;
        let height: usize =
            std::convert::TryFrom::try_from(height).map_err(|_| DecodingError::LimitsExceeded)?;
        if rowlen.checked_mul(height).is_none() {
            return Err(DecodingError::LimitsExceeded);
        }

        reader.read_until_image_data()?;
        Ok(reader)
    }

    /// Set the allowed and performed transformations.
    ///
    /// A transformation is a pre-processing on the raw image data modifying content or encoding.
    /// Many options have an impact on memory or CPU usage during decoding.
    pub fn set_transformations(&mut self, transform: Transformations) {
        self.transform = transform;
    }

    /// Set the decoder to ignore all text chunks while parsing.
    ///
    /// eg.
    /// ```
    /// use std::fs::File;
    /// use png::Decoder;
    /// let mut decoder = Decoder::new(File::open("tests/pngsuite/basi0g01.png").unwrap());
    /// decoder.set_ignore_text_chunk(true);
    /// assert!(decoder.read_info().is_ok());
    /// ```
    pub fn set_ignore_text_chunk(&mut self, ignore_text_chunk: bool) {
        self.read_decoder
            .decoder
            .set_ignore_text_chunk(ignore_text_chunk);
    }

    /// Set the decoder to ignore and not verify the Adler-32 checksum
    /// and CRC code.
    pub fn ignore_checksums(&mut self, ignore_checksums: bool) {
        self.read_decoder
            .decoder
            .set_ignore_adler32(ignore_checksums);
        self.read_decoder.decoder.set_ignore_crc(ignore_checksums);
    }
}

struct ReadDecoder<R: Read> {
    reader: BufReader<R>,
    decoder: StreamingDecoder,
    at_eof: bool,
}

impl<R: Read> ReadDecoder<R> {
    /// Returns the next decoded chunk. If the chunk is an ImageData chunk, its contents are written
    /// into image_data.
    fn decode_next(&mut self, image_data: &mut Vec<u8>) -> Result<Option<Decoded>, DecodingError> {
        while !self.at_eof {
            let (consumed, result) = {
                let buf = self.reader.fill_buf()?;
                if buf.is_empty() {
                    return Err(DecodingError::Format(
                        FormatErrorInner::UnexpectedEof.into(),
                    ));
                }
                self.decoder.update(buf, image_data)?
            };
            self.reader.consume(consumed);
            match result {
                Decoded::Nothing => (),
                Decoded::ImageEnd => self.at_eof = true,
                result => return Ok(Some(result)),
            }
        }
        Ok(None)
    }

    fn finish_decoding(&mut self) -> Result<(), DecodingError> {
        while !self.at_eof {
            let buf = self.reader.fill_buf()?;
            if buf.is_empty() {
                return Err(DecodingError::Format(
                    FormatErrorInner::UnexpectedEof.into(),
                ));
            }
            let (consumed, event) = self.decoder.update(buf, &mut vec![])?;
            self.reader.consume(consumed);
            match event {
                Decoded::Nothing => (),
                Decoded::ImageEnd => self.at_eof = true,
                // ignore more data
                Decoded::ChunkComplete(_, _) | Decoded::ChunkBegin(_, _) | Decoded::ImageData => {}
                Decoded::ImageDataFlushed => return Ok(()),
                Decoded::PartialChunk(_) => {}
                new => unreachable!("{:?}", new),
            }
        }

        Err(DecodingError::Format(
            FormatErrorInner::UnexpectedEof.into(),
        ))
    }

    fn info(&self) -> Option<&Info> {
        self.decoder.info.as_ref()
    }
}

/// PNG reader (mostly high-level interface)
///
/// Provides a high level that iterates over lines or whole images.
pub struct Reader<R: Read> {
    decoder: ReadDecoder<R>,
    bpp: BytesPerPixel,
    subframe: SubframeInfo,
    /// Number of frame control chunks read.
    /// By the APNG specification the total number must equal the count specified in the animation
    /// control chunk. The IDAT image _may_ have such a chunk applying to it.
    fctl_read: u32,
    next_frame: SubframeIdx,
    /// Previous raw line
    prev: Vec<u8>,
    /// Current raw line
    current: Vec<u8>,
    /// Start index of the current scan line.
    scan_start: usize,
    /// Output transformations
    transform: Transformations,
    /// This buffer is only used so that `next_row` and `next_interlaced_row` can return reference
    /// to a byte slice. In a future version of this library, this buffer will be removed and
    /// `next_row` and `next_interlaced_row` will write directly into a user provided output buffer.
    scratch_buffer: Vec<u8>,
    /// How resources we can spend (for example, on allocation).
    limits: Limits,
}

/// The subframe specific information.
///
/// In APNG the frames are constructed by combining previous frame and a new subframe (through a
/// combination of `dispose_op` and `overlay_op`). These sub frames specify individual dimension
/// information and reuse the global interlace options. This struct encapsulates the state of where
/// in a particular IDAT-frame or subframe we are.
struct SubframeInfo {
    width: u32,
    height: u32,
    rowlen: usize,
    interlace: InterlaceIter,
    consumed_and_flushed: bool,
}

#[derive(Clone)]
enum InterlaceIter {
    None(Range<u32>),
    Adam7(utils::Adam7Iterator),
}

/// Denote a frame as given by sequence numbers.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum SubframeIdx {
    /// The initial frame in an IDAT chunk without fcTL chunk applying to it.
    /// Note that this variant precedes `Some` as IDAT frames precede fdAT frames and all fdAT
    /// frames must have a fcTL applying to it.
    Initial,
    /// An IDAT frame with fcTL or an fdAT frame.
    Some(u32),
    /// The past-the-end index.
    End,
}

impl<R: Read> Reader<R> {
    /// Reads all meta data until the next frame data starts.
    /// Requires IHDR before the IDAT and fcTL before fdAT.
    fn read_until_image_data(&mut self) -> Result<(), DecodingError> {
        loop {
            // This is somewhat ugly. The API requires us to pass a buffer to decode_next but we
            // know that we will stop before reading any image data from the stream. Thus pass an
            // empty buffer and assert that remains empty.
            let mut buf = Vec::new();
            let state = self.decoder.decode_next(&mut buf)?;
            assert!(buf.is_empty());

            match state {
                Some(Decoded::ChunkBegin(_, chunk::IDAT))
                | Some(Decoded::ChunkBegin(_, chunk::fdAT)) => break,
                Some(Decoded::FrameControl(_)) => {
                    self.subframe = SubframeInfo::new(self.info());
                    // The next frame is the one to which this chunk applies.
                    self.next_frame = SubframeIdx::Some(self.fctl_read);
                    // TODO: what about overflow here? That would imply there are more fctl chunks
                    // than can be specified in the animation control but also that we have read
                    // several gigabytes of data.
                    self.fctl_read += 1;
                }
                None => {
                    return Err(DecodingError::Format(
                        FormatErrorInner::MissingImageData.into(),
                    ))
                }
                // Ignore all other chunk events. Any other chunk may be between IDAT chunks, fdAT
                // chunks and their control chunks.
                _ => {}
            }
        }

        let info = self
            .decoder
            .info()
            .ok_or(DecodingError::Format(FormatErrorInner::MissingIhdr.into()))?;
        self.bpp = info.bpp_in_prediction();
        self.subframe = SubframeInfo::new(info);

        // Allocate output buffer.
        let buflen = self.output_line_size(self.subframe.width);
        if buflen > self.limits.bytes {
            return Err(DecodingError::LimitsExceeded);
        }

        self.prev.clear();
        self.prev.resize(self.subframe.rowlen, 0);

        Ok(())
    }

    /// Get information on the image.
    ///
    /// The structure will change as new frames of an animated image are decoded.
    pub fn info(&self) -> &Info {
        self.decoder.info().unwrap()
    }

    /// Decodes the next frame into `buf`.
    ///
    /// Note that this decodes raw subframes that need to be mixed according to blend-op and
    /// dispose-op by the caller.
    ///
    /// The caller must always provide a buffer large enough to hold a complete frame (the APNG
    /// specification restricts subframes to the dimensions given in the image header). The region
    /// that has been written be checked afterwards by calling `info` after a successful call and
    /// inspecting the `frame_control` data. This requirement may be lifted in a later version of
    /// `png`.
    ///
    /// Output lines will be written in row-major, packed matrix with width and height of the read
    /// frame (or subframe), all samples are in big endian byte order where this matters.
    pub fn next_frame(&mut self, buf: &mut [u8]) -> Result<OutputInfo, DecodingError> {
        let subframe_idx = match self.decoder.info().unwrap().frame_control() {
            None => SubframeIdx::Initial,
            Some(_) => SubframeIdx::Some(self.fctl_read - 1),
        };

        if self.next_frame == SubframeIdx::End {
            return Err(DecodingError::Parameter(
                ParameterErrorKind::PolledAfterEndOfImage.into(),
            ));
        } else if self.next_frame != subframe_idx {
            // Advance until we've read the info / fcTL for this frame.
            self.read_until_image_data()?;
        }

        if buf.len() < self.output_buffer_size() {
            return Err(DecodingError::Parameter(
                ParameterErrorKind::ImageBufferSize {
                    expected: buf.len(),
                    actual: self.output_buffer_size(),
                }
                .into(),
            ));
        }

        let (color_type, bit_depth) = self.output_color_type();
        let output_info = OutputInfo {
            width: self.subframe.width,
            height: self.subframe.height,
            color_type,
            bit_depth,
            line_size: self.output_line_size(self.subframe.width),
        };

        self.current.clear();
        self.scan_start = 0;
        let width = self.info().width;
        if self.info().interlaced {
            while let Some(InterlacedRow {
                data: row,
                interlace,
                ..
            }) = self.next_interlaced_row()?
            {
                let (line, pass) = match interlace {
                    InterlaceInfo::Adam7 { line, pass, .. } => (line, pass),
                    InterlaceInfo::Null => unreachable!("expected interlace information"),
                };
                let samples = color_type.samples() as u8;
                utils::expand_pass(buf, width, row, pass, line, samples * (bit_depth as u8));
            }
        } else {
            for row in buf
                .chunks_exact_mut(output_info.line_size)
                .take(self.subframe.height as usize)
            {
                self.next_interlaced_row_impl(self.subframe.rowlen, row)?;
            }
        }

        // Advance over the rest of data for this (sub-)frame.
        if !self.subframe.consumed_and_flushed {
            self.decoder.finish_decoding()?;
        }

        // Advance our state to expect the next frame.
        let past_end_subframe = self
            .info()
            .animation_control()
            .map(|ac| ac.num_frames)
            .unwrap_or(0);
        self.next_frame = match self.next_frame {
            SubframeIdx::End => unreachable!("Next frame called when already at image end"),
            // Reached the end of non-animated image.
            SubframeIdx::Initial if past_end_subframe == 0 => SubframeIdx::End,
            // An animated image, expecting first subframe.
            SubframeIdx::Initial => SubframeIdx::Some(0),
            // This was the last subframe, slightly fuzzy condition in case of programmer error.
            SubframeIdx::Some(idx) if past_end_subframe <= idx + 1 => SubframeIdx::End,
            // Expecting next subframe.
            SubframeIdx::Some(idx) => SubframeIdx::Some(idx + 1),
        };

        Ok(output_info)
    }

    /// Returns the next processed row of the image
    pub fn next_row(&mut self) -> Result<Option<Row>, DecodingError> {
        self.next_interlaced_row()
            .map(|v| v.map(|v| Row { data: v.data }))
    }

    /// Returns the next processed row of the image
    pub fn next_interlaced_row(&mut self) -> Result<Option<InterlacedRow>, DecodingError> {
        let (rowlen, interlace) = match self.next_pass() {
            Some((rowlen, interlace)) => (rowlen, interlace),
            None => return Ok(None),
        };

        let width = if let InterlaceInfo::Adam7 { width, .. } = interlace {
            width
        } else {
            self.subframe.width
        };
        let output_line_size = self.output_line_size(width);

        // TODO: change the interface of `next_interlaced_row` to take an output buffer instead of
        // making us return a reference to a buffer that we own.
        let mut output_buffer = mem::take(&mut self.scratch_buffer);
        output_buffer.resize(output_line_size, 0u8);
        let ret = self.next_interlaced_row_impl(rowlen, &mut output_buffer);
        self.scratch_buffer = output_buffer;
        ret?;

        Ok(Some(InterlacedRow {
            data: &self.scratch_buffer[..output_line_size],
            interlace,
        }))
    }

    /// Fetch the next interlaced row and filter it according to our own transformations.
    fn next_interlaced_row_impl(
        &mut self,
        rowlen: usize,
        output_buffer: &mut [u8],
    ) -> Result<(), DecodingError> {
        self.next_raw_interlaced_row(rowlen)?;
        let row = &self.prev[1..rowlen];

        // Apply transformations and write resulting data to buffer.
        let (color_type, bit_depth, trns) = {
            let info = self.info();
            (
                info.color_type,
                info.bit_depth as u8,
                info.trns.is_some() || self.transform.contains(Transformations::ALPHA),
            )
        };
        let expand = self.transform.contains(Transformations::EXPAND)
            || self.transform.contains(Transformations::ALPHA);
        let strip16 = bit_depth == 16 && self.transform.contains(Transformations::STRIP_16);
        let info = self.decoder.info().unwrap();
        let trns = if trns {
            Some(info.trns.as_deref())
        } else {
            None
        };
        match (color_type, trns) {
            (ColorType::Indexed, _) if expand => {
                output_buffer[..row.len()].copy_from_slice(row);
                expand_paletted(output_buffer, info, trns)?;
            }
            (ColorType::Grayscale | ColorType::GrayscaleAlpha, _) if bit_depth < 8 && expand => {
                output_buffer[..row.len()].copy_from_slice(row);
                expand_gray_u8(output_buffer, info, trns)
            }
            (ColorType::Grayscale | ColorType::Rgb, Some(trns)) if expand => {
                let channels = color_type.samples();
                if bit_depth == 8 {
                    utils::expand_trns_line(row, output_buffer, trns, channels);
                } else if strip16 {
                    utils::expand_trns_and_strip_line16(row, output_buffer, trns, channels);
                } else {
                    assert_eq!(bit_depth, 16);
                    utils::expand_trns_line16(row, output_buffer, trns, channels);
                }
            }
            (
                ColorType::Grayscale | ColorType::GrayscaleAlpha | ColorType::Rgb | ColorType::Rgba,
                _,
            ) if strip16 => {
                for i in 0..row.len() / 2 {
                    output_buffer[i] = row[2 * i];
                }
            }
            _ => output_buffer.copy_from_slice(row),
        }

        Ok(())
    }

    /// Returns the color type and the number of bits per sample
    /// of the data returned by `Reader::next_row` and Reader::frames`.
    pub fn output_color_type(&self) -> (ColorType, BitDepth) {
        use crate::common::ColorType::*;
        let t = self.transform;
        let info = self.info();
        if t == Transformations::IDENTITY {
            (info.color_type, info.bit_depth)
        } else {
            let bits = match info.bit_depth as u8 {
                16 if t.intersects(Transformations::STRIP_16) => 8,
                n if n < 8
                    && (t.contains(Transformations::EXPAND)
                        || t.contains(Transformations::ALPHA)) =>
                {
                    8
                }
                n => n,
            };
            let color_type =
                if t.contains(Transformations::EXPAND) || t.contains(Transformations::ALPHA) {
                    let has_trns = info.trns.is_some() || t.contains(Transformations::ALPHA);
                    match info.color_type {
                        Grayscale if has_trns => GrayscaleAlpha,
                        Rgb if has_trns => Rgba,
                        Indexed if has_trns => Rgba,
                        Indexed => Rgb,
                        ct => ct,
                    }
                } else {
                    info.color_type
                };
            (color_type, BitDepth::from_u8(bits).unwrap())
        }
    }

    /// Returns the number of bytes required to hold a deinterlaced image frame
    /// that is decoded using the given input transformations.
    pub fn output_buffer_size(&self) -> usize {
        let (width, height) = self.info().size();
        let size = self.output_line_size(width);
        size * height as usize
    }

    /// Returns the number of bytes required to hold a deinterlaced row.
    pub fn output_line_size(&self, width: u32) -> usize {
        let (color, depth) = self.output_color_type();
        color.raw_row_length_from_width(depth, width) - 1
    }

    fn next_pass(&mut self) -> Option<(usize, InterlaceInfo)> {
        match self.subframe.interlace {
            InterlaceIter::Adam7(ref mut adam7) => {
                let last_pass = adam7.current_pass();
                let (pass, line, width) = adam7.next()?;
                let rowlen = self.info().raw_row_length_from_width(width);
                if last_pass != pass {
                    self.prev.clear();
                    self.prev.resize(rowlen, 0u8);
                }
                Some((rowlen, InterlaceInfo::Adam7 { pass, line, width }))
            }
            InterlaceIter::None(ref mut height) => {
                let _ = height.next()?;
                Some((self.subframe.rowlen, InterlaceInfo::Null))
            }
        }
    }

    /// Write the next raw interlaced row into `self.prev`.
    ///
    /// The scanline is filtered against the previous scanline according to the specification.
    fn next_raw_interlaced_row(&mut self, rowlen: usize) -> Result<(), DecodingError> {
        // Read image data until we have at least one full row (but possibly more than one).
        while self.current.len() - self.scan_start < rowlen {
            if self.subframe.consumed_and_flushed {
                return Err(DecodingError::Format(
                    FormatErrorInner::NoMoreImageData.into(),
                ));
            }

            // Clear the current buffer before appending more data.
            if self.scan_start > 0 {
                self.current.drain(..self.scan_start).for_each(drop);
                self.scan_start = 0;
            }

            match self.decoder.decode_next(&mut self.current)? {
                Some(Decoded::ImageData) => {}
                Some(Decoded::ImageDataFlushed) => {
                    self.subframe.consumed_and_flushed = true;
                }
                None => {
                    return Err(DecodingError::Format(
                        if self.current.is_empty() {
                            FormatErrorInner::NoMoreImageData
                        } else {
                            FormatErrorInner::UnexpectedEndOfChunk
                        }
                        .into(),
                    ));
                }
                _ => (),
            }
        }

        // Get a reference to the current row and point scan_start to the next one.
        let row = &mut self.current[self.scan_start..];
        self.scan_start += rowlen;

        // Unfilter the row.
        let filter = FilterType::from_u8(row[0]).ok_or(DecodingError::Format(
            FormatErrorInner::UnknownFilterMethod(row[0]).into(),
        ))?;
        unfilter(filter, self.bpp, &self.prev[1..rowlen], &mut row[1..rowlen]);

        // Save the current row for the next pass.
        self.prev[..rowlen].copy_from_slice(&row[..rowlen]);

        Ok(())
    }
}

impl SubframeInfo {
    fn not_yet_init() -> Self {
        SubframeInfo {
            width: 0,
            height: 0,
            rowlen: 0,
            interlace: InterlaceIter::None(0..0),
            consumed_and_flushed: false,
        }
    }

    fn new(info: &Info) -> Self {
        // The apng fctnl overrides width and height.
        // All other data is set by the main info struct.
        let (width, height) = if let Some(fc) = info.frame_control {
            (fc.width, fc.height)
        } else {
            (info.width, info.height)
        };

        let interlace = if info.interlaced {
            InterlaceIter::Adam7(utils::Adam7Iterator::new(width, height))
        } else {
            InterlaceIter::None(0..height)
        };

        SubframeInfo {
            width,
            height,
            rowlen: info.raw_row_length_from_width(width),
            interlace,
            consumed_and_flushed: false,
        }
    }
}

fn expand_paletted(
    buffer: &mut [u8],
    info: &Info,
    trns: Option<Option<&[u8]>>,
) -> Result<(), DecodingError> {
    if let Some(palette) = info.palette.as_ref() {
        if let BitDepth::Sixteen = info.bit_depth {
            // This should have been caught earlier but let's check again. Can't hurt.
            Err(DecodingError::Format(
                FormatErrorInner::InvalidColorBitDepth {
                    color_type: ColorType::Indexed,
                    bit_depth: BitDepth::Sixteen,
                }
                .into(),
            ))
        } else {
            let black = [0, 0, 0];
            if let Some(trns) = trns {
                let trns = trns.unwrap_or(&[]);
                // > The tRNS chunk shall not contain more alpha values than there are palette
                // entries, but a tRNS chunk may contain fewer values than there are palette
                // entries. In this case, the alpha value for all remaining palette entries is
                // assumed to be 255.
                //
                // It seems, accepted reading is to fully *ignore* an invalid tRNS as if it were
                // completely empty / all pixels are non-transparent.
                let trns = if trns.len() <= palette.len() / 3 {
                    trns
                } else {
                    &[]
                };

                utils::unpack_bits(buffer, 4, info.bit_depth as u8, |i, chunk| {
                    let (rgb, a) = (
                        palette
                            .get(3 * i as usize..3 * i as usize + 3)
                            .unwrap_or(&black),
                        *trns.get(i as usize).unwrap_or(&0xFF),
                    );
                    chunk[0] = rgb[0];
                    chunk[1] = rgb[1];
                    chunk[2] = rgb[2];
                    chunk[3] = a;
                });
            } else {
                utils::unpack_bits(buffer, 3, info.bit_depth as u8, |i, chunk| {
                    let rgb = palette
                        .get(3 * i as usize..3 * i as usize + 3)
                        .unwrap_or(&black);
                    chunk[0] = rgb[0];
                    chunk[1] = rgb[1];
                    chunk[2] = rgb[2];
                })
            }
            Ok(())
        }
    } else {
        Err(DecodingError::Format(
            FormatErrorInner::PaletteRequired.into(),
        ))
    }
}

fn expand_gray_u8(buffer: &mut [u8], info: &Info, trns: Option<Option<&[u8]>>) {
    let rescale = true;
    let scaling_factor = if rescale {
        (255) / ((1u16 << info.bit_depth as u8) - 1) as u8
    } else {
        1
    };
    if let Some(trns) = trns {
        utils::unpack_bits(buffer, 2, info.bit_depth as u8, |pixel, chunk| {
            chunk[1] = if let Some(trns) = trns {
                if pixel == trns[0] {
                    0
                } else {
                    0xFF
                }
            } else {
                0xFF
            };
            chunk[0] = pixel * scaling_factor
        })
    } else {
        utils::unpack_bits(buffer, 1, info.bit_depth as u8, |val, chunk| {
            chunk[0] = val * scaling_factor
        })
    }
}

#[cfg(test)]
mod tests {
    use super::Decoder;
    use std::io::{BufRead, Read, Result};
    use std::mem::discriminant;

    /// A reader that reads at most `n` bytes.
    struct SmalBuf<R: BufRead> {
        inner: R,
        cap: usize,
    }

    impl<R: BufRead> SmalBuf<R> {
        fn new(inner: R, cap: usize) -> Self {
            SmalBuf { inner, cap }
        }
    }

    impl<R: BufRead> Read for SmalBuf<R> {
        fn read(&mut self, buf: &mut [u8]) -> Result<usize> {
            let len = buf.len().min(self.cap);
            self.inner.read(&mut buf[..len])
        }
    }

    impl<R: BufRead> BufRead for SmalBuf<R> {
        fn fill_buf(&mut self) -> Result<&[u8]> {
            let buf = self.inner.fill_buf()?;
            let len = buf.len().min(self.cap);
            Ok(&buf[..len])
        }

        fn consume(&mut self, amt: usize) {
            assert!(amt <= self.cap);
            self.inner.consume(amt)
        }
    }

    #[test]
    fn no_data_dup_on_finish() {
        const IMG: &[u8] = include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/bugfixes/x_issue#214.png"
        ));

        let mut normal = Decoder::new(IMG).read_info().unwrap();

        let mut buffer = vec![0; normal.output_buffer_size()];
        let normal = normal.next_frame(&mut buffer).unwrap_err();

        let smal = Decoder::new(SmalBuf::new(IMG, 1))
            .read_info()
            .unwrap()
            .next_frame(&mut buffer)
            .unwrap_err();

        assert_eq!(discriminant(&normal), discriminant(&smal));
    }
}
