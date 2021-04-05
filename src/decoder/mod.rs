mod stream;
mod zlib;

pub use self::stream::{Decoded, DecodingError, StreamingDecoder};
use self::stream::{FormatErrorInner, CHUNCK_BUFFER_SIZE};

use std::io::{BufRead, BufReader, Read, Write};
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
    /// Reader
    r: R,
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
            r,
            transform: Transformations::IDENTITY,
            limits,
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

    /// Reads all meta data until the first IDAT chunk
    pub fn read_info(self) -> Result<Reader<R>, DecodingError> {
        let mut reader = Reader::new(self.r, StreamingDecoder::new(), self.transform, self.limits);
        reader.init()?;

        let color_type = reader.info().color_type;
        let bit_depth = reader.info().bit_depth;
        if color_type.is_combination_invalid(bit_depth) {
            return Err(DecodingError::Format(
                FormatErrorInner::InvalidColorBitDepth {
                    color: color_type,
                    depth: bit_depth,
                }
                .into(),
            ));
        }

        // Check if the output buffer can be represented at all.
        if reader.checked_output_buffer_size().is_none() {
            return Err(DecodingError::LimitsExceeded);
        }

        Ok(reader)
    }

    /// Set the allowed and performed transformations.
    ///
    /// A transformation is a pre-processing on the raw image data modifying content or encoding.
    /// Many options have an impact on memory or CPU usage during decoding.
    pub fn set_transformations(&mut self, transform: Transformations) {
        self.transform = transform;
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

    fn finished_decoding(&mut self) -> Result<(), DecodingError> {
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
    /// Processed line
    processed: Vec<u8>,
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
    /// The info has not yet been decoded.
    Uninit,
    /// The initial frame in an IDAT chunk without fcTL chunk applying to it.
    /// Note that this variant precedes `Some` as IDAT frames precede fdAT frames and all fdAT
    /// frames must have a fcTL applying to it.
    Initial,
    /// An IDAT frame with fcTL or an fdAT frame.
    Some(u32),
    /// The past-the-end index.
    End,
}

macro_rules! get_info(
    ($this:expr) => {
        $this.decoder.info().unwrap()
    }
);

impl<R: Read> Reader<R> {
    /// Creates a new PNG reader
    fn new(r: R, d: StreamingDecoder, t: Transformations, limits: Limits) -> Reader<R> {
        Reader {
            decoder: ReadDecoder {
                reader: BufReader::with_capacity(CHUNCK_BUFFER_SIZE, r),
                decoder: d,
                at_eof: false,
            },
            bpp: BytesPerPixel::One,
            subframe: SubframeInfo::not_yet_init(),
            fctl_read: 0,
            next_frame: SubframeIdx::Initial,
            prev: Vec::new(),
            current: Vec::new(),
            scan_start: 0,
            transform: t,
            processed: Vec::new(),
            limits,
        }
    }

    /// Reads all meta data until the next frame data starts.
    /// Requires IHDR before the IDAT and fcTL before fdAT.
    fn init(&mut self) -> Result<OutputInfo, DecodingError> {
        if self.next_frame == self.subframe_idx() {
            return Ok(self.output_info());
        } else if self.next_frame == SubframeIdx::End {
            return Err(DecodingError::Parameter(
                ParameterErrorKind::PolledAfterEndOfImage.into(),
            ));
        }

        loop {
            match self.decoder.decode_next(&mut Vec::new())? {
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
                Some(Decoded::Header { .. }) => {
                    self.validate_buffer_sizes()?;
                }
                // Ignore all other chunk events. Any other chunk may be between IDAT chunks, fdAT
                // chunks and their control chunks.
                _ => {}
            }
        }
        {
            let info = match self.decoder.info() {
                Some(info) => info,
                None => return Err(DecodingError::Format(FormatErrorInner::MissingIhdr.into())),
            };
            self.bpp = info.bpp_in_prediction();
            // Check if the output buffer can be represented at all.
            // Now we can init the subframe info.
            // TODO: reuse the results obtained during the above check.
            self.subframe = SubframeInfo::new(info);
        }
        self.allocate_out_buf()?;
        self.prev = vec![0; self.subframe.rowlen];
        Ok(self.output_info())
    }

    fn output_info(&self) -> OutputInfo {
        let width = self.subframe.width;
        let height = self.subframe.height;

        let (color_type, bit_depth) = self.output_color_type();

        OutputInfo {
            width,
            height,
            color_type,
            bit_depth,
            line_size: self.output_line_size(width),
        }
    }

    fn reset_current(&mut self) {
        self.current.clear();
        self.scan_start = 0;
    }

    /// Get information on the image.
    ///
    /// The structure will change as new frames of an animated image are decoded.
    pub fn info(&self) -> &Info {
        self.decoder.info().unwrap()
    }

    /// Get the subframe index of the current info.
    fn subframe_idx(&self) -> SubframeIdx {
        let info = match self.decoder.info() {
            None => return SubframeIdx::Uninit,
            Some(info) => info,
        };

        match info.frame_control() {
            None => SubframeIdx::Initial,
            Some(_) => SubframeIdx::Some(self.fctl_read - 1),
        }
    }

    /// Call after decoding an image, to advance expected state to the next.
    fn finished_frame(&mut self) {
        // Should only be called after frame is done, so we have an info.
        let info = self.info();

        let past_end_subframe = match info.animation_control() {
            // a non-APNG has no subframes
            None => 0,
            // otherwise the count is the past-the-end index. It can not be 0 per spec.
            Some(ac) => ac.num_frames,
        };

        self.next_frame = match self.next_frame {
            SubframeIdx::Uninit => unreachable!("Next frame can never be initial"),
            SubframeIdx::End => unreachable!("Next frame called when already at image end"),
            // Reached the end of non-animated image.
            SubframeIdx::Initial if past_end_subframe == 0 => SubframeIdx::End,
            // An animated image, expecting first subframe.
            SubframeIdx::Initial => SubframeIdx::Some(0),
            // This was the last subframe, slightly fuzzy condition in case of programmer error.
            SubframeIdx::Some(idx) if past_end_subframe <= idx + 1 => SubframeIdx::End,
            // Expecting next subframe.
            SubframeIdx::Some(idx) => SubframeIdx::Some(idx + 1),
        }
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
        // Advance until we've read the info / fcTL for this frame.
        let info = self.init()?;
        // TODO 16 bit
        let (color_type, bit_depth) = self.output_color_type();
        if buf.len() < self.output_buffer_size() {
            return Err(DecodingError::Parameter(
                ParameterErrorKind::ImageBufferSize {
                    expected: buf.len(),
                    actual: self.output_buffer_size(),
                }
                .into(),
            ));
        }

        self.reset_current();
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
            let mut len = 0;
            while let Some(Row { data: row, .. }) = self.next_row()? {
                len += (&mut buf[len..]).write(row)?;
            }
        }
        // Advance over the rest of data for this (sub-)frame.
        if !self.subframe.consumed_and_flushed {
            self.decoder.finished_decoding()?;
        }
        // Advance our state to expect the next frame.
        self.finished_frame();

        Ok(info)
    }

    /// Returns the next processed row of the image
    pub fn next_row(&mut self) -> Result<Option<Row>, DecodingError> {
        self.next_interlaced_row()
            .map(|v| v.map(|v| Row { data: v.data }))
    }

    /// Returns the next processed row of the image
    pub fn next_interlaced_row(&mut self) -> Result<Option<InterlacedRow>, DecodingError> {
        match self.next_interlaced_row_impl() {
            Err(err) => Err(err),
            Ok(None) => Ok(None),
            Ok(s) => Ok(s),
        }
    }

    /// Fetch the next interlaced row and filter it according to our own transformations.
    fn next_interlaced_row_impl(&mut self) -> Result<Option<InterlacedRow>, DecodingError> {
        use crate::common::ColorType::*;
        let transform = self.transform;

        if transform == Transformations::IDENTITY {
            return self.next_raw_interlaced_row();
        }

        // swap buffer to circumvent borrow issues
        let mut buffer = mem::replace(&mut self.processed, Vec::new());
        let (got_next, adam7) = if let Some(row) = self.next_raw_interlaced_row()? {
            (&mut buffer[..]).write_all(row.data)?;
            (true, row.interlace)
        } else {
            (false, InterlaceInfo::Null)
        };
        // swap back
        let _ = mem::replace(&mut self.processed, buffer);

        if !got_next {
            return Ok(None);
        }

        let (color_type, bit_depth, trns) = {
            let info = self.info();
            (info.color_type, info.bit_depth as u8, info.trns.is_some())
        };
        let output_buffer = if let InterlaceInfo::Adam7 { width, .. } = adam7 {
            let width = self
                .line_size(width)
                .expect("Adam7 interlaced rows are shorter than the buffer.");
            &mut self.processed[..width]
        } else {
            &mut *self.processed
        };

        let mut len = output_buffer.len();
        if transform.contains(Transformations::EXPAND) {
            match color_type {
                Indexed => expand_paletted(output_buffer, get_info!(self))?,
                Grayscale | GrayscaleAlpha if bit_depth < 8 => {
                    expand_gray_u8(output_buffer, get_info!(self))
                }
                Grayscale | Rgb if trns => {
                    let channels = color_type.samples();
                    let trns = get_info!(self).trns.as_ref().unwrap();
                    if bit_depth == 8 {
                        utils::expand_trns_line(output_buffer, &*trns, channels);
                    } else {
                        utils::expand_trns_line16(output_buffer, &*trns, channels);
                    }
                }
                _ => (),
            }
        }

        if bit_depth == 16 && transform.intersects(Transformations::STRIP_16) {
            len /= 2;
            for i in 0..len {
                output_buffer[i] = output_buffer[2 * i];
            }
        }

        Ok(Some(InterlacedRow {
            data: &output_buffer[..len],
            interlace: adam7,
        }))
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
                n if n < 8 && t.contains(Transformations::EXPAND) => 8,
                n => n,
            };
            let color_type = if t.contains(Transformations::EXPAND) {
                let has_trns = info.trns.is_some();
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

    fn validate_buffer_sizes(&self) -> Result<(), DecodingError> {
        // Check if the decoding buffer of a single raw line has a valid size.
        if self.info().checked_raw_row_length().is_none() {
            return Err(DecodingError::LimitsExceeded);
        }

        // Check if the output buffer has a valid size.
        if self.checked_output_buffer_size().is_none() {
            return Err(DecodingError::LimitsExceeded);
        }

        Ok(())
    }

    fn checked_output_buffer_size(&self) -> Option<usize> {
        let (width, height) = self.info().size();
        let (color, depth) = self.output_color_type();
        let rowlen = color.checked_raw_row_length(depth, width)? - 1;
        let height: usize = std::convert::TryFrom::try_from(height).ok()?;
        rowlen.checked_mul(height)
    }

    /// Returns the number of bytes required to hold a deinterlaced row.
    pub fn output_line_size(&self, width: u32) -> usize {
        let (color, depth) = self.output_color_type();
        color.raw_row_length_from_width(depth, width) - 1
    }

    /// Returns the number of bytes required to decode a deinterlaced row.
    fn line_size(&self, width: u32) -> Option<usize> {
        use crate::common::ColorType::*;
        let t = self.transform;
        let info = self.info();
        let trns = info.trns.is_some();

        let expanded = if info.bit_depth == BitDepth::Sixteen {
            BitDepth::Sixteen
        } else {
            BitDepth::Eight
        };
        // The color type and depth representing the decoded line
        // TODO 16 bit
        let (color, depth) = match info.color_type {
            Indexed if trns && t.contains(Transformations::EXPAND) => (Rgba, expanded),
            Indexed if t.contains(Transformations::EXPAND) => (Rgb, expanded),
            Rgb if trns && t.contains(Transformations::EXPAND) => (Rgba, expanded),
            Grayscale if trns && t.contains(Transformations::EXPAND) => (GrayscaleAlpha, expanded),
            Grayscale if t.contains(Transformations::EXPAND) => (Grayscale, expanded),
            GrayscaleAlpha if t.contains(Transformations::EXPAND) => (GrayscaleAlpha, expanded),
            other => (other, info.bit_depth),
        };

        // Without the filter method byte
        color.checked_raw_row_length(depth, width).map(|n| n - 1)
    }

    fn allocate_out_buf(&mut self) -> Result<(), DecodingError> {
        let width = self.subframe.width;
        let bytes = self.limits.bytes;
        let buflen = match self.line_size(width) {
            Some(buflen) if buflen <= bytes => buflen,
            // Should we differentiate between platform limits and others?
            _ => return Err(DecodingError::LimitsExceeded),
        };
        self.processed.resize(buflen, 0u8);
        Ok(())
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

    /// Returns the next raw scanline of the image interlace pass.
    /// The scanline is filtered against the previous scanline according to the specification.
    fn next_raw_interlaced_row(&mut self) -> Result<Option<InterlacedRow<'_>>, DecodingError> {
        let bpp = self.bpp;
        let (rowlen, passdata) = match self.next_pass() {
            Some((rowlen, passdata)) => (rowlen, passdata),
            None => return Ok(None),
        };
        loop {
            if self.current.len() - self.scan_start >= rowlen {
                let row = &mut self.current[self.scan_start..];
                let filter = match FilterType::from_u8(row[0]) {
                    None => {
                        self.scan_start += rowlen;
                        return Err(DecodingError::Format(
                            FormatErrorInner::UnknownFilterMethod(row[0]).into(),
                        ));
                    }
                    Some(filter) => filter,
                };

                if let Err(message) =
                    unfilter(filter, bpp, &self.prev[1..rowlen], &mut row[1..rowlen])
                {
                    return Err(DecodingError::Format(
                        FormatErrorInner::BadFilter(message).into(),
                    ));
                }

                self.prev[..rowlen].copy_from_slice(&row[..rowlen]);
                self.scan_start += rowlen;

                return Ok(Some(InterlacedRow {
                    data: &self.prev[1..rowlen],
                    interlace: passdata,
                }));
            } else {
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

                let val = self.decoder.decode_next(&mut self.current)?;
                match val {
                    Some(Decoded::ImageData) => {}
                    Some(Decoded::ImageDataFlushed) => {
                        self.subframe.consumed_and_flushed = true;
                    }
                    None => {
                        if !self.current.is_empty() {
                            return Err(DecodingError::Format(
                                FormatErrorInner::UnexpectedEndOfChunk.into(),
                            ));
                        } else {
                            return Ok(None);
                        }
                    }
                    _ => (),
                }
            }
        }
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

fn expand_paletted(buffer: &mut [u8], info: &Info) -> Result<(), DecodingError> {
    if let Some(palette) = info.palette.as_ref() {
        if let BitDepth::Sixteen = info.bit_depth {
            // This should have been caught earlier but let's check again. Can't hurt.
            Err(DecodingError::Format(
                FormatErrorInner::InvalidColorBitDepth {
                    color: ColorType::Indexed,
                    depth: BitDepth::Sixteen,
                }
                .into(),
            ))
        } else {
            let black = [0, 0, 0];
            if let Some(ref trns) = info.trns {
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

fn expand_gray_u8(buffer: &mut [u8], info: &Info) {
    let rescale = true;
    let scaling_factor = if rescale {
        (255) / ((1u16 << info.bit_depth as u8) - 1) as u8
    } else {
        1
    };
    if let Some(ref trns) = info.trns {
        utils::unpack_bits(buffer, 2, info.bit_depth as u8, |pixel, chunk| {
            if pixel == trns[0] {
                chunk[1] = 0
            } else {
                chunk[1] = 0xFF
            }
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
