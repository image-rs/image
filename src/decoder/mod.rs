mod stream;
mod zlib;

use self::stream::{get_info, CHUNCK_BUFFER_SIZE};
pub use self::stream::{Decoded, DecodingError, StreamingDecoder};

use std::borrow;
use std::io::{BufRead, BufReader, Read, Write};
use std::mem;
use std::ops::Range;

use crate::chunk;
use crate::common::{BitDepth, BytesPerPixel, ColorType, Info, Transformations};
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

/// Output info
#[derive(Debug, PartialEq, Eq)]
pub struct OutputInfo {
    pub width: u32,
    pub height: u32,
    pub color_type: ColorType,
    pub bit_depth: BitDepth,
    pub line_size: usize,
}

impl OutputInfo {
    /// Returns the size needed to hold a decoded frame
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

struct InterlacedRow<'data> {
    data: &'data [u8],
    interlace: InterlaceInfo,
}

enum InterlaceInfo {
    None,
    Adam7 { pass: u8, line: u32, width: u32 },
}

impl<R: Read> Decoder<R> {
    pub fn new(r: R) -> Decoder<R> {
        Decoder::new_with_limits(r, Limits::default())
    }

    pub fn new_with_limits(r: R, limits: Limits) -> Decoder<R> {
        Decoder {
            r,
            transform: crate::Transformations::EXPAND
                | crate::Transformations::SCALE_16
                | crate::Transformations::STRIP_16,
            limits,
        }
    }

    /// Limit resource usage
    ///
    /// ```
    /// use std::fs::File;
    /// use png::{Decoder, Limits};
    /// // This image is 32x32 pixels, so the deocder will allocate more than four bytes
    /// let mut limits = Limits::default();
    /// limits.bytes = 4;
    /// let mut decoder = Decoder::new_with_limits(File::open("tests/pngsuite/basi0g01.png").unwrap(), limits);
    /// assert!(decoder.read_info().is_err());
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
    pub fn read_info(self) -> Result<(OutputInfo, Reader<R>), DecodingError> {
        let mut r = Reader::new(self.r, StreamingDecoder::new(), self.transform, self.limits);
        r.init()?;

        let color_type = r.info().color_type;
        let bit_depth = r.info().bit_depth;
        if color_type.is_combination_invalid(bit_depth) {
            return Err(DecodingError::Format(
                format!(
                    "Invalid color/depth combination in header: {:?}/{:?}",
                    color_type, bit_depth
                )
                .into(),
            ));
        }

        // Check if the output buffer can be represented at all.
        if r.checked_output_buffer_size().is_none() {
            return Err(DecodingError::LimitsExceeded);
        }

        let (ct, bits) = r.output_color_type();
        let info = {
            let info = r.info();
            OutputInfo {
                width: info.width,
                height: info.height,
                color_type: ct,
                bit_depth: bits,
                line_size: r.output_line_size(info.width),
            }
        };
        Ok((info, r))
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
                    return Err(DecodingError::Format("unexpected EOF".into()));
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
                return Err(DecodingError::Format("unexpected EOF after image".into()));
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

        Err(DecodingError::Format("unexpected EOF after image".into()))
    }

    fn info(&self) -> Option<&Info> {
        get_info(&self.decoder)
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
    fn init(&mut self) -> Result<(), DecodingError> {
        if self.next_frame == self.subframe_idx() {
            return Ok(());
        } else if self.next_frame == SubframeIdx::End {
            return Err(DecodingError::Other("End of image has been reached".into()));
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
                None => return Err(DecodingError::Format("IDAT chunk missing".into())),
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
                None => return Err(DecodingError::Format("IHDR chunk missing".into())),
            };
            self.bpp = info.bpp_in_prediction();
            // Check if the output buffer can be represented at all.
            // Now we can init the subframe info.
            // TODO: reuse the results obtained during the above check.
            self.subframe = SubframeInfo::new(info);
        }
        self.allocate_out_buf()?;
        self.prev = vec![0; self.subframe.rowlen];
        Ok(())
    }

    fn reset_current(&mut self) {
        self.current.clear();
        self.scan_start = 0;
    }

    /// Get information on the image.
    ///
    /// The structure will change as new frames of an animated image are decoded.
    pub fn info(&self) -> &Info {
        get_info!(self)
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
    pub fn next_frame(&mut self, buf: &mut [u8]) -> Result<(), DecodingError> {
        // Advance until we've read the info / fcTL for this frame.
        self.init()?;
        // TODO 16 bit
        let (color_type, bit_depth) = self.output_color_type();
        if buf.len() < self.output_buffer_size() {
            return Err(DecodingError::Other(
                "supplied buffer is too small to hold the image".into(),
            ));
        }

        self.reset_current();
        let width = self.info().width;
        if get_info!(self).interlaced {
            while let Some((row, adam7)) = self.next_interlaced_row()? {
                let (pass, line, _) = adam7.unwrap();
                let samples = color_type.samples() as u8;
                utils::expand_pass(buf, width, row, pass, line, samples * (bit_depth as u8));
            }
        } else {
            let mut len = 0;
            while let Some(row) = self.next_row()? {
                len += (&mut buf[len..]).write(row)?;
            }
        }
        // Advance over the rest of data for this (sub-)frame.
        if !self.subframe.consumed_and_flushed {
            self.decoder.finished_decoding()?;
        }
        // Advance our state to expect the next frame.
        self.finished_frame();
        Ok(())
    }

    /// Returns the next processed row of the image
    pub fn next_row(&mut self) -> Result<Option<&[u8]>, DecodingError> {
        self.next_interlaced_row().map(|v| v.map(|v| v.0))
    }

    /// Returns the next processed row of the image
    pub fn next_interlaced_row(
        &mut self,
    ) -> Result<Option<(&[u8], Option<(u8, u32, u32)>)>, DecodingError> {
        match self.next_interlaced_row_impl() {
            Err(err) => Err(err),
            Ok(None) => Ok(None),
            Ok(Some(row)) => {
                let interlace = match row.interlace {
                    InterlaceInfo::None => None,
                    InterlaceInfo::Adam7 { pass, line, width } => Some((pass, line, width)),
                };

                Ok(Some((row.data, interlace)))
            }
        }
    }

    /// Fetch the next interlaced row and filter it according to our own transformations.
    fn next_interlaced_row_impl(&mut self) -> Result<Option<InterlacedRow<'_>>, DecodingError> {
        use crate::common::ColorType::*;
        let transform = self.transform;

        if transform == crate::Transformations::IDENTITY {
            return self.next_raw_interlaced_row();
        }

        // swap buffer to circumvent borrow issues
        let mut buffer = mem::replace(&mut self.processed, Vec::new());
        let (got_next, adam7) = if let Some(row) = self.next_raw_interlaced_row()? {
            (&mut buffer[..]).write_all(row.data)?;
            (true, row.interlace)
        } else {
            (false, InterlaceInfo::None)
        };
        // swap back
        let _ = mem::replace(&mut self.processed, buffer);

        if !got_next {
            return Ok(None);
        }

        let (color_type, bit_depth, trns) = {
            let info = get_info!(self);
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
        if transform.contains(crate::Transformations::EXPAND) {
            match color_type {
                Indexed => expand_paletted(output_buffer, get_info!(self))?,
                Grayscale | GrayscaleAlpha if bit_depth < 8 => {
                    expand_gray_u8(output_buffer, get_info!(self))
                }
                Grayscale | RGB if trns => {
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

        if bit_depth == 16
            && transform
                .intersects(crate::Transformations::SCALE_16 | crate::Transformations::STRIP_16)
        {
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
    pub fn output_color_type(&mut self) -> (ColorType, BitDepth) {
        self.imm_output_color_type()
    }

    pub(crate) fn imm_output_color_type(&self) -> (ColorType, BitDepth) {
        use crate::common::ColorType::*;
        let t = self.transform;
        let info = get_info!(self);
        if t == crate::Transformations::IDENTITY {
            (info.color_type, info.bit_depth)
        } else {
            let bits = match info.bit_depth as u8 {
                16 if t.intersects(
                    crate::Transformations::SCALE_16 | crate::Transformations::STRIP_16,
                ) =>
                {
                    8
                }
                n if n < 8 && t.contains(crate::Transformations::EXPAND) => 8,
                n => n,
            };
            let color_type = if t.contains(crate::Transformations::EXPAND) {
                let has_trns = info.trns.is_some();
                match info.color_type {
                    Grayscale if has_trns => GrayscaleAlpha,
                    RGB if has_trns => RGBA,
                    Indexed if has_trns => RGBA,
                    Indexed => RGB,
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
        let (width, height) = get_info!(self).size();
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
        let (width, height) = get_info!(self).size();
        let (color, depth) = self.imm_output_color_type();
        let rowlen = color.checked_raw_row_length(depth, width)? - 1;
        let height: usize = std::convert::TryFrom::try_from(height).ok()?;
        rowlen.checked_mul(height)
    }

    /// Returns the number of bytes required to hold a deinterlaced row.
    pub fn output_line_size(&self, width: u32) -> usize {
        let (color, depth) = self.imm_output_color_type();
        color.raw_row_length_from_width(depth, width) - 1
    }

    /// Returns the number of bytes required to decode a deinterlaced row.
    fn line_size(&self, width: u32) -> Option<usize> {
        use crate::common::ColorType::*;
        let t = self.transform;
        let info = get_info!(self);
        let trns = info.trns.is_some();

        let expanded = if info.bit_depth == BitDepth::Sixteen {
            BitDepth::Sixteen
        } else {
            BitDepth::Eight
        };
        // The color type and depth representing the decoded line
        // TODO 16 bit
        let (color, depth) = match info.color_type {
            Indexed if trns && t.contains(Transformations::EXPAND) => (RGBA, expanded),
            Indexed if t.contains(Transformations::EXPAND) => (RGB, expanded),
            RGB if trns && t.contains(Transformations::EXPAND) => (RGBA, expanded),
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
                let rowlen = get_info!(self).raw_row_length_from_width(width);
                if last_pass != pass {
                    self.prev.clear();
                    self.prev.resize(rowlen, 0u8);
                }
                Some((rowlen, InterlaceInfo::Adam7 { pass, line, width }))
            }
            InterlaceIter::None(ref mut height) => {
                let _ = height.next()?;
                Some((self.subframe.rowlen, InterlaceInfo::None))
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
                            format!("invalid filter method ({})", row[0]).into(),
                        ));
                    }
                    Some(filter) => filter,
                };

                if let Err(message) =
                    unfilter(filter, bpp, &self.prev[1..rowlen], &mut row[1..rowlen])
                {
                    return Err(DecodingError::Format(borrow::Cow::Borrowed(message)));
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
                        format!("not enough data for image").into(),
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
                            return Err(DecodingError::Format("file truncated".into()));
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
            rowlen: info.raw_row_length_from_width(width),
            interlace,
            consumed_and_flushed: false,
        }
    }
}

fn expand_paletted(buffer: &mut [u8], info: &Info) -> Result<(), DecodingError> {
    if let Some(palette) = info.palette.as_ref() {
        if let BitDepth::Sixteen = info.bit_depth {
            Err(DecodingError::Format(
                "Bit depth '16' is not valid for paletted images".into(),
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
        Err(DecodingError::Format("missing palette".into()))
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

        let (info, mut normal) = Decoder::new(IMG).read_info().unwrap();

        let mut buffer = vec![0; info.buffer_size()];
        let normal = normal.next_frame(&mut buffer).unwrap_err();

        let smal = Decoder::new(SmalBuf::new(IMG, 1))
            .read_info()
            .unwrap()
            .1
            .next_frame(&mut buffer)
            .unwrap_err();

        assert_eq!(discriminant(&normal), discriminant(&smal));
    }
}
