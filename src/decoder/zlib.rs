use super::{DecodingError, CHUNCK_BUFFER_SIZE};
use std::io;

use miniz_oxide::inflate::core::{decompress, inflate_flags, DecompressorOxide};
use miniz_oxide::inflate::TINFLStatus;

/// Ergonomics wrapper around `miniz_oxide::inflate::stream` for zlib compressed data.
pub(super) struct ZlibStream {
    /// Current decoding state.
    state: Box<DecompressorOxide>,
    /// If there has been a call to decompress already.
    started: bool,
    /// A buffer of compressed data.
    /// We use this for a progress guarantee. The data in the input stream is chunked as given by
    /// the underlying stream buffer. We will not read any more data until the current buffer has
    /// been fully consumed. The zlib decompression can not fully consume all the data when it is
    /// in the middle of the stream, it will treat full symbols and maybe the last bytes need to be
    /// treated in a special way. The exact reason isn't as important but the interface does not
    /// promise us this. Now, the complication is that the _current_ chunking information of PNG
    /// alone is not enough to determine this as indeed the compressed stream is the concatenation
    /// of all consecutive `IDAT`/`fdAT` chunks. We would need to inspect the next chunk header.
    ///
    /// Thus, there needs to be a buffer that allows fully clearing a chunk so that the next chunk
    /// type can be inspected.
    in_buffer: Vec<u8>,
    /// The logical start of the `in_buffer`.
    in_pos: usize,
    /// Remaining buffered decoded bytes.
    /// The decoder sometimes wants inspect some already finished bytes for further decoding. So we
    /// keep a total of 32KB of decoded data available as long as more data may be appended.
    out_buffer: Vec<u8>,
    /// The cursor position in the output stream as a buffer index.
    out_pos: usize,
}

impl ZlibStream {
    pub(crate) fn new() -> Self {
        ZlibStream {
            state: Box::default(),
            started: false,
            in_buffer: Vec::with_capacity(CHUNCK_BUFFER_SIZE),
            in_pos: 0,
            out_buffer: vec![0; 2 * CHUNCK_BUFFER_SIZE],
            out_pos: 0,
        }
    }

    pub(crate) fn reset(&mut self) {
        self.started = false;
        self.in_buffer.clear();
        self.out_buffer.clear();
        self.out_pos = 0;
        *self.state = DecompressorOxide::default();
    }

    /// Fill the decoded buffer as far as possible from `data`.
    /// On success returns the number of consumed input bytes.
    pub(crate) fn decompress(
        &mut self,
        data: &[u8],
        image_data: &mut Vec<u8>,
    ) -> Result<usize, DecodingError> {
        const BASE_FLAGS: u32 = inflate_flags::TINFL_FLAG_PARSE_ZLIB_HEADER
            | inflate_flags::TINFL_FLAG_USING_NON_WRAPPING_OUTPUT_BUF
            | inflate_flags::TINFL_FLAG_HAS_MORE_INPUT;

        self.prepare_vec_for_appending();

        let (status, mut in_consumed, out_consumed) = {
            let mut cursor = io::Cursor::new(self.out_buffer.as_mut_slice());
            cursor.set_position(self.out_pos as u64);
            let in_data = if self.in_buffer.is_empty() {
                data
            } else {
                &self.in_buffer[self.in_pos..]
            };
            decompress(&mut self.state, in_data, &mut cursor, BASE_FLAGS)
        };

        if !self.in_buffer.is_empty() {
            self.in_pos += in_consumed;
        }

        if self.in_buffer.len() == self.in_pos {
            self.in_buffer.clear();
            self.in_pos = 0;
        }

        if in_consumed == 0 {
            self.in_buffer.extend_from_slice(data);
            in_consumed = data.len();
        }

        self.started = true;
        self.out_pos += out_consumed;
        self.transfer_finished_data(image_data);

        match status {
            TINFLStatus::Done | TINFLStatus::HasMoreOutput | TINFLStatus::NeedsMoreInput => {
                Ok(in_consumed)
            }
            _err => Err(DecodingError::CorruptFlateStream),
        }
    }

    /// Called after all consecutive IDAT chunks were handled.
    ///
    /// The compressed stream can be split on arbitrary byte boundaries. This enables some cleanup
    /// within the decompressor and flushing additional data which may have been kept back in case
    /// more data were passed to it.
    pub(crate) fn finish_compressed_chunks(
        &mut self,
        image_data: &mut Vec<u8>,
    ) -> Result<(), DecodingError> {
        const BASE_FLAGS: u32 = inflate_flags::TINFL_FLAG_PARSE_ZLIB_HEADER
            | inflate_flags::TINFL_FLAG_USING_NON_WRAPPING_OUTPUT_BUF;

        if !self.started {
            return Ok(());
        }

        let tail = self.in_buffer.split_off(0);
        let tail = &tail[self.in_pos..];

        let mut start = 0;
        loop {
            self.prepare_vec_for_appending();

            let (status, in_consumed, out_consumed) = {
                // TODO: we may be able to avoid the indirection through the buffer here.
                // First append all buffered data and then create a cursor on the image_data
                // instead.
                let mut cursor = io::Cursor::new(self.out_buffer.as_mut_slice());
                cursor.set_position(self.out_pos as u64);
                decompress(&mut self.state, &tail[start..], &mut cursor, BASE_FLAGS)
            };

            start += in_consumed;
            self.out_pos += out_consumed;

            match status {
                TINFLStatus::Done => {
                    self.out_buffer.truncate(self.out_pos as usize);
                    image_data.append(&mut self.out_buffer);
                    return Ok(());
                }
                TINFLStatus::HasMoreOutput => {
                    let transferred = self.transfer_finished_data(image_data);
                    assert!(
                        transferred > 0 || in_consumed > 0 || out_consumed > 0,
                        "No more forward progress made in stream decoding."
                    );
                }
                _err => {
                    return Err(DecodingError::CorruptFlateStream);
                }
            }
        }
    }

    /// Resize the vector to allow allocation of more data.
    fn prepare_vec_for_appending(&mut self) {
        if self.out_buffer.len().saturating_sub(self.out_pos) >= CHUNCK_BUFFER_SIZE {
            return;
        }

        let buffered_len = self.decoding_size(self.out_buffer.len());
        debug_assert!(self.out_buffer.len() <= buffered_len);
        self.out_buffer.resize(buffered_len, 0u8);
    }

    fn decoding_size(&self, len: usize) -> usize {
        // Allocate one more chunk size than currently or double the length while ensuring that the
        // allocation is valid and that any cursor within it will be valid.
        len
            // This keeps the buffer size a power-of-two, required by miniz_oxide.
            .saturating_add(CHUNCK_BUFFER_SIZE.max(len))
            // Ensure all buffer indices are valid cursor positions.
            // Note: both cut off and zero extension give correct results.
            .min(u64::max_value() as usize)
            // Ensure the allocation request is valid.
            // TODO: maximum allocation limits?
            .min(isize::max_value() as usize)
    }

    fn transfer_finished_data(&mut self, image_data: &mut Vec<u8>) -> usize {
        let safe = self.out_pos.saturating_sub(CHUNCK_BUFFER_SIZE);
        // TODO: allocation limits.
        image_data.extend(self.out_buffer.drain(..safe));
        self.out_pos -= safe;
        safe
    }
}
