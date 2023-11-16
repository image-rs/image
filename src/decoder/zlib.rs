use super::{stream::FormatErrorInner, DecodingError, CHUNCK_BUFFER_SIZE};

use fdeflate::Decompressor;

/// Ergonomics wrapper around `miniz_oxide::inflate::stream` for zlib compressed data.
pub(super) struct ZlibStream {
    /// Current decoding state.
    state: Box<fdeflate::Decompressor>,
    /// If there has been a call to decompress already.
    started: bool,
    /// Remaining buffered decoded bytes.
    /// The decoder sometimes wants inspect some already finished bytes for further decoding. So we
    /// keep a total of 32KB of decoded data available as long as more data may be appended.
    out_buffer: Vec<u8>,
    /// The cursor position in the output stream as a buffer index.
    out_pos: usize,
    /// Ignore and do not calculate the Adler-32 checksum. Defaults to `true`.
    ///
    /// This flag overrides `TINFL_FLAG_COMPUTE_ADLER32`.
    ///
    /// This flag should not be modified after decompression has started.
    ignore_adler32: bool,
}

impl ZlibStream {
    pub(crate) fn new() -> Self {
        ZlibStream {
            state: Box::new(Decompressor::new()),
            started: false,
            out_buffer: vec![0; 2 * CHUNCK_BUFFER_SIZE],
            out_pos: 0,
            ignore_adler32: true,
        }
    }

    pub(crate) fn reset(&mut self) {
        self.started = false;
        self.out_buffer.clear();
        self.out_pos = 0;
        *self.state = Decompressor::new();
    }

    /// Set the `ignore_adler32` flag and return `true` if the flag was
    /// successfully set.
    ///
    /// The default is `true`.
    ///
    /// This flag cannot be modified after decompression has started until the
    /// [ZlibStream] is reset.
    pub(crate) fn set_ignore_adler32(&mut self, flag: bool) -> bool {
        if !self.started {
            self.ignore_adler32 = flag;
            true
        } else {
            false
        }
    }

    /// Return the `ignore_adler32` flag.
    pub(crate) fn ignore_adler32(&self) -> bool {
        self.ignore_adler32
    }

    /// Fill the decoded buffer as far as possible from `data`.
    /// On success returns the number of consumed input bytes.
    pub(crate) fn decompress(
        &mut self,
        data: &[u8],
        image_data: &mut Vec<u8>,
    ) -> Result<usize, DecodingError> {
        self.prepare_vec_for_appending();

        if !self.started && self.ignore_adler32 {
            self.state.ignore_adler32();
        }

        let (in_consumed, out_consumed) = self
            .state
            .read(data, self.out_buffer.as_mut_slice(), self.out_pos, false)
            .map_err(|err| {
                DecodingError::Format(FormatErrorInner::CorruptFlateStream { err }.into())
            })?;

        self.started = true;
        self.out_pos += out_consumed;
        self.transfer_finished_data(image_data);

        Ok(in_consumed)
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
        if !self.started {
            return Ok(());
        }

        loop {
            self.prepare_vec_for_appending();

            let (in_consumed, out_consumed) = self
                .state
                .read(&[], self.out_buffer.as_mut_slice(), self.out_pos, true)
                .map_err(|err| {
                    DecodingError::Format(FormatErrorInner::CorruptFlateStream { err }.into())
                })?;

            self.out_pos += out_consumed;

            if self.state.is_done() {
                self.out_buffer.truncate(self.out_pos);
                image_data.append(&mut self.out_buffer);
                return Ok(());
            } else {
                let transferred = self.transfer_finished_data(image_data);
                assert!(
                    transferred > 0 || in_consumed > 0 || out_consumed > 0,
                    "No more forward progress made in stream decoding."
                );
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
