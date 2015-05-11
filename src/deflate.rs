//! Low-level wrapper around `flate2`.

use std::io;
use std::mem::zeroed;

use libc::{c_uint, c_int};
use miniz_sys::*;

/// Flush mode
#[allow(dead_code)]
pub enum Flush {
    /// Default strategy
    None = MZ_NO_FLUSH as isize,
    /// Tries to output as much as possible
    Sync = MZ_SYNC_FLUSH as isize,
    /// Most efficient if the output buffer is big enough
    /// to decompress everythin at once.
    Finish = MZ_FINISH as isize,
}

/// Deflater is a minimal interface to `miniz`.
///
/// It merely provides a safe way of calling `mz_inflate`.
pub struct Inflater {
    stream: mz_stream
}

impl Inflater {
    /// Creates a new inflater.
    pub fn new() -> Inflater {
        let stream = unsafe { 
            let mut stream = zeroed();
            // Since `stream` is zeroed `mz_inflateInit2` should not allocate
            // (according to docs) and thus not fail.
            assert_eq!(mz_inflateInit2(&mut stream, MZ_DEFAULT_WINDOW_BITS), MZ_OK);
            stream
        };
        Inflater {
            stream: stream
        }
    }
    
    /// Inflates the `input` stream.
    ///
    /// The decoded bytes are written into `output` which is returns as `out` which has been
    /// truncated to only contain the decoded bytes. `flush` sets the flushing strategy.
    ///
    /// The return value `Ok(at_eof, consumed, out)` indicates how many bytes have been
    /// `consumed` from `input`. `out` contains the bytes that were decoded from the input
    /// stream. This function should be called until `at_eof` indicates that the end of the
    /// stream has been reached.
    pub fn inflate<'a>(&mut self, input: &[u8], output: &'a mut [u8], flush: Flush)
    -> io::Result<(bool, usize, &'a mut [u8])> {
        let s = &mut self.stream;
        let out_len = output.len();
        s.next_in = input.as_ptr();
        s.avail_in = input.len() as c_uint;
        s.next_out = output.as_mut_ptr();
        s.avail_out = out_len as c_uint;
        let result = unsafe { mz_inflate(s, flush as c_int) };
        match result {
            MZ_OK | MZ_STREAM_END | MZ_BUF_ERROR => {
                Ok((if result == MZ_STREAM_END { true } else { false },
                    input.len() - s.avail_in as usize,
                    &mut output[..out_len - s.avail_out as usize]
                ))
            },
            err => {
                Err(io::Error::new(
                    io::ErrorKind::Other,
                    format!("mz_inflate returned error code: {}", err)
                ))
            }
        }

    }
}

impl Drop for Inflater {
    fn drop(&mut self) {
        let _ = unsafe { mz_inflateEnd(&mut self.stream) };
    }
}