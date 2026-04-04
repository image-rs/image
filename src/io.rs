//! Input and output of images.

use std::io;
use std::io::Read as _;

/// The decoder traits.
pub(crate) mod decoder;
/// The encoder traits.
pub(crate) mod encoder;

pub(crate) mod format;
pub(crate) mod free_functions;
pub(crate) mod image_reader_type;
pub(crate) mod limits;

/// Adds `read_exact_vec`
pub(crate) trait ReadExt {
    fn read_exact_vec(&mut self, vec: &mut Vec<u8>, len: usize) -> io::Result<()>;
}

impl<R: io::Read> ReadExt for R {
    fn read_exact_vec(&mut self, vec: &mut Vec<u8>, len: usize) -> io::Result<()> {
        let initial_len = vec.len();
        vec.try_reserve(len)?;
        match self.take(len as u64).read_to_end(vec) {
            Ok(read) if read == len => Ok(()),
            fail => {
                vec.truncate(initial_len);
                Err(fail.err().unwrap_or(io::ErrorKind::UnexpectedEof.into()))
            }
        }
    }
}
