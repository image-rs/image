/*!
A DEFLATE encoder (RFC 1951).
*/
use std::io::{self, Write};
use byteorder::{WriteBytesExt, LittleEndian};

/// A DEFLATE encoder (RFC 1951).
// TODO: this deflater only uses non-compressed blocks
//       this is easier to implement but not very efficient
pub struct Deflater<W> where W: Write {
    writer: Option<W>,
    buf: Vec<u8>,
}

impl<W> Deflater<W> where W: Write {
    /// Creates a new encoder that writes to the underlying writer.
    pub fn new(writer: W) -> Deflater<W> {
        Deflater {
            writer: Some(writer),
            buf: Vec::with_capacity(65535),
        }
    }

    /// Stops encoding and returns the underlying writer.
    pub fn into_inner(mut self) -> io::Result<W> {
        try!(self.flush_buf(true));
        Ok(self.writer.take().unwrap())
    }

    /// Encodes the buffer into a block and writes it to the output.
    ///
    /// `last` indicates whether this is the last block.
    fn flush_buf(&mut self, last: bool) -> io::Result<()> {
        if self.buf.len() == 0 {
            return Ok(());
        }

        assert!(self.buf.len() <= u16::max_value() as usize);
        let len = self.buf.len() as u16;
        let nlen = !len;    // one's complement

        let writer = self.writer.as_mut().unwrap();
        try!(writer.write_u8(if last { 0x29 } else { 0 }));       // bits and stuff
                  // TODO: should be 1  ^, but a bug in the PNGDecoder can be bypassed with 0x29
        try!(writer.write_u16::<LittleEndian>(len));
        try!(writer.write_u16::<LittleEndian>(nlen));
        try!(writer.write_all(&self.buf));

        self.buf.clear();

        Ok(())
    }
}

impl<W> Write for Deflater<W> where W: Write {
    fn write(&mut self, mut buf: &[u8]) -> io::Result<usize> {
        let total_written = buf.len();

        loop {
            if self.buf.len() + buf.len() >= u16::max_value() as usize {
                let rest = u16::max_value() as usize - self.buf.len();
                self.buf.extend(buf.iter().take(rest).cloned());
                buf = &buf[rest..];
                try!(self.flush_buf(false));
                continue;

            } else {
                self.buf.extend(buf.iter().cloned());
                break;
            }
        }

        Ok(total_written)
    }

    fn flush(&mut self) -> io::Result<()> {
        try!(self.flush_buf(false));
        self.writer.as_mut().unwrap().flush()
    }
}

impl<W> Drop for Deflater<W> where W: Write {
    fn drop(&mut self) {
        if self.writer.is_some() {
            let _ = self.flush_buf(true);
            let _ = self.writer.as_mut().unwrap().flush();
        }
    }
}
