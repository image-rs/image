use std::io::{BufRead, BufReader, BufWriter, Read, Seek, Write};

trait ReadSeek: Read + Seek {}
impl<T: Read + Seek> ReadSeek for T {}

trait WriteSeek: Write + Seek {}
impl<T: Write + Seek> WriteSeek for T {}

/// A wrapper around a type-erased trait object that implements [`Read`] and [`Seek`].
pub struct GenericReader<'a>(BufReader<Box<dyn ReadSeek + 'a>>);
impl<'a> GenericReader<'a> {
    /// Creates a new `GenericReader` with a given underlying reader.
    pub(crate) fn new<R: Read + Seek + 'a>(reader: R) -> Self {
        Self(BufReader::new(Box::new(reader)))
    }
}
impl Read for GenericReader<'_> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        self.0.read(buf)
    }
    fn read_vectored(&mut self, bufs: &mut [std::io::IoSliceMut<'_>]) -> std::io::Result<usize> {
        self.0.read_vectored(bufs)
    }
    fn read_to_end(&mut self, buf: &mut Vec<u8>) -> std::io::Result<usize> {
        self.0.read_to_end(buf)
    }
    fn read_to_string(&mut self, buf: &mut String) -> std::io::Result<usize> {
        self.0.read_to_string(buf)
    }
    fn read_exact(&mut self, buf: &mut [u8]) -> std::io::Result<()> {
        self.0.read_exact(buf)
    }
}
impl BufRead for GenericReader<'_> {
    fn fill_buf(&mut self) -> std::io::Result<&[u8]> {
        self.0.fill_buf()
    }
    fn consume(&mut self, amt: usize) {
        self.0.consume(amt)
    }
    fn read_until(&mut self, byte: u8, buf: &mut Vec<u8>) -> std::io::Result<usize> {
        self.0.read_until(byte, buf)
    }
    fn read_line(&mut self, buf: &mut String) -> std::io::Result<usize> {
        self.0.read_line(buf)
    }
}
impl Seek for GenericReader<'_> {
    fn seek(&mut self, pos: std::io::SeekFrom) -> std::io::Result<u64> {
        self.0.seek(pos)
    }
    fn rewind(&mut self) -> std::io::Result<()> {
        self.0.rewind()
    }
    fn stream_position(&mut self) -> std::io::Result<u64> {
        self.0.stream_position()
    }

    // TODO: Add `seek_relative` once MSRV is at least 1.80.0
}

/// A wrapper around a type-erased trait object that implements [`Write`] and [`Seek`].
pub struct GenericWriter<'a>(BufWriter<Box<dyn WriteSeek + 'a>>);
impl<'a> GenericWriter<'a> {
    /// Creates a new `GenericReader` with a given underlying reader.
    pub(crate) fn new<W: Write + Seek + 'a>(writer: W) -> Self {
        Self(BufWriter::new(Box::new(writer)))
    }
}
impl Write for GenericWriter<'_> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.0.write(buf)
    }
    fn flush(&mut self) -> std::io::Result<()> {
        self.0.flush()
    }
    fn write_vectored(&mut self, bufs: &[std::io::IoSlice<'_>]) -> std::io::Result<usize> {
        self.0.write_vectored(bufs)
    }
    fn write_all(&mut self, buf: &[u8]) -> std::io::Result<()> {
        self.0.write_all(buf)
    }
    fn write_fmt(&mut self, args: std::fmt::Arguments<'_>) -> std::io::Result<()> {
        self.0.write_fmt(args)
    }
}
impl Seek for GenericWriter<'_> {
    fn seek(&mut self, pos: std::io::SeekFrom) -> std::io::Result<u64> {
        self.0.seek(pos)
    }
    fn rewind(&mut self) -> std::io::Result<()> {
        self.0.rewind()
    }
    fn stream_position(&mut self) -> std::io::Result<u64> {
        self.0.stream_position()
    }

    // TODO: Add `seek_relative` once MSRV is at least 1.80.0
}
