//! An Implementation of RFC 1950
//!
//! Decoding of zlib compressed streams.
//!
//! # Related Links
//! *http://tools.ietf.org/html/rfc1950 - ZLIB Compressed Data Format Specification

use std::io::{self, Read, Write};
use byteorder::{ReadBytesExt, WriteBytesExt, BigEndian};

use super::hash::Adler32;
use super::deflate::Deflater;
use super::inflate::Inflater;

enum ZlibState {
    Start,
    CompressedData,
    End
}

/// A Zlib compressed stream decoder.
pub struct ZlibDecoder<R> {
    inflate: Inflater<R>,
    adler: Adler32,
    state: ZlibState,
}

/// A Zlib compressed stream encoder.
pub struct ZlibEncoder<W> where W: Write {
    deflate: Option<Deflater<W>>,
    adler: Adler32,
}

impl<R: Read> ZlibDecoder<R> {
    /// Create a new decoder that decodes from a Reader
    pub fn new(r: R) -> ZlibDecoder<R> {
        ZlibDecoder {
            inflate: Inflater::new(r),
            adler: Adler32::new(),
            state: ZlibState::Start,
        }
    }

    /// Return a mutable reference to the wrapped Reader
    pub fn inner(&mut self) -> &mut R {
        self.inflate.inner()
    }

    fn read_header(&mut self) -> io::Result<()> {
        let cmf = try!(self.inner().read_u8());
        let _cm = cmf & 0x0F;
        let _cinfo = cmf >> 4;

        let flg = try!(self.inner().read_u8());
        let fdict  = (flg & 0b100000) != 0;
        if fdict {
            let _dictid = try!(self.inner().read_u32::<BigEndian>());
            panic!("invalid png: zlib detected fdict true")
        }

        assert!((cmf as u16 * 256 + flg as u16) % 31 == 0);

        Ok(())
    }

    fn read_checksum(&mut self) -> io::Result<()> {
        let stream_adler32 = try!(self.inner().read_u32::<BigEndian>());
        let adler32 = self.adler.checksum();

        assert_eq!(adler32, stream_adler32);
        self.adler.reset();

        Ok(())
    }
}

impl<R: Read> Read for ZlibDecoder<R> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        match self.state {
            ZlibState::CompressedData => {
                match self.inflate.read(buf) {
                    Ok(n) => {
                        self.adler.update(&buf[..n]);

                        if self.inflate.eof() {
                            let _ = try!(self.read_checksum());
                            self.state = ZlibState::End;
                        }

                        Ok(n)
                    }

                    e => e
                }
            }

            ZlibState::Start => {
                let _ = try!(self.read_header());
                self.state = ZlibState::CompressedData;
                self.read(buf)
            }

            ZlibState::End => Ok(0)
        }
    }
}

impl<W> ZlibEncoder<W> where W: Write {
    /// Builds a new encoder.
    pub fn new(mut writer: W) -> io::Result<ZlibEncoder<W>> {
        // writing CMF and FLG
        let cm: u8 = 8;
        let cinfo: u8 = 7;       // FIXME: 
        let cmf: u8 = cinfo << 4 | cm;

        let fdict: u8 = 0;
        let flevel: u8 = 0;
        let fcheck: u8 = 31 - ((cmf as u16 * 256 | (flevel << 6 | fdict << 5) as u16) % 31) as u8;
        let flg = flevel << 6 | fdict << 5 | fcheck;

        assert!((cmf as u16 * 256 + flg as u16) % 31 == 0);

        try!(writer.write_all(&[cmf, flg]));

        Ok(ZlibEncoder {
            deflate: Some(Deflater::new(writer)),
            adler: Adler32::new(),
        })
    }

    /// Finish writing and returns the underlying writer.
    pub fn into_inner(mut self) -> io::Result<W> {
        let mut stream = try!(self.deflate.take().unwrap().into_inner());
        try!(stream.write_u32::<BigEndian>(self.adler.checksum()));
        Ok(stream)
    }
}

impl<W> Write for ZlibEncoder<W> where W: Write {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.adler.update(buf);
        try!(self.deflate.as_mut().unwrap().write(buf));
        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        self.deflate.as_mut().unwrap().flush()
    }
}

impl<W> Drop for ZlibEncoder<W> where W: Write {
    fn drop(&mut self) {
        if let Some(deflate) = self.deflate.take() {
            let mut stream = match deflate.into_inner() {
                Ok(s) => s,
                Err(_) => return
            };

            let _ = stream.write_u32::<BigEndian>(self.adler.checksum());
            let _ = stream.flush();
        }
    }
}

#[cfg(test)]
mod tests {
    use std::io::{Cursor, Read, Write};
    use super::{ZlibEncoder, ZlibDecoder};

    #[test]
    fn roundtrip() {
        let mut input = Vec::new();
        let mut encoded = ZlibEncoder::new(Vec::new()).unwrap();

        // TODO: generate random data
        for n in (0u8 .. 200) {
            input.push(n);
            encoded.write(&[n]).unwrap();
        }

        let encoded = encoded.into_inner().unwrap();

        let mut decoder = ZlibDecoder::new(Cursor::new(encoded));
        let mut ret = Vec::new();
        decoder.read_to_end(&mut ret).unwrap();

        assert_eq!(ret, input);
    }
}
