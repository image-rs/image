//! An Implementation of RFC 1950
//!
//! Decoding of zlib compressed streams.
//!
//! # Related Links
//! *http://tools.ietf.org/html/rfc1950 - ZLIB Compressed Data Format Specification

use std::io::{self, Read};

use super::hash::Adler32;
use super::deflate::Inflater;

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
            let _dictid = try!(self.inner().read_be_u32());
            panic!("invalid png: zlib detected fdict true")
        }

        assert!((cmf as u16 * 256 + flg as u16) % 31 == 0);

        Ok(())
    }

    fn read_checksum(&mut self) -> io::Result<()> {
        let stream_adler32 = try!(self.inner().read_be_u32());
        let adler32 = self.adler.checksum();

        assert!(adler32 == stream_adler32);
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
