use std::io;
use std::io::IoResult;

use hash::Adler32;
use deflate::Inflater;

enum ZlibState {Start, CompressedData, End}

pub struct ZlibDecoder<R> {
	inflate: Inflater<R>,
	adler: Adler32,
	state: ZlibState,
}

impl<R: Reader> ZlibDecoder<R> {
	pub fn new(r: R) -> ZlibDecoder<R> {
		ZlibDecoder {
			inflate: Inflater::new(r),
			adler: Adler32::new(),
			state: Start,
		}
	}

	pub fn inner<'a>(&'a mut self) -> &'a mut R {
		self.inflate.inner()
	} 

	fn read_header(&mut self) -> IoResult<()> {
		let cmf = try!(self.inner().read_u8());
		let _cm = cmf & 0x0F;
		let _cinfo = cmf >> 4;

		let flg = try!(self.inner().read_u8());
		let fdict  = (flg & 0b10000) == 0;
		if fdict {
			let _dictid = try!(self.inner().read_be_u32());
			fail!("unimplemented")
		}

		assert!((cmf as u16 * 256 + flg as u16) % 31 == 0);

		Ok(()) 
	}

	fn read_checksum(&mut self) -> IoResult<()> {
		let stream_adler32 = try!(self.inner().read_be_u32());
		let adler32 = self.adler.checksum();

		assert!(adler32 == stream_adler32);

		self.adler.reset();

		Ok(())
	}
}

impl<R: Reader> Reader for ZlibDecoder<R> {
	fn read(&mut self, buf: &mut [u8]) -> IoResult<uint> {
		match self.state {
			CompressedData => {
				match self.inflate.read(buf) {
					Ok(n) => {
						self.adler.update(buf.slice_to(n));

						if self.inflate.eof() {
							let _ = try!(self.read_checksum()); 
							self.state = End;
						}

						Ok(n)
					}

					e => e
				}
			}

			Start => {
				let _ = try!(self.read_header());
				self.state = CompressedData;
				self.read(buf)
			}

			End => Err(io::standard_error(io::EndOfFile))
		}
	}
}