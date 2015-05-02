use std::io;


// Will be replace by stdlib solution
fn read_all<R: io::Read + ?Sized>(this: &mut R, mut buf: &mut [u8]) -> io::Result<()> {
    let mut total = 0;
    while total < buf.len() {
        match this.read(&mut buf[total..]) {
            Ok(0) => return Err(io::Error::new(io::ErrorKind::Other,
                                               "failed to read whole buffer")),
            Ok(n) => total += n,
            Err(ref e) if e.kind() == io::ErrorKind::Interrupted => {}
            Err(e) => return Err(e),
        }
    }
    Ok(())
}

/// Reader extension to read little endian data
pub trait ReadBytesExt<T>: io::Read {
	/// Writes `T` to a bytes stream. Most significant byte first.
	fn read_be(&mut self) -> io::Result<T>;

	#[inline]
	fn read_byte(&mut self) -> io::Result<u8> {
        let mut byte = [0];
		try!(read_all(self, &mut byte));
        Ok(byte[0])
	}
}

impl<W: io::Read + ?Sized> ReadBytesExt<u16> for W {
	#[inline]
	fn read_be(&mut self) -> io::Result<u16> {
        let mut bytes = [0, 0];
		try!(read_all(self, &mut bytes));
        Ok((bytes[0] as u16) << 8 | bytes[1] as u16)
	}
}

impl<W: io::Read + ?Sized> ReadBytesExt<u32> for W {
	#[inline]
	fn read_be(&mut self) -> io::Result<u32> {
        let mut bytes = [0, 0, 0, 0];
		try!(read_all(self, &mut bytes));
        Ok(  (bytes[0] as u32) << 24 
           | (bytes[1] as u32) << 16
           | (bytes[2] as u32) << 8
           |  bytes[3] as u32
        )
	}
}
/*
impl<W: io::Write + ?Sized> WriteBytesExt<u64> for W {
	#[inline]
	fn write_le(&mut self, n: u64) -> io::Result<()> {
		try!(self.write_le(n as u32));
		self.write_le((n >> 32) as u32)
		
	}
}*/