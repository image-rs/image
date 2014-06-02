use std::path::BytesContainer;

pub struct Adler32 {
	s1: u32,
	s2: u32
}

impl Adler32 {
	pub fn new() -> Adler32 {
		Adler32 {s1: 1, s2: 0}
	}

	pub fn update<T: BytesContainer>(&mut self, buf: T) {
		for &byte in buf.container_as_bytes().iter() {
			self.s1 = self.s1 + byte as u32;
			self.s2 = self.s1 + self.s2;

			self.s1 %= 65521;
			self.s2 %= 65521;
		}
	}

	pub fn checksum(&self) -> u32 {
		(self.s2 << 16) | self.s1
	}

	pub fn reset(&mut self) {
		self.s1 = 1;
		self.s2 = 0;
	}
}

static IEEE_POLYNOMIAL: u32 = 0xEDB88320;

pub struct Crc32 {
	table: [u32, ..256],
	crc: u32,
}

impl Crc32 {
	pub fn new() -> Crc32 {
		let mut t = [0u32, ..256];

		for n in range(0u32, 256) {
			let mut c = n;

			for _ in range(0, 8) {
				c = if c & 1 == 1 {
					IEEE_POLYNOMIAL ^ (c >> 1)
				}
				else {
					c >> 1
				};
			}

			t[n as uint] = c;
		}

		Crc32 {table: t, crc: 0xFFFFFFFF}
	}

	pub fn update<T: BytesContainer>(&mut self, buf: T) {
		for &byte in buf.container_as_bytes().iter() {
			let a = (self.crc ^ byte as u32) & 0xFF;
			let b = self.crc >> 8;

			self.crc = self.table[a as uint] as u32 ^ b;
		}
	}

	pub fn checksum(&self) -> u32 {
		self.crc ^ 0xFFFFFFFF
	}

	pub fn reset(&mut self) {
		self.crc = 0xFFFFFFFF;
	}
}