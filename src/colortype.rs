//! Types and methods for representing and manipulating colors

///An enumeration over supported color types and their bit depths
#[deriving(PartialEq, Show, Clone)]
pub enum ColorType {
	///Pixel is greyscale
	Grey(u8),

	///Pixel contains R, G and B channels
	RGB(u8),

	///Pixel is an index into a color palette
	Palette(u8),

	///Pixel is greyscale with an alpha channel
	GreyA(u8),

	///Pixel is RGB with an alpha channel
	RGBA(u8)
}

///Returns the number of bits contained in a pixel of ColorType c
pub fn bits_per_pixel(c: ColorType) -> uint {
	match c {
		Grey(n)    => n as uint,
		RGB(n)     => 3 * n as uint,
		Palette(n) => 3 * n as uint,
		GreyA(n)   => 2 * n as uint,
		RGBA(n)    => 4 * n as uint,
	}
}

///Returns the number of color channels that make up this pixel
pub fn num_components(c: ColorType) -> uint {
	match c {
		Grey(_)    => 1,
		RGB(_)     => 3,
		Palette(_) => 3,
		GreyA(_)   => 2,
		RGBA(_)    => 4,
	}
}
