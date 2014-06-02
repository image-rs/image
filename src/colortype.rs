#[deriving(PartialEq, Show)]
pub enum ColorType {
	Grey(u8),
	RGB(u8),
	Palette(u8),
	GreyA(u8),
	RGBA(u8)
}

pub fn bits_per_pixel(c: ColorType) -> uint {
	match c {
		Grey(n)    => n as uint,
		RGB(n)     => 3 * n as uint,
		Palette(n) => 3 * n as uint,
		GreyA(n)   => 2 * n as uint,
		RGBA(n)    => 4 * n as uint,
	}
}

pub fn num_components(c: ColorType) -> uint {
	match c {
		Grey(_)    => 1,
		RGB(_)     => 3,
		Palette(_) => 3,
		GreyA(_)   => 2,
		RGBA(_)    => 4,
	}
}