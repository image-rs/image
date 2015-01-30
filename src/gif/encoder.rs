enum ColorType {
	/// Image will be encoded in multiple frames if more than 256 colors are present
	TrueColor,
	/// Number of colors will be reduced
	Indexed(u8),
}

struct Encoder<Image> {
	image: Image,
	color_type: ColorType
}

impl<Image, P, Container> Encoder<Image>
where Image: ImageBuffer<P, Container>,
	  P: Pixel {
	fn new(image: Image) -> Encoder {
		Encoder {
			image: image,
			color_type: ColorType::TrueColor
		}
	}
	
}