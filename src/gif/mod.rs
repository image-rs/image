//!  Decoding of GIF Images
//!
//!  GIF (Graphics Interchange Format) is an image format that supports lossless compression.
//!
//!  # Related Links
//!  * http://www.w3.org/Graphics/GIF/spec-gif89a.txt - The GIF Specification
//!

pub use self::decoder::GIFDecoder;
pub use self::encoder::Encoder as GIFEncoder;
pub use self::encoder::ColorMode;

mod decoder;
mod encoder;


#[derive(FromPrimitive)]
/// Known block types
enum Block {
    Image = 0x2C,
    Extension = 0x21,
    Trailer = 0x3B
}

#[derive(FromPrimitive)]
/// Known GIF extensions
enum Extension {
    Text = 0x01,
    Control = 0xF9,
    Comment = 0xFE,
    Application = 0xFF
}

#[derive(FromPrimitive)]
/// Method to dispose the image
enum DisposalMethod {
	Undefined = 0,
	None = 1,
	Previous = 2,
	Background = 3
}
