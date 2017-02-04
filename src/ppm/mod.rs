//! Encoding of portable pixmap Images

pub use self::encoder::PPMEncoder as PPMEncoder;
pub use self::decoder::PPMDecoder as PPMDecoder;

mod encoder;
mod decoder;
