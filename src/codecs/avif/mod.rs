//! Encoding of AVIF images.
///
/// The [AVIF] specification defines an image derivative of the AV1 bitstream, an open video codec.
///
/// [AVIF]: https://aomediacodec.github.io/av1-avif/
#[cfg(feature = "avif-decoder")]
pub use self::decoder::AvifDecoder;
#[cfg(feature = "avif-encoder")]
pub use self::encoder::{AvifEncoder, ColorSpace};

#[cfg(feature = "avif-decoder")]
mod decoder;
#[cfg(feature = "avif-encoder")]
mod encoder;
