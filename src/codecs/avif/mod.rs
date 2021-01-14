//! Encoding of AVIF images.
///
/// The [AVIF] specification defines an image derivative of the AV1 bitstream, an open video codec.
///
/// [AVIF]: https://aomediacodec.github.io/av1-avif/

pub use self::encoder::AvifEncoder;

mod encoder;
