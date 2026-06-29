//! Decoding of TGA Images
//!
//! # Related Links
//! <http://googlesites.inequation.org/tgautilities>

pub use self::decoder::TgaDecoder;

pub use self::encoder::{TgaEncoder, TgaOptions};

mod decoder;
mod encoder;
mod header;
