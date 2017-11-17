//! Decoding of netpbm image formats (pbm, pgm, ppm and pam).
//!
//! The formats pbm, pgm and ppm are fully supported. The pam decoder recognizes the tuple types
//! `BLACKANDWHITE`, `GRAYSCALE` and `RGB` and explicitely recognizes but rejects their `_ALPHA`
//! variants for now as alpha color types are unsupported.

pub use self::decoder::{PNMDecoder, PNMSubtype};

mod decoder;
