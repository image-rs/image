//!  Decoding of Radiance HDR Images
//!
//! A decoder for Radiance HDR images
//!
//! Radiance HDR is an image format using run-length encoding, with floating
//! point pixels in a shared-exponent representation. Each pixel is encoded
//! by three 8-bit unsigned color channel values (R, G, B) and one 8-bit
//! unsigned exponent value E. This decoder, like most other implementations
//! of the format, uses the following formula to convert encoded RGBE pixels
//! with positive exponent E > 0 to real (r,g,b) values:
//!
//! <center>(r,g,b) = (R × 2<sup>E - (128 + 8)</sup>,
//! B × 2<sup>E - (128 + 8)</sup>, G × 2<sup>E - (128 + 8)</sup>)</center>
//!
//! For example, (R,G,B,E)=(128,64,192,129) maps to (r,g,b) = (1.0,0.5,1.5).
//!
//! The original Radiance programs and specification differ slightly from the
//! above formula and would produce very slightly larger values, but the current
//! behavior is well established and has the benefit of making round floating
//! point numbers (like 0.375, 1.0, or 16.0) directly representable.
//!
//! # Related Links
//!
//! * <http://radsite.lbl.gov/radiance/refer/filefmts.pdf> -- Radiance .hdr
//!   format specification on page 28, under "Picture File Format"
//! * <http://www.graphics.cornell.edu/~bjw/rgbe/rgbe.c> -- an implementation
//! *  from 1995 introducing the new conversion formula

mod decoder;
mod encoder;

pub use self::decoder::*;
pub use self::encoder::*;
