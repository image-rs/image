#![feature(collections)]

#[macro_use] extern crate bitflags;

extern crate libc;
extern crate miniz_sys;
extern crate num;

pub mod chunk;
mod crc;
pub mod decoder;
mod deflate;
mod filter;
mod traits;
mod types;
mod utils;

pub use decoder::{Reader, Decoder, Decoded, DecodingError};
pub use types::*;

pub use traits::{Parameter, HasParameters};