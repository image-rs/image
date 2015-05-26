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
mod common;
mod utils;

pub use decoder::{Decoder, Reader, StreamingDecoder, Decoded, DecodingError};
pub use common::*;

pub use traits::{Parameter, HasParameters};