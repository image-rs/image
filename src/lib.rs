#![feature(collections)]

extern crate miniz_sys;
extern crate libc;

mod chunks;
mod crc;
pub mod decoder;
mod deflate;
mod traits;

pub use decoder::{Decoder, DecodingResult};