#![feature(collections)]

#[macro_use] extern crate enum_primitive;

extern crate libc;
extern crate miniz_sys;
extern crate num;

pub mod chunks;
mod crc;
pub mod decoder;
mod deflate;
mod traits;

pub use decoder::{Reader, Decoder, DecodingResult};
pub use decoder::{Info, ColorType};