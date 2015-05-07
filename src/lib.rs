#![feature(collections)]
#![feature(custom_derive)] 
#![feature(plugin)]
#![plugin(num_macros)]

extern crate libc;
extern crate miniz_sys;
extern crate num;

pub mod chunks;
mod crc;
pub mod decoder;
mod deflate;
mod filter;
mod traits;
mod types;

pub use decoder::{Reader, Decoder, Decoded, DecodingError};
pub use types::{Info, ColorType};

pub use traits::{Parameter, HasParameters};