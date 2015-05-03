#![feature(collections)]

extern crate miniz_sys;
extern crate libc;

mod crc;
pub mod decoder;
mod deflate;
mod traits;