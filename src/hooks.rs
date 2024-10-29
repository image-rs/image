//! This module provides a way to register decoding hooks for image formats not directly supported
//! by this crate.

use std::{
    collections::HashMap,
    io::{BufRead, BufReader, Read, Seek},
    sync::RwLock,
};

use crate::{ImageDecoder, ImageFormat, ImageResult};

pub(crate) trait ReadSeek: Read + Seek {}
impl<T: Read + Seek> ReadSeek for T {}

pub(crate) static DECODING_HOOKS: RwLock<Option<HashMap<ImageFormat, DecodingHook>>> =
    RwLock::new(None);

/// A wrapper around a type-erased trait object that implements `Read` and `Seek`.
pub struct GenericReader<'a>(pub(crate) BufReader<Box<dyn ReadSeek + 'a>>);
impl Read for GenericReader<'_> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        self.0.read(buf)
    }
}
impl BufRead for GenericReader<'_> {
    fn fill_buf(&mut self) -> std::io::Result<&[u8]> {
        self.0.fill_buf()
    }
    fn consume(&mut self, amt: usize) {
        self.0.consume(amt)
    }
}
impl Seek for GenericReader<'_> {
    fn seek(&mut self, pos: std::io::SeekFrom) -> std::io::Result<u64> {
        self.0.seek(pos)
    }
}

/// A function to produce an `ImageDecoder` for a given image format.
pub type DecodingHook =
    Box<dyn for<'a> Fn(GenericReader<'a>) -> ImageResult<Box<dyn ImageDecoder + 'a>> + Send + Sync>;

/// Register a new decoding hook or replace an existing one.
pub fn register_decoding_hook(format: ImageFormat, hook: DecodingHook) {
    let mut hooks = DECODING_HOOKS.write().unwrap();
    if hooks.is_none() {
        *hooks = Some(HashMap::new());
    }
    hooks.as_mut().unwrap().insert(format, hook);
}
