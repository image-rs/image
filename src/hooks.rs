//! This module provides a way to register decoding hooks for image formats not directly supported
//! by this crate.

use std::{
    collections::HashMap,
    io::{BufReader, Read, Seek},
    sync::RwLock,
};

use crate::{ImageDecoder, ImageFormat, ImageResult};

pub(crate) trait ReadSeek: Read + Seek {}
impl<T: Read + Seek> ReadSeek for T {}

pub(crate) static DECODING_HOOKS: RwLock<Option<HashMap<ImageFormat, DecodingHook>>> =
    RwLock::new(None);

/// A wrapper around a type-erased trait object that implements `Read` and `Seek`.
pub struct BoxReadSeek<'a>(pub(crate) Box<dyn ReadSeek + 'a>);
impl<'a> Read for BoxReadSeek<'a> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        self.0.read(buf)
    }
}
impl<'a> Seek for BoxReadSeek<'a> {
    fn seek(&mut self, pos: std::io::SeekFrom) -> std::io::Result<u64> {
        self.0.seek(pos)
    }
}

/// A function to produce an `ImageDecoder` for a given image format.
pub type DecodingHook = Box<
    dyn for<'a> Fn(BufReader<BoxReadSeek<'a>>) -> ImageResult<Box<dyn ImageDecoder + 'a>>
        + Send
        + Sync,
>;

/// Register a new decoding hook or replace an existing one.
pub fn register_decoding_hook(format: ImageFormat, hook: DecodingHook) {
    let mut hooks = DECODING_HOOKS.write().unwrap();
    if hooks.is_none() {
        *hooks = Some(HashMap::new());
    }
    hooks.as_mut().unwrap().insert(format, hook);
}
