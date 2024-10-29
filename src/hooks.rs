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
    fn read_vectored(&mut self, bufs: &mut [std::io::IoSliceMut<'_>]) -> std::io::Result<usize> {
        self.0.read_vectored(bufs)
    }
    fn read_to_end(&mut self, buf: &mut Vec<u8>) -> std::io::Result<usize> {
        self.0.read_to_end(buf)
    }
    fn read_to_string(&mut self, buf: &mut String) -> std::io::Result<usize> {
        self.0.read_to_string(buf)
    }
    fn read_exact(&mut self, buf: &mut [u8]) -> std::io::Result<()> {
        self.0.read_exact(buf)
    }
}
impl BufRead for GenericReader<'_> {
    fn fill_buf(&mut self) -> std::io::Result<&[u8]> {
        self.0.fill_buf()
    }
    fn consume(&mut self, amt: usize) {
        self.0.consume(amt)
    }
    fn read_until(&mut self, byte: u8, buf: &mut Vec<u8>) -> std::io::Result<usize> {
        self.0.read_until(byte, buf)
    }
    fn read_line(&mut self, buf: &mut String) -> std::io::Result<usize> {
        self.0.read_line(buf)
    }
}
impl Seek for GenericReader<'_> {
    fn seek(&mut self, pos: std::io::SeekFrom) -> std::io::Result<u64> {
        self.0.seek(pos)
    }
    fn rewind(&mut self) -> std::io::Result<()> {
        self.0.rewind()
    }
    fn stream_position(&mut self) -> std::io::Result<u64> {
        self.0.stream_position()
    }

    // TODO: Add `seek_relative` once MSRV is at least 1.80.0
}

/// A function to produce an `ImageDecoder` for a given image format.
pub type DecodingHook =
    Box<dyn for<'a> Fn(GenericReader<'a>) -> ImageResult<Box<dyn ImageDecoder + 'a>> + Send + Sync>;

/// Register a new decoding hook or returns false if one already exists for the given format.
pub fn register_decoding_hook(format: ImageFormat, hook: DecodingHook) -> bool {
    if format.reading_enabled() {
        return false;
    }

    let mut hooks = DECODING_HOOKS.write().unwrap();
    if hooks.is_none() {
        *hooks = Some(HashMap::new());
    }
    match hooks.as_mut().unwrap().entry(format) {
        std::collections::hash_map::Entry::Vacant(entry) => {
            entry.insert(hook);
            true
        }
        std::collections::hash_map::Entry::Occupied(_) => false,
    }
}

/// Returns whether a decoding hook has been registered for the given format.
pub fn decoding_hook_registered(format: ImageFormat) -> bool {
    DECODING_HOOKS
        .read()
        .unwrap()
        .as_ref()
        .map(|hooks| hooks.contains_key(&format))
        .unwrap_or(false)
}
