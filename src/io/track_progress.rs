
//! Tracks read and write progress on the level of byte writers and readers.
//! The progress is reported based on the size of the uncompressed image.
//! This is not an exact progress report, but good enough for most cases.
//! When file compression is strong, the file will be done writing
//! before the progress is reported as completed. Optionally,
//! the trackers can report a completed progress on drop.
//!
//! You should _never rely on the progress report for your application logic_,
//! it is only an approximate hint.
//!
//! Any buffering, like `BufRead`, should be in the inner reader.
//! Otherwise, the tracker will only be able to see occasional large byte chunks.

use std::fs::File;
use std::io::{Read, Write};
use std::path::Path;
use crate::{ColorType, Progress};

/// A byte reader, like a file or a network stream.
/// Tracks how many bytes are taken to the inner reader.
/// Calls the progress callback occasionally.
/// Does not support seeking currently.
/// The progress callback will never overshoot.
pub struct TrackProgressReader<R, F: FnMut(Progress)> {
    inner: R,
    on_progress: F,
    complete_on_drop: bool,
    loaded_bytes: u64,
    approximate_total_bytes: u64,
}

/// A byte writer, like a file or a network stream.
/// Tracks how many bytes are written to the inner writer.
/// Calls the progress callback occasionally.
/// Does not support seeking currently.
/// The progress callback will never overshoot.
pub struct TrackProgressWriter<R, F: FnMut(Progress)> {
    inner: R,
    on_progress: F,
    complete_on_drop: bool,
    written_bytes: u64,
    approximate_total_bytes: u64,
}

impl<F: FnMut(Progress)> TrackProgressReader<File, F> {
    /// Usually called before anything is decoded, but only available for files. Most precise option.
    pub fn open_file(path: impl AsRef<Path>, on_progress: F) -> std::io::Result<Self> {
        let file = File::open(path)?;

        Ok(Self {
            on_progress,
            approximate_total_bytes: file.metadata()?.len(),
            complete_on_drop: true,
            loaded_bytes: 0,

            inner: file,
        })
    }
}

impl<R: Read, F: FnMut(Progress)> TrackProgressReader<R, F> {
    /// Usually called after meta data has been extracted. May undershoot when file contents are compressed.
    pub fn new(inner: R, on_progress: F, file_size: u64) -> Self {
        Self {
            on_progress,
            inner,
            approximate_total_bytes: file_size,
            complete_on_drop: true,
            loaded_bytes: 0,
        }
    }

    /// Usually called after meta data has been extracted. May undershoot when file contents are compressed.
    pub fn estimate(inner: R, on_progress: F, color: ColorType, width: u64, height: u64) -> Self {
        Self::new(inner, on_progress, color.bytes_per_pixel() as u64 * width * height)
    }

    /// Called after meta data was read, but before reading the content of the file.
    pub fn update_expected_size(&mut self, file_size: u64){
        self.approximate_total_bytes = file_size;
    }
}

impl<W: Write, F: FnMut(Progress)> TrackProgressWriter<W, F> {
    /// Usually called after meta data has been extracted. May undershoot when file contents are compressed.
    pub fn new(inner: W, on_progress: F, estimated_compressed_file_size: u64) -> Self {
        Self {
            inner,
            on_progress,
            approximate_total_bytes: estimated_compressed_file_size,
            complete_on_drop: true,
            written_bytes: 0,
        }
    }

    /// Usually called after meta data has been extracted. May undershoot when file contents are compressed.
    pub fn estimate(inner: W, on_progress: F, color: ColorType, width: u64, height: u64) -> Self {
        Self::new(inner, on_progress, color.bytes_per_pixel() as u64 * width * height)
    }
}

impl<R,F> Read for TrackProgressReader<R, F>
    where R: Read, F: FnMut(Progress),
{
    fn read(&mut self, buffer: &mut [u8]) -> std::io::Result<usize> {
        let result = self.inner.read(buffer);

        if let Ok(count) = result {
            // report progress of the bytes that have definitely been processed
            (self.on_progress)(new_progress(self.loaded_bytes, self.approximate_total_bytes));
            self.loaded_bytes += count as u64;
        }

        result
    }
}


impl<W,F> Write for TrackProgressWriter<W, F>
    where W: Write, F: FnMut(Progress),
{
    fn write(&mut self, buffer: &[u8]) -> std::io::Result<usize> {
        let result = self.inner.write(buffer);

        if let Ok(count) = result {
            self.written_bytes += count as u64;

            // report progress of written state
            (self.on_progress)(new_progress(self.written_bytes, self.approximate_total_bytes));
        }

        result
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.inner.flush()
    }
}

impl<R, F: FnMut(Progress)> Drop for TrackProgressReader<R, F> {
    fn drop(&mut self) {
        if self.complete_on_drop {
            (self.on_progress)(complete_progress(self.approximate_total_bytes))
        }
    }
}

impl<W, F: FnMut(Progress)> Drop for TrackProgressWriter<W, F> {
    fn drop(&mut self) {
        if self.complete_on_drop {
            (self.on_progress)(complete_progress(self.approximate_total_bytes))
        }
    }
}

fn new_progress(current: u64, expected: u64) -> Progress {
    Progress::new(current.min(expected), expected)
}

fn complete_progress(total: u64) -> Progress {
    Progress::new(total, total)
}