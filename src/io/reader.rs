use std::fs::File;
use std::io::{self, BufRead, BufReader, Cursor, Read, Seek, SeekFrom};
use std::path::Path;

use dynimage::DynamicImage;
use image::ImageFormat;
use {ImageError, ImageResult};

use super::free_functions;

/// A multi-format image reader.
///
/// Wraps an input reader to facilitate automatic detection of an image's format, appropriate
/// decoding method, and dispatches into the set of supported `ImageDecoder` implementations.
pub struct Reader<R: Read> {
    /// The reader.
    inner: R,
    /// The format, if one has been set or deduced.
    format: Option<ImageFormat>,
}

impl<R: Read> Reader<R> {
    pub fn new(reader: R) -> Self {
        Reader {
            inner: reader,
            format: None,
        }
    }

    /// Construct a reader with specified format.
    pub fn with_format(reader: R, format: ImageFormat) -> Self {
        Reader {
            inner: reader,
            format: Some(format),
        }
    }

    /// Get the currently determined format.
    pub fn format(&self) -> Option<ImageFormat> {
        self.format
    }

    /// Unwrap the reader.
    pub fn into_inner(self) -> R {
        self.inner
    }
}

impl Reader<BufReader<File>> {
    /// Open a file to read. Format will be guessed.
    /// Note: Failure to guess is not an error. The `Err` is only for `File::open`.
    pub fn open<P>(path: P) -> io::Result<Self> where P: AsRef<Path> {
        let path = path.as_ref();

        let file = File::open(path)?;
        let format = ImageFormat::from_path(path);

        Ok(Reader {
            inner: BufReader::new(file),
            format: format.ok(),
        })
    }
}

impl<R: BufRead + Seek> Reader<R> {
    /// Replace the format with a guess based on the content.
    ///
    /// If the guess fails, the format is unchanged. Error is for seek, hence io::Error
    /// and not ImageError.
    pub fn guess_format(&mut self) -> io::Result<()> {
        let mut start = [0; 16];

        // Save current offset, read start, restore offset.
        let cur = self.inner.seek(SeekFrom::Current(0))?;
        let len = io::copy(
            // Accept shorter files but read at most 16 bytes.
            &mut self.inner.by_ref().take(16),
            &mut Cursor::new(&mut start[..]))?;
        self.inner.seek(SeekFrom::Start(cur))?;

        self.format = free_functions::guess_format(&start[..len as usize]).ok();
        Ok(())
    }

    /// Read the image dimensions.
    /// Uses the current format to construct the correct reader.
    pub fn image_dimensions(mut self) -> ImageResult<(u32, u32)> {
        let format = self.require_format()?;
        free_functions::image_dimensions_with_format_impl(self.inner, format)
    }

    /// Read the image (replaces `load`).
    /// Uses the current format to construct the correct reader.
    pub fn load(self) -> ImageResult<DynamicImage> {
        unimplemented!()
    }

    fn require_format(&mut self) -> ImageResult<ImageFormat> {
        self.format.ok_or_else(||
            ImageError::UnsupportedError("Unable to determine image format".into()))
    }
}

/// Potential apis, not part of the core proposal:
impl<R: Read> Reader<R> {
    /// Load for image formats that don't require seeking.
    pub fn load_forward(self) -> ImageResult<DynamicImage> {
        unimplemented!()
    }
}
