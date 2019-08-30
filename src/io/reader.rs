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
    /// Create a new image reader without a preset format.
    ///
    /// It is possible to guess the format based on the content of the read object with
    /// [`guess_format`], or to set the format directly with [`set_format`].
    ///
    /// [`guess_format`]: #method.guess_format
    /// [`set_format`]: method.set_format
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

    /// Supply the format as which to interpret the read image.
    pub fn set_format(&mut self, format: ImageFormat) {
        self.format = Some(format);
    }

    /// Remove the current information on the image format.
    ///
    /// Note that many operations require format information to be present and will return e.g. an
    /// `ImageError::UnsupportedError` when the image format has not been set.
    pub fn clear_format(&mut self) {
        self.format = None;
    }

    /// Unwrap the reader.
    pub fn into_inner(self) -> R {
        self.inner
    }
}

impl Reader<BufReader<File>> {
    /// Open a file to read, format will be guessed from content or path.
    ///
    /// This will automatically begin reading the start of the file to determine the format, then
    /// fall back to the path. Use [`from_path`] to open a file but guess its format solely based
    /// on the path and not perform any io on the file yet.
    ///
    /// It is always possible to redetect the current format with [`guess_format`].
    ///
    /// [`from_path`]: #method.from_path
    /// [`guess_format`]: #method.guess_format
    pub fn open<P>(path: P) -> io::Result<Self> where P: AsRef<Path> {
        Self::open_impl(path.as_ref())
    }

    /// Open a file to read, format will be guessed from path.
    ///
    /// This will not attempt any io operation on the opened file.
    ///
    /// If you want to inspect the content for a better guess on the format, which does not depend
    /// on file extensions, use [`open`] instead.
    ///
    /// [`open`]: #method.open
    pub fn from_path<P>(path: P) -> io::Result<Self> where P: AsRef<Path> {
        Self::from_path_impl(path.as_ref())
    }

    fn open_impl(path: &Path) -> io::Result<Self> {
        let file = File::open(path)?;

        let mut reader = Reader {
            inner: BufReader::new(file),
            format: None,
        };

        reader.guess_format()?;

        if reader.format.is_none() {
            // If content didn't success, maybe guess the format based on path.
            if let Ok(format) = ImageFormat::from_path(path) {
                reader.format = Some(format);
            }
        }

        Ok(reader)
    }

    fn from_path_impl(path: &Path) -> io::Result<Self> {
        let file = File::open(path)?;
        Ok(Reader {
            inner: BufReader::new(file),
            format: ImageFormat::from_path(path).ok(),
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
    ///
    /// Uses the current format to construct the correct reader for the format.
    pub fn image_dimensions(mut self) -> ImageResult<(u32, u32)> {
        let format = self.require_format()?;
        free_functions::image_dimensions_with_format_impl(self.inner, format)
    }

    /// Read the image (replaces `load`).
    ///
    /// Uses the current format to construct the correct reader for the format.
    pub fn decode(mut self) -> ImageResult<DynamicImage> {
        let format = self.require_format()?;
        free_functions::load(self.inner, format)
    }

    fn require_format(&mut self) -> ImageResult<ImageFormat> {
        self.format.ok_or_else(||
            ImageError::UnsupportedError("Unable to determine image format".into()))
    }
}
