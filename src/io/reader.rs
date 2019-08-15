use std::fs::File;
use std::io::{self, BufReader, Read, Seek};
use std::path::Path;

use dynimage::DynamicImage;
use image::ImageFormat;
use ImageResult;

use super::free_functions;

/// A multi-format image reader.
///
/// Wraps an input reader to facilitate automatic detection of an image's format, appropriate
/// decoding method, and dispatches into the set of supported `ImageDecoder` implementations.
pub struct Reader<R> {
    /// The reader.
    inner: R,
    /// The format, if one has been set or deduced.
    format: Option<ImageFormat>,
}

impl<R: io::Read> Reader<R> {
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

impl<R: Read + Seek> Reader<R> {
    /// Replace the format with a guess based on the content.
    ///
    /// If the guess fails, the format is unchanged. Error is for seek, hence io::Error 
    /// and not ImageError.
    pub fn guess_format(&mut self) -> io::Result<()> {
        unimplemented!()
    }

    /// Read the image dimensions.
    /// Uses the current format to construct the correct reader.
    pub fn image_dimensions(self) -> ImageResult<(u32, u32)> {
        unimplemented!()
    }

    /// Read the image (replaces `load`).
    /// Uses the current format to construct the correct reader.
    pub fn load(self) -> ImageResult<DynamicImage> {
        unimplemented!()
    }
}

/// Potential apis, not part of the core proposal:
impl<R: Read> Reader<R> {
    /// Load for image formats that don't require seeking.
    pub fn load_forward(self) -> ImageResult<DynamicImage> {
        unimplemented!()
    }
}

impl<R> Reader<R> {
    /// Get the currently determined format.
    pub fn format(&self) -> Option<ImageFormat> {
        self.format
    }

    /// Unwrap the reader.
    pub fn into_inner(self) -> R {
        self.inner
    }
}
