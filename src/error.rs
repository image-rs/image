use std::error::Error;
use std::fmt;
use std::io;
use color::ColorType;
use image::ImageFormat;

/// An enumeration of Image errors
#[derive(Debug)]
pub enum ImageError {
    /// The Image is not formatted properly
    FormatError(String),

    /// The Image's dimensions are either too small or too large
    DimensionError,

    /// This image format is not supported
    UnsupportedFormat(String),

    /// The Decoder does not support this color type
    UnsupportedColor(ColorType),

    /// Unsupported feature in a image format
    UnsupportedFeature(ImageFormat, String),

    /// Not enough data was provided to the Decoder
    /// to decode the image
    NotEnoughData,

    /// An I/O Error occurred while decoding the image
    IoError(io::Error),

    /// The end of the image has been reached
    ImageEnd,

    /// There is not enough memory to complete the given operation
    InsufficientMemory,
}

impl fmt::Display for ImageError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match *self {
            ImageError::FormatError(ref e) => write!(fmt, "Format error: {}", e),
            ImageError::DimensionError => write!(
                fmt,
                "The Image's dimensions are either too \
                 small or too large"
            ),
            ImageError::UnsupportedFormat(ref f) => write!(
                fmt,
                "The format `{}` is not supported",
                f
            ),
            ImageError::UnsupportedColor(ref c) => write!(
                fmt,
                "The decoder does not support \
                 the color type `{:?}`",
                c
            ),
            ImageError::UnsupportedFeature(ref fo, ref fe) => write!(
                fmt,
                "Unsupported `{}` in the format {:?}",
                fe,
                fo
            ),
            ImageError::NotEnoughData => write!(
                fmt,
                "Not enough data was provided to the \
                 Decoder to decode the image"
            ),
            ImageError::IoError(ref e) => e.fmt(fmt),
            ImageError::ImageEnd => write!(fmt, "The end of the image has been reached"),
            ImageError::InsufficientMemory => write!(fmt, "Insufficient memory"),
        }
    }
}

impl Error for ImageError {
    fn description(&self) -> &str {
        match *self {
            ImageError::FormatError(..) => "Format error",
            ImageError::DimensionError => "Dimension error",
            ImageError::UnsupportedFormat(..) => "Unsupported format",
            ImageError::UnsupportedColor(..) => "Unsupported color",
            ImageError::UnsupportedFeature(..) => "Unsupported feature",
            ImageError::NotEnoughData => "Not enough data",
            ImageError::IoError(..) => "IO error",
            ImageError::ImageEnd => "Image end",
            ImageError::InsufficientMemory => "Insufficient memory",
        }
    }

    // TODO: use `Error::source` when minimal rust version is updated
    fn cause(&self) -> Option<&Error> {
        match *self {
            ImageError::IoError(ref e) => Some(e),
            _ => None,
        }
    }
}

impl From<io::Error> for ImageError {
    fn from(err: io::Error) -> ImageError {
        ImageError::IoError(err)
    }
}

/// Result of an image decoding/encoding process
pub type ImageResult<T> = Result<T, ImageError>;
