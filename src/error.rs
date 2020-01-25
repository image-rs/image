//! Contains detailed error representation.

use std::error::Error;
use std::fmt;
use std::io;

use crate::color::ExtendedColorType;
use crate::image::ImageFormat;

/// The generic error type for image operations.
///
/// This high level enum is allows, by variant matching, a rough separation of concerns between
/// underlying IO, the caller, format specifications, and the `image` implementation.
#[derive(Debug)]
pub enum NewImageError {
    /// An error was encountered while decoding.
    ///
    /// This means that the input data did not conform to the specification of some image format,
    /// or that no format could be determined, or that it did not match format specific
    /// requirements set by the caller.
    Decoding(DecodingError),

    /// An error was encountered while encoding.
    ///
    /// The input image can not be encoded with the chosen format, for example because the
    /// specification has no representation for its color space or because a necessary conversion
    /// is ambiguous. In some cases it might also happen that the dimensions can not be used with
    /// the format.
    Encoding(EncodingError),

    /// An error was encountered in inputs arguments.
    ///
    /// This is a catch-all case for strictly internal operations such as scaling, conversions,
    /// etc. that involve no external format specifications.
    Parameter(ParameterError),

    /// Completing the operation would have required more resources than allowed.
    ///
    /// Errors of this type are limits set by the user or environment, *not* inherent in a specific
    /// format or operation that was executed.
    Limits(LimitError),

    /// An operation can not be completed by the chosen abstraction.
    ///
    /// This means that it might be possible for the operation to succeed in general but
    /// * it requires a disabled feature,
    /// * the implementation does not yet exist, or
    /// * no abstraction for a lower level could be found.
    Unsupported(UnsupportedError),

    /// An error occurred while interacting with the environment.
    IoError(io::Error),
}

/// An enumeration of Image errors
#[derive(Debug)]
pub enum ImageError {
    /// The Image is not formatted properly
    FormatError(String),

    /// The Image's dimensions are either too small or too large
    DimensionError,

    /// The Decoder does not support this image format
    UnsupportedError(String),

    /// The Decoder does not support this color type
    UnsupportedColor(ExtendedColorType),

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

/// The implementation for an operation was not provided.
///
/// See the variant [`Unsupported`] for more documentation.
///
/// [`Unsupported`]: struct.ImageError.html#variant.Unsupported
#[derive(Debug)]
pub struct UnsupportedError {
    format: ImageFormatHint,
    kind: UnsupportedErrorKind,
}

/// Details what feature is not supported.
#[derive(Clone, Debug, Hash, PartialEq)]
pub enum UnsupportedErrorKind {
    /// The required color type can not be handled.
    Color(ExtendedColorType),
    /// An image format is not supported.
    Format(ImageFormatHint),
    /// Some feature specified by string.
    /// This is discouraged and is likely to get deprecated (but not removed).
    GenericFeature(String),
    #[doc(hidden)]
    __NonExhaustive,
}

#[derive(Debug)]
pub struct EncodingError {
    format: ImageFormatHint,
    message: String,
    underlying: Option<Box<dyn Error>>,
}

#[derive(Debug)]
pub struct ParameterError {
    kind: ParameterErrorKind,
    underlying: Option<Box<dyn Error>>,
}

/// Details how a parameter is malformed.
#[derive(Clone, Debug, Hash, PartialEq)]
pub enum ParameterErrorKind {
    /// Repeated an operation for which error that could not be cloned was emitted already.
    FailedAlready,
    /// The dimensions passed are wrong.
    DimensionMismatch,
    /// A string describing the parameter.
    /// This is discouraged and is likely to get deprecated (but not removed).
    Generic(String),
    #[doc(hidden)]
    __NonExhaustive,
}

#[derive(Debug)]
pub struct DecodingError {
    format: ImageFormatHint,
    message: String,
    underlying: Option<Box<dyn Error>>,
}

#[derive(Debug)]
pub struct LimitError {
    kind: LimitErrorKind,
    // do we need an underlying error?
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[allow(missing_copy_implementations)] // Might be non-Copy in the future.
pub enum LimitErrorKind {
    DimensionError,
    InsufficientMemory,
    #[doc(hidden)]
    __NonExhaustive,
}

/// A best effort representation for image formats.
#[derive(Clone, Debug, Hash, PartialEq)]
pub enum ImageFormatHint {
    /// The format is known exactly.
    Exact(ImageFormat),

    /// The format can be identified by a name.
    Name(String),

    /// A common path extension for the format is known.
    PathExtension(std::path::PathBuf),

    /// The format is not known or could not be determined.
    Unknown,

    #[doc(hidden)]
    __NonExhaustive,
}

#[allow(non_upper_case_globals)]
#[allow(non_snake_case)]
impl NewImageError {
    pub(crate) const InsufficientMemory: Self =
        NewImageError::Limits(LimitError {
            kind: LimitErrorKind::InsufficientMemory,
        });

    pub(crate) const DimensionError: Self =
        NewImageError::Parameter(ParameterError {
            kind: ParameterErrorKind::DimensionMismatch,
            underlying: None,
        });

    pub(crate) const ImageEnd: Self =
        NewImageError::Parameter(ParameterError {
            kind: ParameterErrorKind::FailedAlready,
            underlying: None,
        });

    pub(crate) fn UnsupportedError(message: String) -> Self {
        NewImageError::Unsupported(UnsupportedError::legacy_from_string(message))
    }

    pub(crate) fn UnsupportedColor(color: ExtendedColorType) -> Self {
        NewImageError::Unsupported(UnsupportedError::new(
            ImageFormatHint::Unknown,
            UnsupportedErrorKind::Color(color),
        ))
    }

    pub(crate) fn FormatError(message: String) -> Self {
        NewImageError::Decoding(DecodingError::legacy_from_string(message))
    }
}

impl UnsupportedError {
    pub fn new(format: ImageFormatHint, kind: UnsupportedErrorKind) -> Self {
        UnsupportedError {
            format,
            kind,
        }
    }

    pub(crate) fn legacy_from_string(message: String) -> Self {
        UnsupportedError {
            format: ImageFormatHint::Unknown,
            kind: UnsupportedErrorKind::GenericFeature(message),
        }
    }

    pub fn kind(&self) -> UnsupportedErrorKind {
        self.kind.clone()
    }

    pub fn format_hint(&self) -> ImageFormatHint {
        self.format.clone()
    }
}

impl DecodingError {
    pub fn format_hint(&self) -> ImageFormatHint {
        self.format.clone()
    }

    pub(crate) fn legacy_from_string(message: String) -> Self {
        DecodingError {
            format: ImageFormatHint::Unknown,
            message,
            underlying: None,
        }
    }
}

impl EncodingError {
    pub fn format_hint(&self) -> ImageFormatHint {
        self.format.clone()
    }
}

impl LimitError {
    pub fn kind(&self) -> LimitErrorKind {
        self.kind.clone()
    }
}

impl From<io::Error> for NewImageError {
    fn from(err: io::Error) -> NewImageError {
        NewImageError::IoError(err)
    }
}

impl From<ImageFormat> for ImageFormatHint {
    fn from(format: ImageFormat) -> Self {
        ImageFormatHint::Exact(format)
    }
}

impl From<&'_ std::path::Path> for ImageFormatHint {
    fn from(path: &'_ std::path::Path) -> Self {
        match path.extension() {
            Some(ext) => ImageFormatHint::PathExtension(ext.into()),
            None => ImageFormatHint::Unknown,
        }
    }
}

impl From<ImageFormatHint> for UnsupportedError {
    fn from(hint: ImageFormatHint) -> Self {
        UnsupportedError {
            format: hint.clone(),
            kind: UnsupportedErrorKind::Format(hint),
        }
    }
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
            ImageError::UnsupportedError(ref f) => write!(
                fmt,
                "The Decoder does not support the \
                 image format `{}`",
                f
            ),
            ImageError::UnsupportedColor(ref c) => write!(
                fmt,
                "The decoder does not support \
                 the color type `{:?}`",
                c
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
    fn source(&self) -> Option<&(dyn Error + 'static)> {
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

/// Result of an image decoding/encoding process
pub(crate) type NewImageResult<T> = Result<T, NewImageError>;

impl fmt::Display for NewImageError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            NewImageError::IoError(err) => err.fmt(fmt),
            NewImageError::Decoding(err) => err.fmt(fmt),
            NewImageError::Encoding(err) => err.fmt(fmt),
            NewImageError::Parameter(err) => err.fmt(fmt),
            NewImageError::Limits(err) => err.fmt(fmt),
            NewImageError::Unsupported(err) => err.fmt(fmt),
        }
    }
}

impl fmt::Display for UnsupportedError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match &self.kind {
            UnsupportedErrorKind::Format(ImageFormatHint::Unknown) => write!(
                fmt,
                "The image format could not be determined",
            ),
            UnsupportedErrorKind::Format(format @ ImageFormatHint::PathExtension(_)) => write!(
                fmt,
                "The file extension {} was not recognized as an image format",
                format,
            ),
            UnsupportedErrorKind::Format(format) => write!(
                fmt,
                "The image format {} is not supported",
                format,
            ),
            UnsupportedErrorKind::Color(color) => write!(
                fmt,
                "The decoder for {} does not support the color type `{:?}`",
                self.format,
                color,
            ),
            UnsupportedErrorKind::GenericFeature(message) => {
                match &self.format {
                    ImageFormatHint::Unknown => write!(
                        fmt,
                        "The decoder does not support the format feature {}",
                        message,
                    ),
                    other => write!(
                        fmt,
                        "The decoder for {} does not support the format features {}",
                        other,
                        message,
                    ),
                }
            },
            UnsupportedErrorKind::__NonExhaustive => unreachable!()
        }
    }
}

impl Error for UnsupportedError { }

impl fmt::Display for ParameterError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match &self.kind {
            ParameterErrorKind::DimensionMismatch => write!(
                fmt,
                "The Image's dimensions are either too \
                 small or too large"
            ),
            ParameterErrorKind::FailedAlready => write!(
                fmt,
                "The end the image stream has been reached due to a previous error"
            ),
            ParameterErrorKind::Generic(message) => write!(
                fmt,
                "The parameter is malformed: {}",
                message,
            ),
            ParameterErrorKind::__NonExhaustive => unreachable!(),
        }?;

        if let Some(underlying) = &self.underlying {
            write!(fmt, "\n{}", underlying)?;
        }

        Ok(())
    }
}

impl Error for ParameterError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        self.underlying.as_ref().map(|b| &**b)
    }
}

impl fmt::Display for EncodingError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match &self.underlying {
            Some(underlying) => write!(
                fmt,
                "Format error encoding {}: {}\n{}",
                self.format,
                self.message,
                underlying,
            ),
            None => write!(
                fmt,
                "Format error encoding {}: {}",
                self.format,
                self.message,
            ),
        }
    }
}

impl Error for EncodingError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        self.underlying.as_ref().map(|b| &**b)
    }
}

impl fmt::Display for DecodingError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match &self.underlying {
            Some(underlying) => write!(
                fmt,
                "Format error decoding {}: {}\n{}",
                self.format,
                self.message,
                underlying,
            ),
            None => write!(
                fmt,
                "Format error decoding {}: {}",
                self.format,
                self.message,
            ),
        }
    }
}

impl Error for DecodingError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        self.underlying.as_ref().map(|b| &**b)
    }
}

impl fmt::Display for LimitError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self.kind {
            LimitErrorKind::InsufficientMemory => write!(fmt, "Insufficient memory"),
            LimitErrorKind::DimensionError => write!(fmt, "Image is too large"),
            LimitErrorKind::__NonExhaustive => unreachable!(),
        }
    }
}

impl Error for LimitError { }

impl fmt::Display for ImageFormatHint {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            ImageFormatHint::Exact(format) => write!(fmt, "{:?}", format),
            ImageFormatHint::Name(name) => write!(fmt, "`{}`", name),
            ImageFormatHint::PathExtension(ext) => write!(fmt, "`.{:?}`", ext),
            ImageFormatHint::Unknown => write!(fmt, "`Unknown`"),
            ImageFormatHint::__NonExhaustive => unreachable!(),
        }
    }
}
