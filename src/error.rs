//! Contains detailed error representation.

use std::{fmt, io, mem};
use std::error::Error;

use crate::color::ExtendedColorType;
use crate::image::ImageFormat;

/// The generic error type for image operations.
///
/// This high level enum is allows, by variant matching, a rough separation of concerns between
/// underlying IO, the caller, format specifications, and the `image` implementation.
#[derive(Debug)]
pub enum ImageError {
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
    underlying: Option<Box<dyn Error + Send + Sync>>,
}

#[derive(Debug)]
pub struct ParameterError {
    kind: ParameterErrorKind,
    underlying: Option<Box<dyn Error + Send + Sync>>,
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
    message: Option<String>,
    underlying: Option<Box<dyn Error + Send + Sync>>,
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

// Internal implementation block for ImageError.
#[allow(non_upper_case_globals)]
#[allow(non_snake_case)]
impl ImageError {
    pub(crate) const InsufficientMemory: Self =
        ImageError::Limits(LimitError {
            kind: LimitErrorKind::InsufficientMemory,
        });

    pub(crate) const DimensionError: Self =
        ImageError::Parameter(ParameterError {
            kind: ParameterErrorKind::DimensionMismatch,
            underlying: None,
        });

    pub(crate) const ImageEnd: Self =
        ImageError::Parameter(ParameterError {
            kind: ParameterErrorKind::FailedAlready,
            underlying: None,
        });

    pub(crate) fn UnsupportedError(message: String) -> Self {
        ImageError::Unsupported(UnsupportedError::legacy_from_string(message))
    }

    pub(crate) fn UnsupportedColor(color: ExtendedColorType) -> Self {
        ImageError::Unsupported(UnsupportedError::new(
            ImageFormatHint::Unknown,
            UnsupportedErrorKind::Color(color),
        ))
    }

    pub(crate) fn FormatError(message: String) -> Self {
        ImageError::Decoding(DecodingError::legacy_from_string(message))
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
    pub fn new(format: ImageFormatHint) -> Self {
        DecodingError {
            format,
            message: None,
            underlying: None,
        }
    }

    pub fn with_underlying(
        format: ImageFormatHint,
        err: impl Into<Box<dyn Error + Send + Sync>>,
    ) -> Self {
        DecodingError {
            format,
            message: None,
            underlying: Some(err.into()),
        }
    }

    pub fn format_hint(&self) -> ImageFormatHint {
        self.format.clone()
    }

    pub(crate) fn legacy_from_string(message: String) -> Self {
        DecodingError {
            format: ImageFormatHint::Unknown,
            message: Some(message),
            underlying: None,
        }
    }

    /// Not quite legacy but also highly discouraged.
    /// This is just since the string typing is prevalent in the `image` decoders...
    // TODO: maybe a Cow? A constructor from `&'static str` wouldn't be too bad.
    pub(crate) fn with_message(
        format: ImageFormatHint,
        message: String,
    ) -> Self {
        DecodingError {
            format,
            message: Some(message),
            underlying: None,
        }
    }

    fn message(&self) -> &str {
        match self.message {
            Some(ref st) => st.as_str(),
            None => "",
        }
    }
}

impl EncodingError {
    pub fn format_hint(&self) -> ImageFormatHint {
        self.format.clone()
    }
}

impl ParameterError {
    pub fn new(kind: ParameterErrorKind) -> Self {
        ParameterError {
            kind,
            underlying: None,
        }
    }

    pub fn kind(&self) -> ParameterErrorKind {
        self.kind.clone()
    }
}

impl LimitError {
    pub fn kind(&self) -> LimitErrorKind {
        self.kind.clone()
    }
}

impl From<io::Error> for ImageError {
    fn from(err: io::Error) -> ImageError {
        ImageError::IoError(err)
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

/// Result of an image decoding/encoding process
pub type ImageResult<T> = Result<T, ImageError>;

impl fmt::Display for ImageError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            ImageError::IoError(err) => err.fmt(fmt),
            ImageError::Decoding(err) => err.fmt(fmt),
            ImageError::Encoding(err) => err.fmt(fmt),
            ImageError::Parameter(err) => err.fmt(fmt),
            ImageError::Limits(err) => err.fmt(fmt),
            ImageError::Unsupported(err) => err.fmt(fmt),
        }
    }
}

impl Error for ImageError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            ImageError::IoError(err) => err.source(),
            ImageError::Decoding(err) => err.source(),
            ImageError::Encoding(err) => err.source(),
            ImageError::Parameter(err) => err.source(),
            ImageError::Limits(err) => err.source(),
            ImageError::Unsupported(err) => err.source(),
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
        match &self.underlying {
            None => None,
            Some(source) => Some(&**source),
        }
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
        match &self.underlying {
            None => None,
            Some(source) => Some(&**source),
        }
    }
}

impl fmt::Display for DecodingError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match &self.underlying {
            None => match self.format {
                ImageFormatHint::Unknown => write!(
                    fmt,
                    "Format error: {}",
                    self.message(),
                ),
                _ => write!(
                    fmt,
                    "Format error decoding {}: {}",
                    self.format,
                    self.message(),
                ),
            },
            Some(underlying) => write!(
                fmt,
                "Format error decoding {}: {}\n{}",
                self.format,
                self.message(),
                underlying,
            ),
        }
    }
}

impl Error for DecodingError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match &self.underlying {
            None => None,
            Some(source) => Some(&**source),
        }
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

#[cfg(test)]
mod tests {
    use super::*;

    #[allow(dead_code)]
    // This will fail to compile if the size of this type is large.
    const ASSERT_SMALLISH: usize = [0][(mem::size_of::<ImageError>() >= 200) as usize];

    #[test]
    fn test_send_sync_stability() {
        fn assert_send_sync<T: Send + Sync>() { }

        assert_send_sync::<ImageError>();
    }
}
