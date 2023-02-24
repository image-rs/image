//! Contains detailed error representation.
//!
//! See the main [`ImageError`] which contains a variant for each specialized error type. The
//! subtypes used in each variant are opaque by design. They can be roughly inspected through their
//! respective `kind` methods which work similar to `std::io::Error::kind`.
//!
//! The error interface makes it possible to inspect the error of an underlying decoder or encoder,
//! through the `Error::source` method. Note that this is not part of the stable interface and you
//! may not rely on a particular error value for a particular operation. This means mainly that
//! `image` does not promise to remain on a particular version of its underlying decoders but if
//! you ensure to use the same version of the dependency (or at least of the error type) through
//! external means then you could inspect the error type in slightly more detail.
//!
//! [`ImageError`]: enum.ImageError.html

use alloc::boxed::Box;
use alloc::string::String;
use core::fmt;
use snafu::{prelude::*, Error};
#[cfg(feature = "std")]
use std::io;

use crate::color::ExtendedColorType;
use crate::image::ImageFormat;

/// The generic error type for image operations.
///
/// This high level enum allows, by variant matching, a rough separation of concerns between
/// underlying IO, the caller, format specifications, and the `image` implementation.
#[derive(Snafu, Debug)]
pub enum ImageError {
    /// An error was encountered while decoding.
    ///
    /// This means that the input data did not conform to the specification of some image format,
    /// or that no format could be determined, or that it did not match format specific
    /// requirements set by the caller.
    ///
    /// An error was encountered while decoding an image.
    ///
    /// This is used as an opaque representation for the [`ImageError::Decoding`] variant. See its
    /// documentation for more information.
    ///
    /// [`ImageError::Decoding`]: enum.ImageError.html#variant.Decoding
    Decoding { format: ImageFormatHint },

    /// An error was encountered while encoding.
    ///
    /// The input image can not be encoded with the chosen format, for example because the
    /// specification has no representation for its color space or because a necessary conversion
    /// is ambiguous. In some cases it might also happen that the dimensions can not be used with
    /// the format.
    ///
    /// An error was encountered while encoding an image.
    ///
    /// This is used as an opaque representation for the [`ImageError::Encoding`] variant. See its
    /// documentation for more information.
    ///
    /// [`ImageError::Encoding`]: enum.ImageError.html#variant.Encoding
    Encoding { format: ImageFormatHint },

    /// An error was encountered in input arguments.
    ///
    /// This is a catch-all case for strictly internal operations such as scaling, conversions,
    /// etc. that involve no external format specifications.
    ///
    /// An error was encountered in inputs arguments.
    ///
    /// This is used as an opaque representation for the [`ImageError::Parameter`] variant. See its
    /// documentation for more information.
    ///
    /// [`ImageError::Parameter`]: enum.ImageError.html#variant.Parameter
    Parameter { kind: ParameterErrorKind },

    /// Completing the operation would have required more resources than allowed.
    ///
    /// Errors of this type are limits set by the user or environment, *not* inherent in a specific
    /// format or operation that was executed.
    ///
    /// Completing the operation would have required more resources than allowed.
    ///
    /// This is used as an opaque representation for the [`ImageError::Limits`] variant. See its
    /// documentation for more information.
    ///
    /// [`ImageError::Limits`]: enum.ImageError.html#variant.Limits
    Limits { kind: LimitErrorKind },

    /// An operation can not be completed by the chosen abstraction.
    ///
    /// This means that it might be possible for the operation to succeed in general but
    /// * it requires a disabled feature,
    /// * the implementation does not yet exist, or
    /// * no abstraction for a lower level could be found.
    ///
    /// The implementation for an operation was not provided.
    ///
    /// See the variant [`Unsupported`] for more documentation.
    ///
    /// [`Unsupported`]: enum.ImageError.html#variant.Unsupported
    Unsupported {
        format: ImageFormatHint,
        kind: UnsupportedErrorKind,
    },

    /// An error occurred while interacting with the environment.
    #[cfg(feature = "std")]
    IoError { err: io::Error },
}

/// Details what feature is not supported.
#[derive(Clone, Debug, Hash, PartialEq)]
#[non_exhaustive]
pub enum UnsupportedErrorKind {
    /// The required color type can not be handled.
    Color(ExtendedColorType),
    /// An image format is not supported.
    Format(ImageFormatHint),
    /// Some feature specified by string.
    /// This is discouraged and is likely to get deprecated (but not removed).
    GenericFeature(String),
}

/// Details how a parameter is malformed.
#[derive(Clone, Debug, Hash, PartialEq)]
#[non_exhaustive]
pub enum ParameterErrorKind {
    /// The dimensions passed are wrong.
    DimensionMismatch,
    /// Repeated an operation for which error that could not be cloned was emitted already.
    FailedAlready,
    /// A string describing the parameter.
    /// This is discouraged and is likely to get deprecated (but not removed).
    Generic(String),
    /// The end of the image has been reached.
    NoMoreData,
}

/// Indicates the limit that prevented an operation from completing.
///
/// Note that this enumeration is not exhaustive and may in the future be extended to provide more
/// detailed information or to incorporate other resources types.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[non_exhaustive]
#[allow(missing_copy_implementations)] // Might be non-Copy in the future.
pub enum LimitErrorKind {
    /// The resulting image exceed dimension limits in either direction.
    DimensionError,
    /// The operation would have performed an allocation larger than allowed.
    InsufficientMemory,
    /// The specified strict limits are not supported for this operation
    Unsupported {
        /// The given limits
        limits: crate::io::Limits,
        /// The supported strict limits
        supported: crate::io::LimitSupport,
    },
}

/// A best effort representation for image formats.
#[derive(Clone, Debug, Hash, PartialEq)]
#[non_exhaustive]
pub enum ImageFormatHint {
    /// The format is known exactly.
    Exact(ImageFormat),

    /// The format can be identified by a name.
    Name(String),

    /// A common path extension for the format is known.
    #[cfg(feature = "std")]
    PathExtension(std::path::PathBuf),

    /// The format is not known or could not be determined.
    Unknown,
}

#[cfg(feature = "std")]
impl From<io::Error> for ImageError {
    fn from(err: io::Error) -> ImageError {
        ImageError::IoError { err }
    }
}

impl From<ImageFormat> for ImageFormatHint {
    fn from(format: ImageFormat) -> Self {
        ImageFormatHint::Exact(format)
    }
}

#[cfg(feature = "std")]
impl From<&'_ std::path::Path> for ImageFormatHint {
    fn from(path: &'_ std::path::Path) -> Self {
        match path.extension() {
            Some(ext) => ImageFormatHint::PathExtension(ext.into()),
            None => ImageFormatHint::Unknown,
        }
    }
}

/// Result of an image decoding/encoding process
pub type ImageResult<T> = Result<T, ImageError>;

#[cfg(test)]
mod tests {
    use super::*;
    use std::mem;

    #[allow(dead_code)]
    // This will fail to compile if the size of this type is large.
    const ASSERT_SMALLISH: usize = [0][(mem::size_of::<ImageError>() >= 200) as usize];

    #[test]
    fn test_send_sync_stability() {
        fn assert_send_sync<T: Send + Sync>() {}

        assert_send_sync::<ImageError>();
    }
}
