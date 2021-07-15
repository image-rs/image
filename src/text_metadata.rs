use crate::{chunk, encoder, EncodingError};
use encoding::all::ISO_8859_1;
use encoding::{DecoderTrap, EncoderTrap, Encoding};
use std::io::Write;

/// Text encoding errors that is wrapped by the standard EncodingError type
#[derive(Debug, Clone, Copy)]
pub(crate) enum TextEncodingError {
    /// Unrepresentable characters in string
    Unrepresentable,
    /// Keyword longer than 79 bytes or empty
    InvalidKeywordSize,
}

/// Text decoding error that is wrapped by the standard DecodingError type
#[derive(Debug, Clone, Copy)]
pub(crate) enum TextDecodingError {
    /// Unrepresentable characters in string
    Unrepresentable,
    /// Keyword longer than 79 bytes or empty
    InvalidKeywordSize,
    /// Missing null separator
    MissingNullSeparator,
}

/// Struct representing a tEXt chunk
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TEXtChunk {
    keyword: String,
    text: String,
}

impl TEXtChunk {
    /// Constructs a new TEXtChunk.
    /// Not sure whether it should take &str or String.
    pub fn new(keyword: &str, text: &str) -> Self {
        Self {
            keyword: keyword.to_string(),
            text: text.to_string(),
        }
    }

    /// Decodes a slice of bytes to a String using Latin-1 decoding.
    /// The decoder runs in strict mode, and any decoding errors are passed along to the caller.
    pub(crate) fn decode(
        keyword_slice: &[u8],
        text_slice: &[u8],
    ) -> Result<Self, TextDecodingError> {
        if keyword_slice.is_empty() || keyword_slice.len() > 79 {
            return Err(TextDecodingError::InvalidKeywordSize);
        }

        Ok(Self {
            keyword: ISO_8859_1
                .decode(keyword_slice, DecoderTrap::Strict)
                .map_err(|_| TextDecodingError::Unrepresentable)?,
            text: ISO_8859_1
                .decode(text_slice, DecoderTrap::Strict)
                .map_err(|_| TextDecodingError::Unrepresentable)?,
        })
    }

    /// Encodes TEXtChunk to a Writer. The keyword and text are separated by a byte of zeroes.
    pub fn encode<W: Write>(&self, w: &mut W) -> Result<(), EncodingError> {
        let mut data = ISO_8859_1
            .encode(&self.keyword, EncoderTrap::Strict)
            .map_err(|_| EncodingError::from(TextEncodingError::Unrepresentable))?;

        if data.is_empty() || data.len() > 79 {
            return Err(TextEncodingError::InvalidKeywordSize.into());
        }

        data.push(0);

        ISO_8859_1
            .encode_to(&self.text, EncoderTrap::Strict, &mut data)
            .map_err(|_| EncodingError::from(TextEncodingError::Unrepresentable))?;

        encoder::write_chunk(w, chunk::tEXt, &data)
    }
}
