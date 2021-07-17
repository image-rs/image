use crate::{chunk, encoder, DecodingError, EncodingError};
use deflate::write::ZlibEncoder;
use deflate::Compression;
use encoding::all::{ASCII, ISO_8859_1};
use encoding::{DecoderTrap, EncoderTrap, Encoding};
use miniz_oxide::inflate::decompress_to_vec_zlib;
use std::io::Write;

/// Text encoding errors that is wrapped by the standard EncodingError type
#[derive(Debug, Clone, Copy)]
pub(crate) enum TextEncodingError {
    /// Unrepresentable characters in string
    Unrepresentable,
    /// Keyword longer than 79 bytes or empty
    InvalidKeywordSize,
    /// Error encountered while compressing text
    CompressionError,
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
    /// Compressed text cannot be uncompressed
    InflationError,
    /// Using an unspecified value for the compression method
    InvalidCompressionMethod,
    /// Using a byte that is not 0 or 255 as compression flag in iTXt chunk
    InvalidCompressionFlag,
    /// Missing the compression flag
    MissingCompressionFlag,
}

/// A generalized text chunk trait
pub trait EncodableTextChunk {
    fn encode<W: Write>(&self, w: &mut W) -> Result<(), EncodingError>;
}

/// Struct representing a tEXt chunk
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TEXtChunk {
    pub keyword: String,
    pub text: String,
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
}

impl EncodableTextChunk for TEXtChunk {
    /// Encodes TEXtChunk to a Writer. The keyword and text are separated by a byte of zeroes.
    fn encode<W: Write>(&self, w: &mut W) -> Result<(), EncodingError> {
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ZTXtChunk {
    pub keyword: String,
    pub optionally_compressed_text: OptCompressed,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum OptCompressed {
    Compressed(Vec<u8>),
    Uncompressed(String),
}

impl ZTXtChunk {
    pub fn new(keyword: &str, text: &str) -> Self {
        Self {
            keyword: keyword.to_string(),
            optionally_compressed_text: OptCompressed::Uncompressed(text.to_string()),
        }
    }

    pub(crate) fn decode(
        keyword_slice: &[u8],
        compression_method: u8,
        text_slice: &[u8],
    ) -> Result<Self, TextDecodingError> {
        if keyword_slice.is_empty() || keyword_slice.len() > 79 {
            return Err(TextDecodingError::InvalidKeywordSize);
        }

        if compression_method != 0 {
            return Err(TextDecodingError::InvalidCompressionMethod);
        }

        Ok(Self {
            keyword: ISO_8859_1
                .decode(keyword_slice, DecoderTrap::Strict)
                .map_err(|_| TextDecodingError::Unrepresentable)?,
            optionally_compressed_text: OptCompressed::Compressed(
                text_slice.iter().cloned().collect(),
            ),
        })
    }

    pub fn decompress_text(&mut self) -> Result<(), DecodingError> {
        match &self.optionally_compressed_text {
            OptCompressed::Compressed(v) => {
                let uncompressed_raw = decompress_to_vec_zlib(&v[..])
                    .map_err(|_| DecodingError::from(TextDecodingError::InflationError))?;
                self.optionally_compressed_text = OptCompressed::Uncompressed(
                    ISO_8859_1
                        .decode(&uncompressed_raw, DecoderTrap::Strict)
                        .map_err(|_| DecodingError::from(TextDecodingError::Unrepresentable))?,
                )
            }
            OptCompressed::Uncompressed(_) => {}
        };
        Ok(())
    }

    pub fn compress_text(&mut self) -> Result<(), EncodingError> {
        match &self.optionally_compressed_text {
            OptCompressed::Uncompressed(s) => {
                let uncompressed_raw = ISO_8859_1
                    .encode(s, EncoderTrap::Strict)
                    .map_err(|_| EncodingError::from(TextEncodingError::Unrepresentable))?;
                let mut encoder = ZlibEncoder::new(Vec::new(), Compression::Fast);
                encoder
                    .write_all(&uncompressed_raw)
                    .map_err(|_| EncodingError::from(TextEncodingError::CompressionError))?;
                self.optionally_compressed_text = OptCompressed::Compressed(
                    encoder
                        .finish()
                        .map_err(|_| EncodingError::from(TextEncodingError::CompressionError))?,
                )
            }
            OptCompressed::Compressed(_) => {}
        }

        Ok(())
    }
}

impl EncodableTextChunk for ZTXtChunk {
    fn encode<W: Write>(&self, w: &mut W) -> Result<(), EncodingError> {
        let mut data = ISO_8859_1
            .encode(&self.keyword, EncoderTrap::Strict)
            .map_err(|_| EncodingError::from(TextEncodingError::Unrepresentable))?;

        if data.is_empty() || data.len() > 79 {
            return Err(TextEncodingError::InvalidKeywordSize.into());
        }

        // Null separator
        data.push(0);

        // Compression method: the only valid value is 0, as of 2021.
        data.push(0);

        match &self.optionally_compressed_text {
            OptCompressed::Compressed(v) => {
                data.extend_from_slice(&v[..]);
            }
            OptCompressed::Uncompressed(s) => {
                // This code may have a bug. Check for correctness.
                let uncompressed_raw = ISO_8859_1
                    .encode(s, EncoderTrap::Strict)
                    .map_err(|_| EncodingError::from(TextEncodingError::Unrepresentable))?;
                let mut encoder = ZlibEncoder::new(data, Compression::Fast);
                encoder
                    .write_all(&uncompressed_raw)
                    .map_err(|_| EncodingError::from(TextEncodingError::CompressionError))?;
                data = encoder
                    .finish()
                    .map_err(|_| EncodingError::from(TextEncodingError::CompressionError))?;
            }
        };

        encoder::write_chunk(w, chunk::zTXt, &data)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ITXtChunk {
    pub keyword: String,
    pub compressed: bool,
    pub language_tag: String,
    pub translated_keyword: String,
    pub optionally_compressed_text: OptCompressed,
}

impl Default for ITXtChunk {
    fn default() -> Self {
        Self {
            keyword: "Default keyword".to_string(),
            compressed: false,
            language_tag: String::default(),
            translated_keyword: String::default(),
            optionally_compressed_text: OptCompressed::Uncompressed(String::default()),
        }
    }
}

impl ITXtChunk {
    pub fn new(keyword: &str, text: &str) -> Self {
        Self {
            keyword: keyword.to_string(),
            optionally_compressed_text: OptCompressed::Uncompressed(text.to_string()),
            ..Default::default()
        }
    }

    pub(crate) fn decode(
        keyword_slice: &[u8],
        compression_flag: u8,
        compression_method: u8,
        language_tag_slice: &[u8],
        translated_keyword_slice: &[u8],
        text_slice: &[u8],
    ) -> Result<Self, TextDecodingError> {
        if keyword_slice.is_empty() || keyword_slice.len() > 79 {
            return Err(TextDecodingError::InvalidKeywordSize);
        }
        let keyword = ISO_8859_1
            .decode(keyword_slice, DecoderTrap::Strict)
            .map_err(|_| TextDecodingError::Unrepresentable)?;

        let compressed = match compression_flag {
            0 => false,
            255 => true,
            _ => return Err(TextDecodingError::InvalidCompressionFlag),
        };

        if compressed && compression_method != 0 {
            return Err(TextDecodingError::InvalidCompressionMethod);
        }

        let language_tag = ASCII
            .decode(language_tag_slice, DecoderTrap::Strict)
            .map_err(|_| TextDecodingError::Unrepresentable)?;

        let translated_keyword =
            String::from_utf8(translated_keyword_slice.iter().cloned().collect())
                .map_err(|_| TextDecodingError::Unrepresentable)?;
        let optionally_compressed_text = if compressed {
            OptCompressed::Compressed(text_slice.iter().cloned().collect())
        } else {
            OptCompressed::Uncompressed(
                String::from_utf8(text_slice.iter().cloned().collect())
                    .map_err(|_| TextDecodingError::Unrepresentable)?,
            )
        };

        Ok(Self {
            keyword,
            compressed,
            language_tag,
            translated_keyword,
            optionally_compressed_text,
        })
    }

    pub fn decompress_text(&mut self) -> Result<(), DecodingError> {
        match &self.optionally_compressed_text {
            OptCompressed::Compressed(v) => {
                let uncompressed_raw = decompress_to_vec_zlib(&v[..])
                    .map_err(|_| DecodingError::from(TextDecodingError::InflationError))?;
                self.optionally_compressed_text = OptCompressed::Uncompressed(
                    String::from_utf8(uncompressed_raw)
                        .map_err(|_| TextDecodingError::Unrepresentable)?,
                )
            }
            OptCompressed::Uncompressed(_) => {}
        };
        Ok(())
    }

    pub fn compress_text(&mut self) -> Result<(), EncodingError> {
        match &self.optionally_compressed_text {
            OptCompressed::Uncompressed(s) => {
                let uncompressed_raw = s.as_bytes();
                let mut encoder = ZlibEncoder::new(Vec::new(), Compression::Fast);
                encoder
                    .write_all(&uncompressed_raw)
                    .map_err(|_| EncodingError::from(TextEncodingError::CompressionError))?;
                self.optionally_compressed_text = OptCompressed::Compressed(
                    encoder
                        .finish()
                        .map_err(|_| EncodingError::from(TextEncodingError::CompressionError))?,
                )
            }
            OptCompressed::Compressed(_) => {}
        }

        Ok(())
    }
}

impl EncodableTextChunk for ITXtChunk {
    fn encode<W: Write>(&self, w: &mut W) -> Result<(), EncodingError> {
        // Keyword
        let mut data = ISO_8859_1
            .encode(&self.keyword, EncoderTrap::Strict)
            .map_err(|_| EncodingError::from(TextEncodingError::Unrepresentable))?;

        if data.is_empty() || data.len() > 79 {
            return Err(TextEncodingError::InvalidKeywordSize.into());
        }

        // Null separator
        data.push(0);

        // Compression flag
        if self.compressed {
            data.push(255);
        } else {
            data.push(0);
        }

        // Compression method
        data.push(0);

        // Language tag
        ASCII
            .encode_to(&self.language_tag, EncoderTrap::Strict, &mut data)
            .map_err(|_| EncodingError::from(TextEncodingError::Unrepresentable))?;

        // Null separator
        data.push(0);

        // Translated keyword
        data.extend_from_slice(&self.translated_keyword.as_bytes());

        // Null separator
        data.push(0);

        // Text
        if self.compressed {
            match &self.optionally_compressed_text {
                OptCompressed::Compressed(v) => {
                    data.extend_from_slice(&v[..]);
                }
                OptCompressed::Uncompressed(s) => {
                    let uncompressed_raw = s.as_bytes();
                    let mut encoder = ZlibEncoder::new(data, Compression::Fast);
                    encoder
                        .write_all(&uncompressed_raw)
                        .map_err(|_| EncodingError::from(TextEncodingError::CompressionError))?;
                    data = encoder
                        .finish()
                        .map_err(|_| EncodingError::from(TextEncodingError::CompressionError))?;
                }
            }
        } else {
            match &self.optionally_compressed_text {
                OptCompressed::Compressed(v) => {
                    let uncompressed_raw = decompress_to_vec_zlib(&v[..])
                        .map_err(|_| EncodingError::from(TextEncodingError::CompressionError))?;
                    data.extend_from_slice(&uncompressed_raw[..]);
                }
                OptCompressed::Uncompressed(s) => {
                    data.extend_from_slice(s.as_bytes());
                }
            }
        }

        encoder::write_chunk(w, chunk::iTXt, &data)
    }
}
