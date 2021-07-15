use crate::encoder::FormatError;
use crate::encoder::FormatErrorKind;
use crate::{chunk, encoder, EncodingError};
use encoding::all::ISO_8859_1;
use encoding::{EncoderTrap, Encoding};
use std::io::Write;

/// Struct representing a tEXt chunk
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TEXtChunk {
    keyword: String,
    text: String,
}

impl TEXtChunk {
    pub fn encode<W: Write>(&self, w: &mut W) -> encoder::Result<()> {
        let mut data = ISO_8859_1
            .encode(&self.keyword, EncoderTrap::Strict)
            .map_err(|_| {
                EncodingError::Format(FormatError {
                    inner: FormatErrorKind::BadTextEncoding,
                })
            })?;

        if data.is_empty() || data.len() > 79 {
            return Err(EncodingError::Format(FormatError {
                inner: FormatErrorKind::BadTextEncoding,
            }));
        }

        data.push(0);

        ISO_8859_1
            .encode_to(&self.text, EncoderTrap::Strict, &mut data)
            .map_err(|_| {
                EncodingError::Format(FormatError {
                    inner: FormatErrorKind::BadTextEncoding,
                })
            })?;

        encoder::write_chunk(w, chunk::tEXt, &data)
    }
}
