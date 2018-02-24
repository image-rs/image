//! Encoding of PPM Images
use std::io;
use std::io::Write;

use super::{PNMHeader, PNMSubtype};

enum HeaderStrategy {
    Dynamic,
    Subtype(PNMSubtype),
    Chosen(PNMHeader),
}

pub struct PNMEncoder<'a> {
    writer: &'a mut Write,
    header: HeaderStrategy,
}


impl<'a> PNMEncoder<'a> {
    /// Create new PNMEncoder from the `writer`.
    ///
    /// The encoded images will have some `pnm` format. If more control over the image type is
    /// required, use either one of `with_subtype` or `with_header`. For more information on the
    /// behaviour, see `with_dynamic_header`.
    pub fn new(writer: &'a mut Write) -> Self {
        PNMEncoder {
            writer,
            header: HeaderStrategy::Dynamic,
        }
    }

    /// Encode a specific pnm subtype image.
    ///
    /// The magic number and encoding type will be chosen as provided while the rest of the header
    /// data will be generated dynamically. Trying to encode incompatible images (e.g. encoding an
    /// RGB image as Graymap) will result in an error.
    ///
    /// This will overwrite the effect of earlier calls to `with_header` and `with_dynamic_header`.
    pub fn with_subtype(self, subtype: PNMSubtype) -> Self {
        PNMEncoder {
            writer: self.writer,
            header: HeaderStrategy::Subtype(subtype),
        }
    }

    /// Enforce the use of a chosen header.
    ///
    /// While this option gives the most control over the actual written data, the encoding process
    /// will error in case the header data and image parameters do not agree. It is the users
    /// obligation to ensure that the width and height are set accordingly, for example.
    ///
    /// Choose this option if you want a lossless decoding/encoding round trip.
    ///
    /// This will overwrite the effect of earlier calls to `with_subtype` and `with_dynamic_header`.
    pub fn with_header(self, header: PNMHeader) -> Self {
        PNMEncoder {
            writer: self.writer,
            header: HeaderStrategy::Chosen(header)
        }
    }

    /// Create the header dynamically for each image.
    ///
    /// This is the default option upon creation of the encoder. With this, most images should be
    /// encodable but the specific format chosen is out of the users control. The pnm subtype is
    /// chosen arbitrarily by the library.
    ///
    /// This will overwrite the effect of earlier calls to `with_subtype` and `with_header`.
    pub fn with_dynamic_header(self) -> Self {
        PNMEncoder {
            writer: self.writer,
            header: HeaderStrategy::Dynamic,
        }
    }
}
