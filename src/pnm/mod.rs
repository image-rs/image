//! Decoding of netpbm image formats (pbm, pgm, ppm and pam).
//!
//! The formats pbm, pgm and ppm are fully supported. The pam decoder recognizes the tuple types
//! `BLACKANDWHITE`, `GRAYSCALE` and `RGB` and explicitely recognizes but rejects their `_ALPHA`
//! variants for now as alpha color types are unsupported.

pub use self::decoder::PNMDecoder;
pub use self::encoder::PNMEncoder;

/// The kind of encoding used to store sample values
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum SampleEncoding {
    /// Samples are unsigned binary integers in big endian
    Binary,
    /// Samples are encoded as decimal ascii strings separated by whitespace
    Ascii,
}

/// Denotes the category of the magic number
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum PNMSubtype {
    /// Magic numbers P1 and P4
    Bitmap(SampleEncoding),
    /// Magic numbers P2 and P5
    Graymap(SampleEncoding),
    /// Magic numbers P3 and P6
    Pixmap(SampleEncoding),
    /// Magic number P7
    ArbitraryMap,
}

/// Stores the complete header data of a file.
///
/// Internally, provides mechanisms for lossless reencoding. After reading a file with the decoder
/// it is possible to recover the header and construct an encoder. Using the encoder on the just
/// loaded image should result in a byte copy of the original file (for single image pnms without
/// additional trailing data).
pub struct PNMHeader {
    decoded: HeaderRecord,
    encoded: Option<Vec<u8>>,
}

enum HeaderRecord {
    BitmapHeader {
        encoding: SampleEncoding,
        height: u32,
        width: u32,
    },
    GraymapHeader {
        encoding: SampleEncoding,
        height: u32,
        width: u32,
        depth: u32,
    },
    PixmapHeader{
        encoding: SampleEncoding,
        height: u32,
        width: u32,
        depth: u32,
    },
    ArbitraryHeader {
        height: u32,
        width: u32,
        depth: u32,
        maxval: u32,
        tupltype: ArbitraryTuplType,
    },
}

enum ArbitraryTuplType {
    BlackAndWhite,
    BlackAndWhiteAlpha,
    Grayscale,
    GrayscaleAlpha,
    RBG,
    RGBAlpha,
    Custom(String),
}

impl PNMSubtype {
    /// Get the two magic constant bytes corresponding to this format subtype.
    pub fn magic_constant(self) -> &'static [u8; 2] {
        match self {
            PNMSubtype::Bitmap(SampleEncoding::Ascii) => b"P1",
            PNMSubtype::Graymap(SampleEncoding::Ascii) => b"P2",
            PNMSubtype::Pixmap(SampleEncoding::Ascii) => b"P3",
            PNMSubtype::Bitmap(SampleEncoding::Binary) => b"P4",
            PNMSubtype::Graymap(SampleEncoding::Binary) => b"P5",
            PNMSubtype::Pixmap(SampleEncoding::Binary) => b"P6",
            PNMSubtype::ArbitraryMap => b"P7",
        }
    }
}

impl PNMHeader {
    /// Retrieve the format subtype from which the header was created.
    pub fn subtype(&self) -> PNMSubtype {
        match &self.decoded {
            &HeaderRecord::BitmapHeader { encoding, .. } => PNMSubtype::Bitmap(encoding),
            &HeaderRecord::GraymapHeader { encoding, .. } => PNMSubtype::Graymap(encoding),
            &HeaderRecord::PixmapHeader { encoding, .. } => PNMSubtype::Pixmap(encoding),
            &HeaderRecord::ArbitraryHeader { .. } => PNMSubtype::ArbitraryMap,
        }
    }

    /// The width of the image this header is for.
    pub fn width(&self) -> u32 {
        match &self.decoded {
            &HeaderRecord::BitmapHeader { width, .. } => width,
            &HeaderRecord::GraymapHeader { width, .. } => width,
            &HeaderRecord::PixmapHeader { width, .. } => width,
            &HeaderRecord::ArbitraryHeader { width, .. } => width,
        }
    }

    /// The height of the image this header is for.
    pub fn height(&self) -> u32 {
        match &self.decoded {
            &HeaderRecord::BitmapHeader { height, .. } => height,
            &HeaderRecord::GraymapHeader { height, .. } => height,
            &HeaderRecord::PixmapHeader { height, .. } => height,
            &HeaderRecord::ArbitraryHeader { height, .. } => height,
        }
    }
}

mod decoder;
mod encoder;
