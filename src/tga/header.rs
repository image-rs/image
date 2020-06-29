pub(crate) const ALPHA_BIT_MASK: u8 = 0b1111;
pub(crate) const SCREEN_ORIGIN_BIT_MASK: u8 = 0b10_0000;

pub(crate) enum ImageType {
    NoImageData = 0,
    /// Uncompressed images.
    RawColorMap = 1,
    RawTrueColor = 2,
    RawGrayScale = 3,
    /// Run length encoded images.
    RunColorMap = 9,
    RunTrueColor = 10,
    RunGrayScale = 11,
    Unknown,
}

impl ImageType {
    /// Create a new image type from a u8.
    pub(crate) fn new(img_type: u8) -> ImageType {
        match img_type {
            0 => ImageType::NoImageData,

            1 => ImageType::RawColorMap,
            2 => ImageType::RawTrueColor,
            3 => ImageType::RawGrayScale,

            9 => ImageType::RunColorMap,
            10 => ImageType::RunTrueColor,
            11 => ImageType::RunGrayScale,

            _ => ImageType::Unknown,
        }
    }

    /// Check if the image format uses colors as opposed to gray scale.
    pub(crate) fn is_color(&self) -> bool {
        match *self {
            ImageType::RawColorMap
            | ImageType::RawTrueColor
            | ImageType::RunTrueColor
            | ImageType::RunColorMap => true,
            _ => false,
        }
    }

    /// Does the image use a color map.
    pub(crate) fn is_color_mapped(&self) -> bool {
        match *self {
            ImageType::RawColorMap | ImageType::RunColorMap => true,
            _ => false,
        }
    }

    /// Is the image run length encoded.
    pub(crate) fn is_encoded(&self) -> bool {
        match *self {
            ImageType::RunColorMap | ImageType::RunTrueColor | ImageType::RunGrayScale => true,
            _ => false,
        }
    }
}

/// Header used by TGA image files.
#[derive(Debug, Default)]
pub(crate) struct Header {
    pub(crate) id_length: u8,      // length of ID string
    pub(crate) map_type: u8,       // color map type
    pub(crate) image_type: u8,     // image type code
    pub(crate) map_origin: u16,    // starting index of map
    pub(crate) map_length: u16,    // length of map
    pub(crate) map_entry_size: u8, // size of map entries in bits
    pub(crate) x_origin: u16,      // x-origin of image
    pub(crate) y_origin: u16,      // y-origin of image
    pub(crate) image_width: u16,   // width of image
    pub(crate) image_height: u16,  // height of image
    pub(crate) pixel_depth: u8,    // bits per pixel
    pub(crate) image_desc: u8,     // image descriptor
}
