/// Describes the transformations to be applied to the image.
/// Compatible with [Exif orientation](https://web.archive.org/web/20200412005226/https://www.impulseadventure.com/photo/exif-orientation.html).
///
/// Orientation is specified in the Exif metadata, and is often written by cameras.
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum Orientation {
    /// Do not perform any transformations.
    NoTransforms,
    /// Rotate by 90 degrees clockwise.
    Rotate90,
    /// Rotate by 180 degrees. Can be performed in-place.
    Rotate180,
    /// Rotate by 270 degrees clockwise. Equivalent to rotating by 90 degrees counter-clockwise.
    Rotate270,
    /// Flip horizontally. Can be performed in-place.
    FlipHorizontal,
    /// Flip vertically. Can be performed in-place.
    FlipVertical,
    /// Rotate by 90 degrees clockwise and flip horizontally.
    Rotate90FlipH,
    /// Rotate by 270 degrees clockwise and flip horizontally.
    Rotate270FlipH,
}

impl Orientation {
    /// Converts from [Exif orientation](https://web.archive.org/web/20200412005226/https://www.impulseadventure.com/photo/exif-orientation.html)
    pub fn from_exif(exif_orientation: u8) -> Option<Self> {
        match exif_orientation {
            1 => Some(Self::NoTransforms),
            2 => Some(Self::FlipHorizontal),
            3 => Some(Self::Rotate180),
            4 => Some(Self::FlipVertical),
            5 => Some(Self::Rotate90FlipH),
            6 => Some(Self::Rotate90),
            7 => Some(Self::Rotate90FlipH),
            8 => Some(Self::Rotate270),
            0 | 9.. => None,
        }
    }
}
