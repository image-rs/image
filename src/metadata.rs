//! Types describing image metadata

use std::io::{Cursor, Read};

use byteorder_lite::{BigEndian, LittleEndian, ReadBytesExt};

/// Describes the transformations to be applied to the image.
/// Compatible with [Exif orientation](https://web.archive.org/web/20200412005226/https://www.impulseadventure.com/photo/exif-orientation.html).
///
/// Orientation is specified in the file's metadata, and is often written by cameras.
///
/// You can apply it to an image via [`DynamicImage::apply_orientation`](crate::DynamicImage::apply_orientation).
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
    #[must_use]
    pub fn from_exif(exif_orientation: u8) -> Option<Self> {
        match exif_orientation {
            1 => Some(Self::NoTransforms),
            2 => Some(Self::FlipHorizontal),
            3 => Some(Self::Rotate180),
            4 => Some(Self::FlipVertical),
            5 => Some(Self::Rotate90FlipH),
            6 => Some(Self::Rotate90),
            7 => Some(Self::Rotate270FlipH),
            8 => Some(Self::Rotate270),
            0 | 9.. => None,
        }
    }

    /// Converts into [Exif orientation](https://web.archive.org/web/20200412005226/https://www.impulseadventure.com/photo/exif-orientation.html)
    #[must_use]
    pub fn to_exif(self) -> u8 {
        match self {
            Self::NoTransforms => 1,
            Self::FlipHorizontal => 2,
            Self::Rotate180 => 3,
            Self::FlipVertical => 4,
            Self::Rotate90FlipH => 5,
            Self::Rotate90 => 6,
            Self::Rotate270FlipH => 7,
            Self::Rotate270 => 8,
        }
    }

    pub(crate) fn from_exif_chunk(chunk: &[u8]) -> Option<Self> {
        let mut reader = Cursor::new(chunk);

        let mut magic = [0; 4];
        reader.read_exact(&mut magic).ok()?;

        match magic {
            [0x49, 0x49, 42, 0] => {
                let ifd_offset = reader.read_u32::<LittleEndian>().ok()?;
                reader.set_position(u64::from(ifd_offset));
                let entries = reader.read_u16::<LittleEndian>().ok()?;
                for _ in 0..entries {
                    let tag = reader.read_u16::<LittleEndian>().ok()?;
                    let format = reader.read_u16::<LittleEndian>().ok()?;
                    let count = reader.read_u32::<LittleEndian>().ok()?;
                    let value = reader.read_u16::<LittleEndian>().ok()?;
                    let _padding = reader.read_u16::<LittleEndian>().ok()?;
                    if tag == 0x112 && format == 3 && count == 1 {
                        return Self::from_exif(value.min(255) as u8);
                    }
                }
            }
            [0x4d, 0x4d, 0, 42] => {
                let ifd_offset = reader.read_u32::<BigEndian>().ok()?;
                reader.set_position(u64::from(ifd_offset));
                let entries = reader.read_u16::<BigEndian>().ok()?;
                for _ in 0..entries {
                    let tag = reader.read_u16::<BigEndian>().ok()?;
                    let format = reader.read_u16::<BigEndian>().ok()?;
                    let count = reader.read_u32::<BigEndian>().ok()?;
                    let value = reader.read_u16::<BigEndian>().ok()?;
                    let _padding = reader.read_u16::<BigEndian>().ok()?;
                    if tag == 0x112 && format == 3 && count == 1 {
                        return Self::from_exif(value.min(255) as u8);
                    }
                }
            }
            _ => {}
        }
        None
    }
}
