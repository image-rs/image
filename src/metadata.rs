//! Types describing image metadata

use core::mem::offset_of;

use byteorder_lite::{BigEndian, ByteOrder, LittleEndian};

/// Describes the transformations to be applied to the image.
/// Compatible with [Exif orientation](https://web.archive.org/web/20200412005226/https://www.impulseadventure.com/photo/exif-orientation.html).
///
/// Orientation is specified in the file's metadata, and is often written by cameras.
///
/// You can apply it to an image via [`DynamicImage::apply_orientation`](crate::DynamicImage::apply_orientation).
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
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
        match chunk {
            [0x49, 0x49, 42, 0, ..] => Self::parse_exif_chunk::<LittleEndian>(chunk),
            [0x4d, 0x4d, 0, 42, ..] => Self::parse_exif_chunk::<BigEndian>(chunk),
            _ => None,
        }
    }

    fn parse_exif_chunk<T: ByteOrder>(chunk: &[u8]) -> Option<Self> {
        // First 4 bytes are magic
        let mut i = 4usize;

        if chunk.len() < i + size_of::<u32>() {
            return None;
        }
        let ifd_offset = T::read_u32(&chunk[i..]);
        i = usize::try_from(ifd_offset).ok()?;

        if chunk.len() < i + size_of::<u16>() {
            return None;
        }
        let entries = T::read_u16(&chunk[i..]);
        i += size_of::<u16>();

        let value = chunk[i..]
            .chunks_exact(size_of::<ExifEntry>())
            .take(usize::from(entries))
            .filter_map(ExifEntry::try_from_bytes::<T>)
            .find(|entry| entry.tag == 0x112 && entry.format == 3 && entry.count == 1)
            .map(|entry| entry.value.min(255) as u8)?;

        Self::from_exif(value)
    }
}

#[repr(C)]
struct ExifEntry {
    tag: u16,
    format: u16,
    count: u32,
    value: u16,
    _padding: u16,
}

impl ExifEntry {
    fn try_from_bytes<T: ByteOrder>(bytes: &[u8]) -> Option<Self> {
        if bytes.len() < size_of::<Self>() {
            return None;
        }

        let tag = T::read_u16(
            &bytes[(offset_of!(Self, tag))..(offset_of!(Self, tag) + size_of::<u16>())],
        );
        let format = T::read_u16(
            &bytes[(offset_of!(Self, format))..(offset_of!(Self, format) + size_of::<u16>())],
        );
        let count = T::read_u32(
            &bytes[(offset_of!(Self, count))..(offset_of!(Self, count) + size_of::<u32>())],
        );
        let value = T::read_u16(
            &bytes[(offset_of!(Self, value))..(offset_of!(Self, value) + size_of::<u16>())],
        );
        let _padding = T::read_u16(
            &bytes[(offset_of!(Self, _padding))..(offset_of!(Self, _padding) + size_of::<u16>())],
        );

        Some(Self {
            tag,
            format,
            count,
            value,
            _padding,
        })
    }
}
