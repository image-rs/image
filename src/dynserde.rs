use crate::{
    DynamicImage, ImageMetadata, Gray16Image, GrayAlpha16Image,
    GrayAlphaImage, GrayImage, Rgb16Image, Rgb32FImage, RgbImage,
    Rgba16Image, Rgba32FImage, RgbaImage,
};
use serde::{
    de::{self, Visitor},
    ser, Deserialize, Deserializer, Serialize,
};
use std::{
    fmt,
    io::{Read, Write},
};
use base64::{engine::general_purpose::STANDARD_NO_PAD, Engine};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
enum DynaColor {
    Luma8,
    LumaA8,
    Rgb8,
    Rgba8,
    Luma16,
    LumaA16,
    Rgb16,
    Rgba16,
    Rgb32F,
    Rgba32F,
}

struct SerialBuffer<'a> {
    data: &'a [u8],
    width: u32,
    height: u32,
    color: DynaColor,
    le: bool,
    meta: Option<&'a ImageMetadata>,
}

struct AllocSerialBuffer {
    data: Vec<u8>,
    width: u32,
    height: u32,
    color: DynaColor,
    le: bool,
    meta: Option<ImageMetadata>,
}

impl<'a> Serialize for SerialBuffer<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use flate2::write::ZlibEncoder;
        use serde::ser::SerializeStruct;
        let mut state = serializer.serialize_struct("SerialBuffer", 6)?;
        let mut encoder = ZlibEncoder::new(Vec::new(), flate2::Compression::default());
        let res = encoder.write_all(self.data);
        if res.is_err() {
            return Err(ser::Error::custom("Failed to compress data."));
        }
        let compressed_data = encoder.finish();
        if let Ok(compressed_data) = compressed_data {
            let compressed_data = compressed_data.as_slice();
            let compressed_data = STANDARD_NO_PAD.encode(compressed_data);
            state.serialize_field("data", &compressed_data)?;
            state.serialize_field("width", &self.width)?;
            state.serialize_field("height", &self.height)?;
            state.serialize_field("color", &self.color)?;
            state.serialize_field("le", &self.le)?;
            state.serialize_field("metadata", &self.meta)?;
            state.end()
        } else {
            Err(ser::Error::custom("Failed to compress data."))
        }
    }
}

impl<'de> Deserialize<'de> for DynamicImage {
    fn deserialize<D>(deserializer: D) -> Result<DynamicImage, D::Error>
    where
        D: Deserializer<'de>,
    {
        enum Field {
            Data,
            Width,
            Height,
            Color,
            Le,
            Metadata,
        }

        impl<'de> Deserialize<'de> for Field {
            fn deserialize<D>(deserializer: D) -> Result<Field, D::Error>
            where
                D: Deserializer<'de>,
            {
                struct FieldVisitor;

                impl<'de> Visitor<'de> for FieldVisitor {
                    type Value = Field;

                    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                        formatter.write_str("`secs` or `nanos`")
                    }

                    fn visit_str<E>(self, value: &str) -> Result<Field, E>
                    where
                        E: de::Error,
                    {
                        match value {
                            "data" => Ok(Field::Data),
                            "width" => Ok(Field::Width),
                            "height" => Ok(Field::Height),
                            "color" => Ok(Field::Color),
                            "le" => Ok(Field::Le),
                            "metadata" => Ok(Field::Metadata),
                            _ => Err(de::Error::unknown_field(value, FIELDS)),
                        }
                    }
                }

                deserializer.deserialize_identifier(FieldVisitor)
            }
        }

        struct SerialBufferVisitor;

        impl<'de> Visitor<'de> for SerialBufferVisitor {
            type Value = AllocSerialBuffer;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("struct SerialBuffer")
            }

            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: de::SeqAccess<'de>,
            {
                let data: String = seq
                    .next_element()?
                    .ok_or_else(|| de::Error::invalid_length(0, &self))?;
                let width = seq
                    .next_element()?
                    .ok_or_else(|| de::Error::invalid_length(1, &self))?;
                let height = seq
                    .next_element()?
                    .ok_or_else(|| de::Error::invalid_length(2, &self))?;
                let color = seq
                    .next_element()?
                    .ok_or_else(|| de::Error::invalid_length(3, &self))?;
                let le = seq
                    .next_element()?
                    .ok_or_else(|| de::Error::invalid_length(4, &self))?;
                let meta = seq
                    .next_element()?
                    .ok_or_else(|| de::Error::invalid_length(5, &self))?;
                let data = STANDARD_NO_PAD.decode(data.as_bytes()).map_err(|e| {
                    de::Error::custom(format!("Failed to decode base64: {}", e))
                })?;
                Ok(AllocSerialBuffer {
                    data,
                    width,
                    height,
                    color,
                    le,
                    meta,
                })
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: de::MapAccess<'de>,
            {
                let mut data: Option<String> = None;
                let mut width = None;
                let mut height = None;
                let mut color = None;
                let mut le = None;
                let mut meta = None;

                while let Some(key) = map.next_key()? {
                    match key {
                        Field::Data => {
                            if data.is_some() {
                                return Err(de::Error::duplicate_field("data"));
                            }
                            data = Some(map.next_value()?);
                        }
                        Field::Width => {
                            if width.is_some() {
                                return Err(de::Error::duplicate_field("width"));
                            }
                            width = Some(map.next_value()?);
                        }
                        Field::Height => {
                            if height.is_some() {
                                return Err(de::Error::duplicate_field("height"));
                            }
                            height = Some(map.next_value()?);
                        }
                        Field::Color => {
                            if color.is_some() {
                                return Err(de::Error::duplicate_field("color"));
                            }
                            color = Some(map.next_value()?);
                        }
                        Field::Le => {
                            if le.is_some() {
                                return Err(de::Error::duplicate_field("le"));
                            }
                            le = Some(map.next_value()?);
                        }
                        Field::Metadata => {
                            if meta.is_some() {
                                return Err(de::Error::duplicate_field("metadata"));
                            }
                            meta = Some(map.next_value()?);
                        }
                    }
                }

                let data = data.ok_or_else(|| de::Error::missing_field("data"))?;
                let data = STANDARD_NO_PAD.decode(data.as_bytes()).map_err(|e| {
                    de::Error::custom(format!("Failed to decode base64: {}", e))
                })?;
                let width = width.ok_or_else(|| de::Error::missing_field("width"))?;
                let height = height.ok_or_else(|| de::Error::missing_field("height"))?;
                let color = color.ok_or_else(|| de::Error::missing_field("color"))?;
                let le = le.ok_or_else(|| de::Error::missing_field("le"))?;
                let meta = meta.ok_or_else(|| de::Error::missing_field("metadata"))?;

                Ok(AllocSerialBuffer {
                    data,
                    width,
                    height,
                    color,
                    le,
                    meta,
                })
            }
        }

        const FIELDS: &[&str] = &["data", "width", "height", "color", "le", "metadata"];
        let res = deserializer.deserialize_struct("SerialBuffer", FIELDS, SerialBufferVisitor)?;

        use DynaColor::*;
        use DynamicImage::*;

        use flate2::read::ZlibDecoder;
        let mut decoder = ZlibDecoder::new(res.data.as_slice());
        let mut data = Vec::new();
        let dlen = decoder
            .read_to_end(&mut data)
            .map_err(|e| de::Error::custom(e.to_string()))?;
        let mut img = match res.color {
            Luma8 => {
                if dlen != res.width as usize * res.height as usize {
                    return Err(de::Error::custom("Data length does not match image size"));
                }
                let img = GrayImage::from_vec(res.width, res.height, data)
                    .ok_or(de::Error::custom("Could not convert"))?;
                ImageLuma8(img)
            }
            LumaA8 => {
                if dlen != res.width as usize * res.height as usize * 2 {
                    return Err(de::Error::custom("Data length does not match image size"));
                }
                let img = GrayAlphaImage::from_vec(res.width, res.height, data)
                    .ok_or(de::Error::custom("Could not convert"))?;
                ImageLumaA8(img)
            }
            Rgb8 => {
                if dlen != res.width as usize * res.height as usize * 3 {
                    return Err(de::Error::custom("Data length does not match image size"));
                }
                let img = RgbImage::from_raw(res.width, res.height, data)
                    .ok_or(de::Error::custom("Could not convert"))?;
                ImageRgb8(img)
            }
            Rgba8 => {
                if dlen != res.width as usize * res.height as usize * 4 {
                    return Err(de::Error::custom("Data length does not match image size"));
                }
                let img = RgbaImage::from_raw(res.width, res.height, data)
                    .ok_or(de::Error::custom("Could not convert"))?;
                ImageRgba8(img)
            }
            Luma16 => {
                if dlen != res.width as usize * res.height as usize * 2 {
                    return Err(de::Error::custom("Data length does not match image size"));
                }
                let data = bytemuck::cast_slice_mut::<u8, u16>(&mut data);
                if little_endian() != res.le {
                    data.iter_mut().for_each(|x| *x = x.swap_bytes());
                }
                let img = Gray16Image::from_vec(res.width, res.height, data.into())
                    .ok_or(de::Error::custom("Could not convert"))?;
                ImageLuma16(img)
            }
            LumaA16 => {
                if dlen != res.width as usize * res.height as usize * 4 {
                    return Err(de::Error::custom("Data length does not match image size"));
                }
                let data = bytemuck::cast_slice_mut::<u8, u16>(&mut data);
                if little_endian() != res.le {
                    data.iter_mut().for_each(|x| *x = x.swap_bytes());
                }
                let img = GrayAlpha16Image::from_vec(res.width, res.height, data.into())
                    .ok_or(de::Error::custom("Could not convert"))?;
                ImageLumaA16(img)
            }
            Rgb16 => {
                if dlen != res.width as usize * res.height as usize * 6 {
                    return Err(de::Error::custom("Data length does not match image size"));
                }
                let data = bytemuck::cast_slice_mut::<u8, u16>(&mut data);
                if little_endian() != res.le {
                    data.iter_mut().for_each(|x| *x = x.swap_bytes());
                }
                let img = Rgb16Image::from_raw(res.width, res.height, data.into())
                    .ok_or(de::Error::custom("Could not convert"))?;
                ImageRgb16(img)
            }
            Rgba16 => {
                if dlen != res.width as usize * res.height as usize * 8 {
                    return Err(de::Error::custom("Data length does not match image size"));
                }
                let data = bytemuck::cast_slice_mut::<u8, u16>(&mut data);
                if little_endian() != res.le {
                    data.iter_mut().for_each(|x| *x = x.swap_bytes());
                }
                let img = Rgba16Image::from_raw(res.width, res.height, data.into())
                    .ok_or(de::Error::custom("Could not convert"))?;
                ImageRgba16(img)
            }
            Rgb32F => {
                if dlen != res.width as usize * res.height as usize * 12 {
                    return Err(de::Error::custom("Data length does not match image size"));
                }
                let data = bytemuck::cast_slice_mut::<u8, u32>(&mut data);
                if little_endian() != res.le {
                    data.iter_mut().for_each(|x| *x = x.swap_bytes());
                }
                let data = bytemuck::cast_slice_mut::<u32, f32>(data);
                let img = Rgb32FImage::from_raw(res.width, res.height, data.into())
                    .ok_or(de::Error::custom("Could not convert"))?;
                ImageRgb32F(img)
            }
            Rgba32F => {
                if dlen != res.width as usize * res.height as usize * 16 {
                    return Err(de::Error::custom("Data length does not match image size"));
                }
                let data = bytemuck::cast_slice_mut::<u8, u32>(&mut data);
                if little_endian() != res.le {
                    data.iter_mut().for_each(|x| *x = x.swap_bytes());
                }
                let data = bytemuck::cast_slice_mut::<u32, f32>(data);
                let img = Rgba32FImage::from_raw(res.width, res.height, data.into())
                    .ok_or(de::Error::custom("Could not convert"))?;
                ImageRgba32F(img)
            }
        };
        if let Some(meta) = res.meta {
            img.set_metadata(meta);
        }
        Ok(img)
    }
}

impl From<&DynamicImage> for DynaColor {
    fn from(dynimage: &DynamicImage) -> Self {
        use DynamicImage::*;
        match dynimage {
            ImageLuma8(_) => DynaColor::Luma8,
            ImageLumaA8(_) => DynaColor::LumaA8,
            ImageRgb8(_) => DynaColor::Rgb8,
            ImageRgba8(_) => DynaColor::Rgba8,
            ImageLuma16(_) => DynaColor::Luma16,
            ImageLumaA16(_) => DynaColor::LumaA16,
            ImageRgb16(_) => DynaColor::Rgb16,
            ImageRgba16(_) => DynaColor::Rgba16,
            ImageRgb32F(_) => DynaColor::Rgb32F,
            ImageRgba32F(_) => DynaColor::Rgba32F,
        }
    }
}

impl<'a> From<&'a DynamicImage> for SerialBuffer<'a> {
    fn from(value: &'a DynamicImage) -> Self {
        let kind: DynaColor = value.into();
        let data = value.as_bytes();
        let meta = value.metadata();
        let width = value.width();
        let height = value.height();
        SerialBuffer {
            data,
            width,
            height,
            color: kind,
            le: little_endian(),
            meta,
        }
    }
}

impl Serialize for DynamicImage {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        SerialBuffer::from(self).serialize(serializer)
    }
}

const fn little_endian() -> bool {
    u16::from_ne_bytes([1, 0]) == 1
}
