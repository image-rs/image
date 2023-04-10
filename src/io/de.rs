use crate::{ColorType, DynamicImage, ImageBuffer};
use num_traits::Zero;
use serde::de::{MapAccess, Visitor};
use serde::{Deserialize, Deserializer};
use std::fmt::Formatter;

impl<'de> Deserialize<'de> for ColorType {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let image_type = String::deserialize(deserializer)?;
        let color_type = match image_type.as_str() {
            "ImageLuma8" => ColorType::L8,
            "ImageLumaA8" => ColorType::La8,
            "ImageRgb8" => ColorType::Rgb8,
            "ImageRgba8" => ColorType::Rgba8,
            "ImageLuma16" => ColorType::L16,
            "ImageLumaA16" => ColorType::La16,
            "ImageRgb16" => ColorType::Rgb16,
            "ImageRgba16" => ColorType::Rgba16,
            "ImageRgb32F" => ColorType::Rgb32F,
            "ImageRgba32F" => ColorType::Rgba32F,
            _ => unreachable!("Unsupported color type: {:?}", image_type),
        };
        Ok(color_type)
    }
}

impl<'de> Deserialize<'de> for DynamicImage {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_struct(
            "DynamicImage",
            &["type", "width", "height", "data"],
            DynamicImageVisitor {
                color_type: "".to_string(),
                width: 0,
                height: 0,
                data: Vec::new(),
            },
        )
    }
}

struct DynamicImageVisitor {
    color_type: String,
    width: u32,
    height: u32,
    data: Vec<u8>,
}

impl DynamicImageVisitor {
    fn to_image<E: serde::de::Error>(self) -> Result<DynamicImage, E> {
        let color = match self.color_type.as_str() {
            "ImageLuma8" => ColorType::L8,
            "ImageLumaA8" => ColorType::La8,
            "ImageRgb8" => ColorType::Rgb8,
            "ImageRgba8" => ColorType::Rgba8,
            "ImageLuma16" => ColorType::L16,
            "ImageLumaA16" => ColorType::La16,
            "ImageRgb16" => ColorType::Rgb16,
            "ImageRgba16" => ColorType::Rgba16,
            "ImageRgb32F" => ColorType::Rgb32F,
            "ImageRgba32F" => ColorType::Rgba32F,
            "" => Err(E::missing_field("type"))?,
            _ => Err(E::custom(format!(
                "Unsupported color type: {:?}",
                self.color_type
            )))?,
        };
        if self.width.is_zero() {
            Err(E::missing_field("width"))?;
        }
        if self.height.is_zero() {
            Err(E::missing_field("height"))?;
        }
        if self.data.is_empty() {
            Err(E::missing_field("data"))?;
        }
        match color {
            ColorType::L8 => match ImageBuffer::from_raw(self.width, self.height, self.data) {
                Some(image) => Ok(DynamicImage::ImageLuma8(image)),
                None => Err(E::custom("Invalid luma8 image data")),
            },
            ColorType::La8 => match ImageBuffer::from_raw(self.width, self.height, self.data) {
                Some(image) => Ok(DynamicImage::ImageLumaA8(image)),
                None => Err(E::custom("Invalid lumaA8 image data")),
            },
            ColorType::Rgb8 => match ImageBuffer::from_raw(self.width, self.height, self.data) {
                Some(image) => Ok(DynamicImage::ImageRgb8(image)),
                None => Err(E::custom("Invalid rgb8 image data")),
            },
            ColorType::Rgba8 => match ImageBuffer::from_raw(self.width, self.height, self.data) {
                Some(image) => Ok(DynamicImage::ImageRgba8(image)),
                None => Err(E::custom("Invalid rgba8 image data")),
            },
            ColorType::L16 => view_as_u16(&self.data)
                .and_then(|view| ImageBuffer::from_raw(self.width, self.height, view))
                .map(DynamicImage::ImageLuma16)
                .ok_or(E::custom("Invalid luma16 image data")),
            ColorType::La16 => view_as_u16(&self.data)
                .and_then(|view| ImageBuffer::from_raw(self.width, self.height, view))
                .map(DynamicImage::ImageLumaA16)
                .ok_or(E::custom("Invalid lumaA16 image data")),
            ColorType::Rgb16 => view_as_u16(&self.data)
                .and_then(|view| ImageBuffer::from_raw(self.width, self.height, view))
                .map(DynamicImage::ImageRgb16)
                .ok_or(E::custom("Invalid rgb16 image data")),
            ColorType::Rgba16 => view_as_u16(&self.data)
                .and_then(|view| ImageBuffer::from_raw(self.width, self.height, view))
                .map(DynamicImage::ImageRgba16)
                .ok_or(E::custom("Invalid rgba16 image data")),
            ColorType::Rgb32F => view_as_f32(&self.data)
                .and_then(|view| ImageBuffer::from_raw(self.width, self.height, view))
                .map(DynamicImage::ImageRgb32F)
                .ok_or(E::custom("Invalid rgb32f image data")),
            ColorType::Rgba32F => view_as_f32(&self.data)
                .and_then(|view| ImageBuffer::from_raw(self.width, self.height, view))
                .map(DynamicImage::ImageRgba32F)
                .ok_or(E::custom("Invalid rgba32f image data")),
        }
    }
}

fn view_as_u16(data: &[u8]) -> Option<Vec<u16>> {
    if data.len() % 2 != 0 {
        return None;
    }
    Some(
        data.chunks_exact(2)
            .into_iter()
            .map(|a| u16::from_ne_bytes([a[0], a[1]]))
            .collect(),
    )
}

fn view_as_f32(data: &[u8]) -> Option<Vec<f32>> {
    if data.len() % 4 != 0 {
        return None;
    }
    Some(
        data.chunks_exact(4)
            .into_iter()
            .map(|a| f32::from_ne_bytes([a[0], a[1], a[2], a[3]]))
            .collect(),
    )
}

impl<'de> Visitor<'de> for DynamicImageVisitor {
    type Value = DynamicImage;

    fn expecting(&self, formatter: &mut Formatter) -> std::fmt::Result {
        formatter.write_str("struct DynamicImage")
    }
    fn visit_map<A>(mut self, mut map: A) -> Result<Self::Value, A::Error>
    where
        A: MapAccess<'de>,
    {
        while let Some(key) = map.next_key::<String>()? {
            match key.as_str() {
                "type" => self.color_type = map.next_value()?,
                "width" => self.width = map.next_value()?,
                "height" => self.height = map.next_value()?,
                "data" => self.data = map.next_value()?,
                _ => {}
            }
        }
        self.to_image()
    }
}
