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

pub struct DynamicImageVisitor {
    color_type: String,
    width: u32,
    height: u32,
    data: Vec<u8>,
}

impl DynamicImageVisitor {
    pub fn to_image<E: serde::de::Error>(self) -> Result<DynamicImage, E> {
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
            ColorType::L16 => ImageBuffer::from_raw(
                self.width,
                self.height,
                view_as_u16(&self.data, "Invalid luma16 image data")?,
            )
            .ok_or(E::custom("Invalid luma16 image data")),
            ColorType::La16 => {}
            ColorType::Rgb16 => {}
            ColorType::Rgba16 => {}
            ColorType::Rgb32F => {
                image.copy_from_slice(&self.data);
                Ok(DynamicImage::ImageRgb32F(image))
            }
            ColorType::Rgba32F => {
                image.copy_from_slice(&self.data);
                Ok(DynamicImage::ImageRgba32F(image))
            }
        }
    }
}

/// SAFETY: it is safe to view `Vec<u8>` as `&[u16]` because the length is even
pub fn view_as_u16<'a, E: serde::de::Error>(
    data: &'a [u8],
    assert: &'static str,
) -> Result<&'a [u16], E> {
    if data.len() % 2 != 0 {
        Err(E::custom(assert))?;
    }
    let view = unsafe { std::slice::from_raw_parts(data.as_ptr() as *const u16, data.len() / 2) };
    Ok(view)
}

/// SAFETY: it is safe to view `Vec<u8>` as `&[f32]` because the length is multiple of 4
pub fn view_as_f32<'a, E: serde::de::Error>(
    data: &'a [u8],
    assert: &'static str,
) -> Result<&'a [f32], E> {
    if data.len() % 4 != 0 {
        Err(E::custom(assert))?;
    }
    let view = unsafe { std::slice::from_raw_parts(data.as_ptr() as *const f32, data.len() / 4) };
    Ok(view)
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
