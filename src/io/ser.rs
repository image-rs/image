use crate::{ColorType, DynamicImage};
use serde::ser::SerializeStruct;
use serde::{Serialize, Serializer};

impl Serialize for ColorType {
    #[allow(unreachable_patterns)]
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let image_type = match self {
            ColorType::L8 => "ImageLuma8",
            ColorType::La8 => "ImageLumaA8",
            ColorType::Rgb8 => "ImageRgb8",
            ColorType::Rgba8 => "ImageRgba8",
            ColorType::L16 => "ImageLuma16",
            ColorType::La16 => "ImageLumaA16",
            ColorType::Rgb16 => "ImageRgb16",
            ColorType::Rgba16 => "ImageRgba16",
            ColorType::Rgb32F => "ImageRgb32F",
            ColorType::Rgba32F => "ImageRgba32F",
            // exhaustive check
            _ => unreachable!("Unsupported color type: {:?}", self),
        };
        serializer.serialize_str(image_type)
    }
}

impl Serialize for DynamicImage {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut ser = serializer.serialize_struct("DynamicImage", 4)?;
        ser.serialize_field("type", &self.color())?;
        ser.serialize_field("width", &self.width())?;
        ser.serialize_field("height", &self.height())?;
        ser.serialize_field("data", &self.as_bytes())?;
        ser.end()
    }
}
