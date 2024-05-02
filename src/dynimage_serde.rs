use crate::{DynamicImage, ImageFormat};
use base64::{engine::general_purpose::STANDARD_NO_PAD, Engine};
use serde::{
    de::{self, Visitor},
    ser, Deserialize, Deserializer, Serialize,
};
use std::{fmt, io::{Cursor, Write}};

impl<'de> Deserialize<'de> for DynamicImage {
    fn deserialize<D>(deserializer: D) -> Result<DynamicImage, D::Error>
    where
        D: Deserializer<'de>,
    {
        enum Field {
            Dtype,
            Data,
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
                            "dtype" => Ok(Field::Dtype),
                            "data" => Ok(Field::Data),
                            _ => Err(de::Error::unknown_field(value, FIELDS)),
                        }
                    }
                }

                deserializer.deserialize_identifier(FieldVisitor)
            }
        }

        struct SerialBufferVisitor;
        struct DynamicSerialImage {
            dtype: String,
            data: String,
        }

        impl<'de> Visitor<'de> for SerialBufferVisitor {
            type Value = DynamicSerialImage;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("struct DynamicSerialImage")
            }

            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: de::SeqAccess<'de>,
            {
                let dtype: String = seq
                    .next_element()?
                    .ok_or_else(|| de::Error::invalid_length(0, &self))?;
                let data: String = seq
                    .next_element()?
                    .ok_or_else(|| de::Error::invalid_length(1, &self))?;
                Ok(DynamicSerialImage { dtype, data })
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: de::MapAccess<'de>,
            {
                let mut data: Option<String> = None;
                let mut dtype: Option<String> = None;

                while let Some(key) = map.next_key()? {
                    match key {
                        Field::Dtype => {
                            if dtype.is_some() {
                                return Err(de::Error::duplicate_field("dtype"));
                            }
                            dtype = Some(map.next_value()?);
                        }
                        Field::Data => {
                            if data.is_some() {
                                return Err(de::Error::duplicate_field("data"));
                            }
                            data = Some(map.next_value()?);
                        }
                    }
                }

                let dtype = dtype.ok_or_else(|| de::Error::missing_field("dtype"))?;
                let data = data.ok_or_else(|| de::Error::missing_field("data"))?;

                Ok(DynamicSerialImage { dtype, data })
            }
        }

        const FIELDS: &[&str] = &["dtype", "data"];
        let res =
            deserializer.deserialize_struct("DynamicSerialImage", FIELDS, SerialBufferVisitor)?;

        let mut imgdata = STANDARD_NO_PAD.decode(res.data.as_bytes()).map_err(|e| {
            de::Error::custom(format!("Failed to decode base64 string: {}", e.to_string()))
        })?;
        let imgfmt = match res.dtype.as_str() {
            "png" => Ok(ImageFormat::Png),
            "tiff" => {
                use flate2::read::ZlibDecoder;
                use std::io::Read;
                let mut decoder = ZlibDecoder::new(imgdata.as_slice());
                let mut buf = Vec::new();
                decoder.read_to_end(&mut buf).map_err(|e| {
                    de::Error::custom(format!("Failed to decompress TIFF image: {}", e))
                })?;
                imgdata = buf;
                Ok(ImageFormat::Tiff)
            },
            _ => Err(de::Error::custom(format!(
                "Unknown image format: {}",
                res.dtype
            ))),
        }?;
        let img = crate::load_from_memory_with_format(&imgdata, imgfmt)
            .map_err(|e| de::Error::custom(e.to_string()))?;

        Ok(img)
    }
}

impl Serialize for DynamicImage {
    /// Serialize the image to a buffer.
    /// The image is encoded to TIFF format, then encoded
    /// as a base64 string.
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use flate2::write::ZlibEncoder;
        use serde::ser::SerializeStruct;
        // Buffer to store TIFF image
        let mut bytes: Vec<u8> = Vec::new();
        let (fmt, fmtname) = image_to_format(self);
        // Write PNG/TIFF to the buffer
        self.write_to(&mut Cursor::new(&mut bytes), fmt)
            .map_err(|e| ser::Error::custom(format!("Failed to write image to buffer: {}", e)))?;
        // If TIFF, compress
        if fmt == ImageFormat::Tiff {
            let mut encoder = ZlibEncoder::new(Vec::new(), flate2::Compression::default());
            encoder.write_all(&bytes).map_err(|e| {
                ser::Error::custom(format!("Failed to compress TIFF image: {}", e))
            })?;
            bytes = encoder.finish().map_err(|e| {
                ser::Error::custom(format!("Failed to finish compression: {}", e))
            })?;
        }
        // Encode the buffer as base64
        let b64 = STANDARD_NO_PAD.encode(&bytes);
        // Serialize `DynamicImage`
        let mut state = serializer.serialize_struct("DynamicSerialImage", 2)?;
        state.serialize_field("dtype", fmtname)?;
        // Add the field
        state.serialize_field("data", &b64)?;
        // Clean up
        state.end()
    }
}

fn image_to_format(img: &DynamicImage) -> (ImageFormat, &str) {
    use DynamicImage::*;
    match img {
        ImageRgb32F(_) | ImageRgba32F(_) => (ImageFormat::Tiff, "tiff"),
        _ => (ImageFormat::Png, "png"),
    }
}
