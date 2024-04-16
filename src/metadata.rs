#![deny(missing_docs)]
use std::{
    fmt::Display,
    time::{Duration, SystemTime, UNIX_EPOCH},
};

use serde::{Deserialize, Serialize};

use crate::ROI;

/// Image metadata builder.
pub struct ImageMetadataBuilder {
    roi: ROI,
    temperature: f32,
    exposure: Duration,
    timestamp: SystemTime,
    camera_name: Option<String>,
    gain: Option<i64>,
    offset: Option<i64>,
    min_gain: Option<i32>,
    max_gain: Option<i32>,
    extended_metadata: ExtendedMetadata,
}

#[derive(Clone, Serialize, Deserialize, Debug, PartialEq, Eq)]
/// Extended metadata row.
pub struct ExtendedMetadataRow {
    /// Metadata key.
    pub name: String,
    /// Metadata value.
    pub value: String,
    /// Metadata comment.
    pub comment: Option<String>,
}

/// Extended metadata, a collection of `ExtendedMetadataRow`.
pub type ExtendedMetadata = Vec<ExtendedMetadataRow>;

impl Default for ImageMetadataBuilder {
    fn default() -> Self {
        Self {
            roi: ROI::default(),
            temperature: 0f32,
            exposure: Duration::from_secs(0),
            timestamp: UNIX_EPOCH,
            camera_name: None,
            gain: None,
            offset: None,
            min_gain: None,
            max_gain: None,
            extended_metadata: Vec::new(),
        }
    }
}

impl ImageMetadataBuilder {
    /// Set the region of interest.
    pub fn roi(mut self, roi: &ROI) -> Self {
        self.roi = *roi;
        self
    }

    /// Set the temperature.
    pub fn temperature(mut self, temperature: f32) -> Self {
        self.temperature = temperature;
        self
    }

    /// Set the exposure.
    pub fn exposure(mut self, exposure: Duration) -> Self {
        self.exposure = exposure;
        self
    }

    /// Set the timestamp.
    pub fn timestamp(mut self, timestamp: SystemTime) -> Self {
        self.timestamp = timestamp;
        self
    }

    /// Set the camera name.
    pub fn camera_name(mut self, camera_name: &str) -> Self {
        self.camera_name = Some(camera_name.to_string());
        self
    }

    /// Set the gain.
    pub fn gain(mut self, gain: i64, min: i64, max: i64) -> Self {
        self.gain = Some(gain);
        self.min_gain = Some(min as i32);
        self.max_gain = Some(max as i32);
        self
    }

    /// Set the offset.
    pub fn offset(mut self, offset: i64) -> Self {
        self.offset = Some(offset);
        self
    }

    /// Add an extended attribute to the image metadata.
    pub fn write_key(mut self, key: &str, val: &str) -> Self {
        self.extended_metadata.push(ExtendedMetadataRow {
            name: key.to_string(),
            value: val.to_string(),
            comment: None,
        });
        self
    }

    /// Add an extended attribute to the image metadata, with a comment.
    pub fn write_key_comment(mut self, key: &str, val: &str, comment: &str) -> Self {
        self.extended_metadata.push(ExtendedMetadataRow {
            name: key.to_string(),
            value: val.to_string(),
            comment: Some(comment.to_string()),
        });
        self
    }

    /// Build the image metadata.
    pub fn build(self) -> ImageMetadata {
        ImageMetadata {
            bin_x: self.roi.bin_x,
            bin_y: self.roi.bin_y,
            img_top: self.roi.y_min,
            img_left: self.roi.x_min,
            temperature: self.temperature,
            exposure: self.exposure,
            timestamp: self.timestamp,
            camera_name: self.camera_name.unwrap_or_default(),
            gain: self.gain.unwrap_or_default(),
            offset: self.offset.unwrap_or_default(),
            min_gain: self.min_gain.unwrap_or_default(),
            max_gain: self.max_gain.unwrap_or_default(),
            extended_metadata: self.extended_metadata,
        }
    }
}

#[derive(Clone, Serialize, Deserialize, Debug)]
/// Image metadata structure.
/// This structure implements the [`std::fmt::Display`] and [`std::clone::Clone`] traits.
pub struct ImageMetadata {
    bin_x: u32,
    bin_y: u32,
    img_top: u32,
    img_left: u32,
    temperature: f32,
    exposure: Duration,
    timestamp: SystemTime,
    camera_name: String,
    gain: i64,
    offset: i64,
    min_gain: i32,
    max_gain: i32,
    extended_metadata: ExtendedMetadata,
}

impl PartialEq for ImageMetadata {
    fn eq(&self, other: &Self) -> bool {
        let mut res = true;
        res &= self.bin_x == other.bin_x;
        res &= self.bin_y == other.bin_y;
        res &= self.img_top == other.img_top;
        res &= self.img_left == other.img_left;
        res &= self.temperature == other.temperature;
        res &= self.exposure == other.exposure;
        res &= self.timestamp == other.timestamp;
        res &= self.camera_name == other.camera_name;
        res &= self.gain == other.gain;
        res &= self.offset == other.offset;
        res &= self.min_gain == other.min_gain;
        res &= self.max_gain == other.max_gain;

        res
    }
}

impl Eq for ImageMetadata {

}

impl ImageMetadata {
    /// Create a builder for the image metadata.
    pub fn builder(self) -> ImageMetadataBuilder {
        let mut builder = ImageMetadataBuilder::default();
        builder.roi.bin_x = self.bin_x;
        builder.roi.bin_y = self.bin_y;
        builder.roi.x_min = self.img_left;
        builder.roi.y_min = self.img_top;

        builder.temperature = self.temperature;
        builder.exposure = self.exposure;
        builder.timestamp = self.timestamp;
        builder.camera_name = Some(self.camera_name);
        builder.gain = Some(self.gain);
        builder.offset = Some(self.offset);
        builder.min_gain = Some(self.min_gain);
        builder.max_gain = Some(self.max_gain);

        builder.extended_metadata = self.extended_metadata;

        builder
    }
}

impl Display for ImageMetadata {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "ImageMetaData [{:#?}]:\n
            \tCamera name: {}\n
            \tImage Bin: {} x {}\n
            \tImage Origin: {} x {}
            \tExposure: {} s\n
            \tGain: {}, Offset: {}\n
            \tTemperature: {} C\n",
            self.timestamp,
            self.camera_name,
            self.bin_x,
            self.bin_y,
            self.img_left,
            self.img_top,
            self.exposure.as_secs(),
            self.gain,
            self.offset,
            self.temperature
        )?;
        if !self.extended_metadata.is_empty() {
            writeln!(f, "\tExtended Metadata:")?;
            for obj in self.extended_metadata.iter() {
                writeln!(f, "\t\t{}: {}", obj.name, obj.value)?;
            }
        };
        Ok(())
    }
}

impl ImageMetadata {
    /// Add an extended attribute to the image metadata using `vec::push()`.
    ///
    /// # Panics
    ///
    /// If the new capacity exceeds `isize::MAX` bytes.
    pub fn write_key(&mut self, key: &str, val: &str) {
        self.extended_metadata.push(ExtendedMetadataRow {
            name: key.to_string(),
            value: val.to_string(),
            comment: None,
        });
    }

    /// Get the extended attributes of the image metadata.
    pub fn write_key_comment(&mut self, key: &str, val: &str, comment: &str) {
        self.extended_metadata.push(ExtendedMetadataRow {
            name: key.to_string(),
            value: val.to_string(),
            comment: Some(comment.to_string()),
        });
    }

    /// Get the extended attributes of the image metadata.
    pub fn extended_metadata(&self) -> &ExtendedMetadata {
        &self.extended_metadata
    }

    /// Get the origin of the image metadata.
    pub fn origin(&self) -> (u32, u32) {
        (self.img_left, self.img_top)
    }

    /// Get the binning factors of the image metadata.
    pub fn binning(&self) -> (u32, u32) {
        (self.bin_x, self.bin_y)
    }

    /// Get the temperature of the image metadata.
    pub fn temperature(&self) -> f32 {
        self.temperature
    }

    /// Get the exposure of the image metadata.
    pub fn exposure(&self) -> Duration {
        self.exposure
    }

    /// Get the timestamp of the image metadata.
    pub fn timestamp(&self) -> SystemTime {
        self.timestamp
    }

    /// Get the camera name of the image metadata.
    pub fn camera_name(&self) -> &str {
        &self.camera_name
    }

    /// Get the gain of the image metadata.
    pub fn gain(&self) -> (i32, i32, i32) {
        (self.gain as i32, self.min_gain, self.max_gain)
    }

    /// Get the offset of the image metadata.
    pub fn offset(&self) -> i64 {
        self.offset
    }
}
