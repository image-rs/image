//! Image Statistics Functions

use color;
use dynimage::DynamicImage;
use image::GenericImage;

/// Contains the values for a channel.
#[derive(Clone)]
pub struct HistogramChannel {
    /// Contains the count of pixels for each value.
    pub values: Vec<i32>,
}

/// Contains the tonal information for an image.
/// Supports both color and greyscale images.
pub struct Histogram {
    /// The color channels for the histogram.
    pub channels: Vec<HistogramChannel>,
}

impl Histogram {
    /// Sets the count of pixels for a particular value.
    pub fn set_count(&mut self, channel: usize, value: usize, count: i32) {
        self.channels[channel].values[value] = count;
    }
    
    /// Gets the count of pixels at a specific value.
    pub fn get_count(&self, channel: usize, value: usize) -> i32 {
        self.channels[channel].values[value]
    }
    
    /// Increments the count of pixels at a specific value.
    pub fn increment_count(&mut self, channel: usize, value: usize) {
        self.channels[channel].values[value] += 1;
    }
    
    /// Decrements the count of pixels at a specific value.
    pub fn decrement_count(&mut self, channel: usize, value: usize) {
        self.channels[channel].values[value] -= 1;
    }
}

/// Create a histogram from an image.
pub fn create_histogram(im: &DynamicImage) -> Histogram {
    let color = im.color();
    let bits = color::bits_per_pixel(color);
    let number_of_channels = color::num_components(color);
    let mut h = Histogram { channels: vec![HistogramChannel { values: vec![0; 1 << bits] }; number_of_channels]};

    for (_, _, pixel) in im.pixels() {
        for channel in 0..number_of_channels {
            h.increment_count(channel as usize, pixel.data[channel as usize] as usize);
        }
    }
    
    h
}