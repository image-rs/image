//! Image Statistics Functions

use color;
use color::ColorType;
use color::Rgba;
use dynimage::DynamicImage;
use image::GenericImage;
use num;
use std;

#[allow(missing_copy_implementations)]
/// Contains the values for a channel.
pub struct HistogramChannel {
	/// Contains the count of pixels for each value.
	pub values: Vec<i32>,
}

#[allow(missing_copy_implementations)]
/// Contains the tonal information for an image.
/// Supports both color and greyscale images.
pub struct Histogram {
	/// The color channels for the histogram.
	pub channels: Vec<HistogramChannel>,
}

impl Histogram {
	/// Sets the count for a particular value of pixels.
    pub fn set_count(&mut self, channel: usize, value: usize, count: i32)
	{
		self.channels[channel].values[value] = count;
	}
	
	pub fn get_count(&self, channel: usize, value: usize) -> i32
	{
		self.channels[channel].values[value]
	}
	
	pub fn increment_count(&mut self, channel: usize, value: usize)
	{
		self.channels[channel].values[value] += 1;
	}
}

/// Create a histogram from an image.
pub fn create_histogram(im: &DynamicImage) -> Histogram {
	let color = im.color();
	let mut h = Histogram {channels: Vec::new()};
	let bits = color::bits_per_pixel(color);
	let number_of_channels = color::num_components(color);
	
	// Setup the histogram with the correct number of channels.
	for x in 0..number_of_channels {
		h.channels.push(HistogramChannel { values: std::iter::repeat(0).take(num::pow(2, bits)).collect::<Vec<_>>() });
	}
	
	let (width, height) = im.dimensions();
	
	// Go through the image and record the pixel data.
	// Todo: Is there a way to simply iterate through pixels using a Dynamic Image?
	for x in 0..width {
		for y in 0..height {
			let pixel_color = im.get_pixel(x, y);
			for channel in 0..number_of_channels {
				h.increment_count(channel as usize, pixel_color.data[channel as usize] as usize);
			}
		}
	}
	
	h
}