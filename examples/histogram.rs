//! An example of generating a histogram.
extern crate image;

use std::env;
use std::path::Path;
use image::imagestat;

fn main() {
    
    let file = if env::args().count() == 2 {
        env::args().nth(1).unwrap()
    } else {
        panic!("Please enter a file.")
    };
    
    // Use the open function to load an image from a Path.
    // ```open``` returns a dynamic image.
    let im = image::open(&Path::new(&file)).unwrap();
    
    // Create a histogram from the image.
    let h = imagestat::create_histogram(&im);
    
    // Print the data from the histogram.
    let number_of_channels = h.channels.len();
    for channel in 0..number_of_channels {
        println!("Channel: {:?}", channel);
        for value in 0..h.channels[channel as usize].values.len() {
            println!("value[{:?}]: {:?}", value, h.get_count(channel as usize, value as usize));
        }
    }    
}