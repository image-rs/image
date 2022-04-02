//! An example of handling a 32-bit float image.
use std::env;
use std::path::Path;
use image::{Rgba32FImage, Rgba};

fn main() {
    let (width, height) = (200, 200);

    // construct an image with 32-bit float pixels
    let mut f32_buffer = Rgba32FImage::from_fn(
        width as u32, height as u32,
        |x, y| image::Rgba([
            2.0 * (x as f32 / width as f32),
            2.0 * (y as f32 / height as f32),
            (y+x) as f32 / (width+height) as f32,
            1.0,
        ])
    );

    // save the float image as exr
    f32_buffer.save("generated_image.exr").unwrap();
}
