//! Verifies that any data encoded with WebP lossless encoder can be decoded,
//! and is unaltered after the encode-decode roundtrip.

#![no_main]

use std::io::Cursor;

use image::{SerialImageBuffer, Rgba};
#[macro_use] extern crate libfuzzer_sys;
extern crate image;

fuzz_target!(|data: &[u8]| {
    if data.len() > 3 {
        let width = data[0] as usize;
        let height = data[1] as usize;
        let content = &data[2..];

        if width == 0 || height == 0 {
            return;
        }

        let content_len = width * height * 4;
        if content.len() >= content_len {
            let content = content[..content_len].to_owned();
            let image : SerialImageBuffer<Rgba<u8>, Vec<u8>> = SerialImageBuffer::from_vec(
                width as u32, height as u32, 
                content
            ).unwrap();

            let encoded: Vec<u8> = Vec::new();
            let mut cursor = Cursor::new(encoded);
            image.write_to(&mut cursor, image::ImageFormat::WebP).unwrap();
            let encoded = cursor.into_inner();
            // verify that the imade decoded without errors
            let decoded = image::load_from_memory_with_format(&encoded, image::ImageFormat::WebP).unwrap();
            // compare contents - the encoding should be lossless and roundtrip bit-perfectly
            assert_eq!(image.into_vec(), decoded.into_bytes());
        }
    }
});
