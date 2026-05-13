// Example: How to encode 1-bit BMP files using the image-rs library
//
// This demonstrates 1-bit palette BMP encoding, useful for:
// - Monochrome displays (e-ink, embedded systems)
// - Binary image data (QR codes, barcodes)
// - Minimal file size (8x smaller than 8-bit)

use image::codecs::bmp::BmpEncoder;
use image::ExtendedColorType;

fn main() {
    // Create an 8x8 checkerboard pattern in packed 1-bit format
    // L1 format packs 8 pixels per byte, MSB first:
    //   Bit 7 = pixel 0, Bit 6 = pixel 1, ..., Bit 0 = pixel 7
    let width = 8;
    let height = 8;
    let mut image_data = Vec::new();

    for y in 0..height {
        let mut byte = 0u8;
        for x in 0..8 {
            // Checkerboard: alternating 0 (black) and 1 (white)
            let bit_val = if (x + y) % 2 == 0 { 0 } else { 1 };
            byte |= bit_val << (7 - x);
        }
        image_data.push(byte);
    }

    // Example 1: Encode with default black/white palette
    let mut output = Vec::new();
    BmpEncoder::new(&mut output)
        .encode(&image_data, width, height, ExtendedColorType::L1)
        .expect("Failed to encode 1-bit BMP");

    std::fs::write("checkerboard_1bit.bmp", &output).expect("Failed to write file");
    println!("Saved checkerboard_1bit.bmp ({} bytes)", output.len());

    // Example 2: Encode with custom red/blue palette
    let custom_palette = vec![
        [255, 0, 0], // Red for 0
        [0, 0, 255], // Blue for 1
    ];

    let mut output_custom = Vec::new();
    BmpEncoder::new(&mut output_custom)
        .encode_with_palette(
            &image_data,
            width,
            height,
            ExtendedColorType::L1,
            Some(&custom_palette),
        )
        .expect("Failed to encode with custom palette");

    std::fs::write("checkerboard_custom.bmp", &output_custom).expect("Failed to write file");
    println!(
        "Saved checkerboard_custom.bmp with red/blue palette ({} bytes)",
        output_custom.len()
    );
}
