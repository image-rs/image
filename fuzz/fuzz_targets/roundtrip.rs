#![no_main]

use png::{FilterType, ColorType};
#[macro_use] extern crate libfuzzer_sys;
extern crate png;

fuzz_target!(|data: (u8, u8, u8, u8, Vec<u8>)| {
    if let Some((raw, encoded)) = encode_png(data.0, data.1, data.2, data.3, &data.4) {
        let (_info, raw_decoded) = decode_png(&encoded);
        // raw_decoded can be padded with zeroes at the end, not sure if that's correct
        let raw_decoded = &raw_decoded[..raw.len()];
        assert_eq!(raw, raw_decoded);
    }
});

fn encode_png(height: u8, filter: u8, compression: u8, color_type: u8, data: &[u8]) -> Option<(&[u8], Vec<u8>)> {
    // Convert untyped bytes to the correct types and validate them:
    // height
    let height = height as u32;
    if height == 0 { return None }
    // filter
    let filter = FilterType::from_u8(filter)?;
    // color type
    let color_type = ColorType::from_u8(color_type)?;
    if let ColorType::Indexed = color_type {
        return None // TODO: palette needs more data, not supported yet
    }
    // compression
    let compression = match compression {
        0 => png::Compression::Default,
        1 => png::Compression::Fast,
        2 => png::Compression::Best,
        3 => png::Compression::Huffman,
        4 => png::Compression::Rle,
        _ => return None,
    };

    // infer the rest of the parameters
    let bytes_per_pixel = color_type.samples() as u32; // safe to cast - actually u8 internally
    let width = data.len() as u32 / height / bytes_per_pixel;
    if width == 0 { return None }
    let total_pixels = height.checked_mul(width)?;
    let total_bytes = total_pixels.checked_mul(bytes_per_pixel)?;

    // perform the PNG encoding
    let data_to_encode = &data[..total_bytes as usize];
    let mut output: Vec<u8> = Vec::new();
    let mut encoder = png::Encoder::new(&mut output, width, height);
    // TODO: randomize bit depth, perhaps other settings
    encoder.set_depth(png::BitDepth::Eight);
    encoder.set_color(color_type);
    encoder.set_filter(filter);
    encoder.set_compression(compression);
    let mut writer = encoder.write_header().unwrap();
    writer.write_image_data(data_to_encode).expect("Encoding failed");

    Some((data_to_encode, output))
}

fn decode_png(data: &[u8]) -> (png::OutputInfo, Vec<u8>) {
    let decoder = png::Decoder::new(data);
    let  mut reader = decoder.read_info().unwrap();

    let mut img_data = vec![0u8; reader.info().raw_bytes()];

    let info = reader.next_frame(&mut img_data).unwrap();

    (info, img_data)
}