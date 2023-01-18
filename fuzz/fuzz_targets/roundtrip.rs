#![no_main]

use png::{FilterType, ColorType, BitDepth};
#[macro_use] extern crate libfuzzer_sys;
extern crate png;

fuzz_target!(|data: (u8, u8, u8, u8, u8, Vec<u8>, Vec<u8>)| {
    if let Some((raw, encoded)) = encode_png(data.0, data.1, data.2, data.3, data.4, &data.5, &data.6) {
        let (_info, raw_decoded) = decode_png(&encoded);
        // raw_decoded can be padded with zeroes at the end, not sure if that's correct
        let raw_decoded = &raw_decoded[..raw.len()];
        assert_eq!(raw, raw_decoded);
    }
});

fn encode_png<'a>(width: u8, filter: u8, compression: u8, color_type: u8, raw_bit_depth: u8, raw_palette: &'a [u8], data: &'a [u8]) -> Option<(&'a [u8], Vec<u8>)> {
    // Convert untyped bytes to the correct types and validate them:
    let width = width as u32;
    if width == 0 { return None };
    let filter = FilterType::from_u8(filter)?;
    let bit_depth = BitDepth::from_u8(raw_bit_depth)?;
    let max_palette_length = 3 * u32::pow(2, raw_bit_depth as u32) as usize;
    let mut palette = raw_palette;
    let color_type = ColorType::from_u8(color_type)?;
    if let ColorType::Indexed = color_type {
        // when palette is needed, ensure that palette.len() <= 2 ^ bit_depth
        if raw_palette.len() > max_palette_length {
            palette = &raw_palette[..max_palette_length];
        }
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
    // raw_row_length_from_width() will add +1 to the row length in bytes
    // to account for the first bit in the row indicating the filter, so subtract 1
    let bytes_per_row = raw_row_length_from_width(bit_depth, color_type, width) - 1;
    let height = data.len() / bytes_per_row;
    let total_bytes = bytes_per_row * height;
    let data_to_encode = &data[..total_bytes];

    // perform the PNG encoding
    let mut output: Vec<u8> = Vec::new();
    { // scoped so that we could return the Vec
        let mut encoder = png::Encoder::new(&mut output, width, height as u32);
        encoder.set_depth(bit_depth);
        encoder.set_color(color_type);
        encoder.set_filter(filter);
        encoder.set_compression(compression);
        if let ColorType::Indexed = color_type {
            encoder.set_palette(palette)
        }
        // write_header will return an error given invalid parameters,
        // such as height 0, or invalid color mode and bit depth combination
        let mut writer = encoder.write_header().ok()?;
        writer.write_image_data(data_to_encode).expect("Encoding failed");
    }
    Some((data_to_encode, output))
}

fn decode_png(data: &[u8]) -> (png::OutputInfo, Vec<u8>) {
    let decoder = png::Decoder::new(data);
    let  mut reader = decoder.read_info().unwrap();

    let mut img_data = vec![0u8; reader.info().raw_bytes()];

    let info = reader.next_frame(&mut img_data).unwrap();

    (info, img_data)
}

// copied from the `png` codebase because it's pub(crate)
fn raw_row_length_from_width(depth: BitDepth, color: ColorType, width: u32) -> usize {
    let samples = width as usize * color.samples();
    1 + match depth {
        BitDepth::Sixteen => samples * 2,
        BitDepth::Eight => samples,
        subbyte => {
            let samples_per_byte = 8 / subbyte as usize;
            let whole = samples / samples_per_byte;
            let fract = usize::from(samples % samples_per_byte > 0);
            whole + fract
        }
    }
}
