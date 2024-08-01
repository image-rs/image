use crate::codecs::dds::convert::{div_round, snorm8_to_unorm8};

/// Decodes a BC1 block into 16 RGBA pixels.
pub(crate) fn decode_bc1_block(block_bytes: [u8; 8]) -> [[u8; 4]; 16] {
    let mut pixels: [[u8; 4]; 16] = Default::default();
    let pixel_buf = bytemuck::cast_slice_mut(pixels.as_mut_slice());
    bcdec_rs::bc1(&block_bytes, pixel_buf, 4 * 4);
    pixels
}

/// Decodes a BC2 block into 16 RGBA pixels.
pub(crate) fn decode_bc2_block(block_bytes: [u8; 16]) -> [[u8; 4]; 16] {
    let mut pixels: [[u8; 4]; 16] = Default::default();
    let pixel_buf = bytemuck::cast_slice_mut(pixels.as_mut_slice());
    bcdec_rs::bc2(&block_bytes, pixel_buf, 4 * 4);
    pixels
}

/// Decodes a BC3 block into 16 RGBA pixels.
pub(crate) fn decode_bc3_block(block_bytes: [u8; 16]) -> [[u8; 4]; 16] {
    // https://learn.microsoft.com/en-us/windows/win32/direct3d10/d3d10-graphics-programming-guide-resources-block-compression#bc3
    let alpha_bytes: [u8; 8] = block_bytes[0..8].try_into().unwrap();
    let bc1_bytes: [u8; 8] = block_bytes[8..16].try_into().unwrap();

    let mut pixels = decode_bc1_block(bc1_bytes);
    let alpha = decode_bc4_unsigned_block(alpha_bytes);

    for i in 0..4 {
        for j in 0..4 {
            pixels[i * 4 + j][3] = alpha[i * 4 + j][0];
        }
    }

    pixels
}

/// Decodes a BC4 UNORM block of into 16 grayscale pixels.
pub(crate) fn decode_bc4_unsigned_block(block_bytes: [u8; 8]) -> [[u8; 1]; 16] {
    // https://learn.microsoft.com/en-us/windows/win32/direct3d10/d3d10-graphics-programming-guide-resources-block-compression#bc4
    let c0 = block_bytes[0];
    let c1 = block_bytes[1];

    let c0_16 = c0 as u16;
    let c1_16 = c1 as u16;

    let (c2, c3, c4, c5, c6, c7) = if c0 > c1 {
        // 6 interpolated colors
        (
            div_round(c0_16 * 6 + c1_16, 7) as u8,
            div_round(c0_16 * 5 + c1_16 * 2, 7) as u8,
            div_round(c0_16 * 4 + c1_16 * 3, 7) as u8,
            div_round(c0_16 * 3 + c1_16 * 4, 7) as u8,
            div_round(c0_16 * 2 + c1_16 * 5, 7) as u8,
            div_round(c0_16 + c1_16 * 6, 7) as u8,
        )
    } else {
        // 4 interpolated colors
        (
            div_round(c0_16 * 4 + c1_16, 5) as u8,
            div_round(c0_16 * 3 + c1_16 * 2, 5) as u8,
            div_round(c0_16 * 2 + c1_16 * 3, 5) as u8,
            div_round(c0_16 + c1_16 * 4, 5) as u8,
            0,
            255,
        )
    };

    let mut pixels: [[u8; 1]; 16] = Default::default();

    let lut = [c0, c1, c2, c3, c4, c5, c6, c7];
    let indexes0 = u32::from_le_bytes([block_bytes[2], block_bytes[3], block_bytes[4], 0]);
    let indexes1 = u32::from_le_bytes([block_bytes[5], block_bytes[6], block_bytes[7], 0]);
    for (i, indexes) in [indexes0, indexes1].into_iter().enumerate() {
        for j in 0..8 {
            let index = (indexes >> (j * 3)) & 0b111;
            pixels[i * 8 + j][0] = lut[index as usize];
        }
    }

    pixels
}

/// Decodes a BC4 SNORM block of into 16 grayscale pixels.
pub(crate) fn decode_bc4_signed_block(block_bytes: [u8; 8]) -> [[u8; 1]; 16] {
    // https://learn.microsoft.com/en-us/windows/win32/direct3d10/d3d10-graphics-programming-guide-resources-block-compression#bc4
    let red0 = block_bytes[0];
    let red1 = block_bytes[1];

    let c0 = snorm8_to_unorm8(red0);
    let c1 = snorm8_to_unorm8(red1);

    // exact f32 values of c0 and c1
    const CONVERSION_FACTOR: f32 = 255.0 / 254.0;
    let c0_f = red0.wrapping_add(128).saturating_sub(1) as f32 * CONVERSION_FACTOR;
    let c1_f = red1.wrapping_add(128).saturating_sub(1) as f32 * CONVERSION_FACTOR;

    fn interpolate(red0: f32, red1: f32, blend: f32) -> u8 {
        (red0 * (1.0 - blend) + red1 * blend + 0.5) as u8
    }
    let (c2, c3, c4, c5, c6, c7) = if c0 > c1 {
        // 6 interpolated colors
        (
            interpolate(c0_f, c1_f, 1.0 / 7.0),
            interpolate(c0_f, c1_f, 2.0 / 7.0),
            interpolate(c0_f, c1_f, 3.0 / 7.0),
            interpolate(c0_f, c1_f, 4.0 / 7.0),
            interpolate(c0_f, c1_f, 5.0 / 7.0),
            interpolate(c0_f, c1_f, 6.0 / 7.0),
        )
    } else {
        // 4 interpolated colors
        (
            interpolate(c0_f, c1_f, 1.0 / 5.0),
            interpolate(c0_f, c1_f, 2.0 / 5.0),
            interpolate(c0_f, c1_f, 3.0 / 5.0),
            interpolate(c0_f, c1_f, 4.0 / 5.0),
            0,
            255,
        )
    };

    let mut pixels: [[u8; 1]; 16] = Default::default();

    let lut = [c0, c1, c2, c3, c4, c5, c6, c7];
    let indexes0 = u32::from_le_bytes([block_bytes[2], block_bytes[3], block_bytes[4], 0]);
    let indexes1 = u32::from_le_bytes([block_bytes[5], block_bytes[6], block_bytes[7], 0]);
    for (i, indexes) in [indexes0, indexes1].into_iter().enumerate() {
        for j in 0..8 {
            let index = (indexes >> (j * 3)) & 0b111;
            pixels[i * 8 + j][0] = lut[index as usize];
        }
    }

    pixels
}

/// Decodes a BC5 UNORM block into 16 RGB pixels.
pub(crate) fn decode_bc5_unsigned_block(block_bytes: [u8; 16]) -> [[u8; 3]; 16] {
    let red = decode_bc4_unsigned_block(block_bytes[0..8].try_into().unwrap());
    let green = decode_bc4_unsigned_block(block_bytes[8..16].try_into().unwrap());

    let mut pixels: [[u8; 3]; 16] = Default::default();
    for (i, pixel) in pixels.iter_mut().enumerate() {
        pixel[0] = red[i][0];
        pixel[1] = green[i][0];
        pixel[2] = 0;
    }

    pixels
}

/// Decodes a BC5 UNORM block into 16 RGB pixels.
pub(crate) fn decode_bc5_signed_block(block_bytes: [u8; 16]) -> [[u8; 3]; 16] {
    let red = decode_bc4_signed_block(block_bytes[0..8].try_into().unwrap());
    let green = decode_bc4_signed_block(block_bytes[8..16].try_into().unwrap());

    let mut pixels: [[u8; 3]; 16] = Default::default();
    for (i, pixel) in pixels.iter_mut().enumerate() {
        pixel[0] = red[i][0];
        pixel[1] = green[i][0];
        pixel[2] = 128;
    }

    pixels
}

/// Decodes a BC6 block into 16 RGB pixels.
pub(crate) fn decode_bc6_unsigned_block(block_bytes: [u8; 16]) -> [[f32; 3]; 16] {
    let mut pixels: [[f32; 3]; 16] = Default::default();
    let pixel_buf = bytemuck::cast_slice_mut(pixels.as_mut_slice());
    bcdec_rs::bc6h_float(block_bytes.as_slice(), pixel_buf, 12, false);
    pixels
}
/// Decodes a BC6 block into 16 RGB pixels.
pub(crate) fn decode_bc6_signed_block(block_bytes: [u8; 16]) -> [[f32; 3]; 16] {
    let mut pixels: [[f32; 3]; 16] = Default::default();
    let pixel_buf = bytemuck::cast_slice_mut(pixels.as_mut_slice());
    bcdec_rs::bc6h_float(block_bytes.as_slice(), pixel_buf, 12, true);
    pixels
}

/// Decodes a BC7 block into 16 RGBA pixels.
pub(crate) fn decode_bc7_block(block_bytes: [u8; 16]) -> [[u8; 4]; 16] {
    let mut pixels: [[u8; 4]; 16] = Default::default();
    let pixel_buf = bytemuck::cast_slice_mut(pixels.as_mut_slice());
    bcdec_rs::bc7(block_bytes.as_slice(), pixel_buf, 16);
    pixels
}
