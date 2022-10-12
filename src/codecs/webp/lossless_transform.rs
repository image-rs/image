use std::convert::TryFrom;
use std::convert::TryInto;

use super::lossless::subsample_size;
use super::lossless::DecoderError;

#[derive(Debug, Clone)]
pub(crate) enum TransformType {
    PredictorTransform {
        size_bits: u8,
        predictor_data: Vec<u32>,
    },
    ColorTransform {
        size_bits: u8,
        transform_data: Vec<u32>,
    },
    SubtractGreen,
    ColorIndexingTransform {
        table_size: u16,
        table_data: Vec<u32>,
    },
}

impl TransformType {
    /// Applies a transform to the image data
    pub(crate) fn apply_transform(
        &self,
        image_data: &mut Vec<u32>,
        width: u16,
        height: u16,
    ) -> Result<(), DecoderError> {
        match self {
            TransformType::PredictorTransform {
                size_bits,
                predictor_data,
            } => {
                let block_xsize = usize::from(subsample_size(width, *size_bits));
                let width = usize::from(width);
                let height = usize::from(height);

                if image_data.len() < width * height {
                    return Err(DecoderError::TransformError);
                }

                //handle top and left borders specially
                //this involves ignoring mode and just setting prediction values like this
                image_data[0] = add_pixels(image_data[0], 0xff000000);

                for x in 1..width {
                    image_data[x] = add_pixels(image_data[x], get_left(image_data, x, 0, width));
                }

                for y in 1..height {
                    image_data[y * width] =
                        add_pixels(image_data[y * width], get_top(image_data, 0, y, width));
                }

                for y in 1..height {
                    for x in 1..width {
                        let block_index = (y >> size_bits) * block_xsize + (x >> size_bits);

                        let index = y * width + x;

                        let green = (predictor_data[block_index] >> 8) & 0xff;

                        match green {
                            0 => image_data[index] = add_pixels(image_data[index], 0xff000000),
                            1 => {
                                image_data[index] =
                                    add_pixels(image_data[index], get_left(image_data, x, y, width))
                            }
                            2 => {
                                image_data[index] =
                                    add_pixels(image_data[index], get_top(image_data, x, y, width))
                            }
                            3 => {
                                image_data[index] = add_pixels(
                                    image_data[index],
                                    get_top_right(image_data, x, y, width),
                                )
                            }
                            4 => {
                                image_data[index] = add_pixels(
                                    image_data[index],
                                    get_top_left(image_data, x, y, width),
                                )
                            }
                            5 => {
                                image_data[index] = add_pixels(image_data[index], {
                                    let first = average2(
                                        get_left(image_data, x, y, width),
                                        get_top_right(image_data, x, y, width),
                                    );
                                    average2(first, get_top(image_data, x, y, width))
                                })
                            }
                            6 => {
                                image_data[index] = add_pixels(
                                    image_data[index],
                                    average2(
                                        get_left(image_data, x, y, width),
                                        get_top_left(image_data, x, y, width),
                                    ),
                                )
                            }
                            7 => {
                                image_data[index] = add_pixels(
                                    image_data[index],
                                    average2(
                                        get_left(image_data, x, y, width),
                                        get_top(image_data, x, y, width),
                                    ),
                                )
                            }
                            8 => {
                                image_data[index] = add_pixels(
                                    image_data[index],
                                    average2(
                                        get_top_left(image_data, x, y, width),
                                        get_top(image_data, x, y, width),
                                    ),
                                )
                            }
                            9 => {
                                image_data[index] = add_pixels(
                                    image_data[index],
                                    average2(
                                        get_top(image_data, x, y, width),
                                        get_top_right(image_data, x, y, width),
                                    ),
                                )
                            }
                            10 => {
                                image_data[index] = add_pixels(image_data[index], {
                                    let first = average2(
                                        get_left(image_data, x, y, width),
                                        get_top_left(image_data, x, y, width),
                                    );
                                    let second = average2(
                                        get_top(image_data, x, y, width),
                                        get_top_right(image_data, x, y, width),
                                    );
                                    average2(first, second)
                                })
                            }
                            11 => {
                                image_data[index] = add_pixels(
                                    image_data[index],
                                    select(
                                        get_left(image_data, x, y, width),
                                        get_top(image_data, x, y, width),
                                        get_top_left(image_data, x, y, width),
                                    ),
                                )
                            }
                            12 => {
                                image_data[index] = add_pixels(
                                    image_data[index],
                                    clamp_add_subtract_full(
                                        get_left(image_data, x, y, width),
                                        get_top(image_data, x, y, width),
                                        get_top_left(image_data, x, y, width),
                                    ),
                                )
                            }
                            13 => {
                                image_data[index] = add_pixels(image_data[index], {
                                    let first = average2(
                                        get_left(image_data, x, y, width),
                                        get_top(image_data, x, y, width),
                                    );
                                    clamp_add_subtract_half(
                                        first,
                                        get_top_left(image_data, x, y, width),
                                    )
                                })
                            }
                            _ => {}
                        }
                    }
                }
            }
            TransformType::ColorTransform {
                size_bits,
                transform_data,
            } => {
                let block_xsize = usize::from(subsample_size(width, *size_bits));
                let width = usize::from(width);
                let height = usize::from(height);

                for y in 0..height {
                    for x in 0..width {
                        let block_index = (y >> size_bits) * block_xsize + (x >> size_bits);

                        let index = y * width + x;

                        let multiplier =
                            ColorTransformElement::from_color_code(transform_data[block_index]);

                        image_data[index] = transform_color(&multiplier, image_data[index]);
                    }
                }
            }
            TransformType::SubtractGreen => {
                let width = usize::from(width);
                for y in 0..usize::from(height) {
                    for x in 0..width {
                        image_data[y * width + x] = add_green(image_data[y * width + x]);
                    }
                }
            }
            TransformType::ColorIndexingTransform {
                table_size,
                table_data,
            } => {
                let mut new_image_data =
                    Vec::with_capacity(usize::from(width) * usize::from(height));

                let table_size = *table_size;
                let width_bits: u8 = if table_size <= 2 {
                    3
                } else if table_size <= 4 {
                    2
                } else if table_size <= 16 {
                    1
                } else {
                    0
                };

                let bits_per_pixel = 8 >> width_bits;
                let mask = (1 << bits_per_pixel) - 1;

                let mut src = 0;
                let width = usize::from(width);

                let pixels_per_byte = 1 << width_bits;
                let count_mask = pixels_per_byte - 1;
                let mut packed_pixels = 0;

                for _y in 0..usize::from(height) {
                    for x in 0..width {
                        if (x & count_mask) == 0 {
                            packed_pixels = (image_data[src] >> 8) & 0xff;
                            src += 1;
                        }

                        let pixels: usize = (packed_pixels & mask).try_into().unwrap();
                        let new_val = if pixels >= table_size.into() {
                            0x00000000
                        } else {
                            table_data[pixels]
                        };

                        new_image_data.push(new_val);

                        packed_pixels >>= bits_per_pixel;
                    }
                }

                *image_data = new_image_data;
            }
        }

        Ok(())
    }
}

//predictor functions

/// Adds 2 pixels mod 256 for each pixel
pub(crate) fn add_pixels(a: u32, b: u32) -> u32 {
    let new_alpha = ((a >> 24) + (b >> 24)) & 0xff;
    let new_red = (((a >> 16) & 0xff) + ((b >> 16) & 0xff)) & 0xff;
    let new_green = (((a >> 8) & 0xff) + ((b >> 8) & 0xff)) & 0xff;
    let new_blue = ((a & 0xff) + (b & 0xff)) & 0xff;

    (new_alpha << 24) + (new_red << 16) + (new_green << 8) + new_blue
}

/// Get left pixel
fn get_left(data: &[u32], x: usize, y: usize, width: usize) -> u32 {
    data[y * width + x - 1]
}

/// Get top pixel
fn get_top(data: &[u32], x: usize, y: usize, width: usize) -> u32 {
    data[(y - 1) * width + x]
}

/// Get pixel to top right
fn get_top_right(data: &[u32], x: usize, y: usize, width: usize) -> u32 {
    // if x == width - 1 this gets the left most pixel of the current row
    // as described in the specification
    data[(y - 1) * width + x + 1]
}

/// Get pixel to top left
fn get_top_left(data: &[u32], x: usize, y: usize, width: usize) -> u32 {
    data[(y - 1) * width + x - 1]
}

/// Get average of 2 pixels
fn average2(a: u32, b: u32) -> u32 {
    let mut avg = 0u32;
    for i in 0..4 {
        let sub_a: u8 = ((a >> (i * 8)) & 0xff).try_into().unwrap();
        let sub_b: u8 = ((b >> (i * 8)) & 0xff).try_into().unwrap();
        avg |= u32::from(sub_average2(sub_a, sub_b)) << (i * 8);
    }
    avg
}

/// Get average of 2 bytes
fn sub_average2(a: u8, b: u8) -> u8 {
    ((u16::from(a) + u16::from(b)) / 2).try_into().unwrap()
}

/// Get a specific byte from argb pixel
fn get_byte(val: u32, byte: u8) -> u8 {
    ((val >> (byte * 8)) & 0xff).try_into().unwrap()
}

/// Get byte as i32 for convenience
fn get_byte_i32(val: u32, byte: u8) -> i32 {
    i32::from(get_byte(val, byte))
}

/// Select left or top byte
fn select(left: u32, top: u32, top_left: u32) -> u32 {
    let predict_alpha = get_byte_i32(left, 3) + get_byte_i32(top, 3) - get_byte_i32(top_left, 3);
    let predict_red = get_byte_i32(left, 2) + get_byte_i32(top, 2) - get_byte_i32(top_left, 2);
    let predict_green = get_byte_i32(left, 1) + get_byte_i32(top, 1) - get_byte_i32(top_left, 1);
    let predict_blue = get_byte_i32(left, 0) + get_byte_i32(top, 0) - get_byte_i32(top_left, 0);

    let predict_left = i32::abs(predict_alpha - get_byte_i32(left, 3))
        + i32::abs(predict_red - get_byte_i32(left, 2))
        + i32::abs(predict_green - get_byte_i32(left, 1))
        + i32::abs(predict_blue - get_byte_i32(left, 0));
    let predict_top = i32::abs(predict_alpha - get_byte_i32(top, 3))
        + i32::abs(predict_red - get_byte_i32(top, 2))
        + i32::abs(predict_green - get_byte_i32(top, 1))
        + i32::abs(predict_blue - get_byte_i32(top, 0));

    if predict_left < predict_top {
        left
    } else {
        top
    }
}

/// Clamp a to [0, 255]
fn clamp(a: i32) -> i32 {
    if a < 0 {
        0
    } else if a > 255 {
        255
    } else {
        a
    }
}

/// Clamp add subtract full on one part
fn clamp_add_subtract_full_sub(a: i32, b: i32, c: i32) -> i32 {
    clamp(a + b - c)
}

/// Clamp add subtract half on one part
fn clamp_add_subtract_half_sub(a: i32, b: i32) -> i32 {
    clamp(a + (a - b) / 2)
}

/// Clamp add subtract full on 3 pixels
fn clamp_add_subtract_full(a: u32, b: u32, c: u32) -> u32 {
    let mut value: u32 = 0;
    for i in 0..4u8 {
        let sub_a: i32 = ((a >> (i * 8)) & 0xff).try_into().unwrap();
        let sub_b: i32 = ((b >> (i * 8)) & 0xff).try_into().unwrap();
        let sub_c: i32 = ((c >> (i * 8)) & 0xff).try_into().unwrap();
        value |=
            u32::try_from(clamp_add_subtract_full_sub(sub_a, sub_b, sub_c)).unwrap() << (i * 8);
    }
    value
}

/// Clamp add subtract half on 2 pixels
fn clamp_add_subtract_half(a: u32, b: u32) -> u32 {
    let mut value = 0;
    for i in 0..4u8 {
        let sub_a: i32 = ((a >> (i * 8)) & 0xff).try_into().unwrap();
        let sub_b: i32 = ((b >> (i * 8)) & 0xff).try_into().unwrap();
        value |= u32::try_from(clamp_add_subtract_half_sub(sub_a, sub_b)).unwrap() << (i * 8);
    }

    value
}

//color transform

#[derive(Debug, Clone, Copy)]
struct ColorTransformElement {
    green_to_red: u8,
    green_to_blue: u8,
    red_to_blue: u8,
}

impl ColorTransformElement {
    fn from_color_code(color_code: u32) -> ColorTransformElement {
        ColorTransformElement {
            green_to_red: (color_code & 0xff).try_into().unwrap(),
            green_to_blue: ((color_code >> 8) & 0xff).try_into().unwrap(),
            red_to_blue: ((color_code >> 16) & 0xff).try_into().unwrap(),
        }
    }
}

/// Does color transform on red and blue transformed by green
fn color_transform(red: u8, blue: u8, green: u8, trans: &ColorTransformElement) -> (u8, u8) {
    let mut temp_red = u32::from(red);
    let mut temp_blue = u32::from(blue);

    //as does the conversion from u8 to signed two's complement i8 required
    temp_red += color_transform_delta(trans.green_to_red as i8, green as i8);
    temp_blue += color_transform_delta(trans.green_to_blue as i8, green as i8);
    temp_blue += color_transform_delta(trans.red_to_blue as i8, temp_red as i8);

    (
        (temp_red & 0xff).try_into().unwrap(),
        (temp_blue & 0xff).try_into().unwrap(),
    )
}

/// Does color transform on 2 numbers
fn color_transform_delta(t: i8, c: i8) -> u32 {
    ((i16::from(t) * i16::from(c)) as u32) >> 5
}

// Does color transform on a pixel with a color transform element
fn transform_color(multiplier: &ColorTransformElement, color_value: u32) -> u32 {
    let alpha = get_byte(color_value, 3);
    let red = get_byte(color_value, 2);
    let green = get_byte(color_value, 1);
    let blue = get_byte(color_value, 0);

    let (new_red, new_blue) = color_transform(red, blue, green, multiplier);

    (u32::from(alpha) << 24)
        + (u32::from(new_red) << 16)
        + (u32::from(green) << 8)
        + u32::from(new_blue)
}

//subtract green function

/// Adds green to red and blue of a pixel
fn add_green(argb: u32) -> u32 {
    let red = (argb >> 16) & 0xff;
    let green = (argb >> 8) & 0xff;
    let blue = argb & 0xff;

    let new_red = (red + green) & 0xff;
    let new_blue = (blue + green) & 0xff;

    (argb & 0xff00ff00) | (new_red << 16) | (new_blue)
}
