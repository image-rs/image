use num_traits::Unsigned;

#[derive(Debug, Clone, Copy)]
pub(crate) struct B5G6R5 {
    r5: u16,
    g6: u16,
    b5: u16,
}
impl B5G6R5 {
    #[inline(always)]
    pub(crate) fn from_le_bytes(bytes: [u8; 2]) -> Self {
        Self::from_u16(u16::from_le_bytes(bytes))
    }
    #[inline(always)]
    pub(crate) fn from_u16(u: u16) -> Self {
        Self {
            b5: u & 0x1F,
            g6: (u >> 5) & 0x3F,
            r5: (u >> 11) & 0x1F,
        }
    }

    #[inline(always)]
    pub(crate) fn to_rgb8(self) -> [u8; 3] {
        [x5_to_x8(self.r5), x6_to_x8(self.g6), x5_to_x8(self.b5)]
    }
    #[inline(always)]
    pub(crate) fn to_rgba8(self) -> [u8; 4] {
        let [r, g, b] = self.to_rgb8();
        [r, g, b, 255]
    }

    pub(crate) fn one_third_color_rgb8(self, color: Self) -> [u8; 3] {
        let r = self.r5 * 2 + color.r5;
        let g = self.g6 * 2 + color.g6;
        let b = self.b5 * 2 + color.b5;

        let r = ((r * 351 + 61) >> 7) as u8;
        let g = ((g as u32 * 2763 + 1039) >> 11) as u8;
        let b = ((b * 351 + 61) >> 7) as u8;
        [r, g, b]
    }
    pub(crate) fn two_third_color_rgb8(self, color: Self) -> [u8; 3] {
        let r = self.r5 + color.r5 * 2;
        let g = self.g6 + color.g6 * 2;
        let b = self.b5 + color.b5 * 2;

        let r = ((r * 351 + 61) >> 7) as u8;
        let g = ((g as u32 * 2763 + 1039) >> 11) as u8;
        let b = ((b * 351 + 61) >> 7) as u8;
        [r, g, b]
    }
    pub(crate) fn mid_color_rgb8(self, color: Self) -> [u8; 3] {
        let r = self.r5 + color.r5;
        let g = self.g6 + color.g6;
        let b = self.b5 + color.b5;

        let r = ((r * 1053 + 125) >> 8) as u8;
        let g = ((g as u32 * 4145 + 1019) >> 11) as u8;
        let b = ((b * 1053 + 125) >> 8) as u8;
        [r, g, b]
    }

    #[allow(unused)] // used in tests
    fn blend_rgb8(self, color_1: Self, blend: f32) -> [u8; 3] {
        let blend0 = 1.0 - blend;
        let r =
            ((self.r5 as f32 * blend0 + color_1.r5 as f32 * blend) * 255.0 / 31.0).round() as u8;
        let g =
            ((self.g6 as f32 * blend0 + color_1.g6 as f32 * blend) * 255.0 / 63.0).round() as u8;
        let b =
            ((self.b5 as f32 * blend0 + color_1.b5 as f32 * blend) * 255.0 / 31.0).round() as u8;
        [r, g, b]
    }
}

#[inline(always)]
pub(crate) fn x5_to_x8(x: u16) -> u8 {
    debug_assert!(x < 32);

    // These constants were found using a brute force search.
    ((x * 527 + 23) >> 6) as u8
}
#[inline(always)]
pub(crate) fn x6_to_x8(x: u16) -> u8 {
    debug_assert!(x < 64);

    // These constants were found using a brute force search.
    ((x * 259 + 33) >> 6) as u8
}
#[inline(always)]
pub(crate) fn x4_to_x8(x: u8) -> u8 {
    debug_assert!(x < 16);

    x * 17
}
#[inline(always)]
pub(crate) fn x1_to_x8(x: u16) -> u8 {
    debug_assert!(x < 2);

    if x == 0 {
        0
    } else {
        255
    }
}

#[inline(always)]
pub(crate) fn x2_to_x16(x: u32) -> u16 {
    debug_assert!(x < 4);

    (x * 21845) as u16
}
#[inline(always)]
pub(crate) fn x10_to_x16(x: u32) -> u16 {
    debug_assert!(x < 1024);

    // These constants were found using a brute force search.
    ((x * 1049585 + 8165) >> 14) as u16
}

#[inline(always)]
pub(crate) fn snorm8_to_unorm8(x: u8) -> u8 {
    // If you think that we can just do `x.wrapping_add(128)`, you'd be wrong.
    // https://learn.microsoft.com/en-us/windows/win32/api/dxgiformat/ne-dxgiformat-dxgi_format#format-modifiers
    // Both -128 and -127 map to -1.0. So we have to do more work:
    //
    // We start with `y = x.wrapping_add(128).saturating_sub(1)`. This maps
    // [-128, 127] to [0, 254] and correctly maps both -128 and -127 to 0.
    let y = x.wrapping_add(128).saturating_sub(1) as u16;

    // So now, we only have to map the interval [0, 254] to [0, 255].
    // As above, these constants were found using a brute force search.
    ((y * 129 + 1) >> 7) as u8
}
#[inline(always)]
pub(crate) fn snorm16_to_unorm16(x: u16) -> u16 {
    // Same as above, just 16 bits.
    let y = x.wrapping_add(32768).saturating_sub(1) as u32;
    ((y * 32769 + 1) >> 15) as u16
}

pub(crate) fn f16_to_f32(half: u16) -> f32 {
    // https://stackoverflow.com/questions/36008434/how-can-i-decode-f16-to-f32-using-only-the-stable-standard-library
    let exp: u16 = half >> 10 & 0b1_1111;
    let mant: u16 = half & 0b11_1111_1111;
    let val: f32 = if exp == 0 {
        // denorm
        mant as f32 * 2.0_f32.powi(-24)
    } else if exp != 31 {
        (mant as f32 + 1024_f32) * 2.0_f32.powi(exp as i32 - 25)
    } else if mant == 0 {
        f32::INFINITY
    } else {
        f32::NAN
    };
    if half & 0x8000 != 0 {
        -val
    } else {
        val
    }
}
pub(crate) fn f11_to_f32(half: u16) -> f32 {
    // based on f16_to_f32
    let exp: u16 = half >> 6 & 0b1_1111;
    let mant: u16 = half & 0b11_1111;
    let val: f32 = if exp == 0 {
        // denorm
        mant as f32 * 2.0_f32.powi(-20)
    } else if exp != 31 {
        (mant as f32 + 64_f32) * 2.0_f32.powi(exp as i32 - 21)
    } else if mant == 0 {
        f32::INFINITY
    } else {
        f32::NAN
    };
    // no sign bit
    val
}
pub(crate) fn f10_to_f32(half: u16) -> f32 {
    // based on f16_to_f32
    let exp: u16 = half >> 5 & 0b1_1111;
    let mant: u16 = half & 0b1_1111;
    let val: f32 = if exp == 0 {
        // denorm
        mant as f32 * 2.0_f32.powi(-19)
    } else if exp != 31 {
        (mant as f32 + 32_f32) * 2.0_f32.powi(exp as i32 - 20)
    } else if mant == 0 {
        f32::INFINITY
    } else {
        f32::NAN
    };
    // no sign bit
    val
}

/// Computes `(a as f64 / b as f64).ceil() as T`.
///
/// We can't use std's div_ceil because of the MSRV
#[inline(always)]
pub(crate) fn div_ceil<T>(a: T, b: T) -> T
where
    T: Copy
        + From<u8>
        + PartialEq
        + std::ops::Add<T, Output = T>
        + std::ops::Div<T, Output = T>
        + std::ops::Rem<T, Output = T>
        + Unsigned,
{
    let d = a / b;
    if a % b != 0_u8.into() {
        d + 1_u8.into()
    } else {
        d
    }
}

/// Computes `(a as f64 / b as f64).round() as T`.
#[inline(always)]
pub(crate) fn div_round<T>(a: T, b: T) -> T
where
    T: Copy + From<u8> + std::ops::Add<T, Output = T> + std::ops::Div<T, Output = T> + Unsigned,
{
    (a + b / 2.into()) / b
}

pub(crate) fn float3_to_bytes(floats: [f32; 3]) -> [u8; 12] {
    bytemuck::cast(floats)
}
pub(crate) fn float4_to_bytes(floats: [f32; 4]) -> [u8; 16] {
    bytemuck::cast(floats)
}

#[cfg(test)]
mod test {
    #[test]
    fn x4_to_x8() {
        assert_eq!(super::x4_to_x8(0), 0);
        assert_eq!(super::x4_to_x8(15), 255);

        for x in 1..15 {
            let expected = (x as f64 / 15.0 * 255.0).round() as u8;
            assert_eq!(super::x4_to_x8(x), expected);
        }
    }
    #[test]
    fn x5_to_x8() {
        assert_eq!(super::x5_to_x8(0), 0);
        assert_eq!(super::x5_to_x8(31), 255);

        for x in 1..31 {
            let expected = (x as f64 / 31.0 * 255.0).round() as u8;
            assert_eq!(super::x5_to_x8(x), expected);
        }
    }
    #[test]
    fn x6_to_x8() {
        assert_eq!(super::x6_to_x8(0), 0);
        assert_eq!(super::x6_to_x8(63), 255);

        for x in 1..63 {
            let expected = (x as f64 / 63.0 * 255.0).round() as u8;
            assert_eq!(super::x6_to_x8(x), expected);
        }
    }
    #[test]
    fn x1_to_x8() {
        assert_eq!(super::x1_to_x8(0), 0);
        assert_eq!(super::x1_to_x8(1), 255);
    }
    #[test]
    fn x2_to_x16() {
        assert_eq!(super::x2_to_x16(0), 0);
        assert_eq!(super::x2_to_x16(3), 65535);

        for x in 1..3 {
            let expected = (x as f64 / 3.0 * 65535.0).round() as u16;
            assert_eq!(super::x2_to_x16(x), expected);
        }
    }
    #[test]
    fn x10_to_x16() {
        assert_eq!(super::x10_to_x16(0), 0);
        assert_eq!(super::x10_to_x16(1023), 65535);

        for x in 1..1023 {
            let expected = (x as f64 / 1023.0 * 65535.0).round() as u16;
            assert_eq!(super::x10_to_x16(x), expected);
        }
    }

    #[test]
    fn snorm8_to_unorm8() {
        assert_eq!(super::snorm8_to_unorm8(bytemuck::cast(-128_i8)), 0);
        assert_eq!(super::snorm8_to_unorm8(bytemuck::cast(-127_i8)), 0);
        assert_eq!(super::snorm8_to_unorm8(bytemuck::cast(0_i8)), 128);
        assert_eq!(super::snorm8_to_unorm8(bytemuck::cast(127_i8)), 255);

        for x in 0..255 {
            let xi: i8 = bytemuck::cast(x);
            let expected = ((xi.max(-127) as f64 / 127.0 + 1.0) / 2.0 * 255.0).round() as u8;
            assert_eq!(super::snorm8_to_unorm8(x), expected);
        }
    }
    #[test]
    fn snorm16_to_unorm16() {
        assert_eq!(super::snorm16_to_unorm16(bytemuck::cast(-32768_i16)), 0);
        assert_eq!(super::snorm16_to_unorm16(bytemuck::cast(-32767_i16)), 0);
        assert_eq!(super::snorm16_to_unorm16(bytemuck::cast(0_i16)), 32768);
        assert_eq!(super::snorm16_to_unorm16(bytemuck::cast(32767_i16)), 65535);

        for x in 0..65535u16 {
            let xi: i16 = bytemuck::cast(x);
            let expected = ((xi.max(-32767) as f64 / 32767.0 + 1.0) / 2.0 * 65535.0).round() as u16;
            assert_eq!(super::snorm16_to_unorm16(x), expected);
        }
    }

    #[test]
    fn b5g6r5_color_interpolation() {
        // this test isn't exhaustive for performance reasons, but it should be good enough
        for x in (0..65535u16).step_by(15) {
            let color = super::B5G6R5::from_u16(x);
            for y in (0..65535u16).step_by(314) {
                let color_1 = super::B5G6R5::from_u16(y);

                assert_eq!(
                    color.mid_color_rgb8(color_1),
                    color.blend_rgb8(color_1, 1.0 / 2.0)
                );
                assert_eq!(
                    color.one_third_color_rgb8(color_1),
                    color.blend_rgb8(color_1, 1.0 / 3.0)
                );
                assert_eq!(
                    color.two_third_color_rgb8(color_1),
                    color.blend_rgb8(color_1, 2.0 / 3.0)
                );
            }
        }
    }

    #[test]
    fn div_ceil() {
        for a in 0..32 {
            for b in 1..32 {
                let expected = (a as f64 / b as f64).ceil() as u8;
                assert_eq!(super::div_ceil(a, b), expected, "a={}, b={}", a, b);
            }
        }
    }
    #[test]
    fn div_round() {
        for a in 0..32 {
            for b in 1..32 {
                let expected = (a as f64 / b as f64).round() as u8;
                assert_eq!(super::div_round(a, b), expected, "a={}, b={}", a, b);
            }
        }
    }
}
