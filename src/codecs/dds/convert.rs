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

    pub(crate) fn blend_rgb8(self, color_1: Self, blend: f32) -> [u8; 3] {
        let blend0 = 1.0 - blend;
        let r =
            ((self.r5 as f32 * blend0 + color_1.r5 as f32 * blend) * 255.0 / 31.0).round() as u8;
        let g =
            ((self.g6 as f32 * blend0 + color_1.g6 as f32 * blend) * 255.0 / 63.0).round() as u8;
        let b =
            ((self.b5 as f32 * blend0 + color_1.b5 as f32 * blend) * 255.0 / 31.0).round() as u8;
        [r, g, b]
    }
    #[inline(always)]
    pub(crate) fn blend_rgba8(self, color_1: Self, blend: f32) -> [u8; 4] {
        let [r, g, b] = self.blend_rgb8(color_1, blend);
        [r, g, b, 255]
    }
}

#[inline(always)]
pub(crate) fn x5_to_x8(x: u16) -> u8 {
    debug_assert!(x < 32);

    // This is equivalent to: (x as f64 / 31.0 * 255.0).round() as u8
    // Here's how it works:
    //   (x as f64 / 31.0 * 255.0).round() as u8
    // We only deal with positive numbers, so we can rewrite x.round() as (x + 0.5).floor()
    //   (x as f64 / 31.0 * 255.0 + 0.5).floor() as u8
    // Multiplication is commutative
    //   ((x as f64 * 255.0) / 31.0 + 0.5).floor() as u8
    // Move + 0.5 inside the denominator
    //   ((x as f64 * 255.0 + 0.5*31.0) / 31.0).floor() as u8
    // Expand the fraction by 2
    //   ((x as f64 * 255.0 * 2.0 + 31.0) / (31.0 * 2.0)).floor() as u8
    // Realize that we have integers everywhere.
    //   ((x * 510 + 31) as f64 / 62_f64).floor() as u8
    // The last trick that we can replace (a as f64/b as f64).floor() with just
    // (a as uN / b as uN), because integer division is a floor division.
    //   ((x * 510 + 31) / 62) as u8
    ((x * 510 + 31) / 62) as u8
}
#[inline(always)]
pub(crate) fn x6_to_x8(x: u16) -> u8 {
    debug_assert!(x < 64);

    // see x5_to_x8 for an explanation
    ((x * 510 + 63) / 126) as u8
}
#[inline(always)]
pub(crate) fn x4_to_x8(x: u16) -> u8 {
    debug_assert!(x < 16);

    // see x5_to_x8 for an explanation
    ((x * 510 + 15) / 30) as u8
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

    // see x5_to_x8 for an explanation
    ((x * 131070 + 3) / 6) as u16
}
#[inline(always)]
pub(crate) fn x10_to_x16(x: u32) -> u16 {
    debug_assert!(x < 1024);

    // see x5_to_x8 for an explanation
    ((x * 131070 + 1023) / 2046) as u16
}

#[inline(always)]
pub(crate) fn snorm8_to_unorm8(x: u8) -> u8 {
    // If you think that we can just do `x.wrapping_add(128)`, you'd be wrong.
    // https://learn.microsoft.com/en-us/windows/win32/api/dxgiformat/ne-dxgiformat-dxgi_format#format-modifiers
    // Both -128 and -127 map to -1.0. So we have to do more work:
    //
    // We start with `y = x.wrapping_add(128).saturating_sub(1)`. This maps
    // [-128, 127] to [0, 254] and correctly maps both -128 and -127 to 0.
    // So now, we only have to map the interval [0, 254] to [0, 255]. This is
    // done with the same tricks used in x5_to_x8:
    //   (y as f64 / 254.0 * 255.0).round() as u8
    //   (y as f64 * 255.0 / 254.0 + 0.5).floor() as u8
    //   ((y as f64 * 255.0 + 127.0) / 254.0).floor() as u8
    //   ((y * 255 + 127) / 254) as u8
    let y = x.wrapping_add(128).saturating_sub(1) as u16;
    ((y * 255 + 127) / 254) as u8
}
#[inline(always)]
pub(crate) fn snorm16_to_unorm16(x: u16) -> u16 {
    // Same as above, just 16 bits.
    let y = x.wrapping_add(32768).saturating_sub(1) as u32;
    ((y * 65535 + 32767) / 65534) as u16
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
        ::std::f32::INFINITY
    } else {
        ::std::f32::NAN
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
        ::std::f32::INFINITY
    } else {
        ::std::f32::NAN
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
        ::std::f32::INFINITY
    } else {
        ::std::f32::NAN
    };
    // no sign bit
    val
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
}
