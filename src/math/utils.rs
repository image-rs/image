//! Shared mathematical utility functions.

/// Cut value to be inside given range
///
/// ```
/// use image::math::utils;
///
/// assert_eq!(utils::clamp(-5, 0, 10),  0);
/// assert_eq!(utils::clamp( 6, 0, 10),  6);
/// assert_eq!(utils::clamp(15, 0, 10), 10);
/// ```
#[inline]
#[deprecated]
pub fn clamp<N>(a: N, min: N, max: N) -> N
where
    N: PartialOrd,
{
    if a < min {
        return min;
    }
    if a > max {
        return max;
    }
    a
}

/// Calculates the width and height an image should be resized to.
/// This preserves aspect ratio, and based on the `fill` parameter
/// will either fill the dimensions to fit inside the smaller constraint
/// (will overflow the specified bounds on one axis to preserve
/// aspect ratio), or will shrink so that both dimensions are
/// completely contained with in the given `width` and `height`,
/// with empty space on one axis.
pub(crate) fn resize_dimensions(width: u32, height: u32, nwidth: u32, nheight: u32, fill: bool) -> (u32, u32) {
    let ratio = u64::from(width) * u64::from(nheight);
    let nratio = u64::from(nwidth) * u64::from(height);

    let use_width = if fill {
        nratio > ratio
    } else {
        nratio <= ratio
    };
    let intermediate = if use_width {
        u64::from(height) * u64::from(nwidth) / u64::from(width)
    } else {
        u64::from(width) * u64::from(nheight) / u64::from(height)
    };
    let intermediate = std::cmp::max(1, intermediate);
    if use_width {
        if intermediate <= u64::from(::std::u32::MAX) {
            (nwidth, intermediate as u32)
        } else {
            (
                (u64::from(nwidth) * u64::from(::std::u32::MAX) / intermediate) as u32,
                ::std::u32::MAX,
            )
        }
    } else if intermediate <= u64::from(::std::u32::MAX) {
        (intermediate as u32, nheight)
    } else {
        (
            ::std::u32::MAX,
            (u64::from(nheight) * u64::from(::std::u32::MAX) / intermediate) as u32,
        )
    }
}

#[cfg(test)]
mod test {
    quickcheck! {
        fn resize_bounds_correctly_width(old_w: u32, new_w: u32) -> bool {
            if old_w == 0 || new_w == 0 { return true; }
            let result = super::resize_dimensions(old_w, 400, new_w, ::std::u32::MAX, false);
            result.0 == new_w && result.1 == (400 as f64 * new_w as f64 / old_w as f64) as u32
        }
    }

    quickcheck! {
        fn resize_bounds_correctly_height(old_h: u32, new_h: u32) -> bool {
            if old_h == 0 || new_h == 0 { return true; }
            let result = super::resize_dimensions(400, old_h, ::std::u32::MAX, new_h, false);
            result.1 == new_h && result.0 == (400 as f64 * new_h as f64 / old_h as f64) as u32
        }
    }

    #[test]
    fn resize_handles_fill() {
        let result = super::resize_dimensions(100, 200, 200, 500, true);
        assert!(result.0 == 250);
        assert!(result.1 == 500);

        let result = super::resize_dimensions(200, 100, 500, 200, true);
        assert!(result.0 == 500);
        assert!(result.1 == 250);
    }

    #[test]
    fn resize_never_rounds_to_zero() {
        let result = super::resize_dimensions(1, 150, 128, 128, false);
        assert!(result.0 > 0);
        assert!(result.1 > 0);
    }

    #[test]
    fn resize_handles_overflow() {
        let result = super::resize_dimensions(100, ::std::u32::MAX, 200, ::std::u32::MAX, true);
        assert!(result.0 == 100);
        assert!(result.1 == ::std::u32::MAX);

        let result = super::resize_dimensions(::std::u32::MAX, 100, ::std::u32::MAX, 200, true);
        assert!(result.0 == ::std::u32::MAX);
        assert!(result.1 == 100);
    }
}
