//! Shared mathematical utility functions.

use std::cmp::max;

/// Calculates the width and height an image should be resized to.
/// This preserves aspect ratio, and based on the `fill` parameter
/// will either fill the dimensions to fit inside the smaller constraint
/// (will overflow the specified bounds on one axis to preserve
/// aspect ratio), or will shrink so that both dimensions are
/// completely contained within the given `width` and `height`,
/// with empty space on one axis.
pub(crate) fn resize_dimensions(
    width: u32,
    height: u32,
    nwidth: u32,
    nheight: u32,
    fill: bool,
) -> (u32, u32) {
    let wratio = nwidth as f64 / width as f64;
    let hratio = nheight as f64 / height as f64;

    let ratio = if fill {
        f64::max(wratio, hratio)
    } else {
        f64::min(wratio, hratio)
    };

    let nw = max((width as f64 * ratio).round() as u64, 1);
    let nh = max((height as f64 * ratio).round() as u64, 1);

    if nw > u64::from(u32::MAX) {
        let ratio = u32::MAX as f64 / width as f64;
        (u32::MAX, max((height as f64 * ratio).round() as u32, 1))
    } else if nh > u64::from(u32::MAX) {
        let ratio = u32::MAX as f64 / height as f64;
        (max((width as f64 * ratio).round() as u32, 1), u32::MAX)
    } else {
        (nw as u32, nh as u32)
    }
}

#[cfg(test)]
mod test {
    quickcheck! {
        fn resize_bounds_correctly_width(old_w: u32, new_w: u32) -> bool {
            if old_w == 0 || new_w == 0 { return true; }
            // In this case, the scaling is limited by scaling of height.
            // We could check that case separately but it does not conform to the same expectation.
            if new_w as u64 * 400u64 >= old_w as u64 * u64::from(u32::MAX) { return true; }

            let result = super::resize_dimensions(old_w, 400, new_w, ::std::u32::MAX, false);
            let exact = (400_f64 * new_w as f64 / old_w as f64).round() as u32;
            result.0 == new_w && result.1 == exact.max(1)
        }
    }

    quickcheck! {
        fn resize_bounds_correctly_height(old_h: u32, new_h: u32) -> bool {
            if old_h == 0 || new_h == 0 { return true; }
            // In this case, the scaling is limited by scaling of width.
            // We could check that case separately but it does not conform to the same expectation.
            if 400u64 * new_h as u64 >= old_h as u64 * u64::from(u32::MAX) { return true; }

            let result = super::resize_dimensions(400, old_h, ::std::u32::MAX, new_h, false);
            let exact = (400_f64 * new_h as f64 / old_h as f64).round() as u32;
            result.1 == new_h && result.0 == exact.max(1)
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

    #[test]
    fn resize_rounds() {
        // Only truncation will result in (3840, 2229) and (2160, 3719)
        let result = super::resize_dimensions(4264, 2476, 3840, 2160, true);
        assert_eq!(result, (3840, 2230));

        let result = super::resize_dimensions(2476, 4264, 2160, 3840, false);
        assert_eq!(result, (2160, 3720));
    }

    #[test]
    fn resize_handles_zero() {
        let result = super::resize_dimensions(0, 100, 100, 100, false);
        assert_eq!(result, (1, 100));

        let result = super::resize_dimensions(100, 0, 100, 100, false);
        assert_eq!(result, (100, 1));

        let result = super::resize_dimensions(100, 100, 0, 100, false);
        assert_eq!(result, (1, 1));

        let result = super::resize_dimensions(100, 100, 100, 0, false);
        assert_eq!(result, (1, 1));
    }
}
