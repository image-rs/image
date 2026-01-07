use core::cmp;
use core::ops::Range;

use crate::GenericImageView;

/// A Rectangle defined by its top left corner, width and height.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Rect {
    /// The x coordinate of the top left corner.
    pub x: u32,
    /// The y coordinate of the top left corner.
    pub y: u32,
    /// The rectangle's width, going right toward larger x.
    pub width: u32,
    /// The rectangle's height, going down toward larger y.
    pub height: u32,
}

impl Rect {
    /// Construct a rectangle from the pixel range it covers.
    ///
    /// ```
    /// use image::math::Rect;
    ///
    /// let rect = Rect::from_xy_ranges(10..20, 30..50);
    /// assert_eq!(rect.width, 10);
    /// assert_eq!(rect.height, 20);
    /// ```
    ///
    /// # Panics
    ///
    /// Panics with debug assertions if the end of either range is less than the start. In release
    /// configurations that is treated as an empty range, the top-left coordinate is copied from
    /// the start and the width or height respectively is set to zero.
    pub fn from_xy_ranges(x: Range<u32>, y: Range<u32>) -> Self {
        debug_assert!(
            x.start <= x.end,
            "Range for x is incorrectly empty: {}..{}",
            x.start,
            x.end
        );
        debug_assert!(
            y.start <= y.end,
            "Range for y is incorrectly empty: {}..{}",
            x.start,
            x.end
        );

        Self {
            x: x.start,
            y: y.start,
            width: x.end.saturating_sub(x.start),
            height: y.end.saturating_sub(y.start),
        }
    }

    pub(crate) fn test_in_bounds(&self, image: &impl GenericImageView) -> bool {
        image.width().checked_sub(self.width) >= Some(self.x)
            && image.height().checked_sub(self.height) >= Some(self.y)
    }

    /// Check that the rectangle is contained in the image, panic otherwise.
    ///
    /// If exposed outside the library, add `#[inline]`.
    #[track_caller]
    pub(crate) fn assert_in_bounds_of(&self, image: &impl GenericImageView) {
        let (width, height) = image.dimensions();

        // Lots of ways to write this comparison..
        if u64::from(self.x) + u64::from(self.width) <= u64::from(width)
            && u64::from(self.y) + u64::from(self.height) <= u64::from(height)
        {
            return;
        }

        panic_out_of_bounds(self, (width, height));
    }

    /// Return the part of the rectangle that is in-bounds of the image.
    pub(crate) fn crop_dimms(&self, image: &impl GenericImageView) -> Rect {
        let (width, height) = image.dimensions();

        let x = cmp::min(self.x, width);
        let y = cmp::min(self.y, height);

        let width = cmp::min(self.width, width - x);
        let height = cmp::min(self.height, height - y);

        Rect {
            x,
            y,
            width,
            height,
        }
    }
}

#[cold]
fn panic_out_of_bounds(rect: &Rect, dims: (u32, u32)) -> ! {
    panic!(
        "The rectangle {:?} is out of bounds for the image with dimensions {}×{}",
        rect, dims.0, dims.1
    );
}
