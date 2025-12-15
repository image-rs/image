use core::ops::Range;

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
    pub fn from_xy_ranges(x: Range<u32>, y: Range<u32>) -> Self {
        Self {
            x: x.start,
            y: y.start,
            width: x.end.saturating_sub(x.start),
            height: y.end.saturating_sub(y.start),
        }
    }
}
