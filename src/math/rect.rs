use crate::GenericImageView;

/// A Rectangle defined by its top left corner, width and height.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Rect {
    /// The x coordinate of the top left corner.
    pub x: u32,
    /// The y coordinate of the top left corner.
    pub y: u32,
    /// The rectangle's width.
    pub width: u32,
    /// The rectangle's height.
    pub height: u32,
}

impl Rect {
    pub(crate) fn test_in_bounds(&self, image: &(impl GenericImageView + ?Sized)) -> bool {
        image.width().checked_sub(self.width) >= Some(self.x)
            && image.height().checked_sub(self.height) >= Some(self.y)
    }
}
