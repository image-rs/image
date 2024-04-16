use serde::{Deserialize, Serialize};
use std::fmt::Display;

#[derive(Clone, Copy, Debug, Serialize, Deserialize, PartialEq, Hash)]
/// This structure defines a region of interest.
/// The region of interest is defined in the un-binned pixel space.
pub struct ROI {
    /// The minimum X coordinate (in binned pixel space).
    pub x_min: u32,
    /// The minimum Y coordinate (in binned pixel space).
    pub y_min: u32,
    /// The image width (X axis, in binned pixel space).
    pub width: u32,
    /// The image height (Y axis, in binned pixel space).
    pub height: u32,
    /// The X binning factor.
    pub bin_x: u32,
    /// The Y binning factor.
    pub bin_y: u32,
}

impl Default for ROI {
    fn default() -> Self {
        ROI {
            x_min: 0,
            y_min: 0,
            width: 0,
            height: 0,
            bin_x: 1,
            bin_y: 1,
        }
    }
}

impl Display for ROI {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "ROI: Origin = ({}, {}), Image Size = ({} x {}), Bin = ({}, {})",
            self.x_min, self.y_min, self.width, self.height, self.bin_x, self.bin_y
        )
    }
}
