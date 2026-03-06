//! Mathematical helper functions and types.
mod rect;
mod utils;

pub use self::rect::Rect;
pub(crate) use utils::{fast_round_positive_f32, multiply_accumulate};
pub(super) use utils::resize_dimensions;
