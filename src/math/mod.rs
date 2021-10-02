//! Mathematical helper functions and types.
pub mod nq;
pub mod utils;

mod rect;
pub use self::rect::Rect;

pub use self::utils::{resize_to_fit, resize_to_fill};
