//! Mathematical helper functions and types.
pub mod nq;
pub mod utils;

mod rect;
pub use self::rect::Rect;
pub(crate) use self::utils::resize_dimensions;
