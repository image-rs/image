//! This module provides useful traits that were deprecated in rust

// Note copied from the stdlib under MIT license

use num_traits::{ Bounded, Num, NumCast };


/// Primitive trait from old stdlib
pub trait Primitive: Copy + NumCast + Num + PartialOrd<Self> + Clone + Bounded {
}

impl Primitive for usize {
}
impl Primitive for u8 {
}
impl Primitive for u16 {
}
impl Primitive for u32 {
}
impl Primitive for u64 {
}
impl Primitive for isize {
}
impl Primitive for i8 {
}
impl Primitive for i16 {
}
impl Primitive for i32 {
}
impl Primitive for i64 {
}
impl Primitive for f32 {
}
impl Primitive for f64 {
}
