//! This module provides usefull traits that where deprecated in rust

// Note copied from the stdlib under MIT license

use num::{ Float, Num, NumCast };
use std::marker::Reflect;


/// Primitive trait from old stdlib, added max_value
pub trait Primitive: Copy + NumCast + Num + PartialOrd<Self> + Clone + Reflect {
    /// The maximum value of primitive.
    fn max_value() -> Self;
}

impl Primitive for usize {
    fn max_value() -> usize { usize::max_value() }
}
impl Primitive for u8 {
    fn max_value() -> u8 { u8::max_value() }
}
impl Primitive for u16 {
    fn max_value() -> u16 { u16::max_value() }
}
impl Primitive for u32 {
    fn max_value() -> u32 { u32::max_value() }
}
impl Primitive for u64 {
    fn max_value() -> u64 { u64::max_value() }
}
impl Primitive for isize {
    fn max_value() -> isize { isize::max_value() }
}
impl Primitive for i8 {
    fn max_value() -> i8 { i8::max_value() }
}
impl Primitive for i16 {
    fn max_value() -> i16 { i16::max_value() }
}
impl Primitive for i32 {
    fn max_value() -> i32 { i32::max_value() }
}
impl Primitive for i64 {
    fn max_value() -> i64 { i64::max_value() }
}
impl Primitive for f32 {
    fn max_value() -> f32 { Float::max_value() }
}
impl Primitive for f64 {
    fn max_value() -> f64 { Float::max_value() }
}
