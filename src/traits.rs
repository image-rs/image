//! This module provides usefull traits that where deprecated in rust

// Note copied from the stdlib under MIT license

use std::ops::{ Add, Div, Mul, Neg, Rem, Sub };
use std::num::{ Float, Int, NumCast };

/// Num trait from old stdlib
pub trait Num: Zero + One + Add<Self, Self> + Sub<Self, Self> + Mul<Self, Self> + Div<Self, Self> + Rem<Self, Self> + Neg<Self> + PartialEq<Self> { }
impl<A: Zero + One + Add<A, A> + Sub<A, A> + Mul<A, A> + Div<A, A> + Rem<A, A> + Neg<A> + PartialEq<A>> Num for A { }

/// Zero trait from old stdlib
pub trait Zero: Add<Self, Self> {
    /// Returns the zero value for T
    fn zero() -> Self;
    /// Returns true if zero.
    fn is_zero(&self) -> bool;
}

/// Returns the zero value for T
#[allow(unused)]
pub fn zero<T: Zero>() -> T { Zero::zero() }
macro_rules! zero_impl {
    ($t:ty, $v:expr) => {
        impl Zero for $t {
            fn zero() -> $t { $v }
            fn is_zero(&self) -> bool { *self == $v }
        }
    }
}
zero_impl!(uint, 0u);
zero_impl!(u8,   0u8);
zero_impl!(u16,  0u16);
zero_impl!(u32,  0u32);
zero_impl!(u64,  0u64);
zero_impl!(int, 0i);
zero_impl!(i8,  0i8);
zero_impl!(i16, 0i16);
zero_impl!(i32, 0i32);
zero_impl!(i64, 0i64);
zero_impl!(f32, 0.0f32);
zero_impl!(f64, 0.0f64);


/// Primitive trait from old stdlib, added max_value
pub trait Primitive: Copy + NumCast + Num + PartialOrd<Self> + Clone {
    /// The maximum value of primitive.
    fn max_value() -> Self;
}

impl Primitive for uint {
    fn max_value() -> uint { Int::max_value() }
}
impl Primitive for u8 {
    fn max_value() -> u8 { Int::max_value() }
}
impl Primitive for u16 {
    fn max_value() -> u16 { Int::max_value() }
}
impl Primitive for u32 {
    fn max_value() -> u32 { Int::max_value() }
}
impl Primitive for u64 {
    fn max_value() -> u64 { Int::max_value() }
}
impl Primitive for int {
    fn max_value() -> int { Int::max_value() }
}
impl Primitive for i8 {
    fn max_value() -> i8 { Int::max_value() }
}
impl Primitive for i16 {
    fn max_value() -> i16 { Int::max_value() }
}
impl Primitive for i32 {
    fn max_value() -> i32 { Int::max_value() }
}
impl Primitive for i64 {
    fn max_value() -> i64 { Int::max_value() }
}
impl Primitive for f32 {
    fn max_value() -> f32 { Float::max_value() }
}
impl Primitive for f64 {
    fn max_value() -> f64 { Float::max_value() }
}

/// One trait from old stdlib, added max_value
pub trait One: Mul<Self, Self> {
    /// Returns the unit value of Self
    fn one() -> Self;
}
/// Returns the unit value of T
#[allow(unused)]
pub fn one<T: One>() -> T { One::one() }
macro_rules! one_impl {
    ($t:ty, $v:expr) => {
        impl One for $t {
            fn one() -> $t { $v }
        }
    }
}
one_impl!(uint, 1u);
one_impl!(u8,  1u8);
one_impl!(u16, 1u16);
one_impl!(u32, 1u32);
one_impl!(u64, 1u64);
one_impl!(int, 1i);
one_impl!(i8,  1i8);
one_impl!(i16, 1i16);
one_impl!(i32, 1i32);
one_impl!(i64, 1i64);
one_impl!(f32, 1.0f32);
one_impl!(f64, 1.0f64);
