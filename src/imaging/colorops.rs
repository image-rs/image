//! Functions for altering and converting the color of pixelbufs
use std::num::{
    cast,
    Bounded
};

use imaging::pixel::{
    Pixel,
    Luma
};

fn clamp<N: Num + PartialOrd>(a: N, min: N, max: N) -> N {
    if a > max { max }
    else if a < min { min }
    else { a }
}

/// Convert ```pixels``` to grayscale
pub fn grayscale<A: Primitive, T: Pixel<A>>(pixels: &[T]) -> Vec<Luma<A>> {
    pixels.iter().map(|i| i.to_luma()).collect()
}

/// Invert each pixel within ```pixels```
/// This function operates in place.
pub fn invert<A: Primitive, T: Pixel<A>>(pixels: &mut [T]) {
    for i in pixels.mut_iter() {
        i.invert();
    }
}

/// Adjust the contrast of ```pixels```
/// ```contrast``` is the amount to adjust the contrast by.
/// Negative values decrease the constrast and positive values increase the constrast.
pub fn contrast<A: Primitive, T: Pixel<A>>(pixels: &[T], contrast: f32) -> Vec<T> {
    let max: A = Bounded::max_value();
    let max = cast::<A, f32>(max).unwrap();

    let percent = ((100.0 + contrast) / 100.0).powi(2);

    pixels.iter().map(|a| a.map(|b| {
        let c = cast::<A, f32>(b).unwrap();
        let d = ((c / max - 0.5) * percent  + 0.5) * max;
        let e = clamp(d, 0.0, max);

        cast::<f32, A>(e).unwrap()
    })).collect()
}

/// Brighten ```pixels```
/// ```value``` is the amount to brighten each pixel by.
/// Negative values decrease the brightness and positive values increase it.
pub fn brighten<A: Primitive, T: Pixel<A>>(pixels: &[T], value: i32) -> Vec<T> {
    let max: A = Bounded::max_value();
    let max = cast::<A, i32>(max).unwrap();

    pixels.iter().map(|a| a.map(|b| {
        let c = cast::<A, i32>(b).unwrap();
        let d = clamp(c + value, 0, max);

        cast::<i32, A>(d).unwrap()
    })).collect()
}