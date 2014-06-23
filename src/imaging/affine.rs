//! Functions for performing affine transformations.

use std::default::Default;

use imaging::pixel::Pixel;

///Rotate ```pixels``` 90 degrees clockwise.
pub fn rotate90<P: Primitive, T: Pixel<P> + Default + Clone + Copy>(
        pixels: &[T],
        width:  u32,
        height: u32) -> Vec<T> {

        let width  = width as uint;
        let height = height as uint;

        let d: T = Default::default();
        let mut out = Vec::from_elem(width * height, d);

        for y in range(0, height) {
                for x in range(0, width) {
                        let p = pixels.as_slice()[y * width + x];
                        out.as_mut_slice()[x * height + (height -1 - y)] = p;
                }
        }

        out
}

///Rotate ```pixels``` 180 degrees clockwise.
pub fn rotate180<P: Primitive, T: Pixel<P> + Default + Copy + Clone>(
        pixels: &[T],
        width:  u32,
        height: u32) -> Vec<T> {

        let width  = width as uint;
        let height = height as uint;

        let d: T = Default::default();
        let mut out = Vec::from_elem(width * height, d);

        for y in range(0, height) {
                for x in range(0, width) {
                        let p = pixels.as_slice()[y * width + x];
                        out.as_mut_slice()[(height - 1 - y) * width + width - 1 - x] = p;
                }
        }

        out
}

///Rotate ```pixels``` 270 degrees clockwise.
pub fn rotate270<P: Primitive, T: Pixel<P> + Default + Copy + Clone>(
        pixels: &[T],
        width:  u32,
        height: u32) -> Vec<T> {

        let width  = width as uint;
        let height = height as uint;

        let d: T = Default::default();
        let mut out = Vec::from_elem(width * height, d);

        for y in range(0, height) {
                for x in range(0, width) {
                        let p = pixels.as_slice()[y * width + x];
                        out.as_mut_slice()[(width - 1 - x) * height + y] = p;
                }
        }

        out
}

///Flip ```pixels``` horizontally
pub fn flip_horizontal<P: Primitive, T: Pixel<P> + Default + Copy + Clone>(
        pixels: &[T],
        width:  u32,
        height: u32) -> Vec<T> {

        let width  = width as uint;
        let height = height as uint;

        let d: T = Default::default();
        let mut out = Vec::from_elem(width * height, d);

        for y in range(0, height) {
                for x in range(0, width) {
                        let p = pixels.as_slice()[y * width + x];
                        out.as_mut_slice()[y * width + width - 1 - x] = p;
                }
        }

        out
}

///Flip ```pixels``` vertically
pub fn flip_vertical<P: Primitive, T: Pixel<P> + Default + Copy + Clone>(
        pixels: &[T],
        width:  u32,
        height: u32) -> Vec<T> {

        let width  = width as uint;
        let height = height as uint;

        let d: T = Default::default();
        let mut out = Vec::from_elem(width * height, d);

        for y in range(0, height) {
                for x in range(0, width) {
                        let p = pixels.as_slice()[y * width + x];
                        out.as_mut_slice()[(height - 1 - y) * width + x] = p;
                }
        }

        out
}