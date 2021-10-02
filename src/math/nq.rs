//! NEUQUANT Neural-Net quantization algorithm by Anthony Dekker, 1994.
//! See "Kohonen neural networks for optimal colour quantization"
//! in "Network: Computation in Neural Systems" Vol. 5 (1994) pp 351-367.
//! for a discussion of the algorithm.
//! See also <https://scientificgems.wordpress.com/stuff/neuquant-fast-high-quality-image-quantization/>

/* NeuQuant Neural-Net Quantization Algorithm
 * ------------------------------------------
 *
 * Copyright (c) 1994 Anthony Dekker
 *
 * NEUQUANT Neural-Net quantization algorithm by Anthony Dekker, 1994.
 * See "Kohonen neural networks for optimal colour quantization"
 * in "Network: Computation in Neural Systems" Vol. 5 (1994) pp 351-367.
 * for a discussion of the algorithm.
 * See also https://scientificgems.wordpress.com/stuff/neuquant-fast-high-quality-image-quantization/
 *
 * Any party obtaining a copy of these files from the author, directly or
 * indirectly, is granted, free of charge, a full and unrestricted irrevocable,
 * world-wide, paid up, royalty-free, nonexclusive right and license to deal
 * in this software and documentation files (the "Software"), including without
 * limitation the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons who receive
 * copies from any such party to do so, with the only requirement being
 * that this copyright notice remain intact.
 *
 *
 * Incorporated bugfixes and alpha channel handling from pngnq
 * http://pngnq.sourceforge.net
 *
 */

/// Neural network color quantizer
///
/// # Examples
/// ```
/// use image::imageops::colorops::{index_colors, ColorMap};
/// use image::math::nq::NeuQuant;
/// use image::{ImageBuffer, Rgba, RgbaImage};
///
/// // Create simple color image with RGBA pixels.
/// let (w, h) = (2, 2);
/// let red: Rgba<u8> = [255, 0, 0, 255].into();
/// let green: Rgba<u8> = [0, 255, 0, 255].into();
/// let blue: Rgba<u8> = [0, 0, 255, 255].into();
/// let white: Rgba<u8> = [255, 255, 255, 255].into();
/// let mut color_image = RgbaImage::new(w, h);
/// color_image.put_pixel(0, 0, red);
/// color_image.put_pixel(1, 0, green);
/// color_image.put_pixel(0, 1, blue);
/// color_image.put_pixel(1, 1, white);
///
/// // Create a `NeuQuant` colormap that will build an approximate color palette that best matches
/// // the original image.
/// // Note, the NeuQuant algorithm is only designed to work with 6-8 bit output, so `colors`
/// // should be a power of 2 in the range [64, 256].
/// let pixels = color_image.clone().into_raw();
/// let cmap = NeuQuant::new(1, 256, &pixels);
/// // Map the original image through the color map to create an indexed image stored in a
/// // `GrayImage`.
/// let palletized = index_colors(&color_image, &cmap);
/// // Map indexed image back `RgbaImage`.  Note the NeuQuant algorithm creates an approximation of
/// // the original colors, so even in this simple example the output is not pixel equivalent to
/// // the original.
/// let mapped = ImageBuffer::from_fn(w, h, |x, y| -> Rgba<u8> {
///     let p = palletized.get_pixel(x, y);
///     cmap.lookup(p.0[0] as usize)
///         .expect("indexed color out-of-range")
///         .into()
/// });
/// ```
#[deprecated(note = "Use the `color_quant` crate instead")]
pub struct NeuQuant {
    inner: color_quant::NeuQuant,
}

/// The implementation only calls the corresponding inner `color_quant` methods.
///
/// These exist purely to keep a type separate from [`color_quant::NeuQuant`] and the interface
/// stable for this major version. The type will be changed to a pure re-export in the next
/// version or might be removed.
///
/// [`color_quant::NeuQuant`]: https://docs.rs/color_quant/1.1.0/color_quant/struct.NeuQuant.html
#[allow(deprecated)]
#[allow(missing_docs)]
impl NeuQuant {
    pub fn new(samplefac: i32, colors: usize, pixels: &[u8]) -> Self {
        color_quant::NeuQuant::new(samplefac, colors, pixels).into()
    }
    pub fn init(&mut self, pixels: &[u8]) {
        self.inner.init(pixels)
    }
    pub fn map_pixel(&self, pixel: &mut [u8]) {
        self.inner.map_pixel(pixel)
    }
    pub fn index_of(&self, pixel: &[u8]) -> usize {
        self.inner.index_of(pixel)
    }
    pub fn lookup(&self, idx: usize) -> Option<[u8; 4]> {
        self.inner.lookup(idx)
    }
}

#[allow(deprecated)]
impl From<color_quant::NeuQuant> for NeuQuant {
    fn from(inner: color_quant::NeuQuant) -> Self {
        NeuQuant { inner }
    }
}

#[allow(deprecated)]
impl From<NeuQuant> for color_quant::NeuQuant {
    fn from(this: NeuQuant) -> Self {
        this.inner
    }
}
