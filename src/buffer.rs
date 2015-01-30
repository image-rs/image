use std::slice::{ Chunks, ChunksMut };
use std::any::TypeId;
use std::ops::{ Index, IndexMut };
use std::num::Int;
use std::iter::repeat;
use std::old_io::IoResult;

use traits::{ Zero, Primitive };
use color::{ Rgb, Rgba, Luma, LumaA, FromColor, ColorType };
use image::GenericImage;
use dynimage::save_buffer;

/// Mutable equivalent to AsSlice.
/// Should be replaced by a stdlib impl as soon it exists
pub trait AsMutSlice<T>  {
    /// Work with `self` as a mutable slice.
    fn as_mut_slice<'a>(&'a mut self) -> &'a mut [T];
}

impl<T> AsMutSlice<T> for [T] {
    #[inline(always)]
    fn as_mut_slice<'a>(&'a mut self) -> &'a mut [T] { self }
}


impl<T> AsMutSlice<T> for Vec<T> {
    #[inline(always)]
    fn as_mut_slice<'a>(&'a mut self) -> &'a mut [T] { self.as_mut_slice() }
}

/// And array-like type that behaves like Vec<T> or [T].
pub trait ArrayLike<T>: Index<usize, Output=T> + IndexMut<usize, Output=T> + AsSlice<T> + AsMutSlice<T> {}
impl<A: Index<usize, Output=T> + IndexMut<usize, Output=T> + AsSlice<T> + AsMutSlice<T>, T> ArrayLike<T> for A { }

/// A generalized pixel.
///
/// A pixel object is usually not used standalone but as a view into an image buffer.
pub trait Pixel: Copy + Clone {
    /// The underlying subpixel type.
    type Subpixel: Primitive;

    /// Returns the number of channels of this pixel type.
    fn channel_count() -> u8;

    /// Returns the components as a slice.
    fn channels(&self) -> &[Self::Subpixel];

    /// Returns the components as a mutable slice
    fn channels_mut(&mut self) -> &mut [Self::Subpixel];

    /// Returns a string that can help to interprete the meaning each channel
    /// See [gimp babl](http://gegl.org/babl/).
    fn color_model() -> &'static str;

    /// Returns the ColorType for this pixel format
    fn color_type() -> ColorType;

    /// Returns the channels of this pixel as a 4 tuple. If the pixel
    /// has less than 4 channels the remainder is filled with the maximum value
    /// TODO deprecate
    fn channels4(&self) -> (Self::Subpixel, Self::Subpixel, Self::Subpixel, Self::Subpixel);

    /// Construct a pixel from the 4 channels a, b, c and d.
    /// If the pixel does not contain 4 channels the extra are ignored.
    /// TODO deprecate
    fn from_channels(a: Self::Subpixel, b: Self::Subpixel, c: Self::Subpixel, d: Self::Subpixel) -> Self;

    /// Returns a view into a slice.
    ///
    /// Note: The slice length is not checked on creation. Thus the caller has to ensure
    /// that the slice is long enough to precent panics if the pixel is used later on.
    fn from_slice<'a>(slice: &'a [Self::Subpixel]) -> &'a Self;

    /// Returns mutable view into a mutable slice.
    ///
    /// Note: The slice length is not checked on creation. Thus the caller has to ensure
    /// that the slice is long enough to precent panics if the pixel is used later on.
    fn from_slice_mut<'a>(slice: &'a mut [Self::Subpixel]) -> &'a mut Self;

    /// Convert this pixel to RGB
    fn to_rgb(&self) -> Rgb<Self::Subpixel>;

    /// Convert this pixel to RGB with an alpha channel
    fn to_rgba(&self) -> Rgba<Self::Subpixel>;

    /// Convert this pixel to luma
    fn to_luma(&self) -> Luma<Self::Subpixel>;

    /// Convert this pixel to luma with an alpha channel
    fn to_luma_alpha(&self) -> LumaA<Self::Subpixel>;

    /// Apply the function ```f``` to each channel of this pixel.
    fn map<F>(&self, f: F) -> Self where F: Fn(Self::Subpixel) -> Self::Subpixel;

    /// Apply the function ```f``` to each channel of this pixel.
    fn apply<F>(&mut self, f: F) where F: Fn(Self::Subpixel) -> Self::Subpixel;

    /// Apply the function f to each channel except the alpha channel.
    /// Apply the function g to the alpha channel.
    fn map_with_alpha<F, G>(&self, f: F, g: G) -> Self
        where F: Fn(Self::Subpixel) -> Self::Subpixel, G: Fn(Self::Subpixel) -> Self::Subpixel;

    /// Apply the function f to each channel except the alpha channel.
    /// Apply the function g to the alpha channel. Works in-place.
    fn apply_with_alpha<F, G>(&mut self, f: F, g: G)
        where F: Fn(Self::Subpixel) -> Self::Subpixel, G: Fn(Self::Subpixel) -> Self::Subpixel;

    /// Apply the function ```f``` to each channel of this pixel and
    /// ```other``` pairwise.
    fn map2<F>(&self, other: &Self, f: F) -> Self
        where F: Fn(Self::Subpixel, Self::Subpixel) -> Self::Subpixel;

    /// Apply the function ```f``` to each channel of this pixel and
    /// ```other``` pairwise. Works in-place.
    fn apply2<F>(&mut self, other: &Self, f: F)
        where F: Fn(Self::Subpixel, Self::Subpixel) -> Self::Subpixel;

    /// Invert this pixel
    fn invert(&mut self);

    /// Blend the color of a given pixel into ourself, taking into account alpha channels
    fn blend(&mut self, other: &Self);
}

/// Iterate over pixel refs.
pub struct Pixels<'a, P: Pixel + 'a> where P::Subpixel: 'a {
    chunks: Chunks<'a, P::Subpixel>
}

impl<'a, P: Pixel + 'a> Iterator for Pixels<'a, P> where P::Subpixel: 'a {
    type Item = &'a P;

    #[inline(always)]
    fn next(&mut self) -> Option<&'a P> {
        self.chunks.next().map(|v|
            <P as Pixel>::from_slice(v)
        )
    }
}

impl<'a, P: Pixel + 'a> DoubleEndedIterator for Pixels<'a, P> where P::Subpixel: 'a {

    #[inline(always)]
    fn next_back(&mut self) -> Option<&'a P> {
        self.chunks.next_back().map(|v|
            <P as Pixel>::from_slice(v)
        )
    }
}

/// Iterate over mutable pixel refs.
pub struct PixelsMut<'a, P: Pixel + 'a> where P::Subpixel: 'a {
    chunks: ChunksMut<'a, P::Subpixel>
}

impl<'a, P: Pixel + 'a> Iterator for PixelsMut<'a, P> where P::Subpixel: 'a {
    type Item = &'a mut P;

    #[inline(always)]
    fn next(&mut self) -> Option<&'a mut P> {
        self.chunks.next().map(|v|
            <P as Pixel>::from_slice_mut(v)
        )
    }
}

impl<'a, P: Pixel + 'a> DoubleEndedIterator for PixelsMut<'a, P> where P::Subpixel: 'a {
    #[inline(always)]
    fn next_back(&mut self) -> Option<&'a mut P> {
        self.chunks.next_back().map(|v|
            <P as Pixel>::from_slice_mut(v)
        )
    }
}

/// Enumerate the pixels of an image.
pub struct EnumeratePixels<'a, P: Pixel + 'a> {
    pixels: Pixels<'a, P>,
    x:      u32,
    y:      u32,
    width:  u32
}

impl<'a, P: Pixel + 'a> Iterator for EnumeratePixels<'a, P> where P::Subpixel: 'a {
    type Item = (u32, u32, &'a P);

    #[inline(always)]
    fn next(&mut self) -> Option<(u32, u32, &'a P)> {
        if self.x >= self.width {
            self.x =  0;
            self.y += 1;
        }
        let (x, y) = (self.x, self.y);
        self.x += 1;
        match self.pixels.next() {
            None => None,
            Some(p) => Some((x, y, p))
        }
    }
}

/// Enumerate the pixels of an image.
pub struct EnumeratePixelsMut<'a, P: Pixel + 'a> {
    pixels: PixelsMut<'a, P>,
    x:      u32,
    y:      u32,
    width:  u32
}

impl<'a, P: Pixel + 'a> Iterator for EnumeratePixelsMut<'a, P> where P::Subpixel: 'a {
    type Item = (u32, u32, &'a mut P);

    #[inline(always)]
    fn next(&mut self) -> Option<(u32, u32, &'a mut P)> {
        if self.x >= self.width {
            self.x =  0;
            self.y += 1;
        }
        let (x, y) = (self.x, self.y);
        self.x += 1;
        match self.pixels.next() {
            None => None,
            Some(p) => Some((x, y, p))
        }
    }
}



/// Generic image buffer
pub struct ImageBuffer<P: Pixel, Container> {
    width: u32,
    height: u32,
    type_marker: TypeId,
    data: Container,
}

// generic implementation, shared along all image buffers
impl<P: Pixel + 'static, Container: ArrayLike<P::Subpixel>>
    ImageBuffer<P, Container>
    where P::Subpixel: 'static {

    /// Contructs a buffer from a generic container
    /// (for example a `Vec` or a slice)
    /// Returns None if the container is not big enough
    pub fn from_raw(width: u32, height: u32, buf: Container)
                    -> Option<ImageBuffer<P, Container>> {
        if width as usize
           * height as usize
           * <P as Pixel>::channel_count() as usize
           <= buf.as_slice().len() {
            Some(ImageBuffer {
                data: buf,
                width: width,
                height: height,
                type_marker: TypeId::of::<P>()
            })
        } else {
            None
        }
    }

    /// Returns the underlying raw buffer
    pub fn into_raw(self) -> Container {
        self.data
    }

    /// The width and height of this image.
    pub fn dimensions(&self) -> (u32, u32) {
        (self.width, self.height)
    }

    /// The width of this image.
    pub fn width(&self) -> u32 {
        self.width
    }

    /// The height of this image.
    pub fn height(&self) -> u32 {
        self.height
    }

    /// The raw image data as a slice.
    pub fn as_slice(& self) -> &[P::Subpixel] {
        self.data.as_slice()
    }

    /// The raw image data as a slice.
    pub fn as_mut_slice(&mut self) -> &mut [P::Subpixel] {
        self.data.as_mut_slice()
    }

    /// Returns an iterator over the pixels of this image.
    pub fn pixels<'a>(&'a self) -> Pixels<'a, P> {
        Pixels {
            chunks: self.data.as_slice().chunks(
                <P as Pixel>::channel_count() as usize
            )
        }
    }

    /// Returns an iterator over the mutable pixels of this image.
    /// The iterator yields the coordinates of each pixel
    /// along with a mutable reference to them.
    pub fn pixels_mut(&mut self) -> PixelsMut<P> {
        PixelsMut {
            chunks: self.data.as_mut_slice().chunks_mut(
                <P as Pixel>::channel_count() as usize
            )
        }
    }

    /// Enumerates over the pixels of the image.
    /// The iterator yields the coordinates of each pixel
    /// along with a reference to them.
    pub fn enumerate_pixels<'a>(&'a self) -> EnumeratePixels<'a, P> {
        EnumeratePixels {
            pixels: self.pixels(),
            x: 0,
            y: 0,
            width: self.width
        }
    }

    /// Enumerates over the pixels of the image.
    pub fn enumerate_pixels_mut<'a>(&'a mut self) -> EnumeratePixelsMut<'a, P> {
        let width = self.width;
        EnumeratePixelsMut {
            pixels: self.pixels_mut(),
            x: 0,
            y: 0,
            width: width
        }
    }

    /// Gets a reference to the pixel at location `(x, y)`
    ///
    /// # Panics
    ///
    /// Panics if `(x, y)` is out of the bounds `(width, height)`.
    pub fn get_pixel(&self, x: u32, y: u32) -> &P {
        let no_channels = <P as Pixel>::channel_count() as usize;
        let index  = no_channels * (y * self.width + x) as usize;
        <P as Pixel>::from_slice(
            &self.data.as_slice()[index .. index + no_channels]
        )
    }

    /// Gets a reference to the mutable pixel at location `(x, y)`
    ///
    /// # Panics
    ///
    /// Panics if `(x, y)` is out of the bounds `(width, height)`.
    pub fn get_pixel_mut(&mut self, x: u32, y: u32) -> &mut P {
        let no_channels = <P as Pixel>::channel_count() as usize;
        let index  = no_channels * (y * self.width + x) as usize;
        <P as Pixel>::from_slice_mut(
            &mut self.data.as_mut_slice()[index .. index + no_channels]
        )
    }

    /// Puts a pixel at location `(x, y)`
    ///
    /// # Panics
    ///
    /// Panics if `(x, y)` is out of the bounds (width, height)`.
    pub fn put_pixel(&mut self, x: u32, y: u32, pixel: P) {
        *self.get_pixel_mut(x, y) = pixel
    }
}

impl<P: Pixel<Subpixel=u8> + 'static, Container: ArrayLike<u8>>
    ImageBuffer<P, Container> {
    /// Saves the buffer to a file at the path specified.
    ///
    /// The image format is derived from the file extension.
    /// Currently only jpeg and png files are supported.
    pub fn save(&self, path: &Path) -> IoResult<()> {
        // This is valid as the subpixel is u8.
        save_buffer(path,
                    self.as_slice(),
                    self.width(),
                    self.height(),
                    <P as Pixel>::color_type())
    }
}

impl<P: Pixel, Container: ArrayLike<P::Subpixel> + Clone> Clone for ImageBuffer<P, Container> {

    fn clone(&self) -> ImageBuffer<P, Container> {
        ImageBuffer {
            data: self.data.clone(),
            width: self.width,
            height: self.height,
            type_marker: self.type_marker
        }
    }
}

impl<P: Pixel + 'static, Container: ArrayLike<P::Subpixel>> GenericImage
    for ImageBuffer<P, Container> where P::Subpixel: 'static {

    type Pixel = P;

    fn dimensions(&self) -> (u32, u32) {
        self.dimensions()
    }

    fn bounds(&self) -> (u32, u32, u32, u32) {
        (0, 0, self.width, self.height)
    }

    fn get_pixel(&self, x: u32, y: u32) -> P {
        *self.get_pixel(x, y)
    }

    fn get_pixel_mut(&mut self, x: u32, y: u32) -> &mut P {
        self.get_pixel_mut(x, y)
    }

    fn put_pixel(&mut self, x: u32, y: u32, pixel: P) {
        *self.get_pixel_mut(x, y) = pixel
    }

    /// Put a pixel at location (x, y), taking into account alpha channels
    #[deprecated = "This method will be removed. Blend the pixel directly instead."]
    fn blend_pixel(&mut self, x: u32, y: u32, p: P) {
        self.get_pixel_mut(x, y).blend(&p)
    }
}

impl<P: Pixel + 'static, Container: ArrayLike<P::Subpixel>> Index<(u32, u32)>
    for ImageBuffer<P, Container> where P::Subpixel: 'static {

    type Output = P;

    fn index(&self, &(x, y): &(u32, u32)) -> &P {
        self.get_pixel(x, y)
    }
}

// concrete implementation for `Vec`-baked buffers
// TODO: I think that rustc does not "see" this impl any more: the impl with
// Container meets the same requirements. At least, I got compile errors that
// there is no such function as `into_vec`, whereas `into_raw` did work, and
// `into_vec` is redundant anyway, because `into_raw` will give you the vector,
// and it is more generic.
impl<P: Pixel + 'static> ImageBuffer<P, Vec<P::Subpixel>>
    where P::Subpixel: 'static {

    /// Creates a new image buffer based on a `Vec<P::Subpixel>`.
    pub fn new(width: u32, height: u32) -> ImageBuffer<P, Vec<P::Subpixel>> {
        ImageBuffer {
            data: repeat(Zero::zero()).take(
                    (width as u64
                     * height as u64
                     * (<P as Pixel>::channel_count() as u64)
                    ) as usize
                ).collect(),
            width: width,
            height: height,
            type_marker: TypeId::of::<P>()
        }
    }

    /// Constructs a new ImageBuffer by copying a pixel
    pub fn from_pixel(width: u32, height: u32, pixel: P)
                      -> ImageBuffer<P, Vec<P::Subpixel>> {
        let mut buf = ImageBuffer::new(width, height);
        for p in buf.pixels_mut() {
            *p = pixel
        }
        buf
    }

    /// Constructs a new ImageBuffer by repeated application of the supplied function.
    /// The arguments to the function are the pixel's x and y coordinates.
    pub fn from_fn(width: u32, height: u32, f: Box<Fn(u32, u32) -> P>)
                   -> ImageBuffer<P, Vec<P::Subpixel>> {
        let mut buf = ImageBuffer::new(width, height);
        for (x, y,  p) in buf.enumerate_pixels_mut() {
            *p = f(x, y)
        }
        buf
    }

    /// Creates an image buffer out of an existing buffer.
    /// Returns None if the buffer is not big enough.
    pub fn from_vec(width: u32, height: u32, buf: Vec<P::Subpixel>)
                    -> Option<ImageBuffer<P, Vec<P::Subpixel>>> {
        ImageBuffer::from_raw(width, height, buf)
    }

    /// Consumes the image buffer and returns the underlying data
    /// as an owned buffer
    pub fn into_vec(self) -> Vec<P::Subpixel> {
        self.into_raw()
    }
}

/// Provides color conversions for whole image buffers.
pub trait ConvertBuffer<T> {
    /// Converts `self` to a buffer of type T
    ///
    /// A generic impementation is provided to convert any image buffer to a image buffer
    /// based on a `Vec<T>`.
    fn convert(&self) -> T;
}

// concrete implementation Luma -> Rgba
impl GrayImage {
    /// Expands a color palette by re-using the existing buffer.
    /// Assumes 8 bit per pixel. Uses an optionally transparent index to
    /// adjust it's alpha value accordingly.
    pub fn expand_palette(self,
                          palette: &[(u8, u8, u8)],
                          transparent_idx: Option<u8>) -> RgbaImage {
        use std::mem;
        let (width, height) = self.dimensions();
        let mut data = self.into_raw();
        let entries = data.len();
        data.reserve_exact(entries.checked_mul(3).unwrap()); // 3 additional channels
        // set_len is save since type is u8 an the data never read
        unsafe { data.set_len(entries.checked_mul(4).unwrap()) }; // 4 channels in total
        // Aquire a second view into the buffer
        let indicies = unsafe {
            let view: &mut [u8] = mem::transmute_copy(&data.as_mut_slice());
            &view[.. entries]
        };
        let mut buffer = ImageBuffer::from_vec(width, height, data).unwrap();
        for (pixel, &idx) in buffer.pixels_mut().rev().zip(indicies.iter().rev()) {
            let (r, g, b) = palette[idx as usize];
            let alpha = if let Some(t_idx) = transparent_idx {
                if t_idx == idx {
                    0
                } else {
                    255
                }
            } else {
                255
            };
            *pixel = Rgba([r, g, b, alpha])
        }
        buffer
    }
}

// TODO: Equality constraints are not yet supported in where clauses, when they
// are, the T parameter should be removed in favor of ToType::Subpixel, which
// will then be FromType::Subpixel.
impl<'a, 'b, Container, FromType: Pixel + 'static, ToType: Pixel + 'static>
    ConvertBuffer<ImageBuffer<ToType, Vec<ToType::Subpixel>>>
    for ImageBuffer<FromType, Container>
    where Container: ArrayLike<FromType::Subpixel>,
          ToType: FromColor<FromType>,
          FromType::Subpixel: 'static,
          ToType::Subpixel: 'static {

    fn convert(&self) -> ImageBuffer<ToType, Vec<ToType::Subpixel>> {
        let mut buffer: ImageBuffer<ToType, Vec<ToType::Subpixel>>
            = ImageBuffer::new(self.width, self.height);
        for (mut to, from) in buffer.pixels_mut().zip(self.pixels()) {
            to.from_color(from)
        }
        buffer
    }
}

/// Sendable Rgb image buffer
pub type RgbImage = ImageBuffer<Rgb<u8>, Vec<u8>>;
/// Sendable Rgb + alpha channel image buffer
pub type RgbaImage = ImageBuffer<Rgba<u8>, Vec<u8>>;
/// Sendable grayscale image buffer
pub type GrayImage = ImageBuffer<Luma<u8>, Vec<u8>>;
/// Sendable grayscale + alpha channel image buffer
pub type GrayAlphaImage = ImageBuffer<LumaA<u8>, Vec<u8>>;

#[cfg(test)]
mod test {
    use std::rand;

    use super::{ImageBuffer, RgbImage, GrayImage, ConvertBuffer, Pixel};
    use color;
    use test;

    #[test]
    fn test_get_pixel() {
        let mut a: RgbImage = ImageBuffer::new(10, 10);
        a.as_mut_slice()[3*10] = 255;
        assert_eq!(a.get_pixel(0, 1)[0], 255)

    }

    #[test]
    fn test_mut_iter() {
        let mut a: RgbImage = ImageBuffer::new(10, 10);
        {
            let val = a.pixels_mut().next().unwrap();
            *val = color::Rgb([42, 0, 0]);
        }
        assert_eq!(a.data[0], 42)
    }

    #[bench]
    fn bench_conversion(b: &mut test::Bencher) {
        let mut a: RgbImage = ImageBuffer::new(1000, 1000);
        for mut p in a.pixels_mut() {
            let rgb = p.channels_mut();
            rgb[0] = 255;
            rgb[1] = rand::random();
            rgb[2] = rand::random();
        }
        assert!(a.data[0] != 0);
        b.iter(|| {
            let b: GrayImage = a.convert();
            assert!(0 != b.data[0]);
            assert!(a.data[0] != b.data[0]);
            test::black_box(b);
        });
        b.bytes = 1000*1000*3
    }
}
