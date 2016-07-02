use std::slice::{ Chunks, ChunksMut };
use std::ops::{ Deref, DerefMut, Index, IndexMut };
use std::marker::PhantomData;
use std::path::Path;
use std::io;
use num_traits::Zero;

use traits::Primitive;
use color::{ Rgb, Rgba, Luma, LumaA, FromColor, ColorType };
use image::GenericImage;
use dynimage::save_buffer;
use utils::expand_packed;

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
    ///
    /// TODO deprecate
    fn channels4(&self) -> (Self::Subpixel, Self::Subpixel, Self::Subpixel, Self::Subpixel);

    /// Construct a pixel from the 4 channels a, b, c and d.
    /// If the pixel does not contain 4 channels the extra are ignored.
    ///
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

    /// Apply the function ```f``` to each channel except the alpha channel.
    /// Apply the function ```g``` to the alpha channel.
    fn map_with_alpha<F, G>(&self, f: F, g: G) -> Self
        where F: Fn(Self::Subpixel) -> Self::Subpixel, G: Fn(Self::Subpixel) -> Self::Subpixel;

    /// Apply the function ```f``` to each channel except the alpha channel.
    /// Apply the function ```g``` to the alpha channel. Works in-place.
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
pub struct EnumeratePixels<'a, P: Pixel + 'a> where <P as Pixel>::Subpixel: 'a {
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
pub struct EnumeratePixelsMut<'a, P: Pixel + 'a> where <P as Pixel>::Subpixel: 'a {
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
    _phantom: PhantomData<P>,
    data: Container,
}

// generic implementation, shared along all image buffers
impl<P, Container> ImageBuffer<P, Container>
where P: Pixel + 'static,
      P::Subpixel: 'static,
      Container: Deref<Target=[P::Subpixel]> {

    /// Contructs a buffer from a generic container
    /// (for example a `Vec` or a slice)
    ///
    /// Returns None if the container is not big enough
    pub fn from_raw(width: u32, height: u32, buf: Container)
                    -> Option<ImageBuffer<P, Container>> {
        if width as usize
           * height as usize
           * <P as Pixel>::channel_count() as usize
           <= buf.len() {
            Some(ImageBuffer {
                data: buf,
                width: width,
                height: height,
                _phantom: PhantomData,
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

    /// Returns an iterator over the pixels of this image.
    pub fn pixels<'a>(&'a self) -> Pixels<'a, P> {
        Pixels {
            chunks: self.data.chunks(
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

    /// Gets a reference to the pixel at location `(x, y)`
    ///
    /// # Panics
    ///
    /// Panics if `(x, y)` is out of the bounds `(width, height)`.
    pub fn get_pixel(&self, x: u32, y: u32) -> &P {
        let no_channels = <P as Pixel>::channel_count() as usize;
        let index  = no_channels * (y * self.width + x) as usize;
        <P as Pixel>::from_slice(
            &self.data[index .. index + no_channels]
        )
    }    
}

impl<P, Container> ImageBuffer<P, Container>
where P: Pixel + 'static,
      P::Subpixel: 'static,
      Container: Deref<Target=[P::Subpixel]> + DerefMut {

    /// Returns an iterator over the mutable pixels of this image.
    /// The iterator yields the coordinates of each pixel
    /// along with a mutable reference to them.
    pub fn pixels_mut(&mut self) -> PixelsMut<P> {
        PixelsMut {
            chunks: self.data.chunks_mut(
                <P as Pixel>::channel_count() as usize
            )
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

    /// Gets a reference to the mutable pixel at location `(x, y)`
    ///
    /// # Panics
    ///
    /// Panics if `(x, y)` is out of the bounds `(width, height)`.
    pub fn get_pixel_mut(&mut self, x: u32, y: u32) -> &mut P {
        let no_channels = <P as Pixel>::channel_count() as usize;
        let index  = no_channels * (y * self.width + x) as usize;
        <P as Pixel>::from_slice_mut(
            &mut self.data[index .. index + no_channels]
        )
    }

    /// Puts a pixel at location `(x, y)`
    ///
    /// # Panics
    ///
    /// Panics if `(x, y)` is out of the bounds `(width, height)`.
    pub fn put_pixel(&mut self, x: u32, y: u32, pixel: P) {
        *self.get_pixel_mut(x, y) = pixel
    }
}

impl<P, Container> ImageBuffer<P, Container>
where P: Pixel<Subpixel=u8> + 'static,
      Container: Deref<Target=[u8]> {
    /// Saves the buffer to a file at the path specified.
    ///
    /// The image format is derived from the file extension.
    /// Currently only jpeg and png files are supported.
    pub fn save<Q>(&self, path: Q) -> io::Result<()> where Q: AsRef<Path> {
        // This is valid as the subpixel is u8.
        save_buffer(path,
                    self,
                    self.width(),
                    self.height(),
                    <P as Pixel>::color_type())
    }
}

impl<P, Container> Deref for ImageBuffer<P, Container>
where P: Pixel + 'static,
      P::Subpixel: 'static,
      Container: Deref<Target=[P::Subpixel]> {
    type Target = [P::Subpixel];

    fn deref<'a>(&'a self) -> &'a <Self as Deref>::Target {
        &*self.data
    }
}

impl<P, Container> DerefMut for ImageBuffer<P, Container>
where P: Pixel + 'static,
      P::Subpixel: 'static,
      Container: Deref<Target=[P::Subpixel]> + DerefMut {
    fn deref_mut<'a>(&'a mut self) -> &'a mut <Self as Deref>::Target {
        &mut *self.data
    }
}

impl<P, Container> Index<(u32, u32)> for ImageBuffer<P, Container>
where P: Pixel + 'static,
      P::Subpixel: 'static,
      Container: Deref<Target=[P::Subpixel]> {
    type Output = P;

    fn index(&self, (x, y): (u32, u32)) -> &P {
        self.get_pixel(x, y)
    }
}

impl<P, Container> IndexMut<(u32, u32)> for ImageBuffer<P, Container>
where P: Pixel + 'static,
      P::Subpixel: 'static,
      Container: Deref<Target=[P::Subpixel]> + DerefMut {

    fn index_mut(&mut self, (x, y): (u32, u32)) -> &mut P {
        self.get_pixel_mut(x, y)
    }
}

impl<P, Container> Clone for ImageBuffer<P, Container>
where P: Pixel,
      Container: Deref<Target=[P::Subpixel]> + Clone {

    fn clone(&self) -> ImageBuffer<P, Container> {
        ImageBuffer {
            data: self.data.clone(),
            width: self.width,
            height: self.height,
            _phantom: PhantomData,
        }
    }
}

impl<P, Container> GenericImage for ImageBuffer<P, Container>
where P: Pixel + 'static,
      Container: Deref<Target=[P::Subpixel]> + DerefMut,
      P::Subpixel: 'static {

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
    
    /// Returns the pixel located at (x, y), ignoring bounds checking.
    #[inline(always)]
    unsafe fn unsafe_get_pixel(&self, x: u32, y: u32) -> P {
        let no_channels = <P as Pixel>::channel_count() as usize;
        let index  = no_channels as isize * (y * self.width + x) as isize;
        *<P as Pixel>::from_slice(
            ::std::slice::from_raw_parts(self.data.as_ptr().offset(index), 
                                         no_channels)
        )
    }

    fn put_pixel(&mut self, x: u32, y: u32, pixel: P) {
        *self.get_pixel_mut(x, y) = pixel
    }
    
    /// Puts a pixel at location (x, y), ignoring bounds checking.
    #[inline(always)]
    unsafe fn unsafe_put_pixel(&mut self, x: u32, y: u32, pixel: P) {
        let no_channels = <P as Pixel>::channel_count() as usize;
        let index  = no_channels as isize * (y * self.width + x) as isize;
        let p = <P as Pixel>::from_slice_mut(
            ::std::slice::from_raw_parts_mut(self.data.as_mut_ptr().offset(index), 
                                             no_channels)
        );
        *p = pixel
    }

    /// Put a pixel at location (x, y), taking into account alpha channels
    ///
    /// DEPRECATED: This method will be removed. Blend the pixel directly instead.
    fn blend_pixel(&mut self, x: u32, y: u32, p: P) {
        self.get_pixel_mut(x, y).blend(&p)
    }
}

// concrete implementation for `Vec`-backed buffers
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
            data: vec![Zero::zero(); 
                      (width as u64
                      * height as u64
                      * (<P as Pixel>::channel_count() as u64)
                      ) as usize],
            width: width,
            height: height,
            _phantom: PhantomData,
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
    pub fn from_fn<F>(width: u32, height: u32, f: F)
                      -> ImageBuffer<P, Vec<P::Subpixel>>
                      where F: Fn(u32, u32) -> P {
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
        let (width, height) = self.dimensions();
        let mut data = self.into_raw();
        let entries = data.len();
        data.reserve_exact(entries.checked_mul(3).unwrap()); // 3 additional channels
        // set_len is save since type is u8 an the data never read
        unsafe { data.set_len(entries.checked_mul(4).unwrap()) }; // 4 channels in total
        let mut buffer = ImageBuffer::from_vec(width, height, data).unwrap();
        expand_packed(&mut buffer, 4, 8, |idx, pixel| {
            let (r, g, b) = palette[idx as usize];
            let a = if let Some(t_idx) = transparent_idx {
                if t_idx == idx {
                    0
                } else {
                    255
                }
            } else {
                255
            };
            pixel[0] = r;
            pixel[1] = g;
            pixel[2] = b;
            pixel[3] = a;
        });
        buffer
    }
}

// TODO: Equality constraints are not yet supported in where clauses, when they
// are, the T parameter should be removed in favor of ToType::Subpixel, which
// will then be FromType::Subpixel.
impl<'a, 'b, Container, FromType: Pixel + 'static, ToType: Pixel + 'static>
    ConvertBuffer<ImageBuffer<ToType, Vec<ToType::Subpixel>>>
    for ImageBuffer<FromType, Container>
    where Container: Deref<Target=[FromType::Subpixel]>,
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

    use super::{ImageBuffer, RgbImage, GrayImage, ConvertBuffer, Pixel};
    use color;
    use test;

    #[test]
    /// Tests if image buffers from slices work
    fn slice_buffer() {
        let data = [0; 9];
        let buf: ImageBuffer<color::Luma<u8>, _> = ImageBuffer::from_raw(3, 3, &data[..]).unwrap();
        assert_eq!(&*buf, &data[..])
    }

    #[test]
    fn test_get_pixel() {
        let mut a: RgbImage = ImageBuffer::new(10, 10);
        {
            let b = a.get_mut(3 * 10).unwrap();
            *b = 255;
        }
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
            rgb[1] = 23;
            rgb[2] = 42;
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
