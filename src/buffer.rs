use std::slice::{ Chunks, ChunksMut };
use std::any::Any;
use std::ops::{ Index, IndexMut };
use std::num::Int;
use std::intrinsics::TypeId;
use std::iter::repeat;

use traits::{ Zero, Primitive };
use color::{ Rgb, Rgba, Luma, LumaA, FromColor };
use image::GenericImage;

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
pub trait Pixel<T>: Copy + Clone {
    /// Returns the number of channels of this pixel type.
    fn channel_count<'a>(_: Option<&'a Self>) -> u8;

    /// Returns the components as a slice.
    fn channels(&self) -> &[T];

    /// Returns the components as a mutable slice
    fn channels_mut(&mut self) -> &mut [T];

    /// Returns a string that can help to interprete the meaning each channel
    /// See [gimp babl](http://gegl.org/babl/).
    fn color_model<'a>(_: Option<&'a Self>) -> &'static str;

    /// Returns the channels of this pixel as a 4 tuple. If the pixel
    /// has less than 4 channels the remainder is filled with the maximum value
    /// TODO deprecate
    fn channels4(&self) -> (T, T, T, T);

    /// Construct a pixel from the 4 channels a, b, c and d.
    /// If the pixel does not contain 4 channels the extra are ignored.
    /// TODO deprecate
    fn from_channels(a: T, b: T, c: T, d: T) -> Self;

    /// Returns a view into a slice.
    ///
    /// Note: The slice length is not checked on creation. Thus the caller has to ensure
    /// that the slice is long enough to precent panics if the pixel is used later on.
    fn from_slice<'a>(_: Option<&'a Self>, slice: &'a [T]) -> &'a Self;
    
    /// Returns mutable view into a mutable slice.
    ///
    /// Note: The slice length is not checked on creation. Thus the caller has to ensure
    /// that the slice is long enough to precent panics if the pixel is used later on.
    fn from_slice_mut<'a>(_: Option<&'a Self>, slice: &'a mut [T]) -> &'a mut Self;
    
    /// Convert this pixel to RGB
    fn to_rgb(&self) -> Rgb<T>;

    /// Convert this pixel to RGB with an alpha channel
    fn to_rgba(&self) -> Rgba<T>;

    /// Convert this pixel to luma
    fn to_luma(&self) -> Luma<T>;

    /// Convert this pixel to luma with an alpha channel
    fn to_luma_alpha(&self) -> LumaA<T>;

    /// Apply the function ```f``` to each channel of this pixel.
    fn map<F>(&self, f: F) -> Self where F: Fn(T) -> T;

    /// Apply the function ```f``` to each channel of this pixel.
    fn apply<F>(&mut self, f: F) where F: Fn(T) -> T;

    /// Apply the function f to each channel except the alpha channel.
    /// Apply the function g to the alpha channel.
    fn map_with_alpha<F, G>(&self, f: F, g: G) -> Self where F: Fn(T) -> T, G: Fn(T) -> T;

    /// Apply the function f to each channel except the alpha channel.
    /// Apply the function g to the alpha channel. Works in-place.
    fn apply_with_alpha<F, G>(&mut self, f: F, g: G) where F: Fn(T) -> T, G: Fn(T) -> T;

    /// Apply the function ```f``` to each channel of this pixel and
    /// ```other``` pairwise.
    fn map2<F>(&self, other: &Self, f: F) -> Self where F: Fn(T, T) -> T;

    /// Apply the function ```f``` to each channel of this pixel and
    /// ```other``` pairwise. Works in-place.
    fn apply2<F>(&mut self, other: &Self, f: F) where F: Fn(T, T) -> T;

    /// Invert this pixel
    fn invert(&mut self);

    /// Blend the color of a given pixel into ourself, taking into account alpha channels
    fn blend(&mut self, other: &Self);
    
}

/// Iterate over pixel refs. 
pub struct Pixels<'a, T: 'static, PixelType: ?Sized> {
    chunks: Chunks<'a, T>
}

impl<'a, T, PixelType> Iterator for Pixels<'a, T, PixelType> 
where T: Primitive, PixelType: Pixel<T> {
    type Item = &'a PixelType;
    #[inline(always)]
    fn next(&mut self) -> Option<&'a PixelType> {
        self.chunks.next().map(|v| 
            Pixel::from_slice(None::<&'a PixelType>, v)
        )
    }
}

impl<'a, T, PixelType> DoubleEndedIterator for Pixels<'a, T, PixelType> 
where T: Primitive, PixelType: Pixel<T> {
    #[inline(always)]
    fn next_back(&mut self) -> Option<&'a PixelType> {
        self.chunks.next_back().map(|v| 
            Pixel::from_slice(None::<&'a PixelType>, v)
        )
    }
}

/// Iterate over mutable pixel refs.
pub struct PixelsMut<'a, T: 'static, PixelType: ?Sized> {
    chunks: ChunksMut<'a, T>
}

impl<'a, T, PixelType> Iterator for PixelsMut<'a, T, PixelType>
where T: Primitive, PixelType: Pixel<T> {
    type Item = &'a mut PixelType;
    #[inline(always)]
    fn next(&mut self) -> Option<&'a mut PixelType> {
        self.chunks.next().map(|v| 
            Pixel::from_slice_mut(None::<&'a PixelType>, v)
        )
    }
}

impl<'a, T, PixelType> DoubleEndedIterator for PixelsMut<'a, T, PixelType>
where T: Primitive, PixelType: Pixel<T> {
    #[inline(always)]
    fn next_back(&mut self) -> Option<&'a mut PixelType> {
        self.chunks.next_back().map(|v| 
            Pixel::from_slice_mut(None::<&'a PixelType>, v)
        )
    }
}

/// Enumerate the pixels of an image. 
pub struct EnumeratePixels<'a, T: 'static, PixelType: ?Sized> {
    pixels: Pixels<'a, T, PixelType>,
    x:      u32,
    y:      u32,
    width:  u32
}

impl<'a, T, PixelType> Iterator
for EnumeratePixels<'a, T, PixelType>
where T: Primitive, PixelType: Pixel<T> {
    type Item = (u32, u32, &'a PixelType);
    #[inline(always)]
    fn next(&mut self) -> Option<(u32, u32, &'a PixelType)> {
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
pub struct EnumeratePixelsMut<'a, T: 'static, PixelType: ?Sized> {
    pixels: PixelsMut<'a, T, PixelType>,
    x:      u32,
    y:      u32,
    width:  u32
}

impl<'a, T, PixelType> Iterator
for EnumeratePixelsMut<'a, T, PixelType>
where T: Primitive, PixelType: Pixel<T> {
    type Item = (u32, u32, &'a mut PixelType);
    #[inline(always)]
    fn next(&mut self) -> Option<(u32, u32, &'a mut PixelType)> {
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
pub struct ImageBuffer<Container, T, PixelType> 
where T: Primitive, Container: ArrayLike<T>, PixelType: 'static {
    width: u32,
    height: u32,
    type_marker: TypeId,
    data: Container,
} 

// generic implementation, shared along all image buffers 
impl<Container, T, PixelType> ImageBuffer<Container, T, PixelType> 
where Container: ArrayLike<T>, T: Primitive + 'static, PixelType: Pixel<T> + 'static {

    /// Contructs a buffer from a generic container 
    /// (for example a `Vec` or a slice)
    /// Returns None if the container is not big enough
    pub fn from_raw(width: u32, height: u32, buf: Container) -> Option<ImageBuffer<Container, T, PixelType>> {
        if width as usize 
           * height as usize
           * Pixel::channel_count(None::<&PixelType>) as usize 
           <= buf.as_slice().len() {
            Some(ImageBuffer {
                data: buf,
                width: width,
                height: height,
                type_marker: TypeId::of::<PixelType>()
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
    pub fn as_slice(& self) -> &[T] {
        self.data.as_slice()
    }

    /// The raw image data as a slice.
    pub fn as_mut_slice(&mut self) -> &mut [T] {
        self.data.as_mut_slice()
    }

    /// Returns an iterator over the pixels of this image.
    pub fn pixels<'a>(&'a self) -> Pixels<'a, T, PixelType> {
        Pixels {
            chunks: self.data.as_slice().chunks(
                Pixel::channel_count(None::<&PixelType>) as usize
            )
        }
    }
 
    /// Returns an iterator over the mutable pixels of this image.
    /// The iterator yields the coordinates of each pixel
    /// along with a mutable reference to them.
    pub fn pixels_mut(&mut self) -> PixelsMut<T, PixelType> {
        PixelsMut {
            chunks: self.data.as_mut_slice().chunks_mut(
                Pixel::channel_count(None::<&PixelType>) as usize
            )
        }
    }

    /// Enumerates over the pixels of the image.
    /// The iterator yields the coordinates of each pixel
    /// along with a reference to them.
    pub fn enumerate_pixels<'a>(&'a self) -> EnumeratePixels<'a, T, PixelType> {
        EnumeratePixels {
            pixels: self.pixels(),
            x: 0,
            y: 0,
            width: self.width
        }
    }

    /// Enumerates over the pixels of the image.
    pub fn enumerate_pixels_mut<'a>(&'a mut self) -> EnumeratePixelsMut<'a, T, PixelType> {
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
    pub fn get_pixel(&self, x: u32, y: u32) -> &PixelType {
        let no_channels = Pixel::channel_count(None::<&PixelType>) as usize;
        let index  = no_channels * (y * self.width + x) as usize;
        Pixel::from_slice(
            None::<&PixelType>,
            self.data.as_slice().slice(
                index, index + no_channels
            )
        )
    }

    /// Gets a reference to the mutable pixel at location `(x, y)`
    ///
    /// # Panics
    ///
    /// Panics if `(x, y)` is out of the bounds `(width, height)`.
    pub fn get_pixel_mut(&mut self, x: u32, y: u32) -> &mut PixelType {
        let no_channels = Pixel::channel_count(None::<&PixelType>) as usize;
        let index  = no_channels * (y * self.width + x) as usize;
        Pixel::from_slice_mut(
            None::<&PixelType>,
            self.data.as_mut_slice().slice_mut(
                index, index + no_channels
            )
        )
    }

    /// Puts a pixel at location `(x, y)`
    ///
    /// # Panics
    ///
    /// Panics if `(x, y)` is out of the bounds (width, height)`.
    pub fn put_pixel(&mut self, x: u32, y: u32, pixel: PixelType) {
        *self.get_pixel_mut(x, y) = pixel
    }

    /// Casts the buffer into a dynamically typed image buffer
    #[experimental]
    pub fn into_dynamic(self) -> ImageBuffer<Container, T, &'static Any> {
        ImageBuffer {
            data: self.data,
            width: self.width,
            height: self.height,
            type_marker: self.type_marker

        }
    }
}


impl<Container, T, PixelType> Clone for ImageBuffer<Container, T, PixelType>
where Container: ArrayLike<T> + Clone, 
      T: Primitive + 'static, 
      PixelType: Pixel<T> + 'static {
    fn clone(&self) -> ImageBuffer<Container, T, PixelType> {
        ImageBuffer {
            data: self.data.clone(),
            width: self.width,
            height: self.height,
            type_marker: self.type_marker
        }
    }
}

impl<Container, T, PixelType> GenericImage<PixelType> 
for ImageBuffer<Container, T, PixelType>
where Container: ArrayLike<T>, T: Primitive + 'static, PixelType: Pixel<T> + 'static {

    fn dimensions(&self) -> (u32, u32) {
        self.dimensions()
    }

    fn bounds(&self) -> (u32, u32, u32, u32) {
        (0, 0, self.width, self.height)
    }

    fn get_pixel(&self, x: u32, y: u32) -> PixelType {
        *self.get_pixel(x, y)
    }

    fn get_pixel_mut(&mut self, x: u32, y: u32) -> &mut PixelType {
        self.get_pixel_mut(x, y)
    }

    fn put_pixel(&mut self, x: u32, y: u32, pixel: PixelType) {
        *self.get_pixel_mut(x, y) = pixel
    }

    /// Put a pixel at location (x, y), taking into account alpha channels
    #[deprecated = "This method will be removed. Blend the pixel directly instead."]
    fn blend_pixel(&mut self, x: u32, y: u32, p: PixelType) {
        self.get_pixel_mut(x, y).blend(&p)
    }
}

impl<Container, T, PixelType> Index<(u32, u32)>
for ImageBuffer<Container, T, PixelType>
where Container: ArrayLike<T>, T: Primitive + 'static, PixelType: Pixel<T> + 'static {
    type Output = PixelType;
    fn index(&self, &(x, y): &(u32, u32)) -> &PixelType {
        self.get_pixel(x, y)
    }
}
 
// concrete implementation for `Vec`-baked buffers
impl<T, PixelType> ImageBuffer<Vec<T>, T, PixelType>
where T: Primitive + 'static, PixelType: Pixel<T> + 'static {
    /// Creates a new image buffer based on a `Vec<T>`.
    pub fn new(width: u32, height: u32) -> ImageBuffer<Vec<T>, T, PixelType> {
        ImageBuffer {
            data: repeat(Zero::zero()).take(
                    (width as u64
                     * height as u64 
                     * (Pixel::channel_count(None::<&PixelType>) as u64)
                    ) as usize
                ).collect(),
            width: width,
            height: height,
            type_marker: TypeId::of::<PixelType>()
        }
    }

    /// Constructs a new ImageBuffer by copying a pixel
    pub fn from_pixel(width: u32, height: u32, pixel: PixelType) -> ImageBuffer<Vec<T>, T, PixelType> {
        let mut buf = ImageBuffer::new(width, height);
        for p in buf.pixels_mut() {
            *p = pixel
        }
        buf
    }

    /// Constructs a new ImageBuffer by repeated application of the supplied function.
    /// The arguments to the function are the pixel's x and y coordinates.
    pub fn from_fn(width: u32, height: u32, f: Box<Fn(u32, u32) -> PixelType>) -> ImageBuffer<Vec<T>, T, PixelType> {
        let mut buf = ImageBuffer::new(width, height);
        for (x, y,  p) in buf.enumerate_pixels_mut() {
            *p = f(x, y)
        }
        buf
    }

    /// Creates an image buffer out of an existing buffer. 
    /// Returns None if the buffer is not big enough.
    pub fn from_vec(width: u32, height: u32, buf: Vec<T>) -> Option<ImageBuffer<Vec<T>, T, PixelType>> {
        ImageBuffer::from_raw(width, height, buf)
    }

    /// Consumes the image buffer and returns the underlying data
    /// as an owned buffer
    pub fn into_vec(self) -> Vec<T> {
        self.into_raw()
    }
}

/// Provides color conversions for whole image buffers.
pub trait ConvertBuffer<T: ?Sized> {
    /// Converts `self` to a buffer of type T
    ///
    /// A generic impementation is provided to convert any image buffer to a image buffer
    /// based on a `Vec<T>`.
    fn convert(&self) -> T;
}


// concrete implementation Luma -> Rgba
impl GreyImage {
    /// Expands a color palette by re-using the existing buffer.
    /// Assumes 8 bit per pixel. Uses an optionally transparent index to 
    /// adjust it's alpha value accordingly.
    pub fn expand_palette(self, 
                          palette: &[(u8, u8, u8)], 
                          transparent_idx: Option<u8>) -> RgbaImage {
        use std::mem;
        let (width, height) = self.dimensions();
        let mut data = self.into_vec();
        let entries = data.len();
        data.reserve_exact(entries.checked_mul(3).unwrap()); // 3 additional channels
        // set_len is save since type is u8 an the data never read
        unsafe { data.set_len(entries.checked_mul(4).unwrap()) }; // 4 channels in total
        // Aquire a second view into the buffer
        let indicies = unsafe {
            let view: &mut [u8] = mem::transmute_copy(&data.as_mut_slice());
            view.slice_to(entries)
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

impl<'a, 'b, Container, T, FromType, ToType> 
    ConvertBuffer<ImageBuffer<Vec<T>,T, ToType>> 
    for ImageBuffer<Container, T, FromType> 
    where T: Primitive + 'static, 
          Container: ArrayLike<T>, 
          FromType: Pixel<T> + 'static, 
          ToType: Pixel<T> + 'static + FromColor<FromType> {
    fn convert(&self) -> ImageBuffer<Vec<T>, T, ToType> {
        let mut buffer = ImageBuffer::<T, ToType>::new(self.width, self.height);
        for (mut to, from) in buffer.pixels_mut().zip(self.pixels()) {
            to.from_color(from)
        }
        buffer
    }
}

/// Sendable Rgb image buffer 
pub type RgbImage = ImageBuffer<Vec<u8>, u8, Rgb<u8>>;
/// Sendable Rgb + alpha channel image buffer 
pub type RgbaImage = ImageBuffer<Vec<u8>, u8, Rgba<u8>>;
/// Sendable grayscale image buffer 
pub type GreyImage = ImageBuffer<Vec<u8>, u8, Luma<u8>>;
/// Sendable grayscale + alpha channel image buffer 
pub type GreyAlphaImage = ImageBuffer<Vec<u8>, u8, LumaA<u8>>;

#[cfg(test)]
mod test {
    extern crate test;
    use std::rand;

    use super::{ImageBuffer, RgbImage, GreyImage, ConvertBuffer, Pixel};
    use color;

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
            let b: GreyImage = a.convert();
            assert!(0 != b.data[0]);
            assert!(a.data[0] != b.data[0]);
            test::black_box(b);
        });
        b.bytes = 1000*1000*3
    }
}
