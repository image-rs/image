use std::slice::{Chunks, MutChunks};
use std::any::Any;
use std::intrinsics::TypeId;

use traits::{Zero, Primitive};
use colors::{Rgb, Rgba, Luma, LumaA};
use image::GenericImage;

/// Mutable equivalent to AsSlice.
/// Should be replaced by a stdlib impl as soon it exists
pub trait AsMutSlice<T> for Sized? {
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
pub trait ArrayLike<T>: Index<uint, T> + IndexMut<uint, T> + AsSlice<T> + AsMutSlice<T> {}
impl<A: Index<uint, T> + IndexMut<uint, T> + AsSlice<T> + AsMutSlice<T>, T> ArrayLike<T> for A { }

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
}

/// Provides color conversions for the different pixel types.
pub trait FromColor<Sized? Other> for Sized? {
    /// Changes `self` to represent `Other` in the color space of `Self`
    fn from_color(&mut self, &Other);
}

/// Iterate over pixel refs. 
pub struct Pixels<'a, T: 'static, Sized? PixelType> {
    chunks: Chunks<'a, T>
}

impl<'a, T, PixelType> Iterator<&'a PixelType> for Pixels<'a, T, PixelType> 
where T: Primitive, PixelType: Pixel<T> {
    #[inline(always)]
    fn next(&mut self) -> Option<&'a PixelType> {
        self.chunks.next().map(|v| 
            Pixel::from_slice(None::<&'a PixelType>, v)
        )
    }
}

/// Iterate over mutable pixel refs.
pub struct MutPixels<'a, T: 'static, Sized? PixelType> {
    chunks: MutChunks<'a, T>
}

impl<'a, T, PixelType> Iterator<&'a mut PixelType> for MutPixels<'a, T, PixelType>
where T: Primitive, PixelType: Pixel<T> {
    #[inline(always)]
    fn next(&mut self) -> Option<&'a mut PixelType> {
        self.chunks.next().map(|v| 
            Pixel::from_slice_mut(None::<&'a PixelType>, v)
        )
    }
}

/// Enumerate the pixels of an image. 
pub struct EnumeratePixels<'a, T: 'static, Sized? PixelType> {
    pixels: Pixels<'a, T, PixelType>,
    x:      u32,
    y:      u32,
    width:  u32
}

impl<'a, T, PixelType> Iterator<(u32, u32, &'a PixelType)>
for EnumeratePixels<'a, T, PixelType>
where T: Primitive, PixelType: Pixel<T> {
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
pub struct EnumerateMutPixels<'a, T: 'static, Sized? PixelType> {
    pixels: MutPixels<'a, T, PixelType>,
    x:      u32,
    y:      u32,
    width:  u32
}

impl<'a, T, PixelType> Iterator<(u32, u32, &'a mut PixelType)>
for EnumerateMutPixels<'a, T, PixelType>
where T: Primitive, PixelType: Pixel<T> {
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
where Container: ArrayLike<T>, T: Primitive + 'static, PixelType: Pixel<T> {

    /// Contructs a buffer from a generic container 
    /// (for example a `Vec` or a slice)
    /// Returns None if the container is not big enough
    pub fn from_raw(width: u32, height: u32, buf: Container) -> Option<ImageBuffer<Container, T, PixelType>> {
        if width as uint 
           * height as uint
           * Pixel::channel_count(None::<&PixelType>) as uint 
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

    /// Consumes the image buffer and returns the underlying data
    /// as an owned buffer
    pub fn into_vec(self) -> Vec<T> {
        self.data.as_slice().to_vec()
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
                Pixel::channel_count(None::<&PixelType>) as uint
            )
        }
    }
 
    /// Returns an iterator over the mutable pixels of this image.
    /// The iterator yields the coordinates of each pixel
    /// along with a mutable reference to them.
    pub fn pixels_mut(&mut self) -> MutPixels<T, PixelType> {
        MutPixels {
            chunks: self.data.as_mut_slice().chunks_mut(
                Pixel::channel_count(None::<&PixelType>) as uint
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
    pub fn enumerate_pixels_mut<'a>(&'a mut self) -> EnumerateMutPixels<'a, T, PixelType> {
        let width = self.width;
        EnumerateMutPixels {
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
    /// Panics if `(x, y)` is out of bounds.
    pub fn get_pixel(&self, x: u32, y: u32) -> &PixelType {
        let index  = y * self.width + x;
        Pixel::from_slice(
            None::<&PixelType>,
            self.data.as_slice().slice(
                index as uint, Pixel::channel_count(None::<&PixelType>) as uint
            )
        )
    }

    /// Gets a reference to the mutable pixel at location `(x, y)`
    ///
    /// # Panics
    ///
    /// Panics if `(x, y)` is out of bounds.
    pub fn get_pixel_mut(&mut self, x: u32, y: u32) -> &mut PixelType {
        let index  = y * self.width + x;
        Pixel::from_slice_mut(
            None::<&PixelType>,
            self.data.as_mut_slice().slice_mut(
                index as uint, Pixel::channel_count(None::<&PixelType>) as uint
            )
        )
    }

    /// Puts a pixel at location `(x, y)`
    ///
    /// # Panics
    ///
    /// Panics if `(x, y)` is out of bounds.
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

impl<Container, T, PixelType> GenericImage<PixelType> for ImageBuffer<Container, T, PixelType>
where Container: ArrayLike<T>, T: Primitive + 'static, PixelType: Pixel<T> {

    fn dimensions(&self) -> (u32, u32) {
        self.dimensions()
    }

    fn bounds(&self) -> (u32, u32, u32, u32) {
        (0, 0, self.width, self.height)
    }

    fn get_pixel(&self, x: u32, y: u32) -> PixelType {
        (*self.get_pixel(x, y)).clone()
    }

    fn put_pixel(&mut self, x: u32, y: u32, pixel: PixelType) {
        *self.get_pixel_mut(x, y) = pixel
    }

    ///Put a pixel at location (x, y), taking into account alpha channels
    #[deprecated = "This method will be removed. Blend the pixel directly instead."]
    fn blend_pixel(&mut self, _: u32, _: u32, _: PixelType) {
        unimplemented!()
    }
}

impl<Container, T, PixelType> Index<(u32, u32), PixelType> 
for ImageBuffer<Container, T, PixelType>
where Container: ArrayLike<T>, T: Primitive + 'static, PixelType: Pixel<T> {
    fn index(&self, &(x, y): &(u32, u32)) -> &PixelType {
        self.get_pixel(x, y)
    }
}
 
// concrete implementation for `Vec`-baked buffers
impl<T, PixelType> ImageBuffer<Vec<T>, T, PixelType>
where T: Primitive + 'static, PixelType: Pixel<T> {
    /// Creates a new image buffer based on a `Vec<T>`.
    pub fn new(width: u32, height: u32) -> ImageBuffer<Vec<T>, T, PixelType> {
        ImageBuffer {
            data: Vec::from_elem(
                (width as u64
                 * height as u64 
                 * (Pixel::channel_count(None::<&PixelType>) as u64)
                ) as uint,
                Zero::zero()
            ),
            width: width,
            height: height,
            type_marker: TypeId::of::<PixelType>()
        }
    }

    /// Creates an image buffer out of an existing buffer. 
    /// Returns None if the buffer is not big enough.
    pub fn from_vec(width: u32, height: u32, buf: Vec<T>) -> Option<ImageBuffer<Vec<T>, T, PixelType>> {
        ImageBuffer::from_raw(width, height, buf)
    }

    ///// Consumes the image buffer and returns the underlying data
    ///// as an owned buffer
    //pub fn into_vec(self) -> Vec<T> {
    //    self.data
    //}
}

/// Provides color conversions for whole image buffers.
pub trait ConvertBuffer<Sized? T> for Sized? {
    /// Converts `self` to a buffer of type T
    ///
    /// A generic impementation is provided to convert any image buffer to a image buffer
    /// based on a `Vec<T>`.
    fn convert(&self) -> T;
}

impl<'a, 'b, Container, T, FromType, ToType> 
    ConvertBuffer<ImageBuffer<Vec<T>,T, ToType>> 
    for ImageBuffer<Container, T, FromType> 
    where T: Primitive+'static, 
          Container: ArrayLike<T>, 
          FromType: Pixel<T>, 
          ToType: Pixel<T>+FromColor<FromType> {
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
    use colors;

    #[test]
    fn test_mut_iter() {
        let mut a: RgbImage = ImageBuffer::new(10, 10);
        {
            let val = a.pixels_mut().next().unwrap();
            *val = colors::Rgb([42, 0, 0]);
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
            assert!(0 != b.data[0])
            assert!(a.data[0] != b.data[0])
            test::black_box(b);
        });
        b.bytes = 1000*1000*3
    }
}