use std::error::FromError;
use std::mem;
use std::io;
use std::slice;
use std::default::Default;

use color;
use color:: {
    Pixel,
    ColorType,
    Primitive,
};

/// An enumeration of Image Errors
#[deriving(Show, PartialEq, Eq)]
pub enum ImageError {
    ///The Image is not formatted properly
    FormatError(String),

    ///The Image's dimensions are either too small or too large
    DimensionError,

    ///The Decoder does not support this image format
    UnsupportedError(String),

    ///The Decoder does not support this color type
    UnsupportedColor(ColorType),

    ///Not enough data was provided to the Decoder
    ///to decode the image
    NotEnoughData,

    ///An I/O Error occurred while decoding the image
    IoError(io::IoError),

    ///The end of the image has been reached
    ImageEnd
}

impl FromError<io::IoError> for ImageError {
    fn from_error(err: io::IoError) -> ImageError {
        ImageError::IoError(err)
    }
}

pub type ImageResult<T> = Result<T, ImageError>;

/// An enumeration of supported image formats.
/// Not all formats support both encoding and decoding.
#[deriving(PartialEq, Eq, Show)]
pub enum ImageFormat {
    /// An Image in PNG Format
    PNG,

    /// An Image in JPEG Format
    JPEG,

    /// An Image in GIF Format
    GIF,

    /// An Image in WEBP Format
    WEBP,

    /// An Image in PPM Format
    PPM,

    /// An Image in TIFF Format
    TIFF,

    /// An Image in TGA Format
    TGA
}

/// The trait that all decoders implement
pub trait ImageDecoder {
    ///Return a tuple containing the width and height of the image
    fn dimensions(&mut self) -> ImageResult<(u32, u32)>;

    ///Return the color type of the image e.g RGB(8) (8bit RGB)
    fn colortype(&mut self) -> ImageResult<ColorType>;

    ///Returns the length in bytes of one decoded row of the image
    fn row_len(&mut self) -> ImageResult<uint>;

    ///Read one row from the image into buf
    ///Returns the row index
    fn read_scanline(&mut self, buf: &mut [u8]) -> ImageResult<u32>;

    ///Decode the entire image and return it as a Vector
    fn read_image(&mut self) -> ImageResult<Vec<u8>>;

    ///Decode a specific region of the image, represented by the rectangle
    ///starting from ```x``` and ```y``` and having ```length``` and ```width```
    fn load_rect(&mut self, x: u32, y: u32, length: u32, width: u32) -> ImageResult<Vec<u8>> {
        let (w, h) = try!(self.dimensions());

        if length > h || width > w || x > w || y > h {
            return Err(ImageError::DimensionError)
        }

        let c = try!(self.colortype());

        let bpp = color::bits_per_pixel(c) / 8;

        let rowlen  = try!(self.row_len());

        let mut buf = Vec::from_elem(length as uint * width as uint * bpp, 0u8);
        let mut tmp = Vec::from_elem(rowlen, 0u8);

        loop {
            let row = try!(self.read_scanline(tmp.as_mut_slice()));

            if row - 1 == y {
                break
            }
        }

        for i in range(0, length as uint) {
            {
                let from = tmp.slice_from(x as uint * bpp)
                              .slice_to(width as uint * bpp);

                let to   = buf.slice_from_mut(i * width as uint * bpp)
                              .slice_to_mut(width as uint * bpp);

                slice::bytes::copy_memory(to, from);
            }

            let _ = try!(self.read_scanline(tmp.as_mut_slice()));
        }

        Ok(buf)
    }
}

/// Immutable pixel iterator
pub struct Pixels<'a, I:'a> {
    image:  &'a I,
    x:      u32,
    y:      u32,
    width:  u32,
    height: u32
}

impl<'a, T: Primitive, P: Pixel<T>, I: GenericImage<P>> Iterator<(u32, u32, P)> for Pixels<'a, I> {
    fn next(&mut self) -> Option<(u32, u32, P)> {
        if self.x >= self.width {
            self.x =  0;
            self.y += 1;
        }

        if self.y >= self.height {
            None
        } else {
            let pixel = self.image.get_pixel(self.x, self.y);
            let p = (self.x, self.y, pixel);

            self.x += 1;

            Some(p)
        }
    }
}

/// Mutable pixel iterator
pub struct MutPixels<'a, I:'a> {
    image:  &'a mut I,
    x:      u32,
    y:      u32,
    width:  u32,
    height: u32
}

impl<'a, T: Primitive, P: Pixel<T>, I: MutableRefImage<P>> Iterator<(u32, u32, &'a mut P)> for MutPixels<'a, I> {
    fn next(&mut self) -> Option<(u32, u32, &'a mut P)> {
        if self.x >= self.width {
            self.x =  0;
            self.y += 1;
        }

        if self.y >= self.height {
            None
        } else {
            let tmp = self.image.get_mut_pixel(self.x, self.y);

            //error: lifetime of `self` is too short to guarantee its contents
            //       can be safely reborrowed...
            let ptr = unsafe {
                mem::transmute(tmp)
            };

            let p = (self.x, self.y, ptr);

            self.x += 1;

            Some(p)
        }
    }
}

///A trait for manipulating images.
pub trait GenericImage<P> {
    ///The width and height of this image.
    fn dimensions(&self) -> (u32, u32);

    ///The bounding rectangle of this image.
    fn bounds(&self) -> (u32, u32, u32, u32);

    ///Return the pixel located at (x, y)
    fn get_pixel(&self, x: u32, y: u32) -> P;

    ///Put a pixel at location (x, y)
    fn put_pixel(&mut self, x: u32, y: u32, pixel: P);

    ///Return an Iterator over the pixels of this image.
    ///The iterator yields the coordinates of each pixel
    ///along with their value
    fn pixels(&self) -> Pixels<Self> {
        let (width, height) = self.dimensions();

        Pixels {
            image:  self,
            x:      0,
            y:      0,
            width:  width,
            height: height,
        }
    }
}

///A trait for images that allow providing mutable references to pixels.
pub trait MutableRefImage<P>: GenericImage<P> {
    ///Return a mutable reference to the pixel located at (x, y)
    fn get_mut_pixel(&mut self, x: u32, y: u32) -> &mut P;

    ///Return an Iterator over mutable pixels of this image.
    ///The iterator yields the coordinates of each pixel
    ///along with a mutable reference to them.
    fn pixels_mut(&mut self) -> MutPixels<Self> {
        let (width, height) = self.dimensions();

        MutPixels {
            image:  self,
            x:      0,
            y:      0,
            width:  width,
            height: height,
        }
    }
}

///An Image whose pixels are contained within a vector
#[deriving(Clone)]
pub struct ImageBuf<P> {
    pixels:  Vec<P>,
    width:   u32,
    height:  u32,
}

impl<T: Primitive, P: Pixel<T>> ImageBuf<P> {
    ///Construct a new ImageBuf with the specified width and height.
    pub fn new(width: u32, height: u32) -> ImageBuf<P> {
        let pixel: P = Default::default();
        let pixels = Vec::from_elem((width * height) as uint, pixel.clone());

        ImageBuf {
            pixels:  pixels,
            width:   width,
            height:  height,
        }
    }

    ///Construct a new ImageBuf by repeated application of the supplied function.
    ///The arguments to the function are the pixel's x and y coordinates.
    pub fn from_fn(width: u32, height: u32, f: | u32, u32 | -> P) -> ImageBuf<P> {
        let mut pixels: Vec<P> = Vec::with_capacity((width * height) as uint);

        for y in range(0, height) {
            for x in range(0, width) {
                pixels.insert((y * width + x) as uint, f(x, y));
            }
        }

        ImageBuf::from_pixels(pixels, width, height)
    }

    ///Construct a new ImageBuf from a vector of pixels.
    pub fn from_pixels(pixels: Vec<P>, width: u32, height: u32) -> ImageBuf<P> {
        ImageBuf {
            pixels: pixels,
            width:  width,
            height: height,
        }
    }

    ///Construct a new ImageBuf from a pixel.
    pub fn from_pixel(width: u32, height: u32, pixel: P) -> ImageBuf<P> {
        let buf = Vec::from_elem(width as uint * height as uint, pixel.clone());

        ImageBuf::from_pixels(buf, width, height)
    }

    ///Return an immutable reference to this image's pixel buffer
    pub fn pixelbuf(&self) -> & [P] {
        self.pixels.as_slice()
    }

    ///Return a mutable reference to this image's pixel buffer
    pub fn pixelbuf_mut(&mut self) -> &mut [P] {
        self.pixels.as_mut_slice()
    }

    ///Destroy this ImageBuf, returning the internal vector
    pub fn into_vec(self) -> Vec<P> {
        self.pixels
    }

    ///Return an immutable reference to this image's raw data buffer
    pub fn rawbuf(&self) -> &[u8] {
        use std::mem::{ size_of, transmute };
        use std::raw::Slice;
        // Compute size of slice in bytes.
        let len = size_of::<P>() * self.pixels.len();
        let slice = Slice { data: self.pixels.as_ptr() as *const u8, len: len };
        unsafe { transmute(slice) }
    }

    ///Return a mutable reference to this image's raw data buffer
    pub fn rawbuf_mut(&mut self) -> &mut [u8] {
        use std::mem::{ size_of, transmute };
        use std::raw::Slice;
        // Compute size of slice in bytes.
        let len = size_of::<P>() * self.pixels.len();
        let slice = Slice { data: self.pixels.as_mut_ptr() as *const u8, len: len };
        unsafe { transmute(slice) }
    }
}

impl<T: Primitive, P: Pixel<T> + Clone + Copy> GenericImage<P> for ImageBuf<P> {
    fn dimensions(&self) -> (u32, u32) {
        (self.width, self.height)
    }

    fn bounds(&self) -> (u32, u32, u32, u32) {
        (0, 0, self.width, self.height)
    }

    fn get_pixel(&self, x: u32, y: u32) -> P {
        let index  = y * self.width + x;

        self.pixels[index as uint]
    }

    fn put_pixel(&mut self, x: u32, y: u32, pixel: P) {
        let index  = y * self.width + x;
        let buf    = self.pixels.as_mut_slice();

        buf[index as uint] = pixel;
    }
}

impl<T: Primitive, P: Pixel<T> + Clone + Copy> MutableRefImage<P> for ImageBuf<P> {
    fn get_mut_pixel(&mut self, x: u32, y: u32) -> &mut P {
        let index = y * self.width + x;

        &mut self.pixels[index as uint]
    }
}

impl<T: Primitive, P: Pixel<T>> Index<(u32, u32), P> for ImageBuf<P> {
    fn index(&self, coords: &(u32, u32)) -> &P {
        let &(x, y) = coords;
        let index  = y * self.width + x;

        &self.pixels[index as uint]
    }
}

/// A View into another image
pub struct SubImage <'a, I:'a> {
    image:   &'a mut I,
    xoffset: u32,
    yoffset: u32,
    xstride: u32,
    ystride: u32,
}

impl<'a, T: Primitive, P: Pixel<T>, I: GenericImage<P>> SubImage<'a, I> {
    ///Construct a new subimage
    pub fn new(image: &mut I, x: u32, y: u32, width: u32, height: u32) -> SubImage<I> {
        SubImage {
            image:   image,
            xoffset: x,
            yoffset: y,
            xstride: width,
            ystride: height,
        }
    }

    ///Return a mutable reference to the wrapped image.
    pub fn inner_mut(&mut self) -> &mut I {
        &mut (*self.image)
    }

    ///Change the coordinates of this subimage.
    pub fn change_bounds(&mut self, x: u32, y: u32, width: u32, height: u32) {
        self.xoffset = x;
        self.yoffset = y;
        self.xstride = width;
        self.ystride = height;
    }

    ///Convert this subimage to an ImageBuf
    pub fn to_image(&self) -> ImageBuf<P> {
        let p: P = Default::default();
        let mut out = ImageBuf::from_pixel(self.xstride, self.ystride, p.clone());

        for y in range(0, self.ystride) {
            for x in range(0, self.xstride) {
                let p = self.get_pixel(x, y);
                out.put_pixel(x, y, p);
            }
        }

        out
    }
}

impl<'a, T: Primitive, P: Pixel<T>, I: GenericImage<P>> GenericImage<P> for SubImage<'a, I> {
    fn dimensions(&self) -> (u32, u32) {
        (self.xstride, self.ystride)
    }

    fn bounds(&self) -> (u32, u32, u32, u32) {
        (self.xoffset, self.yoffset, self.xstride, self.ystride)
    }

    fn get_pixel(&self, x: u32, y: u32) -> P {
        self.image.get_pixel(x + self.xoffset, y + self.yoffset)
    }

    fn put_pixel(&mut self, x: u32, y: u32, pixel: P) {
        self.image.put_pixel(x + self.xoffset, y + self.yoffset, pixel)
    }
}

impl<'a, T: Primitive, P: Pixel<T>, I: MutableRefImage<P>> MutableRefImage<P> for SubImage<'a, I> {
    fn get_mut_pixel(&mut self, x: u32, y: u32) -> &mut P {
        self.image.get_mut_pixel(x + self.xoffset, y + self.yoffset)
    }
}
