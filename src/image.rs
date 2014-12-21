use std::error::FromError;
use std::mem;
use std::io;
use std::slice;

use color;
use color::ColorType;
use buffer::{ImageBuffer, Pixel};
use traits::Primitive;

/// An enumeration of Image Errors
#[deriving(Show, PartialEq, Eq)]
pub enum ImageError {
    /// The Image is not formatted properly
    FormatError(String),

    /// The Image's dimensions are either too small or too large
    DimensionError,

    /// The Decoder does not support this image format
    UnsupportedError(String),

    /// The Decoder does not support this color type
    UnsupportedColor(ColorType),

    /// Not enough data was provided to the Decoder
    /// to decode the image
    NotEnoughData,

    /// An I/O Error occurred while decoding the image
    IoError(io::IoError),

    /// The end of the image has been reached
    ImageEnd
}

impl FromError<io::IoError> for ImageError {
    fn from_error(err: io::IoError) -> ImageError {
        ImageError::IoError(err)
    }
}


/// Result of an image decoding/encoding process
pub type ImageResult<T> = Result<T, ImageError>;

/// Result of a decoding process
pub enum DecodingResult {
    /// A vector of unsigned bytes
    U8(Vec<u8>),
    /// A vector of unsigned words
    U16(Vec<u16>)
}

// A buffer for image decoding
pub enum DecodingBuffer<'a> {
    /// A slice of unsigned bytes
    U8(&'a mut [u8]),
    /// A slice of unsigned words
    U16(&'a mut [u16])
}

/// An enumeration of supported image formats.
/// Not all formats support both encoding and decoding.
#[deriving(Copy, PartialEq, Eq, Show)]
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
    /// Returns a tuple containing the width and height of the image
    fn dimensions(&mut self) -> ImageResult<(u32, u32)>;

    /// Returns the color type of the image e.g RGB(8) (8bit RGB)
    fn colortype(&mut self) -> ImageResult<ColorType>;

    /// Returns the length in bytes of one decoded row of the image
    fn row_len(&mut self) -> ImageResult<uint>;

    /// Reads one row from the image into buf and returns the row index
    fn read_scanline(&mut self, buf: &mut [u8]) -> ImageResult<u32>;

    /// Decodes the entire image and return it as a Vector
    fn read_image(&mut self) -> ImageResult<DecodingResult>;

    /// Decodes a specific region of the image, represented by the rectangle
    /// starting from ```x``` and ```y``` and having ```length``` and ```width```
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
#[deprecated = "It is currently not possible to create a safe iterator for this in Rust. You have to use an iterator over the image buffer instead."]
pub struct MutPixels<'a, I:'a> {
    image:  &'a mut I,
    x:      u32,
    y:      u32,
    width:  u32,
    height: u32
}

impl<'a, T: Primitive, P: Pixel<T>, I: GenericImage<P>> Iterator<(u32, u32, &'a mut P)> for MutPixels<'a, I> {
    fn next(&mut self) -> Option<(u32, u32, &'a mut P)> {
        if self.x >= self.width {
            self.x =  0;
            self.y += 1;
        }

        if self.y >= self.height {
            None
        } else {
            let tmp = self.image.get_pixel_mut(self.x, self.y);

            // NOTE: This is potentially dangerous. It would require the signature fn next(&'a mut self) to be safe.
            // error: lifetime of `self` is too short to guarantee its contents can be safely reborrowed...
            let ptr = unsafe {
                mem::transmute(tmp)
            };

            let p = (self.x, self.y, ptr);

            self.x += 1;

            Some(p)
        }
    }
}

/// A trait for manipulating images.
pub trait GenericImage<P> {
    /// The width and height of this image.
    fn dimensions(&self) -> (u32, u32);

    /// The bounding rectangle of this image.
    fn bounds(&self) -> (u32, u32, u32, u32);

    /// Returns the pixel located at (x, y)
    ///
    /// # Panics
    ///
    /// Panics if `(x, y)` is out of bounds.
    /// TODO: change this signature to &P
    fn get_pixel(&self, x: u32, y: u32) -> P;

    /// Puts a pixel at location (x, y)
    ///
    /// # Panics
    ///
    /// Panics if `(x, y)` is out of bounds.
    fn get_pixel_mut(&mut self, x: u32, y: u32) -> &mut P;

    /// Put a pixel at location (x, y)
    ///
    /// # Panics
    ///
    /// Panics if `(x, y)` is out of bounds.
    fn put_pixel(&mut self, x: u32, y: u32, pixel: P);

    /// Put a pixel at location (x, y), taking into account alpha channels
    #[deprecated = "This method will be removed. Blend the pixel directly instead."]
    fn blend_pixel(&mut self, x: u32, y: u32, pixel: P);

    /// Returns an Iterator over the pixels of this image.
    /// The iterator yields the coordinates of each pixel
    /// along with their value
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

    /// Returns an Iterator over mutable pixels of this image.
    /// The iterator yields the coordinates of each pixel
    /// along with a mutable reference to them.
    #[allow(deprecated)]
    #[deprecated = "This cannot be implemented safely Rust. Please use the image buffer directly."]
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

/// A View into another image
pub struct SubImage <'a, I:'a> {
    image:   &'a mut I,
    xoffset: u32,
    yoffset: u32,
    xstride: u32,
    ystride: u32,
}

impl<'a, T: Primitive + 'static, P: Pixel<T> + 'static, I: GenericImage<P>> SubImage<'a, I> {
    /// Construct a new subimage
    pub fn new(image: &mut I, x: u32, y: u32, width: u32, height: u32) -> SubImage<I> {
        SubImage {
            image:   image,
            xoffset: x,
            yoffset: y,
            xstride: width,
            ystride: height,
        }
    }

    /// Returns a mutable reference to the wrapped image.
    pub fn inner_mut(&mut self) -> &mut I {
        &mut (*self.image)
    }

    /// Change the coordinates of this subimage.
    pub fn change_bounds(&mut self, x: u32, y: u32, width: u32, height: u32) {
        self.xoffset = x;
        self.yoffset = y;
        self.xstride = width;
        self.ystride = height;
    }

    /// Convert this subimage to an ImageBuffer
    pub fn to_image(&self) -> ImageBuffer<Vec<T>, T, P> {
        let mut out = ImageBuffer::new(self.xstride, self.ystride);

        for y in range(0, self.ystride) {
            for x in range(0, self.xstride) {
                let p = self.get_pixel(x, y);
                out.put_pixel(x, y, p);
            }
        }

        out
    }
}

#[allow(deprecated)]
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

    #[deprecated = "This method will be removed. Blend the pixel directly instead."]
    fn blend_pixel(&mut self, x: u32, y: u32, pixel: P) {
        self.image.blend_pixel(x + self.xoffset, y + self.yoffset, pixel)
    }

    fn get_pixel_mut(&mut self, x: u32, y: u32) -> &mut P {
        self.image.get_pixel_mut(x + self.xoffset, y + self.yoffset)
    }
}

#[cfg(test)]
mod tests {

    use super::GenericImage;
    use buffer::ImageBuffer;
    use color::{Rgba};

    #[test]
    /// Test that alpha blending works as expected
    #[allow(deprecated)]
    fn test_image_alpha_blending() {
        let mut target = ImageBuffer::new(1, 1);
        target.put_pixel(0, 0, Rgba([255u8, 0, 0, 255]));
        assert!(*target.get_pixel(0, 0) == Rgba([255, 0, 0, 255]));
        target.blend_pixel(0, 0, Rgba([0, 255, 0, 255]));
        assert!(*target.get_pixel(0, 0) == Rgba([0, 255, 0, 255]));

        // Blending an alpha channel onto a solid background
        target.blend_pixel(0, 0, Rgba([255, 0, 0, 127]));
        assert!(*target.get_pixel(0, 0) == Rgba([127, 127, 0, 255]));

        // Blending two alpha channels
        target.put_pixel(0, 0, Rgba([0, 255, 0, 127]));
        target.blend_pixel(0, 0, Rgba([255, 0, 0, 127]));
        assert!(*target.get_pixel(0, 0) == Rgba([169, 85, 0, 190]));
    }
}
