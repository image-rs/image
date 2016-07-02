use std::fmt;
use std::mem;
use std::io;
use std::error::Error;

use color;
use color::ColorType;
use buffer::{ImageBuffer, Pixel};

use animation::{Frame, Frames};
use dynimage::decoder_to_image;

/// An enumeration of Image errors
#[derive(Debug)]
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
    IoError(io::Error),

    /// The end of the image has been reached
    ImageEnd
}

impl fmt::Display for ImageError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &ImageError::FormatError(ref e) => write!(fmt, "Format error: {}", e),
            &ImageError::DimensionError => write!(fmt, "The Image's dimensions are either too \
                                                        small or too large"),
            &ImageError::UnsupportedError(ref f) => write!(fmt, "The Decoder does not support the \
                                                                 image format `{}`", f),
            &ImageError::UnsupportedColor(ref c) => write!(fmt, "The decoder does not support \
                                                                 the color type `{:?}`", c),
            &ImageError::NotEnoughData => write!(fmt, "Not enough data was provided to the \
                                                       Decoder to decode the image"),
            &ImageError::IoError(ref e) => e.fmt(fmt),
            &ImageError::ImageEnd => write!(fmt, "The end of the image has been reached")
        }
    }
}

impl Error for ImageError {
    fn description (&self) -> &str {
        match *self {
            ImageError::FormatError(..) => &"Format error",
            ImageError::DimensionError => &"Dimension error",
            ImageError::UnsupportedError(..) => &"Unsupported error",
            ImageError::UnsupportedColor(..) => &"Unsupported color",
            ImageError::NotEnoughData => &"Not enough data",
            ImageError::IoError(..) => &"IO error",
            ImageError::ImageEnd => &"Image end"
        }
    }

    fn cause (&self) -> Option<&Error> {
        match *self {
            ImageError::IoError(ref e) => Some(e),
            _ => None
        }
    }
}

impl From<io::Error> for ImageError {
    fn from(err: io::Error) -> ImageError {
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
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
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
    TGA,

    /// An Image in BMP Format
    BMP,

    /// An Image in ICO Format
    ICO,

    /// An Image in Radiance HDR Format
    HDR,
}

/// The trait that all decoders implement
pub trait ImageDecoder: Sized {
    /// Returns a tuple containing the width and height of the image
    fn dimensions(&mut self) -> ImageResult<(u32, u32)>;

    /// Returns the color type of the image e.g. RGB(8) (8bit RGB)
    fn colortype(&mut self) -> ImageResult<ColorType>;

    /// Returns the length in bytes of one decoded row of the image
    fn row_len(&mut self) -> ImageResult<usize>;

    /// Reads one row from the image into ```buf``` and returns the row index
    fn read_scanline(&mut self, buf: &mut [u8]) -> ImageResult<u32>;

    /// Decodes the entire image and return it as a Vector
    fn read_image(&mut self) -> ImageResult<DecodingResult>;

    /// Returns true if the image is animated
    fn is_animated(&mut self) -> ImageResult<bool> {
        // since most image formats do not support animation
        // just return false by default
        return Ok(false)
    }

    /// Returns the frames of the image
    ///
    /// If the image is not animated it returns a single frame
    fn into_frames(self) -> ImageResult<Frames> {
        Ok(Frames::new(vec![
            Frame::new(try!(decoder_to_image(self)).to_rgba())
        ]))
    }

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

        let mut buf = vec![0u8; length as usize * width as usize * bpp];
        let mut tmp = vec![0u8; rowlen];

        loop {
            let row = try!(self.read_scanline(&mut tmp));

            if row - 1 == y {
                break
            }
        }

        for i in 0..length as usize {
            {
                let from = &tmp[x as usize * bpp..width as usize * bpp];

                let to   = &mut buf[i * width as usize * bpp..width as usize * bpp];

                ::copy_memory(from, to);
            }

            let _ = try!(self.read_scanline(&mut tmp));
        }

        Ok(buf)
    }
}


/// Immutable pixel iterator
pub struct Pixels<'a, I: 'a> {
    image:  &'a I,
    x:      u32,
    y:      u32,
    width:  u32,
    height: u32
}

impl<'a, I: GenericImage> Iterator for Pixels<'a, I> {
    type Item = (u32, u32, I::Pixel);

    fn next(&mut self) -> Option<(u32, u32, I::Pixel)> {
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
///
/// DEPRECATED: It is currently not possible to create a safe iterator for this in Rust. You have to use an iterator over the image buffer instead.
pub struct MutPixels<'a, I: 'a> {
    image:  &'a mut I,
    x:      u32,
    y:      u32,
    width:  u32,
    height: u32
}

impl<'a, I: GenericImage + 'a> Iterator for MutPixels<'a, I>
    where I::Pixel: 'a,
          <I::Pixel as Pixel>::Subpixel: 'a {

    type Item = (u32, u32, &'a mut I::Pixel);

    fn next(&mut self) -> Option<(u32, u32, &'a mut I::Pixel)> {
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
pub trait GenericImage: Sized {
    /// The type of pixel.
    type Pixel: Pixel;

    /// The width and height of this image.
    fn dimensions(&self) -> (u32, u32);

    /// The width of this image.
    fn width(&self) -> u32 {
        let (w, _) = self.dimensions();
        w
    }

    /// The height of this image.
    fn height(&self) -> u32 {
        let (_, h) = self.dimensions();
        h
    }

    /// The bounding rectangle of this image.
    fn bounds(&self) -> (u32, u32, u32, u32);

    /// Returns true if this x, y coordinate is contained inside the image.
    fn in_bounds(&self, x: u32, y: u32) -> bool {
        let (ix, iy, iw, ih) = self.bounds();
        if x < ix || x >= ix + iw {
            false
        } else if y < iy || y >= iy + ih {
            false
        } else {
            true
        }
    }

    /// Returns the pixel located at (x, y)
    ///
    /// # Panics
    ///
    /// Panics if `(x, y)` is out of bounds.
    ///
    /// TODO: change this signature to &P
    fn get_pixel(&self, x: u32, y: u32) -> Self::Pixel;

    /// Puts a pixel at location (x, y)
    ///
    /// # Panics
    ///
    /// Panics if `(x, y)` is out of bounds.
    fn get_pixel_mut(&mut self, x: u32, y: u32) -> &mut Self::Pixel;

    /// Returns the pixel located at (x, y)
    ///
    /// This function can be implemented in a way that ignores bounds checking.
    unsafe fn unsafe_get_pixel(&self, x: u32, y: u32) -> Self::Pixel {
        self.get_pixel(x, y)
    }

    /// Put a pixel at location (x, y)
    ///
    /// # Panics
    ///
    /// Panics if `(x, y)` is out of bounds.
    fn put_pixel(&mut self, x: u32, y: u32, pixel: Self::Pixel);

    /// Puts a pixel at location (x, y)
    ///
    /// This function can be implemented in a way that ignores bounds checking.
    unsafe fn unsafe_put_pixel(&mut self, x: u32, y: u32, pixel: Self::Pixel) {
        self.put_pixel(x, y, pixel);
    }

    /// Put a pixel at location (x, y), taking into account alpha channels
    ///
    /// DEPRECATED: This method will be removed. Blend the pixel directly instead.
    fn blend_pixel(&mut self, x: u32, y: u32, pixel: Self::Pixel);

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
    ///
    /// DEPRECATED: This cannot be implemented safely in Rust. Please use the image buffer directly.
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

    /// Copies all of the pixels from another image into this image.
    ///
    /// The other image is copied with the top-left corner of the
    /// other image placed at (x, y).
    ///
    /// In order to copy only a pice of the other image, use `sub_image`.
    ///
    /// # Returns
    /// `true` if the copy was successful, `false` if the image could not
    /// be copied due to size constraints.
    fn copy_from<O>(&mut self, other: &O, x: u32, y:u32) -> bool
    where O: GenericImage<Pixel=Self::Pixel> {
        // Do bounds checking here so we can use the non-bounds-checking
        // functions to copy pixels.
        if self.width() < other.width() + x {
            return false;
        } else if self.height() < other.height() + y {
            return false;
        }

        for i in 0 .. other.width() {
            for k in 0 .. other.height() {
                unsafe {
                    let p = other.unsafe_get_pixel(i, k);
                    self.unsafe_put_pixel(i + x, k + y, p);
                }
            }
        }
        true
    }

    /// Returns a subimage that is a view into this image.
    fn sub_image<'a>(&'a mut self, x: u32, y: u32, width: u32, height: u32)
    -> SubImage<'a, Self>
    where Self: 'static, <Self::Pixel as Pixel>::Subpixel: 'static,
    Self::Pixel: 'static {
        SubImage::new(self, x, y, width, height)
    }
}

/// A View into another image
pub struct SubImage <'a, I: 'a> {
    image:   &'a mut I,
    xoffset: u32,
    yoffset: u32,
    xstride: u32,
    ystride: u32,
}

// TODO: Do we really need the 'static bound on `I`? Can we avoid it?
impl<'a, I: GenericImage + 'static> SubImage<'a, I>
    where I::Pixel: 'static,
          <I::Pixel as Pixel>::Subpixel: 'static {

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
    pub fn to_image(&self) -> ImageBuffer<I::Pixel, Vec<<I::Pixel as Pixel>::Subpixel>> {
        let mut out = ImageBuffer::new(self.xstride, self.ystride);

        for y in 0..self.ystride {
            for x in 0..self.xstride {
                let p = self.get_pixel(x, y);
                out.put_pixel(x, y, p);
            }
        }

        out
    }
}

#[allow(deprecated)]
// TODO: Is the 'static bound on `I` really required? Can we avoid it?
impl<'a, I: GenericImage + 'static> GenericImage for SubImage<'a, I>
    where I::Pixel: 'static,
          <I::Pixel as Pixel>::Subpixel: 'static {

    type Pixel = I::Pixel;

    fn dimensions(&self) -> (u32, u32) {
        (self.xstride, self.ystride)
    }

    fn bounds(&self) -> (u32, u32, u32, u32) {
        (self.xoffset, self.yoffset, self.xstride, self.ystride)
    }

    fn get_pixel(&self, x: u32, y: u32) -> I::Pixel {
        self.image.get_pixel(x + self.xoffset, y + self.yoffset)
    }

    fn put_pixel(&mut self, x: u32, y: u32, pixel: I::Pixel) {
        self.image.put_pixel(x + self.xoffset, y + self.yoffset, pixel)
    }

    /// DEPRECATED: This method will be removed. Blend the pixel directly instead.
    fn blend_pixel(&mut self, x: u32, y: u32, pixel: I::Pixel) {
        self.image.blend_pixel(x + self.xoffset, y + self.yoffset, pixel)
    }

    fn get_pixel_mut(&mut self, x: u32, y: u32) -> &mut I::Pixel {
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

    #[test]
    fn test_in_bounds() {
        let mut target = ImageBuffer::new(2, 2);
        target.put_pixel(0, 0, Rgba([255u8, 0, 0, 255]));

        assert!(target.in_bounds(0,0));
        assert!(target.in_bounds(1,0));
        assert!(target.in_bounds(0,1));
        assert!(target.in_bounds(1,1));

        assert!(!target.in_bounds(2,0));
        assert!(!target.in_bounds(0,2));
        assert!(!target.in_bounds(2,2));
    }
}
