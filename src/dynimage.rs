use std::io;
use std::io::{Write, Seek, BufRead, BufReader, BufWriter};
use std::path::Path;
use std::fs::File;
use std::iter;
use std::ascii::AsciiExt;
use num_iter;

#[cfg(feature = "ppm")]
use ppm;
#[cfg(feature = "gif_codec")]
use gif;
#[cfg(feature = "webp")]
use webp;
#[cfg(feature = "jpeg")]
use jpeg;
#[cfg(feature = "png_codec")]
use png;
#[cfg(feature = "tiff")]
use tiff;
#[cfg(feature = "tga")]
use tga;
#[cfg(feature = "bmp")]
use bmp;
#[cfg(feature = "ico")]
use ico;
#[cfg(feature = "hdr")]
use hdr;

use color;
use buffer::{ImageBuffer, ConvertBuffer, Pixel, GrayImage, GrayAlphaImage, RgbImage, RgbaImage};
use imageops;
use image;
use image:: {
    GenericImage,
    ImageDecoder,
    ImageResult,
    ImageFormat,
};

use image::DecodingResult::{U8};

/// A Dynamic Image
#[derive(Clone)]
pub enum DynamicImage {
    /// Each pixel in this image is 8-bit Luma
    ImageLuma8(GrayImage),

    /// Each pixel in this image is 8-bit Luma with alpha
    ImageLumaA8(GrayAlphaImage),

    /// Each pixel in this image is 8-bit Rgb
    ImageRgb8(RgbImage),

    /// Each pixel in this image is 8-bit Rgb with alpha
    ImageRgba8(RgbaImage),
}

macro_rules! dynamic_map(
        ($dynimage: expr, ref $image: ident => $action: expr) => (
                match $dynimage {
                        DynamicImage::ImageLuma8(ref $image) => DynamicImage::ImageLuma8($action),
                        DynamicImage::ImageLumaA8(ref $image) => DynamicImage::ImageLumaA8($action),
                        DynamicImage::ImageRgb8(ref $image) => DynamicImage::ImageRgb8($action),
                        DynamicImage::ImageRgba8(ref $image) => DynamicImage::ImageRgba8($action),
                }
        );

        ($dynimage: expr, ref mut $image: ident => $action: expr) => (
                match $dynimage {
                        DynamicImage::ImageLuma8(ref mut $image) => DynamicImage::ImageLuma8($action),
                        DynamicImage::ImageLumaA8(ref mut $image) => DynamicImage::ImageLumaA8($action),
                        DynamicImage::ImageRgb8(ref mut $image) => DynamicImage::ImageRgb8($action),
                        DynamicImage::ImageRgba8(ref mut $image) => DynamicImage::ImageRgba8($action),
                }
        );

        ($dynimage: expr, ref $image: ident -> $action: expr) => (
                match $dynimage {
                        DynamicImage::ImageLuma8(ref $image) => $action,
                        DynamicImage::ImageLumaA8(ref $image) => $action,
                        DynamicImage::ImageRgb8(ref $image) => $action,
                        DynamicImage::ImageRgba8(ref $image) => $action,
                }
        );

        ($dynimage: expr, ref mut $image: ident -> $action: expr) => (
                match $dynimage {
                        DynamicImage::ImageLuma8(ref mut $image) => $action,
                        DynamicImage::ImageLumaA8(ref mut $image) => $action,
                        DynamicImage::ImageRgb8(ref mut $image) => $action,
                        DynamicImage::ImageRgba8(ref mut $image) => $action,
                }
        );
);

impl DynamicImage {
    /// Creates a dynamic image backed by a buffer of grey pixels.
    pub fn new_luma8(w: u32, h: u32) -> DynamicImage {
        DynamicImage::ImageLuma8(ImageBuffer::new(w, h))
    }

    /// Creates a dynamic image backed by a buffer of grey
    /// pixels with transparency.
    pub fn new_luma_a8(w: u32, h: u32) -> DynamicImage {
        DynamicImage::ImageLumaA8(ImageBuffer::new(w, h))
    }

    /// Creates a dynamic image backed by a buffer of RGB pixels.
    pub fn new_rgb8(w: u32, h: u32) -> DynamicImage {
        DynamicImage::ImageRgb8(ImageBuffer::new(w, h))
    }

    /// Creates a dynamic image backed by a buffer of RGBA pixels.
    pub fn new_rgba8(w: u32, h: u32) -> DynamicImage {
        DynamicImage::ImageRgba8(ImageBuffer::new(w, h))
    }

    /// Returns a copy of this image as an RGB image.
    pub fn to_rgb(&self) -> RgbImage {
        dynamic_map!(*self, ref p -> {
            p.convert()
        })
    }

    /// Returns a copy of this image as an RGBA image.
    pub fn to_rgba(&self) -> RgbaImage {
        dynamic_map!(*self, ref p -> {
            p.convert()
        })
    }

    /// Returns a copy of this image as a Luma image.
    pub fn to_luma(&self) -> GrayImage {
        dynamic_map!(*self, ref p -> {
            p.convert()
        })
    }

    /// Returns a copy of this image as a LumaA image.
    pub fn to_luma_alpha(&self) -> GrayAlphaImage {
        dynamic_map!(*self, ref p -> {
            p.convert()
        })
    }

    /// Return a cut out of this image delimited by the bounding rectangle.
    pub fn crop(&mut self,
                x: u32,
                y: u32,
                width: u32,
                height: u32) -> DynamicImage {

        dynamic_map!(*self, ref mut p => imageops::crop(p, x, y, width, height).to_image())
    }

    /// Return a reference to an 8bit RGB image
    pub fn as_rgb8(&self) -> Option<&RgbImage> {
        match *self {
            DynamicImage::ImageRgb8(ref p) => Some(p),
            _                              => None
        }
    }

    /// Return a mutable reference to an 8bit RGB image
    pub fn as_mut_rgb8(&mut self) -> Option<&mut RgbImage> {
        match *self {
            DynamicImage::ImageRgb8(ref mut p) => Some(p),
            _                                  => None
        }
    }

    /// Return a reference to an 8bit RGBA image
    pub fn as_rgba8(&self) -> Option<& RgbaImage> {
        match *self {
            DynamicImage::ImageRgba8(ref p) => Some(p),
            _                               => None
        }
    }

    /// Return a mutable reference to an 8bit RGBA image
    pub fn as_mut_rgba8(&mut self) -> Option<&mut RgbaImage> {
        match *self {
            DynamicImage::ImageRgba8(ref mut p) => Some(p),
            _                                   => None
        }
    }

    /// Return a reference to an 8bit Grayscale image
    pub fn as_luma8(& self) -> Option<& GrayImage> {
        match *self {
            DynamicImage::ImageLuma8(ref p) => Some(p),
            _                               => None
        }
    }

    /// Return a mutable reference to an 8bit Grayscale image
    pub fn as_mut_luma8(&mut self) -> Option<&mut GrayImage> {
        match *self {
            DynamicImage::ImageLuma8(ref mut p) => Some(p),
            _                                   => None
        }
    }

    /// Return a reference to an 8bit Grayscale image with an alpha channel
    pub fn as_luma_alpha8(&self) -> Option<& GrayAlphaImage> {
        match *self {
            DynamicImage::ImageLumaA8(ref p) => Some(p),
            _                                => None
        }
    }

    /// Return a mutable reference to an 8bit Grayscale image with an alpha channel
    pub fn as_mut_luma_alpha8(&mut self) -> Option<&mut GrayAlphaImage> {
        match *self {
            DynamicImage::ImageLumaA8(ref mut p) => Some(p),
            _                                    => None
        }
    }

    /// Return this image's pixels as a byte vector.
    pub fn raw_pixels(&self) -> Vec<u8> {
        image_to_bytes(self)
    }

    /// Return this image's color type.
    pub fn color(&self) -> color::ColorType {
        match *self {
            DynamicImage::ImageLuma8(_) => color::ColorType::Gray(8),
            DynamicImage::ImageLumaA8(_) => color::ColorType::GrayA(8),
            DynamicImage::ImageRgb8(_) => color::ColorType::RGB(8),
            DynamicImage::ImageRgba8(_) => color::ColorType::RGBA(8),
        }
    }

    /// Return a grayscale version of this image.
    pub fn grayscale(&self) -> DynamicImage {
        match *self {
            DynamicImage::ImageLuma8(ref p) => DynamicImage::ImageLuma8(p.clone()),
            DynamicImage::ImageLumaA8(ref p) => DynamicImage::ImageLuma8(imageops::grayscale(p)),
            DynamicImage::ImageRgb8(ref p) => DynamicImage::ImageLuma8(imageops::grayscale(p)),
            DynamicImage::ImageRgba8(ref p) => DynamicImage::ImageLuma8(imageops::grayscale(p)),
        }
    }

    /// Invert the colors of this image.
    /// This method operates inplace.
    pub fn invert(&mut self) {
        dynamic_map!(*self, ref mut p -> imageops::invert(p))
    }

    /// Resize this image using the specified filter algorithm.
    /// Returns a new image. The image's aspect ratio is preserved.
    /// The image is scaled to the maximum possible size that fits
    /// within the bounds specified by ```nwidth``` and ```nheight```.
    pub fn resize(&self,
                  nwidth: u32,
                  nheight: u32,
                  filter: imageops::FilterType) -> DynamicImage {

        let (width, height) = self.dimensions();

        let ratio  = width as f32 / height as f32;
        let nratio = nwidth as f32 / nheight as f32;

        let scale = if nratio > ratio {
            nheight as f32 / height as f32
        } else {
            nwidth as f32 / width as f32
        };

        let width2  = (width as f32 * scale) as u32;
        let height2 = (height as f32 * scale) as u32;

        self.resize_exact(width2, height2, filter)
    }

    /// Resize this image using the specified filter algorithm.
    /// Returns a new image. Does not preserve aspect ratio.
    /// ```nwidth``` and ```nheight``` are the new image's dimensions
    pub fn resize_exact(&self,
                        nwidth: u32,
                        nheight: u32,
                        filter: imageops::FilterType) -> DynamicImage {

        dynamic_map!(*self, ref p => imageops::resize(p, nwidth, nheight, filter))
    }

    /// Performs a Gaussian blur on this image.
    /// ```sigma``` is a measure of how much to blur by.
    pub fn blur(&self, sigma: f32) -> DynamicImage {
        dynamic_map!(*self, ref p => imageops::blur(p, sigma))
    }

    /// Performs an unsharpen mask on this image.
    /// ```sigma``` is the amount to blur the image by.
    /// ```threshold``` is a control of how much to sharpen.
    ///
    /// See https://en.wikipedia.org/wiki/Unsharp_masking#Digital_unsharp_masking
    pub fn unsharpen(&self, sigma: f32, threshold: i32) -> DynamicImage {
        dynamic_map!(*self, ref p => imageops::unsharpen(p, sigma, threshold))
    }

    /// Filters this image with the specified 3x3 kernel.
    pub fn filter3x3(&self, kernel: &[f32]) -> DynamicImage {
        if kernel.len() != 9 {
            panic!("filter must be 3 x 3")
        }

        dynamic_map!(*self, ref p => imageops::filter3x3(p, kernel))
    }

    /// Adjust the contrast of this image.
    /// ```contrast``` is the amount to adjust the contrast by.
    /// Negative values decrease the contrast and positive values increase the contrast.
    pub fn adjust_contrast(&self, c: f32) -> DynamicImage {
        dynamic_map!(*self, ref p => imageops::contrast(p, c))
    }

    /// Brighten the pixels of this image.
    /// ```value``` is the amount to brighten each pixel by.
    /// Negative values decrease the brightness and positive values increase it.
    pub fn brighten(&self, value: i32) -> DynamicImage {
        dynamic_map!(*self, ref p => imageops::brighten(p, value))
    }

    /// Flip this image vertically
    pub fn flipv(&self) -> DynamicImage {
        dynamic_map!(*self, ref p => imageops::flip_vertical(p))
    }

    /// Flip this image horizontally
    pub fn fliph(&self) -> DynamicImage {
        dynamic_map!(*self, ref p => imageops::flip_horizontal(p))
    }

    /// Rotate this image 90 degrees clockwise.
    pub fn rotate90(&self) -> DynamicImage {
        dynamic_map!(*self, ref p => imageops::rotate90(p))
    }

    /// Rotate this image 180 degrees clockwise.
    pub fn rotate180(&self) -> DynamicImage {
        dynamic_map!(*self, ref p => imageops::rotate180(p))
    }

    /// Rotate this image 270 degrees clockwise.
    pub fn rotate270(&self) -> DynamicImage {
        dynamic_map!(*self, ref p => imageops::rotate270(p))
    }

    /// Encode this image and write it to ```w```
    pub fn save<W: Write>(&self, w: &mut W, format: ImageFormat) -> ImageResult<()> {
        let bytes = self.raw_pixels();
        let (width, height) = self.dimensions();
        let color = self.color();

        match format {
            #[cfg(feature = "png_codec")]
            image::ImageFormat::PNG  => {
                let p = png::PNGEncoder::new(w);

                try!(p.encode(&bytes, width, height, color));
                Ok(())
            }
            #[cfg(feature = "ppm")]
            image::ImageFormat::PPM  => {
                let mut p = ppm::PPMEncoder::new(w);

                try!(p.encode(&bytes, width, height, color));
                Ok(())
            }

            #[cfg(feature = "jpeg")]
            image::ImageFormat::JPEG => {
                let mut j = jpeg::JPEGEncoder::new(w);

                try!(j.encode(&bytes, width, height, color));
                Ok(())
            }

            #[cfg(feature = "gif_codec")]
            image::ImageFormat::GIF => {
                let g = gif::Encoder::new(w);

                try!(g.encode(gif::Frame::from_rgba(
                    width as u16,
                    height as u16,
                    &mut *self.to_rgba().iter().cloned().collect::<Vec<u8>>()
                )));
                Ok(())
            }

            #[cfg(feature = "ico")]
            image::ImageFormat::ICO => {
                let i = ico::ICOEncoder::new(w);

                try!(i.encode(&bytes, width, height, color));
                Ok(())
            }

            _ => Err(image::ImageError::UnsupportedError(
                     format!("An encoder for {:?} is not available.", format))
                 ),
        }
    }
}

#[allow(deprecated)]
impl GenericImage for DynamicImage {
    type Pixel = color::Rgba<u8>;

    fn dimensions(&self) -> (u32, u32) {
        dynamic_map!(*self, ref p -> p.dimensions())
    }

    fn bounds(&self) -> (u32, u32, u32, u32) {
        dynamic_map!(*self, ref p -> p.bounds())
    }

    fn get_pixel(&self, x: u32, y: u32) -> color::Rgba<u8> {
        dynamic_map!(*self, ref p -> p.get_pixel(x, y).to_rgba())
    }

    fn put_pixel(&mut self, x: u32, y: u32, pixel: color::Rgba<u8>) {
        match *self {
            DynamicImage::ImageLuma8(ref mut p) => p.put_pixel(x, y, pixel.to_luma()),
            DynamicImage::ImageLumaA8(ref mut p) => p.put_pixel(x, y, pixel.to_luma_alpha()),
            DynamicImage::ImageRgb8(ref mut p) => p.put_pixel(x, y, pixel.to_rgb()),
            DynamicImage::ImageRgba8(ref mut p) => p.put_pixel(x, y, pixel),
        }
    }
    /// DEPRECATED: Use iterator `pixels_mut` to blend the pixels directly.
    fn blend_pixel(&mut self, x: u32, y: u32, pixel: color::Rgba<u8>) {
        match *self {
            DynamicImage::ImageLuma8(ref mut p) => p.blend_pixel(x, y, pixel.to_luma()),
            DynamicImage::ImageLumaA8(ref mut p) => p.blend_pixel(x, y, pixel.to_luma_alpha()),
            DynamicImage::ImageRgb8(ref mut p) => p.blend_pixel(x, y, pixel.to_rgb()),
            DynamicImage::ImageRgba8(ref mut p) => p.blend_pixel(x, y, pixel),
        }
    }

    /// DEPRECATED: Do not use is function: It is unimplemented!
    fn get_pixel_mut(&mut self, _: u32, _: u32) -> &mut color::Rgba<u8> {
        unimplemented!()
    }
}


/// Decodes an image and stores it into a dynamic image
pub fn decoder_to_image<I: ImageDecoder>(codec: I) -> ImageResult<DynamicImage> {
    let mut codec = codec;

    let color  = try!(codec.colortype());
    let buf    = try!(codec.read_image());
    let (w, h) = try!(codec.dimensions());

    let image = match (color, buf) {
        (color::ColorType::RGB(8), U8(buf)) => {
            ImageBuffer::from_raw(w, h, buf).map(|v| DynamicImage::ImageRgb8(v))
        }

        (color::ColorType::RGBA(8), U8(buf)) => {
            ImageBuffer::from_raw(w, h, buf).map(|v| DynamicImage::ImageRgba8(v))
        }

        (color::ColorType::Gray(8), U8(buf)) => {
            ImageBuffer::from_raw(w, h, buf).map(|v| DynamicImage::ImageLuma8(v))
        }

        (color::ColorType::GrayA(8), U8(buf)) => {
            ImageBuffer::from_raw(w, h, buf).map(|v| DynamicImage::ImageLumaA8(v))
        }
        (color::ColorType::Gray(bit_depth), U8(ref buf)) if bit_depth == 1 || bit_depth == 2 || bit_depth == 4 => {
            // Note: this conversion assumes that the scanlines begin on byte boundaries
            let mask = (1u8 << bit_depth as usize) - 1;
            let scaling_factor = (255)/((1 << bit_depth as usize) - 1);
            let skip = (w % 8)/bit_depth as u32;
            let row_len = w + skip;
            let p = buf
                       .iter()
                       .flat_map(|&v|
                           num_iter::range_step_inclusive(8i8-(bit_depth as i8), 0, -(bit_depth as i8))
                           .zip(iter::repeat(v))
                       )
                       // skip the pixels that can be neglected because scanlines should
                       // start at byte boundaries
                       .enumerate().filter(|&(i, _)| i % (row_len as usize) < (w as usize) ).map(|(_, p)| p)
                       .map(|(shift, pixel)|
                           (pixel & mask << shift as usize) >> shift as usize
                       )
                       .map(|pixel| pixel * scaling_factor)
                       .collect();
            ImageBuffer::from_raw(w, h, p).map(|buf| DynamicImage::ImageLuma8(buf))
        },
        _ => return Err(image::ImageError::UnsupportedColor(color))
    };
    match image {
        Some(image) => Ok(image),
        None => Err(image::ImageError::DimensionError)
    }
}

#[allow(deprecated)]
fn image_to_bytes(image: &DynamicImage) -> Vec<u8> {
    match *image {
        // TODO: consider transmuting
        DynamicImage::ImageLuma8(ref a) => {
            a.iter().map(|v| *v).collect()
        }

        DynamicImage::ImageLumaA8(ref a) => {
            a.iter().map(|v| *v).collect()
        }

        DynamicImage::ImageRgb8(ref a)  => {
            a.iter().map(|v| *v).collect()
        }

        DynamicImage::ImageRgba8(ref a) => {
            a.iter().map(|v| *v).collect()
        }
    }
}

/// Open the image located at the path specified.
/// The image's format is determined from the path's file extension.
pub fn open<P>(path: P) -> ImageResult<DynamicImage> where P: AsRef<Path> {
    // thin wrapper function to strip generics before calling open_impl
    open_impl(path.as_ref())
}

fn open_impl(path: &Path) -> ImageResult<DynamicImage> {
    let fin = match File::open(path) {
        Ok(f)  => f,
        Err(err) => return Err(image::ImageError::IoError(err))
    };
    let fin = BufReader::new(fin);

    let ext = path.extension().and_then(|s| s.to_str())
                  .map_or("".to_string(), |s| s.to_ascii_lowercase());

    let format = match &ext[..] {
        "jpg" |
        "jpeg" => image::ImageFormat::JPEG,
        "png"  => image::ImageFormat::PNG,
        "gif"  => image::ImageFormat::GIF,
        "webp" => image::ImageFormat::WEBP,
        "tif" |
        "tiff" => image::ImageFormat::TIFF,
        "tga" => image::ImageFormat::TGA,
        "bmp" => image::ImageFormat::BMP,
        "ico" => image::ImageFormat::ICO,
        "hdr" => image::ImageFormat::HDR,
        format => return Err(image::ImageError::UnsupportedError(format!(
            "Image format image/{:?} is not supported.",
            format
        )))
    };

    load(fin, format)
}

/// Saves the supplied buffer to a file at the path specified.
///
/// The image format is derived from the file extension. The buffer is assumed to have
/// the correct format according to the specified color type.

/// This will lead to corrupted files if the buffer contains malformed data. Currently only
/// jpeg and png files are supported.
pub fn save_buffer<P>(path: P, buf: &[u8], width: u32, height: u32, color: color::ColorType)
                      -> io::Result<()> where P: AsRef<Path> {
    // thin wrapper function to strip generics before calling save_buffer_impl
    save_buffer_impl(path.as_ref(), buf, width, height, color)
}

fn save_buffer_impl(path: &Path, buf: &[u8], width: u32, height: u32, color: color::ColorType)
                      -> io::Result<()> {
    let ref mut fout = BufWriter::new(try!(File::create(path)));
    let ext = path.extension().and_then(|s| s.to_str())
                  .map_or("".to_string(), |s| s.to_ascii_lowercase());

    match &*ext {
        #[cfg(feature = "ico")]
        "ico" => ico::ICOEncoder::new(fout).encode(buf, width, height, color),
        #[cfg(feature = "jpeg")]
        "jpg" |
        "jpeg" => jpeg::JPEGEncoder::new(fout).encode(buf, width, height, color),
        #[cfg(feature = "png_codec")]
        "png"  => png::PNGEncoder::new(fout).encode(buf, width, height, color),
        #[cfg(feature = "ppm")]
        "ppm"  => ppm::PPMEncoder::new(fout).encode(buf, width, height, color),
        format => Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            &format!("Unsupported image format image/{:?}", format)[..],
        ))
    }
}

/// Create a new image from a Reader
pub fn load<R: BufRead+Seek>(r: R, format: ImageFormat) -> ImageResult<DynamicImage> {
    match format {
        #[cfg(feature = "png_codec")]
        image::ImageFormat::PNG  => decoder_to_image(png::PNGDecoder::new(r)),
        #[cfg(feature = "gif_codec")]
        image::ImageFormat::GIF  => decoder_to_image(gif::Decoder::new(r)),
        #[cfg(feature = "jpeg")]
        image::ImageFormat::JPEG => decoder_to_image(jpeg::JPEGDecoder::new(r)),
        #[cfg(feature = "webp")]
        image::ImageFormat::WEBP => decoder_to_image(webp::WebpDecoder::new(r)),
        #[cfg(feature = "tiff")]
        image::ImageFormat::TIFF => decoder_to_image(try!(tiff::TIFFDecoder::new(r))),
        #[cfg(feature = "tga")]
        image::ImageFormat::TGA => decoder_to_image(tga::TGADecoder::new(r)),
        #[cfg(feature = "bmp")]
        image::ImageFormat::BMP => decoder_to_image(bmp::BMPDecoder::new(r)),
        #[cfg(feature = "ico")]
        image::ImageFormat::ICO => decoder_to_image(try!(ico::ICODecoder::new(r))),
        #[cfg(feature = "hdr")]
        image::ImageFormat::HDR => decoder_to_image(try!(hdr::HDRAdapter::new(BufReader::new(r)))),
        _ => Err(image::ImageError::UnsupportedError(format!("A decoder for {:?} is not available.", format))),
    }
}

static MAGIC_BYTES: [(&'static [u8], ImageFormat); 10] = [
    (b"\x89PNG\r\n\x1a\n", ImageFormat::PNG),
    (&[0xff, 0xd8, 0xff], ImageFormat::JPEG),
    (b"GIF89a", ImageFormat::GIF),
    (b"GIF87a", ImageFormat::GIF),
    (b"WEBP", ImageFormat::WEBP),
    (b"MM.*", ImageFormat::TIFF),
    (b"II*.", ImageFormat::TIFF),
    (b"BM", ImageFormat::BMP),
    (&[0, 0, 1, 0], ImageFormat::ICO),
    (b"#?RADIANCE", ImageFormat::HDR),
];

/// Create a new image from a byte slice
///
/// Makes an educated guess about the image format.
/// TGA is not supported by this function.
pub fn load_from_memory(buffer: &[u8]) -> ImageResult<DynamicImage> {
    load_from_memory_with_format(buffer, try!(guess_format(buffer)))
}


/// Create a new image from a byte slice
#[inline(always)]
pub fn load_from_memory_with_format(buf: &[u8], format: ImageFormat) -> ImageResult<DynamicImage> {
    let b = io::Cursor::new(buf);
    load(b, format)
}

/// Guess image format from memory block
///
/// Makes an educated guess about the image format based on the Magic Bytes at the beginning.
/// TGA is not supported by this function.
/// This is not to be trusted on the validity of the whole memory block
pub fn guess_format(buffer: &[u8]) -> ImageResult<ImageFormat> {
    for &(signature, format) in MAGIC_BYTES.iter() {
        if buffer.starts_with(signature) {
            return Ok(format);
        }
    }
    Err(image::ImageError::UnsupportedError(
        "Unsupported image format".to_string())
    )
}

#[cfg(test)]
mod bench {
    use test;

    #[bench]
    fn bench_conversion(b: &mut test::Bencher) {
        let a = super::DynamicImage::ImageRgb8(::ImageBuffer::new(1000, 1000));
        b.iter(|| {
            a.to_luma()
        });
        b.bytes = 1000*1000*3
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn test_empty_file() {
        assert!(super::load_from_memory(b"").is_err());
    }
}
