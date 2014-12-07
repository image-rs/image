use std::io;
use std::mem;
use std::iter;
use std::ascii::OwnedAsciiExt;

use ppm;
use gif;
use webp;
use jpeg;
use png;
use tiff;
use tga;

use color;
use color::Pixel;
use imageops;
use image;
use image:: {
    ImageBuf,
    GenericImage,
    ImageDecoder,
    ImageResult,
    ImageFormat,
};

///A Dynamic Image
#[deriving(Clone)]
pub enum DynamicImage {
    /// Each pixel in this image is 8-bit Luma
    ImageLuma8(ImageBuf<color::Luma<u8>>),

    /// Each pixel in this image is 8-bit Luma with alpha
    ImageLumaA8(ImageBuf<color::LumaA<u8>>),

    /// Each pixel in this image is 8-bit Rgb
    ImageRgb8(ImageBuf<color::Rgb<u8>>),

    /// Each pixel in this image is 8-bit Rgb with alpha
    ImageRgba8(ImageBuf<color::Rgba<u8>>),
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
)

impl DynamicImage {
    ///Returns a copy of this image as an RGB image.
    pub fn to_rgb(&self) -> ImageBuf<color::Rgb<u8>> {
        dynamic_map!(*self, ref p -> {
            let (w, h) = p.dimensions();
            let mut buf: ImageBuf<color::Rgb<u8>> = ImageBuf::new(w, h);
            for ((_, _, new_pix), (_, _, old_pix)) in buf.pixels_mut().zip(p.pixels()) {
                *new_pix = old_pix.to_rgb()
            }
            buf
        })
    }

    ///Returns a copy of this image as an RGBA image.
    pub fn to_rgba(&self) -> ImageBuf<color::Rgba<u8>> {
        dynamic_map!(*self, ref p -> {
            let (w, h) = p.dimensions();
            let mut buf: ImageBuf<color::Rgba<u8>> = ImageBuf::new(w, h);
            for ((_, _, new_pix), (_, _, old_pix)) in buf.pixels_mut().zip(p.pixels()) {
                *new_pix = old_pix.to_rgba()
            }
            buf
        })
    }

    ///Returns a copy of this image as a Luma image.
    pub fn to_luma(&self) -> ImageBuf<color::Luma<u8>> {
        dynamic_map!(*self, ref p -> {
            let (w, h) = p.dimensions();
            let mut buf: ImageBuf<color::Luma<u8>> = ImageBuf::new(w, h);
            for ((_, _, new_pix), (_, _, old_pix)) in buf.pixels_mut().zip(p.pixels()) {
                *new_pix = old_pix.to_luma()
            }
            buf
        })
    }

    ///Returns a copy of this image as a LumaA image.
    pub fn to_luma_alpha(&self) -> ImageBuf<color::LumaA<u8>> {
        dynamic_map!(*self, ref p -> {
            let (w, h) = p.dimensions();
            let mut buf: ImageBuf<color::LumaA<u8>> = ImageBuf::new(w, h);
            for ((_, _, new_pix), (_, _, old_pix)) in buf.pixels_mut().zip(p.pixels()) {
                *new_pix = old_pix.to_luma_alpha()
            }
            buf
        })
    }

    ///Return a cut out of this image delimited by the bounding rectangle.
    pub fn crop(&mut self,
                x: u32,
                y: u32,
                width: u32,
                height: u32) -> DynamicImage {

        dynamic_map!(*self, ref mut p => imageops::crop(p, x, y, width, height).to_image())
    }

    ///Return a reference to an 8bit RGB image
    pub fn as_rgb8(&self) -> Option<&ImageBuf<color::Rgb<u8>>> {
        match *self {
            DynamicImage::ImageRgb8(ref p) => Some(p),
            _                              => None
        }
    }

    ///Return a mutable reference to an 8bit RGB image
    pub fn as_mut_rgb8(&mut self) -> Option<&mut ImageBuf<color::Rgb<u8>>> {
        match *self {
            DynamicImage::ImageRgb8(ref mut p) => Some(p),
            _                                  => None
        }
    }

    ///Return a reference to an 8bit RGBA image
    pub fn as_rgba8(&self) -> Option<& ImageBuf<color::Rgba<u8>>> {
        match *self {
            DynamicImage::ImageRgba8(ref p) => Some(p),
            _                               => None
        }
    }

    ///Return a mutable reference to an 8bit RGBA image
    pub fn as_mut_rgba8(&mut self) -> Option<&mut ImageBuf<color::Rgba<u8>>> {
        match *self {
            DynamicImage::ImageRgba8(ref mut p) => Some(p),
            _                                   => None
        }
    }

    ///Return a reference to an 8bit Grayscale image
    pub fn as_luma8(& self) -> Option<& ImageBuf<color::Luma<u8>>> {
        match *self {
            DynamicImage::ImageLuma8(ref p) => Some(p),
            _                               => None
        }
    }

    ///Return a mutable reference to an 8bit Grayscale image
    pub fn as_mut_luma8(&mut self) -> Option<&mut ImageBuf<color::Luma<u8>>> {
        match *self {
            DynamicImage::ImageLuma8(ref mut p) => Some(p),
            _                                   => None
        }
    }

    ///Return a reference to an 8bit Grayscale image with an alpha channel
    pub fn as_luma_alpha8(&self) -> Option<& ImageBuf<color::LumaA<u8>>> {
        match *self {
            DynamicImage::ImageLumaA8(ref p) => Some(p),
            _                                => None
        }
    }

    ///Return a mutable reference to an 8bit Grayscale image with an alpha channel
    pub fn as_mut_luma_alpha8(&mut self) -> Option<&mut ImageBuf<color::LumaA<u8>>> {
        match *self {
            DynamicImage::ImageLumaA8(ref mut p) => Some(p),
            _                                    => None
        }
    }

    ///Return this image's pixels as a byte vector.
    pub fn raw_pixels(&self) -> Vec<u8> {
        image_to_bytes(self)
    }

    ///Return this image's color type.
    pub fn color(&self) -> color::ColorType {
        match *self {
            DynamicImage::ImageLuma8(_) => color::ColorType::Grey(8),
            DynamicImage::ImageLumaA8(_) => color::ColorType::GreyA(8),
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
    ///```nwidth``` and ```nheight``` are the new image's dimensions
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
    ///```nwidth``` and ```nheight``` are the new image's dimensions
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

    /// Performs an unsharpen mask on this image
    /// ```sigma``` is the amount to blur the image by.
    /// ```threshold``` is a control of how much to sharpen.
    /// see https://en.wikipedia.org/wiki/Unsharp_masking#Digital_unsharp_masking
    pub fn unsharpen(&self, sigma: f32, threshold: i32) -> DynamicImage {
        dynamic_map!(*self, ref p => imageops::unsharpen(p, sigma, threshold))
    }

    /// Filters this image with the specified 3x3 kernel.
    pub fn filter3x3(&self, kernel: &[f32]) -> DynamicImage {
        if kernel.len() != 9 {
            //return self.clone()
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

    ///Flip this image vertically
    pub fn flipv(&self) -> DynamicImage {
        dynamic_map!(*self, ref p => imageops::flip_vertical(p))
    }

    ///Flip this image horizontally
    pub fn fliph(&self) -> DynamicImage {
        dynamic_map!(*self, ref p => imageops::flip_horizontal(p))
    }

    ///Rotate this image 90 degrees clockwise.
    pub fn rotate90(&self) -> DynamicImage {
        dynamic_map!(*self, ref p => imageops::rotate90(p))
    }

    ///Rotate this image 180 degrees clockwise.
    pub fn rotate180(&self) -> DynamicImage {
        dynamic_map!(*self, ref p => imageops::rotate180(p))
    }

    ///Rotate this image 270 degrees clockwise.
    pub fn rotate270(&self) -> DynamicImage {
        dynamic_map!(*self, ref p => imageops::rotate270(p))
    }

    /// Encode this image and write it to ```w```
    pub fn save<W: Writer>(&self, w: W, format: ImageFormat) -> io::IoResult<ImageResult<()>> {
        let bytes = self.raw_pixels();
        let (width, height) = self.dimensions();
        let color = self.color();

        let r = match format {
            image::ImageFormat::PNG  => {
                let mut p = png::PNGEncoder::new(w);

                try!(p.encode(bytes.as_slice(), width, height, color))
                Ok(())
            }

            image::ImageFormat::PPM  => {
                let mut p = ppm::PPMEncoder::new(w);

                try!(p.encode(bytes.as_slice(), width, height, color))
                Ok(())
            }

            image::ImageFormat::JPEG => {
                let mut j = jpeg::JPEGEncoder::new(w);

                try!(j.encode(bytes.as_slice(), width, height, color))
                Ok(())
            }

            _ => Err(image::ImageError::UnsupportedError(
                     format!("An encoder for {} is not available.", format))
                 ),
        };

        Ok(r)
    }
}

#[allow(deprecated)]
impl GenericImage<color::Rgba<u8>> for DynamicImage {
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
    #[deprecated = "Use iterator `pixels_mut` to blend the pixels directly. "]
    fn blend_pixel(&mut self, x: u32, y: u32, pixel: color::Rgba<u8>) {
        match *self {
            DynamicImage::ImageLuma8(ref mut p) => p.blend_pixel(x, y, pixel.to_luma()),
            DynamicImage::ImageLumaA8(ref mut p) => p.blend_pixel(x, y, pixel.to_luma_alpha()),
            DynamicImage::ImageRgb8(ref mut p) => p.blend_pixel(x, y, pixel.to_rgb()),
            DynamicImage::ImageRgba8(ref mut p) => p.blend_pixel(x, y, pixel),
        }
    }

    #[deprecated = "Do not use is function: It is unimplemented!"]
    fn get_pixel_mut(&mut self, _: u32, _: u32) -> &mut color::Rgba<u8> {
        unimplemented!()
    }
}

/// Transmutes a Vec<u8> into a Vec<T> where T is safe to transmute.
fn transmute_vec<T: Send + color::SafeToTransmute>(mut vec: Vec<u8>) -> Vec<T> {
    let new_elem_size = mem::size_of::<T>();
    
    let len = vec.len();
    assert!(len % new_elem_size == 0);
    let new_len = len/new_elem_size;
    
    let cap = vec.capacity();
    let new_cap = match cap % new_elem_size {
        0 => { cap/new_elem_size },
        n => {
            // Resizes `vec` to hold an integer value of new_elements
            vec.reserve_exact(cap + new_elem_size - n);
            let cap = vec.capacity();
            assert!(cap % new_elem_size == 0); // assert this worked
            cap/new_elem_size
        }
    };
    let p = vec.as_mut_ptr();
    unsafe {
        mem::forget(vec);
        Vec::from_raw_parts(mem::transmute(p), new_len, new_cap)
    }
}

#[allow(deprecated)]
fn decoder_to_image<I: ImageDecoder>(codec: I) -> ImageResult<DynamicImage> {
    let mut codec = codec;

    let color  = try!(codec.colortype());
    let buf    = try!(codec.read_image());
    let (w, h) = try!(codec.dimensions());

    let image = match color {
        color::ColorType::RGB(8) => {
            DynamicImage::ImageRgb8(ImageBuf::from_pixels(transmute_vec(buf), w, h))
        }

        color::ColorType::RGBA(8) => {
            DynamicImage::ImageRgba8(ImageBuf::from_pixels(transmute_vec(buf), w, h))
        }

        color::ColorType::Grey(8) => {
            DynamicImage::ImageLuma8(ImageBuf::from_pixels(transmute_vec(buf), w, h))
        }

        color::ColorType::GreyA(8) => {
            DynamicImage::ImageLumaA8(ImageBuf::from_pixels(transmute_vec(buf), w, h))
        }
        color::ColorType::Grey(bit_depth) if bit_depth == 1 || bit_depth == 2 || bit_depth == 4 => {
            // Note: this conversion assumes that the scanlines begin on byte boundaries 
            let mask = (1u8 << bit_depth as uint) - 1;
            let scaling_factor = (255)/((1 << bit_depth as uint) - 1);
            let skip = (w % 8)/bit_depth as u32;
            let row_len = w + skip;
            let p = buf.as_slice()
                       .iter()
                       .flat_map(|&v|
                           iter::range_step_inclusive(8i8-(bit_depth as i8), 0, -(bit_depth as i8))
                           .zip(iter::iterate(
                               v, |v| v
                           )
                       ))
                       // skip the pixels that can be neglected because scanlines should
                       // start at byte boundaries
                       .enumerate().filter(|&(i, _)| i % (row_len as uint) < (w as uint) ).map(|(_, p)| p)
                       .map(|(shift, pixel)|
                           (pixel & mask << shift as uint) >> shift as uint
                       )
                       .map(|pixel| color::Luma::<u8>(pixel * scaling_factor))
                       .collect();
            DynamicImage::ImageLuma8(ImageBuf::from_pixels(p, w, h))
        },
        _ => return Err(image::ImageError::UnsupportedColor(color))
    };

    Ok(image)
}

#[allow(deprecated)]
fn image_to_bytes(image: &DynamicImage) -> Vec<u8> {
    let mut r = Vec::new();

    match *image {
        //TODO: consider transmuting
        DynamicImage::ImageLuma8(ref a) => {
            for & i in a.pixelbuf().iter() {
                r.push(i.channel());
            }
        }

        DynamicImage::ImageLumaA8(ref a) => {
            for & i in a.pixelbuf().iter() {
                let (l, a) = i.channels();
                r.push(l);
                r.push(a);
            }
        }

        DynamicImage::ImageRgb8(ref a)  => {
            for & i in a.pixelbuf().iter() {
                let (red, g, b) = i.channels();
                r.push(red);
                r.push(g);
                r.push(b);
            }
        }

        DynamicImage::ImageRgba8(ref a) => {
            for & i in a.pixelbuf().iter() {
                let (red, g, b, alpha) = i.channels();
                r.push(red);
                r.push(g);
                r.push(b);
                r.push(alpha);
            }
        }
    }

    r
}

/// Open the image located at the path specified.
/// The image's format is determined from the path's file extension.
pub fn open(path: &Path) -> ImageResult<DynamicImage> {
    let fin = match io::File::open(path) {
        Ok(f)  => f,
        Err(err) => return Err(image::ImageError::IoError(err))
    };

    let ext = path.extension_str()
                  .map_or("".to_string(), | s | s.to_string().into_ascii_lower());

    let format = match ext.as_slice() {
        "jpg" |
        "jpeg" => image::ImageFormat::JPEG,
        "png"  => image::ImageFormat::PNG,
        "gif"  => image::ImageFormat::GIF,
        "webp" => image::ImageFormat::WEBP,
        "tif" |
        "tiff" => image::ImageFormat::TIFF,
        "tga" => image::ImageFormat::TGA,
        format => return Err(image::ImageError::UnsupportedError(format!(
            "Image format image/{} is not supported.", 
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
pub fn save_buffer(path: &Path, buf: &[u8], width: u32, height: u32, color: color::ColorType) ->  io::IoResult<()> {
    let fout = try!(io::File::create(path));
    let ext = path.extension_str()
                  .map_or("".to_string(), | s | s.to_string().into_ascii_lower());

    match ext.as_slice() {
        "jpg" |
        "jpeg" => jpeg::JPEGEncoder::new(fout).encode(buf, width, height, color),
        "png"  => png::PNGEncoder::new(fout).encode(buf, width, height, color),
        format => Err(io::IoError {
            kind: io::InvalidInput,
            desc: "Unsupported image format.",
            detail: Some(format!(
                "Image format image/{} is not supported.", 
                format
            ))
        })
    }
}

/// Create a new image from a Reader
pub fn load<R: Reader+Seek>(r: R, format: ImageFormat) -> ImageResult<DynamicImage> {
    match format {
        image::ImageFormat::PNG  => decoder_to_image(png::PNGDecoder::new(io::BufferedReader::new(r))),
        image::ImageFormat::GIF  => decoder_to_image(gif::GIFDecoder::new(io::BufferedReader::new(r))),
        image::ImageFormat::JPEG => decoder_to_image(jpeg::JPEGDecoder::new(io::BufferedReader::new(r))),
        image::ImageFormat::WEBP => decoder_to_image(webp::WebpDecoder::new(io::BufferedReader::new(r))),
        image::ImageFormat::TIFF => decoder_to_image(try!(tiff::TIFFDecoder::new(r))),
        image::ImageFormat::TGA => decoder_to_image(tga::TGADecoder::new(r)),
        _ => Err(image::ImageError::UnsupportedError(format!("A decoder for {} is not available.", format))),
    }
}

/// Create a new image from a byte slice
pub fn load_from_memory(buf: &[u8], format: ImageFormat) -> ImageResult<DynamicImage> {
    let b = io::BufReader::new(buf);

    load(b, format)
}

#[cfg(test)]
mod bench {
    extern crate test;


    #[bench]
    fn bench_conversion(b: &mut test::Bencher) {
        let a = super::DynamicImage::ImageRgb8(::ImageBuf::new(1000, 1000));
        b.iter(|| {
            a.to_luma()
        });
        b.bytes = 1000*1000*3
    }
}
