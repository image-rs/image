use std::io;
use std::iter;
use std::ascii::StrAsciiExt;

use ppm;
use gif;
use webp;
use jpeg;
use png;

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
                        ImageLuma8(ref $image) => ImageLuma8($action),
                        ImageLumaA8(ref $image) => ImageLumaA8($action),
                        ImageRgb8(ref $image) => ImageRgb8($action),
                        ImageRgba8(ref $image) => ImageRgba8($action),
                }
        );

        ($dynimage: expr, ref mut $image: ident => $action: expr) => (
                match $dynimage {
                        ImageLuma8(ref mut $image) => ImageLuma8($action),
                        ImageLumaA8(ref mut $image) => ImageLumaA8($action),
                        ImageRgb8(ref mut $image) => ImageRgb8($action),
                        ImageRgba8(ref mut $image) => ImageRgba8($action),
                }
        );

        ($dynimage: expr, ref $image: ident -> $action: expr) => (
                match $dynimage {
                        ImageLuma8(ref $image) => $action,
                        ImageLumaA8(ref $image) => $action,
                        ImageRgb8(ref $image) => $action,
                        ImageRgba8(ref $image) => $action,
                }
        );

        ($dynimage: expr, ref mut $image: ident -> $action: expr) => (
                match $dynimage {
                        ImageLuma8(ref mut $image) => $action,
                        ImageLumaA8(ref mut $image) => $action,
                        ImageRgb8(ref mut $image) => $action,
                        ImageRgba8(ref mut $image) => $action,
                }
        );
)

impl DynamicImage {
    ///Returns a copy of this image as an RGB image.
    pub fn to_rgb(&self) -> ImageBuf<color::Rgb<u8>> {
        dynamic_map!(*self, ref p -> {
            let (w, h) = p.dimensions();
            let mut pixels = Vec::with_capacity(w as uint * h as uint);
            for (_, _, pix) in p.pixels() {
                pixels.push(pix.to_rgb());
            }
            ImageBuf::from_pixels(pixels, w, h)
        })
    }

    ///Returns a copy of this image as an RGBA image.
    pub fn to_rgba(&self) -> ImageBuf<color::Rgba<u8>> {
        dynamic_map!(*self, ref p -> {
            let (w, h) = p.dimensions();
            let mut pixels = Vec::with_capacity(w as uint * h as uint);
            for (_, _, pix) in p.pixels() {
                pixels.push(pix.to_rgba());
            }
            ImageBuf::from_pixels(pixels, w, h)
        })
    }

    ///Returns a copy of this image as a Luma image.
    pub fn to_luma(&self) -> ImageBuf<color::Luma<u8>> {
        dynamic_map!(*self, ref p -> {
            let (w, h) = p.dimensions();
            let mut pixels = Vec::with_capacity(w as uint * h as uint);
            for (_, _, pix) in p.pixels() {
                pixels.push(pix.to_luma());
            }
            ImageBuf::from_pixels(pixels, w, h)
        })
    }

    ///Returns a copy of this image as a LumaA image.
    pub fn to_luma_alpha(&self) -> ImageBuf<color::LumaA<u8>> {
        dynamic_map!(*self, ref p -> {
            let (w, h) = p.dimensions();
            let mut pixels = Vec::with_capacity(w as uint * h as uint);
            for (_, _, pix) in p.pixels() {
                pixels.push(pix.to_luma_alpha());
            }
            ImageBuf::from_pixels(pixels, w, h)
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
            ImageRgb8(ref p) => Some(p),
            _                => None
        }
    }

    ///Return a mutable reference to an 8bit RGB image
    pub fn as_mut_rgb8(&mut self) -> Option<&mut ImageBuf<color::Rgb<u8>>> {
        match *self {
            ImageRgb8(ref mut p) => Some(p),
            _                    => None
        }
    }

    ///Return a reference to an 8bit RGBA image
    pub fn as_rgba8(&self) -> Option<& ImageBuf<color::Rgba<u8>>> {
        match *self {
            ImageRgba8(ref p) => Some(p),
            _                 => None
        }
    }

    ///Return a mutable reference to an 8bit RGBA image
    pub fn as_mut_rgba8(&mut self) -> Option<&mut ImageBuf<color::Rgba<u8>>> {
        match *self {
            ImageRgba8(ref mut p) => Some(p),
            _                     => None
        }
    }

    ///Return a reference to an 8bit Grayscale image
    pub fn as_luma8(& self) -> Option<& ImageBuf<color::Luma<u8>>> {
        match *self {
            ImageLuma8(ref p) => Some(p),
            _                 => None
        }
    }

    ///Return a mutable reference to an 8bit Grayscale image
    pub fn as_mut_luma8(&mut self) -> Option<&mut ImageBuf<color::Luma<u8>>> {
        match *self {
            ImageLuma8(ref mut p) => Some(p),
            _                     => None
        }
    }

    ///Return a reference to an 8bit Grayscale image with an alpha channel
    pub fn as_luma_alpha8(&self) -> Option<& ImageBuf<color::LumaA<u8>>> {
        match *self {
            ImageLumaA8(ref p) => Some(p),
            _                  => None
        }
    }

    ///Return a mutable reference to an 8bit Grayscale image with an alpha channel
    pub fn as_mut_luma_alpha8(&mut self) -> Option<&mut ImageBuf<color::LumaA<u8>>> {
        match *self {
            ImageLumaA8(ref mut p) => Some(p),
            _                      => None
        }
    }

    ///Return this image's pixels as a byte vector.
    pub fn raw_pixels(&self) -> Vec<u8> {
        image_to_bytes(self)
    }

    ///Return this image's color type.
    pub fn color(&self) -> color::ColorType {
        match *self {
            ImageLuma8(_) => color::Grey(8),
            ImageLumaA8(_) => color::GreyA(8),
            ImageRgb8(_) => color::RGB(8),
            ImageRgba8(_) => color::RGBA(8),
        }
    }

    /// Return a grayscale version of this image.
    pub fn grayscale(&self) -> DynamicImage {
        match *self {
            ImageLuma8(ref p) => ImageLuma8(p.clone()),
            ImageLumaA8(ref p) => ImageLuma8(imageops::grayscale(p)),
            ImageRgb8(ref p) => ImageLuma8(imageops::grayscale(p)),
            ImageRgba8(ref p) => ImageLuma8(imageops::grayscale(p)),
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
            fail!("filter must be 3 x 3")
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
            image::PNG  => {
                let mut p = png::PNGEncoder::new(w);

                try!(p.encode(bytes.as_slice(), width, height, color))
                Ok(())
            }

            image::PPM  => {
                let mut p = ppm::PPMEncoder::new(w);

                try!(p.encode(bytes.as_slice(), width, height, color))
                Ok(())
            }

            image::JPEG => {
                let mut j = jpeg::JPEGEncoder::new(w);

                try!(j.encode(bytes.as_slice(), width, height, color))
                Ok(())
            }

            _    => Err(image::UnsupportedError),
        };

        Ok(r)
    }
}

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
            ImageLuma8(ref mut p) => p.put_pixel(x, y, pixel.to_luma()),
            ImageLumaA8(ref mut p) => p.put_pixel(x, y, pixel.to_luma_alpha()),
            ImageRgb8(ref mut p) => p.put_pixel(x, y, pixel.to_rgb()),
            ImageRgba8(ref mut p) => p.put_pixel(x, y, pixel),
        }
    }
}

fn decoder_to_image<I: ImageDecoder>(codec: I) -> ImageResult<DynamicImage> {
    let mut codec = codec;

    let color  = try!(codec.colortype());
    let buf    = try!(codec.read_image());
    let (w, h) = try!(codec.dimensions());

    let image = match color {
        color::RGB(8) => {
            let p = buf.as_slice()
                       .chunks(3)
                       .map( | a | color::Rgb::<u8>(a[0], a[1], a[2]))
                       .collect();

            ImageRgb8(ImageBuf::from_pixels(p, w, h))
        }

        color::RGBA(8) => {
            let p = buf.as_slice()
                       .chunks(4)
                       .map( | a | color::Rgba::<u8>(a[0], a[1], a[2], a[3]))
                       .collect();

            ImageRgba8(ImageBuf::from_pixels(p, w, h))
        }

        color::Grey(8) => {
            let p = buf.as_slice()
                       .iter()
                       .map( | a | color::Luma::<u8>(*a))
                       .collect();

            ImageLuma8(ImageBuf::from_pixels(p, w, h))
        }

        color::GreyA(8) => {
            let p = buf.as_slice()
                       .chunks(2)
                       .map( | a | color::LumaA::<u8>(a[0], a[1]))
                       .collect();

            ImageLumaA8(ImageBuf::from_pixels(p, w, h))
        }
        color::Grey(bit_depth) if bit_depth == 1 || bit_depth == 2 || bit_depth == 4 => {
            let mask = (1u8 << bit_depth as uint) - 1;
            let scaling_factor = (255)/((1 << bit_depth as uint) - 1);
            let p = buf.as_slice()
                       .iter()
                       .flat_map(|&v|
                           iter::range_step(0, 8, bit_depth)
                           .zip(iter::iterate(
                               |v| v, v
                           )
                       ))
                       .map(|(shift, pixel)|
                           (pixel & mask << shift as uint) >> shift as uint
                       )
                       .map(|pixel| color::Luma::<u8>(pixel * scaling_factor))
                       .collect();
            ImageLuma8(ImageBuf::from_pixels(p, w, h))
        },
        _ => return Err(image::UnsupportedColor(color))
    };

    Ok(image)
}

fn image_to_bytes(image: &DynamicImage) -> Vec<u8> {
    let mut r = Vec::new();

    match *image {
        //TODO: consider transmuting
        ImageLuma8(ref a) => {
            for & i in a.pixelbuf().iter() {
                r.push(i.channel());
            }
        }

        ImageLumaA8(ref a) => {
            for & i in a.pixelbuf().iter() {
                let (l, a) = i.channels();
                r.push(l);
                r.push(a);
            }
        }

        ImageRgb8(ref a)  => {
            for & i in a.pixelbuf().iter() {
                let (red, g, b) = i.channels();
                r.push(red);
                r.push(g);
                r.push(b);
            }
        }

        ImageRgba8(ref a) => {
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
        Err(err) => return Err(image::IoError(err))
    };

    let ext = path.extension_str()
                  .map_or("".to_string(), | s | s.to_ascii_lower());

    let format = match ext.as_slice() {
        "jpg" |
        "jpeg" => image::JPEG,
        "png"  => image::PNG,
        "gif"  => image::GIF,
        "webp" => image::WEBP,
        _      => return Err(image::UnsupportedError)
    };

    load(fin, format)
}

/// Create a new image from a Reader
pub fn load<R: Reader>(r: R, format: ImageFormat) -> ImageResult<DynamicImage> {
    match format {
        image::PNG  => decoder_to_image(png::PNGDecoder::new(r)),
        image::GIF  => decoder_to_image(gif::GIFDecoder::new(r)),
        image::JPEG => decoder_to_image(jpeg::JPEGDecoder::new(r)),
        image::WEBP => decoder_to_image(webp::WebpDecoder::new(r)),
        _    => Err(image::UnsupportedError),
    }
}

/// Create a new image from a byte slice
pub fn load_from_memory(buf: &[u8], format: ImageFormat) -> ImageResult<DynamicImage> {
    let b = io::BufReader::new(buf);

    load(b, format)
}
