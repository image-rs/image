use std::io;
use std::slice;

use pixels;
use colortype;
use colortype::ColorType;

use png;
use ppm;
use gif;
use jpeg;
use webp;

/// An enumeration of Image Errors
#[deriving(Show, PartialEq, Eq)]
pub enum ImageError {
        ///The Image is not formatted properly
	FormatError,

        ///The Image's dimensions are either too small or too large
	DimensionError,

        ///The Decoder does not support this image format
	UnsupportedError,

        ///The Decoder does not support this color type
	UnsupportedColor,

        ///Not enough data was provided to the Decoder
        ///to decode the image
	NotEnoughData,

        ///An I/O Error occurred while decoding the image
	IoError,

        ///The end of the image has been reached
        ImageEnd
}

pub type ImageResult<T> = Result<T, ImageError>;

/// An enumeration of supported image formats.
/// Not all formats support both encoding and decoding.
pub enum ImageFormat {
	PNG,
	JPEG,
	GIF,
	WEBP,
	PPM
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
                        return Err(DimensionError)
                }

                let c = try!(self.colortype());

                let bpp = colortype::bits_per_pixel(c) / 8;
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

                                let to   = buf.mut_slice_from(i * width as uint * bpp)
                                              .mut_slice_to(width as uint * bpp);

                                slice::bytes::copy_memory(to, from);
                        }

                        let _ = try!(self.read_scanline(tmp.as_mut_slice()));
                }

                Ok(buf)
        }
}

/// A Generic representation of an image
#[deriving(Clone)]
pub struct Image {
	pixels:  pixels::PixelBuf,
	width:   u32,
	height:  u32,
	color:   ColorType,
}

impl Image {
	/// Create a new image from ```r```.
	pub fn load<R: Reader>(r: R, format: ImageFormat) -> ImageResult<Image> {
		match format {
			PNG  => decoder_to_image(png::PNGDecoder::new(r)),
			GIF  => decoder_to_image(gif::GIFDecoder::new(r)),
			JPEG => decoder_to_image(jpeg::JPEGDecoder::new(r)),
			WEBP => decoder_to_image(webp::WebpDecoder::new(r)),
			_    => Err(UnsupportedError),
		}
	}

	/// Create a new image from a byte slice
	pub fn load_from_memory(buf: &[u8], format: ImageFormat) -> ImageResult<Image> {
		let b = io::BufReader::new(buf);

		Image::load(b, format)
	}

	/// Encode this image and write it to ```w```
	pub fn save<W: Writer>(&self, w: W, format: ImageFormat) -> io::IoResult<ImageResult<()>> {
		let r = match format {
			PNG  => {
				let mut p = png::PNGEncoder::new(w);
				try!(p.encode(self.raw_pixels().as_slice(),
					      self.width,
					      self.height,
					      self.color))
				Ok(())
			}

			PPM  => {
				let mut p = ppm::PPMEncoder::new(w);
				try!(p.encode(self.raw_pixels().as_slice(),
					      self.width,
					      self.height,
					      self.color))
				Ok(())
			}

			JPEG => {
				let mut j = jpeg::JPEGEncoder::new(w);
				try!(j.encode(self.raw_pixels().as_slice(),
					      self.width,
					      self.height,
					      self.color))
				Ok(())
			}

			_    => Err(UnsupportedError),
		};

		Ok(r)
	}

	/// Return the pixel buffer of this image.
	/// Its interpretation is dependent on the image's ```ColorType```.
	pub fn raw_pixels(& self) -> Vec<u8> {
		self.pixels.to_bytes()
	}

	/// Returns a tuple of the image's width and height.
	pub fn dimensions(&self) -> (u32, u32) {
		(self.width, self.height)
	}

	/// The colortype of this image.
	pub fn colortype(&self) -> ColorType {
		self.color
	}

	/// Return a grayscale version of this image.
	pub fn grayscale(&self) -> Image {
		let pixels = pixels::grayscale(&self.pixels);

		Image {
			pixels: pixels,
			width:  self.width,
			height: self.height,
			color:  colortype::Grey(8),
		}
	}

	/// Invert the colors of this image.
	/// This method operates inplace.
	pub fn invert(&mut self) {
		pixels::invert(&mut self.pixels);
	}

	/// Resize this image using nearest neighnour algorithm.
	/// Returns a new image. Does not preserve aspect ratio.
	///```width``` and ```height``` are the new image's dimensions
	pub fn resize(&self, width: u32, height: u32) -> Image {
		let pixels = pixels::resize(&self.pixels,
					    self.width,
					    self.height,
					    width,
					    height);

		Image {
			pixels: pixels,
			width:  width,
			height: height,
			color:  self.color
		}
	}
}

fn decoder_to_image<I: ImageDecoder>(codec: I) -> ImageResult<Image> {
	let mut codec = codec;

	let color  = try!(codec.colortype());
	let buf    = try!(codec.read_image());
	let (w, h) = try!(codec.dimensions());

	let pixels = match pixels::PixelBuf::from_bytes(buf, color) {
		Some(p) => p,
		None    => return Err(UnsupportedColor)
	};

	let im = Image {
		pixels:  pixels,
		width:   w,
		height:  h,
		color:   color,
	};

	Ok(im)
}