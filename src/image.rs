use std::{
	io,
	slice
};

use std::ascii::StrAsciiExt;

use imaging::{
	sample,
	colorops,
	affine,
	pixelbuf,
	colortype,
};

use imaging::colortype::ColorType;

use imaging::pixelbuf::{
	Luma8,
	LumaA8,
	Rgb8,
	Rgba8
};

use codecs::{
	png,
	jpeg,
	webp,
	gif,
	ppm,
};

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
	/// An Image in PNG Format
	PNG,

	/// An Image in JPEG Format
	JPEG,

	/// An Image in GIF Format
	GIF,

	/// An Image in WEBP Format
	WEBP,

	/// An Image in PPM Format
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
#[deriving(Clone, Show)]
pub struct Image {
	pixels:  pixelbuf::PixelBuf,
	width:   u32,
	height:  u32,
	color:   ColorType,
}

impl Image {
	/// Open the image located at the path specified.
	/// The image's format is determined from the path's file extension.
	pub fn open(path: &Path) -> ImageResult<Image> {
		let fin = match io::File::open(path) {
			Ok(f)  => f,
			Err(_) => return Err(IoError)
		};

		let ext    = path.extension_str()
				 .map_or("".to_string(), |s| s.to_ascii_lower());

		let format = match ext.as_slice()		{
			"jpg" |
			"jpeg" => JPEG,
			"png"  => PNG,
			"gif"  => GIF,
			"webp" => WEBP,
			_      => return Err(UnsupportedError)
		};

		Image::load(fin, format)
	}

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
		let pixels = match self.pixels {
	                Luma8(ref p)  => Luma8(colorops::grayscale(p.as_slice())),
	                LumaA8(ref p) => Luma8(colorops::grayscale(p.as_slice())),
	                Rgb8(ref p)   => Luma8(colorops::grayscale(p.as_slice())),
	                Rgba8(ref p)  => Luma8(colorops::grayscale(p.as_slice())),
        	};

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
		match self.pixels {
	                Luma8(ref mut p)  => colorops::invert(p.as_mut_slice()),
	                LumaA8(ref mut p) => colorops::invert(p.as_mut_slice()),
	                Rgb8(ref mut p)   => colorops::invert(p.as_mut_slice()),
	                Rgba8(ref mut p)  => colorops::invert(p.as_mut_slice()),
        	}
	}

	/// Resize this image using the specified filter algorithm.
	/// Returns a new image. The image's aspect ratio is preserved.
	///```nwidth``` and ```nheight``` are the new image's dimensions
	pub fn resize(&self, nwidth: u32, nheight: u32, filter: sample::FilterType) -> Image {
		let ratio  = self.width as f32 / self.height as f32;
		let nratio = nwidth as f32 / nheight as f32;

		let scale = if nratio > ratio {
			nheight as f32 / self.height as f32
		} else {
			nwidth as f32 / self.width as f32
		};

		let width2  = (self.width as f32 * scale) as u32;
		let height2 = (self.height as f32 * scale) as u32;

		self.resize_exact(width2, height2, filter)
	}

	/// Resize this image using the specified filter algorithm.
	/// Returns a new image. Does not preserve aspect ratio.
	///```nwidth``` and ```nheight``` are the new image's dimensions
	pub fn resize_exact(&self, nwidth: u32, nheight: u32, filter: sample::FilterType) -> Image {
		let width   = self.width;
		let height  = self.height;

		let pixels = match self.pixels {
	                Luma8(ref p)  => Luma8(sample::resize(p.as_slice(), width, height, nwidth, nheight, filter)),
	                LumaA8(ref p) => LumaA8(sample::resize(p.as_slice(), width, height, nwidth, nheight, filter)),
	                Rgb8(ref p)   => Rgb8(sample::resize(p.as_slice(), width, height, nwidth, nheight, filter)),
	                Rgba8(ref p)  => Rgba8(sample::resize(p.as_slice(), width, height, nwidth, nheight, filter)),
        	};

		Image {
			pixels: pixels,
			width:  nwidth,
			height: nheight,
			color:  self.color
		}
	}

	/// Perfomrs a Gausian blur on this image.
	/// ```sigma``` is a meausure of how much to blur by.
	pub fn blur(&self, sigma: f32) -> Image {
		let width   = self.width;
		let height  = self.height;

		let pixels = match self.pixels {
	                Luma8(ref p)  => Luma8(sample::blur(p.as_slice(), width, height, sigma)),
	                LumaA8(ref p) => LumaA8(sample::blur(p.as_slice(), width, height, sigma)),
	                Rgb8(ref p)   => Rgb8(sample::blur(p.as_slice(), width, height, sigma)),
	                Rgba8(ref p)  => Rgba8(sample::blur(p.as_slice(), width, height, sigma)),
        	};

		Image {
			pixels: pixels,
			width:  self.width,
			height: self.height,
			color:  self.color
		}
	}

	/// Performs an unsharpen mask on ```pixels```
	/// ```sigma``` is the amount to blur the image by.
	/// ```threshold``` is a control of how much to sharpen.
	/// see https://en.wikipedia.org/wiki/Unsharp_masking#Digital_unsharp_masking
	pub fn unsharpen(&self, sigma: f32, threshold: i32) -> Image {
		let width   = self.width;
		let height  = self.height;

		let pixels = match self.pixels {
	                Luma8(ref p)  => Luma8(sample::unsharpen(p.as_slice(), width, height, sigma, threshold)),
	                LumaA8(ref p) => LumaA8(sample::unsharpen(p.as_slice(), width, height, sigma, threshold)),
	                Rgb8(ref p)   => Rgb8(sample::unsharpen(p.as_slice(), width, height, sigma, threshold)),
	                Rgba8(ref p)  => Rgba8(sample::unsharpen(p.as_slice(), width, height, sigma, threshold)),
        	};

	    	Image {
			pixels: pixels,
			width:  self.width,
			height: self.height,
			color:  self.color
		}
	}

	/// Filters this image with the specified 3x3 kernel.
	pub fn filter3x3(&self, kernel: &[f32]) -> Image {
		let width   = self.width;
		let height  = self.height;

		if kernel.len() != 9 {
                	return self.clone()
        	}

		let pixels = match self.pixels {
	                Luma8(ref p)  => Luma8(sample::filter3x3(p.as_slice(), width, height, kernel)),
	                LumaA8(ref p) => LumaA8(sample::filter3x3(p.as_slice(), width, height, kernel)),
	                Rgb8(ref p)   => Rgb8(sample::filter3x3(p.as_slice(), width, height, kernel)),
	                Rgba8(ref p)  => Rgba8(sample::filter3x3(p.as_slice(), width, height, kernel)),
	        };

		Image {
			pixels: pixels,
			width:  self.width,
			height: self.height,
			color:  self.color
		}
	}

	/// Adjust the contrast of ```pixels```
	/// ```contrast``` is the amount to adjust the contrast by.
	/// Negative values decrease the constrast and positive values increase the constrast.
	pub fn adjust_contrast(&self, c: f32) -> Image {
		let pixels = match self.pixels {
	                Luma8(ref p)  => Luma8(colorops::contrast(p.as_slice(), c)),
	                LumaA8(ref p) => LumaA8(colorops::contrast(p.as_slice(), c)),
	                Rgb8(ref p)   => Rgb8(colorops::contrast(p.as_slice(), c)),
	                Rgba8(ref p)  => Rgba8(colorops::contrast(p.as_slice(), c)),
        	};

		Image {
			pixels: pixels,
			width:  self.width,
			height: self.height,
			color:  self.color
		}
	}

	/// Brighten ```pixels```
	/// ```value``` is the amount to brighten each pixel by.
	/// Negative values decrease the brightness and positive values increase it.
	pub fn brighten(&self, value: i32) -> Image {
		let pixels = match self.pixels {
	                Luma8(ref p)  => Luma8(colorops::brighten(p.as_slice(), value)),
	                LumaA8(ref p) => LumaA8(colorops::brighten(p.as_slice(), value)),
	                Rgb8(ref p)   => Rgb8(colorops::brighten(p.as_slice(), value)),
	                Rgba8(ref p)  => Rgba8(colorops::brighten(p.as_slice(), value)),
        	};

		Image {
			pixels: pixels,
			width:  self.width,
			height: self.height,
			color:  self.color
		}
	}

	///Flip this image vertically
	pub fn flipv(&self) -> Image {
		let pixels = match self.pixels {
	                Luma8(ref p)  => Luma8(affine::flip_vertical(p.as_slice(), self.width, self.height)),
	                LumaA8(ref p) => LumaA8(affine::flip_vertical(p.as_slice(), self.width, self.height)),
	                Rgb8(ref p)   => Rgb8(affine::flip_vertical(p.as_slice(), self.width, self.height)),
	                Rgba8(ref p)  => Rgba8(affine::flip_vertical(p.as_slice(), self.width, self.height)),
        	};

		Image {
			pixels: pixels,
			width:  self.width,
			height: self.height,
			color:  self.color
		}
	}

	///Flip this image horizontally
	pub fn fliph(&self) -> Image {
		let pixels = match self.pixels {
	                Luma8(ref p)  => Luma8(affine::flip_horizontal(p.as_slice(), self.width, self.height)),
	                LumaA8(ref p) => LumaA8(affine::flip_horizontal(p.as_slice(), self.width, self.height)),
	                Rgb8(ref p)   => Rgb8(affine::flip_horizontal(p.as_slice(), self.width, self.height)),
	                Rgba8(ref p)  => Rgba8(affine::flip_horizontal(p.as_slice(), self.width, self.height)),
        	};

		Image {
			pixels: pixels,
			width:  self.width,
			height: self.height,
			color:  self.color
		}
	}

	///Rotate this image 90 degrees clockwise.
	pub fn rotate90(&self) -> Image {
		let pixels = match self.pixels {
	                Luma8(ref p)  => Luma8(affine::rotate90(p.as_slice(), self.width, self.height)),
	                LumaA8(ref p) => LumaA8(affine::rotate90(p.as_slice(), self.width, self.height)),
	                Rgb8(ref p)   => Rgb8(affine::rotate90(p.as_slice(), self.width, self.height)),
	                Rgba8(ref p)  => Rgba8(affine::rotate90(p.as_slice(), self.width, self.height)),
        	};

		Image {
			pixels: pixels,
			width:  self.width,
			height: self.height,
			color:  self.color
		}
	}

	///Rotate this image 180 degrees clockwise.
	pub fn rotate180(&self) -> Image {
		let pixels = match self.pixels {
	                Luma8(ref p)  => Luma8(affine::rotate180(p.as_slice(), self.width, self.height)),
	                LumaA8(ref p) => LumaA8(affine::rotate180(p.as_slice(), self.width, self.height)),
	                Rgb8(ref p)   => Rgb8(affine::rotate180(p.as_slice(), self.width, self.height)),
	                Rgba8(ref p)  => Rgba8(affine::rotate180(p.as_slice(), self.width, self.height)),
        	};

		Image {
			pixels: pixels,
			width:  self.width,
			height: self.height,
			color:  self.color
		}
	}

	///Rotate this image 270 degrees clockwise.
	pub fn rotate270(&self) -> Image {
		let pixels = match self.pixels {
	                Luma8(ref p)  => Luma8(affine::rotate270(p.as_slice(), self.width, self.height)),
	                LumaA8(ref p) => LumaA8(affine::rotate270(p.as_slice(), self.width, self.height)),
	                Rgb8(ref p)   => Rgb8(affine::rotate270(p.as_slice(), self.width, self.height)),
	                Rgba8(ref p)  => Rgba8(affine::rotate270(p.as_slice(), self.width, self.height)),
        	};

		Image {
			pixels: pixels,
			width:  self.width,
			height: self.height,
			color:  self.color
		}
	}
}

fn decoder_to_image<I: ImageDecoder>(codec: I) -> ImageResult<Image> {
	let mut codec = codec;

	let color  = try!(codec.colortype());
	let buf    = try!(codec.read_image());
	let (w, h) = try!(codec.dimensions());

	let pixels = match pixelbuf::PixelBuf::from_bytes(buf, color) {
		Ok(p) => p,
		_     => return Err(UnsupportedColor)
	};

	let im = Image {
		pixels:  pixels,
		width:   w,
		height:  h,
		color:   color,
	};

	Ok(im)
}