# Image [![Build Status](https://travis-ci.org/PistonDevelopers/image.svg?branch=master)](https://travis-ci.org/PistonDevelopers/image)

Maintainers: @nwin, @ccgn

[How to contribute](https://github.com/PistonDevelopers/piston/blob/master/CONTRIBUTING.md)

## An Image Processing Library

This crate provides basic imaging processing functions and methods for converting to and from image formats.

All image processing functions provided operate on types that implement the ```GenericImage``` trait and return an ```ImageBuffer```.

### Usage

Add the following to the Cargo.toml in your project:

```toml
[dependencies]
image = "*"
```

and import using ```extern crate```:

```rust
extern crate image;

//Use image::
```

## 1. Documentation

http://www.piston.rs/image/image/index.html

## 2. Supported Image Formats
```image``` provides implementations of common image format encoders and decoders.

### 2.1 Supported Image Formats
| Format | Decoding | Encoding |
|---     |---       | --- |
| PNG    | All supported color types | Same as decoding|
| JPEG   | Baseline and progressive | Baseline JPEG |
| GIF    | Yes | Yes |
| BMP    | Yes | No |
| ICO    | Yes | Yes |
| TIFF   | Baseline(no fax and packbits support) + LZW | No |
| Webp   | Lossy(Luma channel only) | No |
| PPM    | No | Yes |

### 2.2 The ```ImageDecoder``` Trait
All image format decoders implement the ```ImageDecoder``` trait which provides the following methods:
+ **dimensions**: Return a tuple containing the width and height of the image
+ **colortype**: Return the color type of the image.
+ **row_len**: Returns the length in bytes of one decoded row of the image
+ **read_scanline**: Read one row from the image into buf Returns the row index
+ **read_image**: Decode the entire image and return it as a Vector
+ **load_rect**: Decode a specific region of the image

## 3 Pixels
```image``` provides the following pixel types:
+ **Rgb**: RGB pixel
+ **Rgba**: RGBA pixel
+ **Luma**: Grayscale pixel
+ **LumaA**: Grayscale with alpha

All pixels are parameterised by their component type.

## 4 Images
### 4.1 The ```GenericImage``` Trait
A trait that provides functions for manipulating images, parameterised over the image's pixel type.

```rust
pub trait GenericImage {
    /// The pixel type.
    type Pixel: Pixel;

    /// The width and height of this image.
    fn dimensions(&self) -> (u32, u32);

    /// The bounding rectangle of this image.
    fn bounds(&self) -> (u32, u32, u32, u32);

    /// Return the pixel located at (x, y)
    fn get_pixel(&self, x: u32, y: u32) -> P;

    /// Put a pixel at location (x, y)
    fn put_pixel(&mut self, x: u32, y: u32, pixel: P);

    /// Return an Iterator over the pixels of this image.
    /// The iterator yields the coordinates of each pixel
    /// along with their value
    fn pixels(&self) -> Pixels<Self>;
}
```

### 4.2 Representation of Images
```image``` provides two main ways of representing image data:

#### 4.2.1 ```ImageBuffer```
An image parametarised by its Pixel types, represented by a width and height and a vector of pixels. It provides direct access to its pixels and implements the ```GenericImage``` trait.

```rust
extern crate image;

use image::{
    GenericImage,
    ImageBuffer
};


//Construct a new ImageBuffer with the specified width and height.
let img = ImageBuffer::new(512, 512);

//Construct a new by repeated calls to the supplied closure.
let img = ImageBuffer::from_fn(512, 512, |x, y| {
    if x % 2 == 0 {
        image::Luma([0u8])
    } else {
        image::Luma([255u8])
    }
});

//Obtain the image's width and height
let (width, height) = img.dimensions();

//Access the pixel at coordinate (100, 100)
let pixel = img[(100, 100)];

//or using the ```get_pixel``` method from the ```GenericImage``` trait
let pixel = img.get_pixel(100, 100);

//Put a pixel at coordinate (100, 100)
img.put_pixel(100, 100, pixel);

//Iterate over all pixels in the image
for pixel in img.pixels() {
    //Do something with pixel
}
```

#### 4.2.2 ```DynamicImage```
A ```DynamicImage``` is an enumeration over all supported ```ImageBuffer<P>``` types.
Its exact image type is determined at runtime. It is the type returned when opening an image.
For convenience ```DynamicImage```'s reimplement all image processing functions.

```DynamicImage``` implement the ```GenericImage``` trait for RGBA pixels.

#### 4.2.3 ```SubImage```
A view into another image, delimited by the coordinates of a rectangle.
This is used to perform image processing functions on a subregion of an image.

```rust
extern crate image;

use image::{
    GenericImage,
    ImageBuffer,
    imageops
};

let ref mut img = ImageBuffer::new(512, 512);
let subimg  = imageops::crop(img, 0, 0, 100, 100);

assert!(subimg.dimensions() == (100, 100));
```

## 5 Image Processing Functions
These are the functions defined in the ```imageops``` module. All functions operate on types that implement the ```GenericImage``` trait.

+ **blur**: Performs a Gaussian blur on the supplied image.
+ **brighten**: Brighten the supplied image
+ **contrast**: Adjust the contrast of the supplied image
+ **crop**: Return a mutable view into an image
+ **filter3x3**: Perform a 3x3 box filter on the supplied image.
+ **flip_horizontal**: Flip an image horizontally
+ **flip_vertical**: Flip an image vertically
+ **grayscale**: Convert the supplied image to grayscale
+ **invert**: Invert each pixel within the supplied image This function operates in place.
+ **resize**: Resize the supplied image to the specified dimensions
+ **rotate180**: Rotate an image 180 degrees clockwise.
+ **rotate270**: Rotate an image 270 degrees clockwise.
+ **rotate90**: Rotate an image 90 degrees clockwise.
+ **unsharpen**: Performs an unsharpen mask on the supplied image

## 6 Examples
### 6.1 Opening And Saving Images
```image``` provides the ```open``` function for opening images from a path.

The image format is determined from the path's file extension.

```rust
extern crate image;

use std::fs::File;
use std::path::Path;

use image::GenericImage;

fn main() {
    // Use the open function to load an image from a Path.
    // ```open``` returns a dynamic image.
    let img = image::open(&Path::new("test.jpg")).unwrap();

    // The dimensions method returns the images width and height
    println!("dimensions {:?}", img.dimensions());

    // The color method returns the image's ColorType
    println!("{:?}", img.color());

    let ref mut fout = File::create(&Path::new("test.png")).unwrap();

    // Write the contents of this image to the Writer in PNG format.
    let _ = img.save(fout, image::PNG).unwrap();
}
```

### 6.2 Generating Fractals
```rust
//!An example of generating julia fractals.
extern crate num;
extern crate image;

use std::fs::File;
use std::path::Path;

use num::complex::Complex;

fn main() {
    let max_iterations = 256u16;

    let imgx = 800;
    let imgy = 800;

    let scalex = 4.0 / imgx as f32;
    let scaley = 4.0 / imgy as f32;

    // Create a new ImgBuf with width: imgx and height: imgy
    let mut imgbuf = image::ImageBuffer::new(imgx, imgy);

    // Iterate over the coordiantes and pixels of the image
    for (x, y, pixel) in imgbuf.enumerate_pixels_mut() {
        let cy = y as f32 * scaley - 2.0;
        let cx = x as f32 * scalex - 2.0;

        let mut z = Complex::new(cx, cy);
        let c = Complex::new(-0.4, 0.6);

        let mut i = 0;

        for t in (0..max_iterations) {
            if z.norm() > 2.0 {
                break
            }
            z = z * z + c;
            i = t;
        }

        // Create an 8bit pixel of type Luma and value i
        // and assign in to the pixel at position (x, y)
        *pixel = image::Luma([i as u8]);

    }


    // Save the image as “fractal.png”
    let ref mut fout = File::create(&Path::new("fractal.png")).unwrap();

    // We must indicate the image’s color type and what format to save as
    let _ = image::ImageLuma8(imgbuf).save(fout, image::PNG);
}
```

Example output:

![alt tag](https://raw.githubusercontent.com/ccgn/rust-image/master/examples/fractal.png "A Julia Fractal, c: -0.4 + 0.6i")

### 6.3 Writing raw buffers
If the high level interface is not needed because the image was obtained by other means, `image` provides the function `save_buffer` to save a buffer to a file.

```rust
extern crate image;

use std::path::Path;

fn main() {

    let buffer: &[u8] = ...; // Generate the image data

    // Save the buffer as "image.png"
    image::save_buffer(&Path::new("image.png"), buffer, 800, 600, image::RGB(8)).unwrap()
}

```
