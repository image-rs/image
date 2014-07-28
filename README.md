#A Basic Rust Image Processing Library

This crate provides basic imaging processing functions and methods for converting to and from image formats.

All image processing functions provided operate on types that implement the ```GenericImage``` trait and return an ```ImageBuf```.

##Documentation

##Overview

###Supported Image Formats
| Format | Decoding | Encoding |
|---     |---       | --- |
| JPEG   | Baseline JPEG | Baseline JPEG |
| PNG    | All supported color types | Same as decoding|
| GIF    | Yes | No |
| Webp   | Lossy(Luma channel only) | No |

###Opening And Saving Images
```rust-image``` provides the ```open``` function for opening images from a path.

The image format is determined from the path's file extension.

```rust
extern crate image;

use std::io::File;

use image::GenericImage;

fn main() {
    //Use the open function to load an image from a PAth.
    //```open``` returns a dynamic image.
    let img = image::open(&Path::new("test.jpg")).unwrap();

    //The dimensions method returns the images width and height
    println!("dimensions {}", img.dimensions());

    //The color method returns the image's ColorType
    println!("{}", img.color());

    let fout = File::create(&Path::new("test.png")).unwrap();

    //Write the contents of this image to the Writer in PNG format.
    let _ = img.save(fout, image::PNG);
}
```

###Representation of Images
```rust-image``` provides two main ways of representing image data:

1. ```ImageBuf```
2. ```DynamicImage```
