//!An example of generating julia fractals.

extern crate num;
extern crate image;

use std::io::File;

use num::complex::Complex;

// The GenericImage trait provides the method put_pixel.
use image::GenericImage;

fn main() {
        let max_iterations = 256u16;

        let imgx = 800;
        let imgy = 800;

        let scalex = 4.0 / imgx as f32;
        let scaley = 4.0 / imgy as f32;

        //Create a new ImgBuf with width: imgx and height: imgy
        let mut imbuf = image::ImageBuf::new(imgx, imgy);

        for y in range(0, imgy) {
                let cy = y as f32 * scaley - 2.0;

                for x in range(0, imgx) {
                        let cx = x as f32 * scalex - 2.0;

                        let mut z = Complex::new(cx, cy);
                        let c = Complex::new(-0.4, 0.6);

                        let mut i = 0;

                        for t in range(0, max_iterations) {
                                if z.norm() > 2.0 {
                                        break
                                }

                                z = z * z + c;
                                i = t;
                        }

                        // Create an 8bit pixel of type Luma and value i
                        let pixel = image::Luma(i as u8);

                        // Put a pixel in the image at coordinates x and y
                        imbuf.put_pixel(x, y, pixel);
                }
        }

        // Save the image as "fractal.png"
        let fout = File::create(&Path::new("fractal.png")).unwrap();

        //We must indicate the image's color type and what format to save as.
        let _    = image::ImageLuma8(imbuf).save(fout, image::PNG);
}