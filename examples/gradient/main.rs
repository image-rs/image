use image::{Pixel, Rgba, SerialRgbaImage};

fn main() {
    let mut img = SerialRgbaImage::new(100, 100);

    let start = Rgba::from_slice(&[0, 128, 0, 0]);
    let end = Rgba::from_slice(&[255, 255, 255, 255]);

    image::imageops::vertical_gradient(&mut img, start, end);
    img.save("vertical_gradient.png").unwrap();
    image::imageops::vertical_gradient(&mut img, end, start);
    img.save("vertical_gradient_reverse.png").unwrap();

    image::imageops::horizontal_gradient(&mut img, start, end);
    img.save("horizontal_gradient.png").unwrap();
}
