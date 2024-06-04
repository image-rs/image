use image::RgbaImage;
use pixeli::Rgba;

fn main() {
    let mut img = RgbaImage::new(100, 100);

    let start = Rgba {
        r: 0,
        g: 128,
        b: 0,
        a: 0,
    };
    let end = Rgba {
        r: 255,
        g: 255,
        b: 255,
        a: 255,
    };

    image::imageops::vertical_gradient(&mut img, &start, &end);
    img.save("vertical_gradient.png").unwrap();
    image::imageops::vertical_gradient(&mut img, &end, &start);
    img.save("vertical_gradient_reverse.png").unwrap();

    image::imageops::horizontal_gradient(&mut img, &start, &end);
    img.save("horizontal_gradient.png").unwrap();
}
