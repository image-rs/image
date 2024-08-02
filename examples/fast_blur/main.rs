use image::ImageReader;

fn main() {
    let img = ImageReader::open("examples/fast_blur/lenna.png")
        .unwrap()
        .decode()
        .unwrap();

    let img2 = img.fast_blur(10.0);

    img2.save("examples/fast_blur/lenna_blurred.png").unwrap();
}
