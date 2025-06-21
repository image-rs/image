use image::imageops::GaussianBlurParameters;
use image::ImageReader;

fn main() {
    let path = concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/tests/images/tiff/testsuite/mandrill.tiff"
    );
    let img = ImageReader::open(path).unwrap().decode().unwrap();

    let img2 = img.blur_advanced(GaussianBlurParameters::new_from_sigma(10.0));

    img2.save("examples/fast_blur/mandril_color_blurred.tif")
        .unwrap();
}
