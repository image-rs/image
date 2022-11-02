use image::buffer::Canvas;
#[cfg(feature = "png")]
use std::{fs, io};

#[test]
#[cfg(feature = "png")]
fn read_canvas() {
    use image::codecs::png::PngDecoder;

    let img_path = format!(
        "{}/tests/images/png/interlaced/basi2c08.png",
        env!("CARGO_MANIFEST_DIR")
    );
    let stream = io::BufReader::new(fs::File::open(&img_path).unwrap());
    let decoder = PngDecoder::new(stream).expect("valid png");

    let mut canvas = Canvas::from_decoder(decoder).expect("valid png");
    assert_eq!(canvas.width(), 32);
    assert_eq!(canvas.height(), 32);

    // No extra allocation is happening here for the canvas.
    let stream = io::BufReader::new(fs::File::open(&img_path).unwrap());
    let decoder = PngDecoder::new(stream).expect("valid png");
    canvas.decode(decoder).expect("again, valid png");

    assert_eq!(canvas.width(), 32);
    assert_eq!(canvas.height(), 32);
}
