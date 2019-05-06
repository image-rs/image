extern crate afl;
extern crate image;

use image::ImageDecoder;

#[inline(always)]
fn webp_decode(data: &[u8]) -> image::ImageResult<Vec<u8>> {
    let decoder = image::webp::WebpDecoder::new(data)?;
    let (width, height) = decoder.dimensions();

    if width.saturating_mul(height) > 4_000_000 {
        return Err(image::ImageError::DimensionError);
    }

    decoder.read_image()
}

fn main() {
    afl::fuzz(|data| {
        let _ = webp_decode(data);
    });
}
