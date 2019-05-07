extern crate afl;
extern crate image;

use image::ImageDecoder;

#[inline(always)]
fn pnm_decode(data: &[u8]) -> image::ImageResult<Vec<u8>> {
    let decoder = image::pnm::PNMDecoder::new(data)?;
    let (width, height) = decoder.dimensions();

    if width.saturating_mul(height) > 4_000_000 {
        return Err(image::ImageError::DimensionError);
    }

    decoder.read_image()
}

fn main() {
    afl::fuzz(|data| {
        let _ = pnm_decode(data);
    });
}
