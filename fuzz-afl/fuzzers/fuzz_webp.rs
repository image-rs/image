extern crate afl;
extern crate image;

use image::ImageDecoder;
use image::error::{ImageError, ImageResult, LimitError, LimitErrorKind};

#[inline(always)]
fn webp_decode(data: &[u8]) -> ImageResult<Vec<u8>> {
    let decoder = image::webp::WebPDecoder::new(data)?;
    let (width, height) = decoder.dimensions();

    if width.saturating_mul(height) > 4_000_000 {
        return Err(ImageError::Limits(LimitError::from_kind(LimitErrorKind::DimensionError)));
    }

    let mut buf = vec![];
    decoder.read_image(&mut buf)?;
    Ok(buf)
}

fn main() {
    afl::fuzz(|data| {
        let _ = webp_decode(data);
    });
}
