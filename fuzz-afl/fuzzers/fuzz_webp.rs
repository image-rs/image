extern crate afl;
extern crate image;

use image::{DynamicImage, ImageDecoder};
use image::error::{ImageError, ImageResult, LimitError, LimitErrorKind};

#[inline(always)]
fn webp_decode(data: &[u8]) -> ImageResult<DynamicImage> {
    let decoder = image::webp::WebPDecoder::new(data)?;
    let (width, height) = decoder.dimensions();

    if width.saturating_mul(height) > 4_000_000 {
        return Err(ImageError::Limits(LimitError::from_kind(LimitErrorKind::DimensionError)));
    }

    DynamicImage::from_decoder(decoder)
}

fn main() {
    afl::fuzz(|data| {
        let _ = webp_decode(data);
    });
}
