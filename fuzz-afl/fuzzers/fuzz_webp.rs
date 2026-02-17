extern crate afl;
extern crate image;

use std::io::Cursor;

use image::error::{ImageError, ImageResult, LimitError, LimitErrorKind};
use image::{DynamicImage, ImageDecoder};

#[inline(always)]
fn webp_decode(data: &[u8]) -> ImageResult<DynamicImage> {
    let mut decoder = image::codecs::webp::WebPDecoder::new(Cursor::new(data))?;
    let (width, height) = decoder.peek_layout()?.dimensions();

    if width.saturating_mul(height) > 4_000_000 {
        return Err(ImageError::Limits(LimitError::from_kind(
            LimitErrorKind::DimensionError,
        )));
    }

    DynamicImage::from_decoder(decoder)
}

fn main() {
    afl::fuzz(true, |data| {
        let _ = webp_decode(data);
    });
}
