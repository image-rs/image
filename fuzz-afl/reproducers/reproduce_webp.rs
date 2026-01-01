extern crate image;

use std::io::Cursor;

use image::error::{ImageError, ImageResult, LimitError, LimitErrorKind};
use image::{DynamicImage, ImageDecoder};

mod utils;

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
    let data = utils::read_file_from_args();
    let _ = webp_decode(&data);
}
