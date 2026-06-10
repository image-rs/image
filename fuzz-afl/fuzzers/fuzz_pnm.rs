extern crate afl;
extern crate image;

use image::error::{ImageError, ImageResult, LimitError, LimitErrorKind};
use image::{DynamicImage, ImageDecoder};

#[inline(always)]
fn pnm_decode(data: &[u8]) -> ImageResult<DynamicImage> {
    let mut decoder = image::codecs::pnm::PnmDecoder::new(data)?;
    let total_bytes = decoder.prepare_image()?.total_bytes();

    if total_bytes > 4_000_000 {
        return Err(ImageError::Limits(LimitError::from_kind(
            LimitErrorKind::DimensionError,
        )));
    }

    DynamicImage::from_decoder(decoder)
}

fn main() {
    afl::fuzz(true, |data| {
        let _ = pnm_decode(data);
    });
}
