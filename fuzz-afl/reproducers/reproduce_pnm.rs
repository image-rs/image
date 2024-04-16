extern crate image;

use image::{DynamicSerialImage, ImageDecoder};
use image::error::{ImageError, ImageResult, LimitError, LimitErrorKind};

mod utils;

#[inline(always)]
fn pnm_decode(data: &[u8]) -> ImageResult<DynamicSerialImage> {
    let decoder = image::codecs::pnm::PnmDecoder::new(data)?;
    let (width, height) = decoder.dimensions();

    if width.saturating_mul(height) > 4_000_000 {
        return Err(ImageError::Limits(LimitError::from_kind(LimitErrorKind::DimensionError)));
    }

    DynamicSerialImage::from_decoder(decoder)
}

fn main() {
    let data = utils::read_file_from_args();
    let _ = pnm_decode(&data);
}
