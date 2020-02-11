extern crate image;

use image::ImageDecoder;
use image::error::{ImageError, ImageResult, LimitError, LimitErrorKind};

mod utils;

#[inline(always)]
fn pnm_decode(data: &[u8]) -> ImageResult<Vec<u8>> {
    let decoder = image::pnm::PnmDecoder::new(data)?;
    let (width, height) = decoder.dimensions();

    if width.saturating_mul(height) > 4_000_000 {
        return Err(ImageError::Limits(LimitError::from_kind(LimitErrorKind::DimensionError)));
    }

    let mut buf = vec![];
    decoder.read_image(&mut buf)?;
    Ok(buf)
}

fn main() {
    let data = utils::read_file_from_args();
    let _ = pnm_decode(&data);
}
