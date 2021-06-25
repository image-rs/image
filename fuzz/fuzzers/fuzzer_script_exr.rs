#![no_main]
#[macro_use] extern crate libfuzzer_sys;
extern crate image;
extern crate bytemuck;

use std::io::Cursor;
use image::ImageResult;
use image::codecs::openexr::*;
use image::Rgba32FImage;
use image::error::LimitError;
use image::ImageError;
use image::ImageBuffer;
use image::Rgb32FImage;
use std::io::Seek;
use std::io::BufRead;

// "just dont panic"
fn roundtrip(bytes: &[u8]) -> ImageResult<()> {
    use std::io::Write;

    /// Read the file from the specified path into an `Rgba32FImage`.
    // TODO this method should probably already exist in the main image crate
    fn read_as_rgba_image(read: impl BufRead + Seek) -> ImageResult<Rgb32FImage> {

        // TODO this method should probably already exist in the main image crate
        fn decoder_to_vec<'a, T>(decoder: impl ImageDecoder<'a>) -> ImageResult<Vec<T>>
            where T: crate::traits::Primitive + bytemuck::Pod,
        {
            let mut buf = vec![num_traits::Zero::zero(); usize::try_from(decoder.total_bytes()).unwrap() / std::mem::size_of::<T>()];
            decoder.read_image(bytemuck::cast_slice_mut(buf.as_mut_slice()))?;
            Ok(buf)
        }


        let decoder = OpenExrDecoder::with_alpha_preference(read, Some(true))?;
        let (width, height) = decoder.dimensions();
        let buffer: Vec<f32> = decoder_to_vec(decoder)?;

        ImageBuffer::from_raw(width, height, buffer)

            // this should be the only reason for the "from raw" call to fail,
            // even though such a large allocation would probably cause an error much earlier
            .ok_or_else(|| ImageError::Limits(LimitError::from_kind(LimitErrorKind::InsufficientMemory)))
    }

    /// Write an `Rgba32FImage`.
    /// Assumes the writer is buffered. In most cases,
    /// you should wrap your writer in a `BufWriter` for best performance.
    // TODO this method should probably already exist in the main image crate
    fn write_rgba_image(write: impl Write/* + Seek*/, image: &Rgba32FImage) -> ImageResult<()> {
        OpenEXREncoder::new(write).write_image(
            bytemuck::cast_slice(image.as_raw().as_slice()),
            image.width(), image.height(),
            ColorType::Rgba32F
        )
    }



    let decoded_image = read_as_rgba_image(Cursor::new(bytes))?;

    let mut bytes = Vec::with_capacity(bytes.len() + 20);
    write_rgba_image(Cursor::new(&mut bytes), &decoded_image)?;
    let redecoded_image = read_as_rgba_image(Cursor::new(bytes))?;

    // if both images are valid, assert read/write consistency
    assert_eq!(decoded_image, redecoded_image, "image was valid but was not reproducible");
    Ok(())
}

fuzz_target!(|data: &[u8]| {
    let _img = roundtrip(data); // fixme not optimized away?
});
