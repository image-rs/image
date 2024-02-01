#![no_main]
#[macro_use] extern crate libfuzzer_sys;
extern crate image;

fuzz_target!(|data: &[u8]| {
    let _ = decode(data);
});

fn decode(data: &[u8]) -> Result<(), image::ImageError> {
    use image::ImageDecoder;
    let decoder = image::codecs::tga::TgaDecoder::new(std::io::Cursor::new(data))?;
    if decoder.total_bytes() > 4_000_000 {
        return Ok(());
    }
    let mut buffer = vec![0; decoder.total_bytes() as usize];
    decoder.read_image(&mut buffer)?;
    Ok(())
}
