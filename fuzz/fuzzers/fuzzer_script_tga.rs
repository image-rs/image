#![no_main]
#[macro_use] extern crate libfuzzer_sys;
extern crate image;

fuzz_target!(|data: &[u8]| {
    let _ = image::load_from_memory_with_format(data, image::ImageFormat::Tga);
});
