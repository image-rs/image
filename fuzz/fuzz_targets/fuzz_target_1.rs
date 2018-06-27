#![no_main]
#[macro_use] extern crate libfuzzer_sys;
extern crate png;

// allocator_may_return_null=1 prevents crash on allocating huge amounts of memory, see #80
// detect_odr_violation=0 is for https://github.com/rust-lang/rust/issues/41807
const ASAN_DEFAULT_OPTIONS: &'static [u8] = b"allocator_may_return_null=1,detect_odr_violation=0\0";

#[no_mangle]
pub extern "C" fn __asan_default_options() -> *const u8 {
    ASAN_DEFAULT_OPTIONS as *const [u8] as *const u8
}


#[inline(always)]
fn png_decode(data: &[u8]) -> Result<(png::OutputInfo, Vec<u8>), ()> {
    let decoder = png::Decoder::new(data);
    let (info, mut reader) = decoder.read_info().map_err(|_| ())?;

    if info.buffer_size() > 5_000_000 {
        return Err(());
    }

    let mut img_data = Vec::with_capacity(info.buffer_size());
    reader.next_frame(&mut img_data).map_err(|_| ())?;

    Ok((info, img_data))
}

#[inline(always)]
fn png_encode(info: &png::OutputInfo, data: &[u8]) -> Result<Vec<u8>, ()> {
    use png::HasParameters;

    let mut out = Vec::with_capacity(data.len());

    {
        let mut encoder = png::Encoder::new(&mut out, info.width, info.height);
        encoder.set(info.color_type).set(info.bit_depth);
        let mut writer = encoder.write_header().map_err(|_| ())?;
        writer.write_image_data(&data).map_err(|_| ())?;
    }

    Ok(out)
}


fuzz_target!(|data: &[u8]| {
    png_decode(data);
});
