#![no_main]
#[macro_use] extern crate libfuzzer_sys;
extern crate png;

#[inline(always)]
fn png_decode(data: &[u8]) -> Result<(png::OutputInfo, Vec<u8>), ()> {
    let limits = png::Limits { bytes: 1 << 16 };
    let decoder = png::Decoder::new_with_limits(data, limits);
    let (info, mut reader) = decoder.read_info().map_err(|_| ())?;

    if info.buffer_size() > 5_000_000 {
        return Err(());
    }

    let mut img_data = Vec::with_capacity(info.buffer_size());

    while let Ok(_) = reader.next_frame(&mut img_data) {}

    Ok((info, img_data))
}

fuzz_target!(|data: &[u8]| {
    let _ = png_decode(&data);
});
