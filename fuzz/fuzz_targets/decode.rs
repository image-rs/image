#![no_main]
#[macro_use] extern crate libfuzzer_sys;
extern crate png;

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

fuzz_target!(|data: &[u8]| {
    let _ = png_decode(&data);
});
