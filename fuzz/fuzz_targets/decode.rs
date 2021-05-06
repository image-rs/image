#![no_main]
#[macro_use] extern crate libfuzzer_sys;
extern crate png;

#[inline(always)]
fn png_decode(data: &[u8]) -> Result<(Option<png::OutputInfo>, Vec<u8>), ()> {
    let limits = png::Limits { bytes: 1 << 16 };
    let decoder = png::Decoder::new_with_limits(data, limits);
    let  mut reader = decoder.read_info().map_err(|_| ())?;

    if reader.info().raw_bytes() > 5_000_000 {
        return Err(());
    }

    let mut img_data = vec![0u8; reader.info().raw_bytes()];

    let mut last_info = None;
    while let Ok(info) = reader.next_frame(&mut img_data) {
        last_info = Some(info);
    }

    Ok((last_info, img_data))
}

fuzz_target!(|data: &[u8]| {
    let _ = png_decode(&data);
});
