#![forbid(unsafe_code)]

#[inline(always)]
fn png_decode(data: &[u8]) -> Result<(png::OutputInfo, Vec<u8>), ()> {
    let decoder = png::Decoder::new(data);

    let mut reader = decoder.read_info().map_err(|_| ())?;

    let buffer_size = reader.output_buffer_size();
    if buffer_size > 5_000_000 {
        return Err(());
    }

    let mut img_data = vec![0u8; buffer_size];
    let info = reader.next_frame(&mut img_data).map_err(|_| ())?;

    Ok((info, img_data))
}

fn main() {
    afl::fuzz!(|data: &[u8]| {
        let _ = png_decode(&data);
    });
}
