extern crate afl;
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

fn main() {
    afl::fuzz(|data| {
    //afl::read_stdio_bytes(|data| {
        png_decode(&data);
    });
}
