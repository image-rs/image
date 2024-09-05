#![no_main]

use libfuzzer_sys::fuzz_target;

use std::io::{Read, Result};

/// A reader that reads at most `n` bytes.
struct SmalBuf<R: Read> {
    inner: R,
    cap: usize,
}

impl<R: Read> SmalBuf<R> {
    fn new(inner: R, cap: usize) -> Self {
        SmalBuf { inner, cap }
    }
}

impl<R: Read> Read for SmalBuf<R> {
    fn read(&mut self, buf: &mut [u8]) -> Result<usize> {
        let len = buf.len().min(self.cap);
        self.inner.read(&mut buf[..len])
    }
}

fuzz_target!(|data: &[u8]| {
    // Small limits, we don't need them hopefully.
    let limits = png::Limits { bytes: 1 << 16 };

    let reference = png::Decoder::new_with_limits(data, limits);
    let smal = png::Decoder::new_with_limits(SmalBuf::new(data, 1), limits);

    let _ = png_compare(reference, smal);
});

#[inline(always)]
fn png_compare<R: Read, S: Read>(reference: png::Decoder<R>, smal: png::Decoder<S>)
    -> std::result::Result<(), ()>
{
    let mut smal = Some(smal);
    let mut reference = reference.read_info().map_err(|_| {
        assert!(smal.take().unwrap().read_info().is_err());
    })?;

    let mut smal = smal.take().unwrap().read_info().expect("Deviation");

    assert_eq!(reference.info().raw_bytes(), smal.info().raw_bytes());
    if reference.info().raw_bytes() > 5_000_000 {
        return Err(());
    }

    let mut ref_data = vec![0; reference.info().raw_bytes()];
    let mut smal_data = vec![0; reference.info().raw_bytes()];

    loop {
        let rref = reference.next_frame(&mut ref_data);
        let rsmal = smal.next_frame(&mut smal_data);
        match (rref, rsmal) {
            (Ok(info), Ok(sinfo)) if ref_data == smal_data => assert_eq!(info, sinfo),
            (Ok(_), Ok(_)) => panic!("Deviating data decoded"),
            // All errors are treated as equivalent - it is okay to get different errors depending
            // on how the input is chunked.  One example is deflating [88, 9, 99, 30, 5, 99] in one
            // chunk will fill up `ZlibStream.out_buffer` and hold onto the final byte in
            // `fdeflate`'s state.  But when deflating byte-by-byte, `out_buffer` will be resized
            // before handling the final byte, leading to `InvalidLiteralLengthCode` being returned
            // by `fdeflate`.
            (Err(_), Err(_)) => break Ok(()),
            (Ok(_), Err(err)) => panic!("Small buffer failed {:?}", err),
            (Err(err), Ok(_)) => panic!("Unexpected success: {:?}", err),
        }
    }
}
