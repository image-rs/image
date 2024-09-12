//! This fuzzer tests that decoding results are the same regardless of the
//! details of how the `Read` trait exposes the underlying input via
//! the `fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize>`
//! method:
//!
//! * Whole slice - `impl Read for &[u8]`:
//!     - The whole slice is available for reading (if it fits into `buf`)
//!     - No IO errors are expected
//!     - Motivation: This is the baseline
//! * Byte-by-byte - `SmalBuf<R>`:
//!     - At most 1 byte can be read in a single call to `read`
//!     - Motivation: Testing that decoding works regardless of how the input is split
//!       into multiple `read` calls.  (The test checks every possible `read` boundary in the input
//!       buffer, even though in practice file or network buffers would split the input into only a
//!       handful of chunks.)
//! * TODO: Intermittent EOFs:
//!     - Intermittently `read` report 0 available bytes.
//!     - Still no IO errors at the `Read` trait level
//!     - Motivation: Testing support for decoding a streaming or partial input
//!       (i.e. scenarios where initially only the first few interlaced passes
//!       can be decoded, and where decoding is resumed after getting more complete
//!       input).

#![no_main]

use libfuzzer_sys::fuzz_target;

use std::fmt::Debug;
use std::io::Read;

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
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        let len = buf.len().min(self.cap);
        self.inner.read(&mut buf[..len])
    }
}

fuzz_target!(|data: &[u8]| {
    let _ = test_data(data);
});

#[inline(always)]
fn test_data<'a>(data: &'a [u8]) -> Result<(), ()> {
    let baseline_reader = Box::new(data);
    let byte_by_byte_reader = Box::new(SmalBuf::new(data, 1));
    let data_readers: Vec<Box<dyn Read + 'a>> = vec![baseline_reader, byte_by_byte_reader];

    let decoders = data_readers
        .into_iter()
        .map(|data_reader| {
            // Small limits, we don't need them hopefully.
            let limits = png::Limits { bytes: 1 << 16 };
            png::Decoder::new_with_limits(data_reader, limits)
        })
        .collect::<Vec<_>>();

    let mut png_readers = decoders
        .into_iter()
        .map(|decoder| decoder.read_info())
        .assert_all_results_are_consistent()
        .collect::<Result<Vec<_>, _>>()
        .map_err(|_| ())?;

    let info = png_readers
        .iter()
        .map(|r| r.info().clone())
        .assert_all_items_are_same(|lhs: &png::Info, rhs: &png::Info| {
            assert_same_info(lhs, rhs);

            // The assert below is somewhat redundant, but we use `raw_bytes`
            // later on, so let's double-check that it's the same everywhere.
            assert_eq!(lhs.raw_bytes(), rhs.raw_bytes());
        });
    if info.raw_bytes() > 5_000_000 {
        return Err(());
    }

    let mut buffers = vec![vec![0; info.raw_bytes()]; png_readers.len()];
    loop {
        let output_infos = png_readers
            .iter_mut()
            .zip(buffers.iter_mut())
            .map(|(png_reader, buffer)| png_reader.next_frame(buffer.as_mut_slice()))
            .assert_all_results_are_consistent()
            .collect::<Result<Vec<_>, _>>()
            .map_err(|_| ())?;
        output_infos.into_iter().assert_all_items_are_equal();
        buffers.iter().assert_all_items_are_equal();
    }
}

fn assert_same_info(lhs: &png::Info, rhs: &png::Info) {
    // Check that all decoders report the same `IHDR` fields.
    assert_eq!(lhs.width, rhs.width);
    assert_eq!(lhs.height, rhs.height);
    assert_eq!(lhs.bit_depth, rhs.bit_depth);
    assert_eq!(lhs.color_type, rhs.color_type);
    assert_eq!(lhs.interlaced, rhs.interlaced);

    // Check all other `Info` fields that implement `Eq`.
    assert_eq!(lhs.chrm_chunk, rhs.chrm_chunk);
    assert_eq!(lhs.gama_chunk, rhs.gama_chunk);
    assert_eq!(lhs.icc_profile, rhs.icc_profile);
    assert_eq!(lhs.palette, rhs.palette);
    assert_eq!(lhs.source_chromaticities, rhs.source_chromaticities);
    assert_eq!(lhs.source_gamma, rhs.source_gamma);
    assert_eq!(lhs.srgb, rhs.srgb);
    assert_eq!(lhs.trns, rhs.trns);
}

trait IteratorExtensionsForFuzzing: Iterator + Sized {
    /// Verifies that either 1) all items in the iterator are `Ok(_)` or 2) all items in the
    /// iterator are `Err(_)`.  Passes through unmodified iterator items.
    fn assert_all_results_are_consistent<T>(self) -> impl Iterator<Item = Self::Item>
    where
        Self: Iterator<Item = Result<T, png::DecodingError>>,
    {
        // Eagerly collect all the items - this makes sure we check consistency of *all* results,
        // even if a downstream iterator combinator consumes items lazily and never "pumps" some
        // items via `next`.  (`iter.take(2)` is one example of such lazy consumer;
        // `iter_of_results.collect::<Result<Vec<_>, _>>()` is another.)
        let all_results = self.collect::<Vec<_>>();

        let any_err = all_results.iter().any(|res| res.is_err());
        let any_ok = all_results.iter().any(|res| res.is_ok());
        if any_err && any_ok {
            // Replacing `Self::Item` with an "ok" string, because we want to support items
            // that do not implement `Debug`.
            let printable_results = all_results.iter().map(|res| res.as_ref().map(|_| "ok"));
            for (i, res) in printable_results.enumerate() {
                eprintln!("Result #{i}: {res:?}");
            }
            panic!("Inconsistent results - some are Ok(_) and some are Err(_)");
        }

        all_results.into_iter()
    }

    /// Verifies that all items in the iterator are the same (according to their `Eq`
    /// implementation).  Returns one of the items.
    fn assert_all_items_are_equal(self) -> Self::Item
    where
        Self::Item: Debug + Eq,
    {
        self.assert_all_items_are_same(|lhs, rhs| assert_eq!(lhs, rhs))
    }

    /// Verifies that all items in the iterator are the same (according to the `assert_same`
    /// function.  Returns one of the items.
    fn assert_all_items_are_same<F>(self, mut assert_same: F) -> <Self as Iterator>::Item
    where
        F: for<'a, 'b> FnMut(&'a Self::Item, &'b Self::Item)
    {
        self
            .enumerate()
            .reduce(|(i, lhs), (j, rhs)| {
                let panic = {
                    let mut assert_same = std::panic::AssertUnwindSafe(&mut assert_same);
                    let lhs = std::panic::AssertUnwindSafe(&lhs);
                    let rhs = std::panic::AssertUnwindSafe(&rhs);
                    std::panic::catch_unwind(move || assert_same(*lhs, *rhs))
                };
                match panic {
                    Ok(_) => (),
                    Err(panic) => {
                        eprintln!("Difference found when comparing item #{i} and #{j}.");
                        std::panic::resume_unwind(panic);
                    },
                }
                (j, lhs /* Arbitrary - could just as well return `rhs` */)
            })
            .map(|(_index, item)| item)
            .expect("Expecting a non-empty iterator")
    }
}

impl<T> IteratorExtensionsForFuzzing for T where T: Iterator + Sized {}
