//! Development-time-only helper module for exporting private APIs so that they can be benchmarked.
//! This module is gated behind the "benchmarks" feature.

use crate::common::BytesPerPixel;
use crate::filter::FilterType;

/// Re-exporting `unfilter` to make it easier to benchmark, despite some items being only
/// `pub(crate)`: `fn unfilter`, `enum BytesPerPixel`.
pub fn unfilter(filter: FilterType, tbpp: u8, previous: &[u8], current: &mut [u8]) {
    let tbpp = BytesPerPixel::for_prediction(tbpp as usize);
    crate::filter::unfilter(filter, tbpp, previous, current)
}
