//! `read_until_ready` loops until dav1d yields a picture. On a crafted file where dav1d has no
//! pending data left and never produces one, that loop has no way to make progress and spins
//! forever. Regression test for the decoder terminating instead.
//!
//! Run on a worker thread with a timeout, so a regression fails the suite rather than hanging it.
#![cfg(all(feature = "avif", feature = "avif-native"))]

use std::sync::mpsc;
use std::time::Duration;

/// 1509 bytes; reaches `read_until_ready` with a stream dav1d can neither finish nor ask for more
/// of. Before the fix this spun ~20 million iterations a second indefinitely.
const SPINS: &[u8] = include_bytes!("bad/avif/Bad_decoder_spins.avif");

#[test]
fn decoder_terminates_when_no_picture_can_arrive() {
    let (tx, rx) = mpsc::channel();

    std::thread::spawn(move || {
        let result = image::load_from_memory_with_format(SPINS, image::ImageFormat::Avif);
        let _ = tx.send(result.is_err());
    });

    match rx.recv_timeout(Duration::from_secs(30)) {
        Ok(is_err) => assert!(is_err, "crafted avif should fail to decode, not succeed"),
        Err(_) => panic!("AvifDecoder did not terminate within 30s"),
    }
}
