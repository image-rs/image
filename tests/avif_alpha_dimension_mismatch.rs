//! Regression test for a crafted AVIF whose primary (color) picture and its
//! alpha auxiliary picture are independently valid AV1 bitstreams with
//! *different* actual dimensions.
//!
//! `AvifDecoder::new` decodes the primary item and the alpha item through
//! two entirely separate `dav1d::Decoder` instances, each from its own
//! coded data. Prior to the fix, nothing checked that the resulting
//! `dav1d::Picture`s had matching width/height before the alpha-plane
//! compositing loop `zip`-ed the two planes together in
//! `src/codecs/avif/decoder.rs`. Because `Iterator::zip` silently stops at
//! the shorter of its two inputs (or, depending on picture-buffer padding,
//! can even read rows beyond the alpha image's real coded height), a
//! mismatch produced no error -- just an incorrect composited image.
//!
//! This test builds a real, `dav1d`-decodable AVIF file with such a
//! mismatch (color item at 8x8, alpha item's own AV1 bitstream at 4x4) by
//! re-muxing two independently `ravif`-encoded AV1 payloads with
//! `avif-serialize`, and asserts that decoding it now fails cleanly instead
//! of silently corrupting the composited alpha.

#![cfg(all(feature = "avif", feature = "avif-native"))]

use std::io::Cursor;

/// Encode a solid-color RGBA image to a real AVIF file via the crate's own
/// (ravif-backed) AVIF encoder.
fn encode_rgba_avif(width: u32, height: u32, fill: [u8; 4]) -> Vec<u8> {
    use image::ImageEncoder;

    let mut pixels = Vec::with_capacity((width * height * 4) as usize);
    for _ in 0..(width * height) {
        pixels.extend_from_slice(&fill);
    }

    let mut out = Vec::new();
    let encoder = image::codecs::avif::AvifEncoder::new_with_speed_quality(&mut out, 10, 90);
    encoder
        .write_image(&pixels, width, height, image::ExtendedColorType::Rgba8)
        .expect("ravif encode failed");
    out
}

/// Craft an AVIF whose declared/primary dimensions are `color_dim`, but
/// whose alpha auxiliary item is an independently-encoded AV1 bitstream
/// with real dimensions `alpha_dim` (potentially different from
/// `color_dim`). Both source images are encoded with alpha so that a real,
/// valid I400/Cs400 AV1 alpha bitstream is available to extract for the
/// (possibly mismatched) alpha item.
fn craft_avif_with_mismatched_alpha(color_dim: (u32, u32), alpha_dim: (u32, u32)) -> Vec<u8> {
    let color_src = encode_rgba_avif(color_dim.0, color_dim.1, [200, 100, 50, 255]);
    let alpha_src = encode_rgba_avif(alpha_dim.0, alpha_dim.1, [10, 20, 30, 128]);

    let ctx_color = mp4parse::read_avif(
        &mut Cursor::new(&color_src),
        mp4parse::ParseStrictness::Permissive,
    )
    .expect("parsing the color source AVIF failed");
    let ctx_alpha = mp4parse::read_avif(
        &mut Cursor::new(&alpha_src),
        mp4parse::ParseStrictness::Permissive,
    )
    .expect("parsing the alpha source AVIF failed");

    let color_coded = ctx_color
        .primary_item_coded_data()
        .expect("color source has no primary item")
        .to_vec();
    let alpha_coded = ctx_alpha
        .alpha_item_coded_data()
        .expect("alpha source has no alpha item (it was encoded with alpha)")
        .to_vec();

    let mut crafted = Vec::new();
    avif_serialize::serialize(
        &mut crafted,
        &color_coded,
        Some(&alpha_coded),
        color_dim.0,
        color_dim.1,
        8,
    )
    .expect("avif_serialize::serialize failed");
    crafted
}

#[test]
fn mismatched_alpha_dimensions_error_instead_of_silently_corrupting() {
    use image::ImageDecoder;

    // Sanity check: matching dimensions must still decode successfully.
    let good = craft_avif_with_mismatched_alpha((8, 8), (8, 8));
    let mut good_decoder = image::codecs::avif::AvifDecoder::new(Cursor::new(&good))
        .expect("AvifDecoder::new should accept a well-formed AVIF");
    let good_prepared = good_decoder
        .prepare_image()
        .expect("prepare_image should succeed for matching dimensions");
    let mut good_buf = vec![0u8; good_prepared.layout.total_bytes() as usize];
    good_decoder
        .read_image(&mut good_buf)
        .expect("read_image should succeed when color/alpha dimensions match");

    // The actual regression case: color item is 8x8, but the alpha item's
    // own AV1 bitstream is 4x4 -- a real dimension mismatch between two
    // independently-decoded dav1d pictures.
    let crafted = craft_avif_with_mismatched_alpha((8, 8), (4, 4));
    let mut decoder = image::codecs::avif::AvifDecoder::new(Cursor::new(&crafted))
        .expect("AvifDecoder::new should still open the container (mismatch is only visible after dav1d-decoding both items)");
    let prepared = decoder
        .prepare_image()
        .expect("prepare_image should still report the primary (color) dimensions");
    assert_eq!(prepared.layout.dimensions(), (8, 8));

    let mut buf = vec![0u8; prepared.layout.total_bytes() as usize];
    let result = decoder.read_image(&mut buf);

    assert!(
        result.is_err(),
        "read_image must reject a color/alpha dimension mismatch instead of silently \
         producing a corrupted/truncated composited image"
    );
    let message = result.unwrap_err().to_string();
    assert!(
        message.contains("Alpha plane dimensions"),
        "expected an alpha-plane-dimension-mismatch error, got: {message}"
    );
}
