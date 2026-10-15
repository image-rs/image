//! Convert an image to JPEG XL.
//!
//! The `image` crate has no JPEG XL encoder, so this example validates
//! and decodes the input with the crate, then encodes with the first
//! available libjxl backend: `cjxl` (the only backend that can also
//! transcode JPEG losslessly, bit-exact), ffmpeg with libjxl, then
//! ImageMagick. Success requires a JPEG XL codestream (`FF 0A`) or
//! container signature in the output.
//!
//! ```sh
//! cargo run --example convert_to_jxl -- [--lossless] <input> [output.jxl]
//! ```

use std::path::{Path, PathBuf};
use std::process::{exit, Command};

const CODESTREAM_MAGIC: &[u8] = b"\xff\x0a";
const CONTAINER_MAGIC: &[u8] = b"\x00\x00\x00\x0cJXL \x0d\x0a\x87\x0a";

fn is_jpeg_xl(path: &Path) -> bool {
    std::fs::read(path)
        .map(|b| b.starts_with(CODESTREAM_MAGIC) || b.starts_with(CONTAINER_MAGIC))
        .unwrap_or(false)
}

fn main() {
    let mut lossless = false;
    let mut positional: Vec<PathBuf> = Vec::new();
    for arg in std::env::args().skip(1) {
        if arg == "--lossless" {
            lossless = true;
        } else {
            positional.push(PathBuf::from(arg));
        }
    }
    if positional.is_empty() || positional.len() > 2 {
        eprintln!("usage: convert_to_jxl [--lossless] <input> [output.jxl]");
        exit(2);
    }
    let src = &positional[0];
    let dst = positional
        .get(1)
        .cloned()
        .unwrap_or_else(|| src.with_extension("jxl"));

    // Validate that this crate can decode the input before encoding it.
    if let Err(e) = image::open(src) {
        eprintln!("error: cannot decode {}: {e}", src.display());
        exit(1);
    }

    let (s, d) = (src.to_string_lossy().into_owned(), dst.to_string_lossy().into_owned());
    let attempts: Vec<Vec<String>> = vec![
        vec![
            "cjxl".into(), s.clone(), d.clone(), "--effort=7".into(),
            if lossless { "--lossless=1".into() } else { "--distance=1.0".into() },
        ],
        vec![
            "ffmpeg".into(), "-y".into(), "-i".into(), s.clone(), "-c:v".into(),
            "libjxl".into(), "-effort".into(), "7".into(),
            if lossless { "-lossless".into() } else { "-distance".into() },
            if lossless { "1".into() } else { "1.0".into() },
            d.clone(),
        ],
        vec!["magick".into(), s, d],
    ];
    for args in attempts {
        let ok = Command::new(&args[0]).args(&args[1..]).status().map(|s| s.success()).unwrap_or(false);
        if ok && is_jpeg_xl(&dst) {
            println!("{} -> {}", src.display(), dst.display());
            return;
        }
        let _ = std::fs::remove_file(&dst);
    }
    eprintln!("error: no working JPEG XL backend (install libjxl cjxl, ffmpeg --enable-libjxl, or ImageMagick with jxl delegate)");
    exit(1);
}
