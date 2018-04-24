//! Ensure truncated images are read without panics.

use std::fs;
use std::io::Read;
use std::path::PathBuf;

extern crate glob;
extern crate image;

const BASE_PATH: [&'static str; 2] = [".", "tests"];
const IMAGE_DIR: &'static str = "images";

fn process_images<F>(dir: &str, input_decoder: Option<&str>, func: F)
where
    F: Fn(PathBuf),
{
    let base: PathBuf = BASE_PATH.iter().collect();
    let decoders = &["tga", "tiff", "png", "gif", "bmp", "ico", "jpg", "hdr"];
    for decoder in decoders {
        let mut path = base.clone();
        path.push(dir);
        path.push(decoder);
        path.push("*");
        path.push(
            "*.".to_string() + match input_decoder {
                Some(val) => val,
                None => decoder,
            },
        );
        let pattern = &*format!("{}", path.display());
        for path in glob::glob(pattern).unwrap().filter_map(Result::ok) {
            func(path)
        }
    }
}

fn truncate_images(decoder: &str) {
    process_images(IMAGE_DIR, Some(decoder), |path| {
        println!("{:?}", path);
        let fin = fs::File::open(&path).unwrap();
        let max_length = 1000;
        let mut buf = Vec::with_capacity(max_length);
        fin.take(max_length as u64).read_to_end(&mut buf).unwrap();
        for i in 0..buf.len() {
            image::load_from_memory(&buf[..i + 1]).ok();
        }
    })
}

#[test]
#[ignore]
fn truncate_tga() {
    truncate_images("tga")
}

#[test]
#[ignore]
fn truncate_tiff() {
    truncate_images("tiff")
}

#[test]
#[ignore]
fn truncate_png() {
    truncate_images("png")
}

#[test]
#[ignore]
fn truncate_gif() {
    truncate_images("gif")
}

#[test]
#[ignore]
fn truncate_bmp() {
    truncate_images("bmp")
}

#[test]
#[ignore]
fn truncate_ico() {
    truncate_images("ico")
}

#[test]
#[ignore]
fn truncate_jpg() {
    truncate_images("jpg")
}

#[test]
#[ignore]
fn truncate_hdr() {
    truncate_images("hdr");
}
