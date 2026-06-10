//! Ensure truncated images are read without panics.

use std::fs;
use std::io::Read;
use std::path::PathBuf;

use image::ImageFormat;

extern crate glob;
extern crate image;

const BASE_PATH: [&str; 2] = [".", "tests"];
const IMAGE_DIR: &str = "images";

fn process_images<F>(dir: &str, input_format: ImageFormat, func: F)
where
    F: Fn(PathBuf),
{
    let base: PathBuf = BASE_PATH.iter().collect();
    let mut path = base.clone();
    path.push(dir);
    path.push("**");
    path.push("*");
    let pattern = &*format!("{}", path.display());
    for path in glob::glob(pattern)
        .unwrap()
        .filter_map(Result::ok)
        .filter(|p| p.is_file())
        .filter(|p| ImageFormat::from_path(p).ok() == Some(input_format))
    {
        func(path);
    }
}

fn truncate_images(format: ImageFormat) {
    process_images(IMAGE_DIR, format, |path| {
        println!("{path:?}");
        let fin = fs::File::open(&path).unwrap();
        let max_length = 1000;
        #[allow(clippy::disallowed_methods)]
        let mut buf = Vec::with_capacity(max_length);
        fin.take(max_length as u64).read_to_end(&mut buf).unwrap();
        for i in (0..buf.len()).step_by(37) {
            image::load_from_memory(&buf[..=i]).ok();
        }
    });
}

#[test]
fn truncate_tga() {
    truncate_images(ImageFormat::Tga);
}

#[test]
fn truncate_tiff() {
    truncate_images(ImageFormat::Tiff);
}

#[test]
fn truncate_png() {
    truncate_images(ImageFormat::Png);
}

#[test]
fn truncate_gif() {
    truncate_images(ImageFormat::Gif);
}

#[test]
fn truncate_bmp() {
    truncate_images(ImageFormat::Bmp);
}

#[test]
fn truncate_ico() {
    truncate_images(ImageFormat::Ico);
}

#[test]
fn truncate_jpg() {
    truncate_images(ImageFormat::Jpeg);
}

#[test]
fn truncate_hdr() {
    truncate_images(ImageFormat::Hdr);
}

#[test]
fn truncate_farbfeld() {
    truncate_images(ImageFormat::Farbfeld);
}

#[test]
fn truncate_exr() {
    truncate_images(ImageFormat::OpenExr);
}

#[test]
fn truncate_webp() {
    truncate_images(ImageFormat::WebP);
}

#[test]
fn truncate_qoi() {
    truncate_images(ImageFormat::Qoi);
}

#[test]
fn truncate_pnm() {
    truncate_images(ImageFormat::Pnm);
}

#[test]
fn truncate_avif() {
    truncate_images(ImageFormat::Avif);
}
