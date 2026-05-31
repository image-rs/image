//! Test saving images to PNM.
#![cfg(all(feature = "png", feature = "pnm"))]

extern crate image;
use std::{fs::File, io::Read, path::Path};

fn compare_exact(dst_path: &str, ref_path: &str) {
    let mut output = Vec::new();
    let mut reference = Vec::new();
    File::open(dst_path)
        .unwrap()
        .read_to_end(&mut output)
        .unwrap();
    File::open(ref_path)
        .unwrap()
        .read_to_end(&mut reference)
        .unwrap();
    assert_eq!(output, reference);
}

/// Convert `src_path` to `dst_path`, and compare with the content of
/// `ref_path`.
fn convert_and_check(src_path: &str, dst_path: &str, ref_path: &str) {
    let img = image::open(src_path).unwrap();
    std::fs::create_dir_all(Path::new(dst_path).parent().unwrap()).unwrap();
    img.save(dst_path).unwrap();
    compare_exact(dst_path, ref_path);
}

fn convert_and_check_pbm(src_path: &str, dst_path: &str, ref_path: &str) {
    let img = image::open(src_path).unwrap();

    let mut img = img.to_luma8();
    img.iter_mut().for_each(|x| {
        *x = match *x {
            255 => 1,
            0 => 0,
            _ => unreachable!(),
        }
    });

    std::fs::create_dir_all(Path::new(dst_path).parent().unwrap()).unwrap();
    img.save(dst_path).unwrap();
    compare_exact(dst_path, ref_path);
}

#[test]
fn save_pbm() {
    convert_and_check_pbm(
        "tests/images/pnm/pbm/l1a.pbm",
        "tests/output/pnm/pbm/l1b.pbm",
        "tests/images/pnm/pbm/l1b.pnm",
    );
}

#[test]
fn save_pgm() {
    convert_and_check(
        "tests/images/pnm/pgm/l8a.pgm",
        "tests/output/pnm/pgm/l8b.pnm",
        "tests/images/pnm/pgm/l8b.pgm",
    );

    convert_and_check(
        "tests/images/pnm/pgm/l16a.pgm",
        "tests/output/pnm/pgm/l16b.pnm",
        "tests/images/pnm/pgm/l16b.pnm",
    );
}

#[test]
fn save_ppm() {
    convert_and_check(
        "tests/images/pnm/ppm/r8a.ppm",
        "tests/output/pnm/ppm/r8b.ppm",
        "tests/images/pnm/ppm/r8b.pnm",
    );

    convert_and_check(
        "tests/images/pnm/ppm/r16a.ppm",
        "tests/output/pnm/ppm/r8a.pnm",
        "tests/images/pnm/ppm/r16b.ppm",
    );
}

#[test]
fn save_pam() {
    convert_and_check(
        "tests/images/pnm/pam/ra16.pam",
        "tests/output/pnm/pam/ra16.pam",
        "tests/images/pnm/pam/ra16.pam",
    );

    convert_and_check(
        "tests/images/pnm/pam/la8.pam",
        "tests/output/pnm/pam/la8.pam",
        "tests/images/pnm/pam/la8.pam",
    );
}
