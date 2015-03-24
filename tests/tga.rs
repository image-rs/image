#![cfg(feature = "tga")]

extern crate image;

use std::fs::{self, File};
use std::path::Path;

#[test]
fn test_open_and_save_tga() {
    let _ = fs::create_dir(&Path::new("./tests/output"));
    let path = Path::new("./tests/images/tga/testsuite/ctc24.tga");
    let img = image::open(&path).unwrap();
    let ref mut fout = File::create(&Path::new("./tests/output/tga-ctc24.png")).unwrap();
    let _ = img.save(fout, image::PNG);

    let path = Path::new("./tests/images/tga/testsuite/cbw8.tga");
    let img = image::open(&path).unwrap();
    let ref mut fout = File::create(&Path::new("./tests/output/tga-cbw8.png")).unwrap();
    let _ = img.save(fout, image::PNG);
}
