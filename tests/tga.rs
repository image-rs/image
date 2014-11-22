extern crate image;

use std::io::{fs, File, USER_RWX};

#[test]
fn test_open_and_save_tga() {
    let _ = fs::mkdir(&Path::new("./tests/output"), USER_RWX);
    let path = Path::new("./tests/images/tga/testsuite/ctc24.tga");
    let img = image::open(&path).unwrap();
    let fout = File::create(&Path::new("./tests/output/tga-ctc24.png")).unwrap();
    let _ = img.save(fout, image::PNG);

    let path = Path::new("./tests/images/tga/testsuite/cbw8.tga");
    let img = image::open(&path).unwrap();
    let fout = File::create(&Path::new("./tests/output/tga-cbw8.png")).unwrap();
    let _ = img.save(fout, image::PNG);
}
