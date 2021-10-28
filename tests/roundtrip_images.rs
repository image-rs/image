use std::path::PathBuf;
use tiff::encoder::ImageEncoder;
use image::{DynamicImage, ImageFormat};
use std::io::Cursor;

const BASE_PATH: [&str; 2] = [".", "tests"];

fn process_images(process_image: impl Fn(PathBuf)) {
    let base: PathBuf = BASE_PATH.iter().collect();
    let extensions = &["tga", "tiff", "png", "gif", "bmp", "ico", "jpg", "hdr", "pbm", "webp", "exr"];
    for extension in extensions {
        let mut path = base.clone();
        path.push("**");
        path.push("*.".to_string() + extension);
        let pattern = &*format!("{}", path.display());
        for path in glob::glob(pattern).unwrap().filter_map(Result::ok) {
            process_image(path)
        }
    }
}

#[test]
fn roundtrip() {
    process_images(|path| {
        println!("round trip test at path {:?}", path);

        if let Ok(decoded) = image::open(&path) {
            let format = ImageFormat::from_path(&path).unwrap(); // TODO get format from dynamic image?

            if format.can_write() {
                let mut byte_buffer = Vec::new();

                decoded.write_to(&mut byte_buffer, format)
                    .expect("writing failed");

                let re_decoded = image::load(Cursor::new(byte_buffer), format)
                    .expect("roundtrip decoding failed");

                assert_eq!(decoded, re_decoded); // TODO tolerance for float images
            }
            else {
                println!("Skipping read-only format {:?}", format);
            }
        }
        else {
            println!("Skipping invalid image at path {:?}", path);
        }
    })
}
