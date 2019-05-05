//! Compares the decoding results with reference renderings.

extern crate crc32fast;
extern crate glob;
extern crate image;

use std::fs;
use std::io;
use std::path::PathBuf;
use std::u32;

use crc32fast::Hasher as Crc32;

const BASE_PATH: [&str; 2] = [".", "tests"];
const IMAGE_DIR: &str = "images";
const OUTPUT_DIR: &str = "output";
const REFERENCE_DIR: &str = "reference";

fn process_images<F>(dir: &str, input_decoder: Option<&str>, func: F)
where
    F: Fn(&PathBuf, PathBuf, &str),
{
    let base: PathBuf = BASE_PATH.iter().collect();
    let decoders = &["tga", "tiff", "png", "gif", "bmp", "ico", "jpg", "hdr", "pbm"];
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
            func(&base, path, decoder)
        }
    }
}

#[cfg(feature = "png")]
#[test]
fn render_images() {
    process_images(IMAGE_DIR, None, |base, path, decoder| {
        let img = match image::open(&path) {
            Ok(img) => img.to_rgba(),
            // Do not fail on unsupported error
            // This might happen because the testsuite contains unsupported images
            // or because a specific decoder included via a feature.
            Err(image::ImageError::UnsupportedError(_)) => return,
            Err(err) => panic!(format!("decoding of {:?} failed with: {}", path, err)),
        };
        let mut crc = Crc32::new();
        crc.update(&*img);

        let (filename, testsuite) = {
            let mut path: Vec<_> = path.components().collect();
            (path.pop().unwrap(), path.pop().unwrap())
        };
        let mut out_path = base.clone();

        out_path.push(OUTPUT_DIR);
        out_path.push(decoder);
        out_path.push(testsuite.as_os_str());
        fs::create_dir_all(&out_path).unwrap();
        out_path.push(format!(
            "{}.{}.{}",
            filename.as_os_str().to_str().unwrap(),
            format!("{:x}", crc.finalize()),
            "png"
        ));
        img.save(out_path).unwrap();
    })
}

#[test]
fn check_references() {
    process_images(REFERENCE_DIR, Some("png"), |base, path, decoder| {
        let ref_img = match image::open(&path) {
            Ok(img) => img.to_rgba(),
            // Do not fail on unsupported error
            // This might happen because the testsuite contains unsupported images
            // or because a specific decoder included via a feature.
            Err(image::ImageError::UnsupportedError(_)) => return,
            Err(err) => panic!(format!("{}", err)),
        };

        let (filename, testsuite) = {
            let mut path: Vec<_> = path.components().collect();
            (path.pop().unwrap(), path.pop().unwrap())
        };
        let mut img_path = base.clone();
        img_path.push(IMAGE_DIR);
        img_path.push(decoder);
        img_path.push(testsuite.as_os_str());
        img_path.push(
            filename
                .as_os_str()
                .to_str()
                .unwrap()
                .split('.')
                .take(2)
                .collect::<Vec<_>>()
                .join("."),
        );
        let ref_crc = u32::from_str_radix(
            filename
                .as_os_str()
                .to_str()
                .unwrap()
                .split('.')
                .nth(2)
                .unwrap(),
            16,
        ).unwrap();
        let test_img = match image::open(&img_path) {
            Ok(img) => img.to_rgba(),
            // Do not fail on unsupported error
            // This might happen because the testsuite contains unsupported images
            // or because a specific decoder included via a feature.
            Err(image::ImageError::UnsupportedError(_)) => return,
            Err(err) => panic!(format!("decoding of {:?} failed with: {}", path, err)),
        };
        let mut test_crc = Crc32::new();
        test_crc.update(&*test_img);
        if *ref_img != *test_img || test_crc.finalize() != ref_crc {
            panic!(
                "Reference rendering does not match for image at {:?}.",
                img_path
            )
        }
    })
}

#[cfg(feature = "hdr")]
#[test]
fn check_hdr_references() {
    let mut ref_path: PathBuf = BASE_PATH.iter().collect();
    ref_path.push(REFERENCE_DIR);
    ref_path.push("hdr");
    let mut path: PathBuf = BASE_PATH.iter().collect();
    path.push(IMAGE_DIR);
    path.push("hdr");
    path.push("*");
    path.push("*.hdr");
    let pattern = &*format!("{}", path.display());
    for path in glob::glob(pattern).unwrap().filter_map(Result::ok) {
        use std::path::Component::Normal;
        let mut ref_path = ref_path.clone();
        // append 2 last components of image path to reference path
        for c in path.components()
            .rev()
            .take(2)
            .collect::<Vec<_>>()
            .iter()
            .rev()
        {
            match *c {
                Normal(name) => ref_path.push(name),
                _ => panic!(),
            }
        }
        ref_path.set_extension("raw");
        println!("{}", ref_path.display());
        println!("{}", path.display());
        let decoder = image::hdr::HDRDecoder::new(io::BufReader::new(
            fs::File::open(&path).unwrap(),
        )).unwrap();
        let decoded = decoder.read_image_hdr().unwrap();
        let reference = image::hdr::read_raw_file(&ref_path).unwrap();
        assert_eq!(decoded, reference);
    }
}

/// Check that BMP files with large values could cause OOM issues are rejected.
///
/// The images are postfixed with `bad_bmp` to not be loaded by the other test.
#[test]
fn bad_bmps() {
    let base_path: PathBuf = BASE_PATH
        .iter()
        .collect::<PathBuf>()
        .join(IMAGE_DIR)
        .join("bmp/images");

    assert!(
        image::open(base_path.join("Bad_clrsUsed.bad_bmp")).is_err(),
        "Image with absurdly large number of colors loaded."
    );
    assert!(
        image::open(base_path.join("Bad_width.bad_bmp")).is_err(),
        "Image with absurdly large width loaded."
    );
    assert!(
        image::open(base_path.join("Bad_height.bad_bmp")).is_err(),
        "Image with absurdly large height loaded."
    );
}
