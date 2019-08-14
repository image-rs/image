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
    let decoders = &["tga", "tiff", "png", "gif", "bmp", "ico", "jpg", "hdr", "pbm", "webp"];
    for decoder in decoders {
        let mut path = base.clone();
        path.push(dir);
        path.push(decoder);
        path.push("**");
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
        println!("render_images {}", path.display());
        let img = match image::open(&path) {
            Ok(img) => img.to_rgba(),
            // Do not fail on unsupported error
            // This might happen because the testsuite contains unsupported images
            // or because a specific decoder included via a feature.
            Err(image::ImageError::UnsupportedError(e)) => {
                println!("UNSUPPORTED {}: {}", path.display(), e);
                return;
            }
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
        println!("check_references {}", path.display());

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

        let mut filename_parts = filename
            .as_os_str()
            .to_str()
            .unwrap()
            .split('.')
            .collect::<Vec<_>>();

        // Ignore the file extension
        filename_parts.pop().unwrap();

        // The penultimate part of `filename_parts` represents the metadata.
        let meta_str = filename_parts.pop().unwrap();
        let mut meta = meta_str.split('_').collect::<Vec<_>>();
        let ref_crc;
        let anim_frame: Option<usize>;

        if meta.len() == 1 {
            // `CRC`
            ref_crc = meta.pop().unwrap();
            anim_frame = None;
        } else if meta.len() == 3 && meta[0] == "anim" {
            // `anim_FRAME_CRC`
            ref_crc = meta.pop().unwrap();
            let frame: usize = meta.pop().unwrap().parse().unwrap();
            anim_frame = Some(frame.checked_sub(1).expect("frame number cannot be 0"));
        } else {
            panic!(
                "unrecognized reference image metadata format: {:?}",
                meta_str
            );
        }

        // The remaining part represents the original file name
        let orig_filename = filename_parts.join(".");

        // Check the reference image's CRC
        let ref_crc = u32::from_str_radix(ref_crc, 16).unwrap();

        let ref_crc_actual = {
            let mut hasher = Crc32::new();
            hasher.update(&*ref_img);
            hasher.finalize()
        };

        if ref_crc_actual != ref_crc {
            panic!(
                "Reference rendering hash check failed (expected = {:08x}, actual = {:08x}).",
                ref_crc, ref_crc_actual
            );
        }

        let mut img_path = base.clone();
        img_path.push(IMAGE_DIR);
        img_path.push(decoder);
        img_path.push(testsuite.as_os_str());
        img_path.push(orig_filename);

        // Load the test image
        let test_img;

        if let Some(anim_frame) = anim_frame {
            #[cfg(feature = "gif_codec")]
            {
                // Interpret the input file as an animation file and extract a
                // single frame
                use image::AnimationDecoder;
                let stream = io::BufReader::new(fs::File::open(&img_path).unwrap());
                let decoder = match image::gif::Decoder::new(stream) {
                    Ok(decoder) => decoder,
                    Err(image::ImageError::UnsupportedError(_)) => return,
                    Err(err) => panic!(format!("decoding of {:?} failed with: {}", img_path, err)),
                };

                let mut frames = match decoder.into_frames().collect_frames() {
                    Ok(frames) => frames,
                    Err(image::ImageError::UnsupportedError(_)) => return,
                    Err(err) => panic!(format!(
                        "collecting frames of {:?} failed with: {}",
                        img_path, err
                    )),
                };

                let frame = frames.drain(anim_frame..).nth(0).unwrap();

                // Convert the frame to a`RgbaImage`
                test_img = frame.into_buffer();
            }

            #[cfg(not(feature = "gif_codec"))]
            {
                println!("Skipping - GIF codec is not enabled");
                return;
            }
        } else {
            // Read the input file as a single image
            match image::open(&img_path) {
                Ok(img) => test_img = img.to_rgba(),
                // Do not fail on unsupported error
                // This might happen because the testsuite contains unsupported images
                // or because a specific decoder included via a feature.
                Err(image::ImageError::UnsupportedError(_)) => return,
                Err(err) => panic!(format!("decoding of {:?} failed with: {}", img_path, err)),
            };
        }

        let ref_img = coalesce_transparent_pixels(ref_img);
        let test_img = coalesce_transparent_pixels(test_img);

        //ref_img.save("/tmp/test-ref.png").unwrap();
        //test_img.save("/tmp/test-in.png").unwrap();

        if *ref_img != *test_img {
            panic!("Reference rendering does not match.");
        }
    })
}

/// Replace transparent pixel values with a single value. This is useful for
/// disregarding differences in the RGB values of completely transparent pixels.
fn coalesce_transparent_pixels(mut img: image::RgbaImage) -> image::RgbaImage {
    for p in img.pixels_mut() {
        if p[3] == 0 {
            *p = image::Rgba([0, 0, 0, 0]);
        }
    }
    img
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
    let path: PathBuf = BASE_PATH
        .iter()
        .collect::<PathBuf>()
        .join(IMAGE_DIR)
        .join("bmp/images")
        .join("*.bad_bmp");

    let pattern = &*format!("{}", path.display());
    for path in glob::glob(pattern).unwrap().filter_map(Result::ok) {
        let im = image::open(path);
        assert!(im.is_err());
    }
}
