//! Compares the decoding results with reference renderings.
use std::fs;
use std::io;
use std::path::PathBuf;
use std::u32;

use crc32fast::Hasher as Crc32;
use image::DynamicImage;

const BASE_PATH: [&str; 2] = [".", "tests"];
const IMAGE_DIR: &str = "images";
const OUTPUT_DIR: &str = "output";
const REFERENCE_DIR: &str = "reference";

fn process_images<F>(dir: &str, input_decoder: Option<&str>, func: F)
where
    F: Fn(&PathBuf, PathBuf, &str),
{
    let base: PathBuf = BASE_PATH.iter().collect();
    let decoders = &[
        "tga", "tiff", "png", "gif", "bmp", "ico", "jpg", "hdr", "pbm", "webp",
    ];
    for decoder in decoders {
        let mut path = base.clone();
        path.push(dir);
        path.push(decoder);
        path.push("**");
        path.push(
            "*.".to_string()
                + match input_decoder {
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
            Ok(img) => img,
            // Do not fail on unsupported error
            // This might happen because the testsuite contains unsupported images
            // or because a specific decoder included via a feature.
            Err(image::ImageError::Unsupported(e)) => {
                println!("UNSUPPORTED {}: {}", path.display(), e);
                return;
            }
            Err(err) => panic!("decoding of {:?} failed with: {}", path, err),
        };
        let mut crc = Crc32::new();
        crc.update(img.as_bytes());

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
            "{}.{:x}.png",
            filename.as_os_str().to_str().unwrap(),
            crc.finalize(),
        ));
        img.save(out_path).unwrap();
    })
}

/// Describes a single test case of `check_references`.
struct ReferenceTestCase {
    orig_filename: String,
    crc: u32,
    kind: ReferenceTestKind,
}

enum ReferenceTestKind {
    /// The test image is loaded using `image::open`, and the result is compared
    /// against the reference image.
    SingleImage,

    /// From the test image file, a single frame is extracted using a fitting animation decoder and
    /// the result is compared against the reference image.
    AnimatedFrame {
        /// A zero-based frame number.
        frame: usize,
    },
}

impl std::str::FromStr for ReferenceTestCase {
    type Err = &'static str;

    /// Construct `ReferenceTestCase` from the file name of a reference
    /// image.
    fn from_str(filename: &str) -> Result<Self, Self::Err> {
        let mut filename_parts = filename.rsplitn(3, '.');

        // Ignore the file extension
        filename_parts.next().unwrap();

        // The penultimate part of `filename_parts` represents the metadata,
        // describing the test type and other details.
        let meta_str = filename_parts.next().ok_or("missing metadata part")?;
        let meta = meta_str.split('_').collect::<Vec<_>>();
        let (crc, kind);

        if meta.len() == 1 {
            // `CRC`
            crc = parse_crc(meta[0]).ok_or("malformed CRC")?;
            kind = ReferenceTestKind::SingleImage;
        } else if meta.len() == 3 && meta[0] == "anim" {
            // `anim_FRAME_CRC`
            crc = parse_crc(meta[2]).ok_or("malformed CRC")?;
            let frame: usize = meta[1].parse().map_err(|_| "malformed frame number")?;
            kind = ReferenceTestKind::AnimatedFrame {
                frame: frame.checked_sub(1).ok_or("frame number must be 1-based")?,
            };
        } else {
            return Err("unrecognized reference image metadata format");
        }

        // The remaining part represents the original file name
        let orig_filename = filename_parts
            .next()
            .ok_or("missing original file name")?
            .to_owned();

        Ok(Self {
            orig_filename,
            crc,
            kind,
        })
    }
}

/// Parse the given string as a hexadecimal CRC hash, used by `check_references`.
fn parse_crc(src: &str) -> Option<u32> {
    u32::from_str_radix(src, 16).ok()
}

#[test]
fn check_references() {
    process_images(REFERENCE_DIR, Some("png"), |base, path, decoder| {
        println!("check_references {}", path.display());

        let ref_img = match image::open(&path) {
            Ok(img) => img,
            // Do not fail on unsupported error
            // This might happen because the testsuite contains unsupported images
            // or because a specific decoder included via a feature.
            Err(image::ImageError::Unsupported(_)) => return,
            Err(err) => panic!("{}", err),
        };

        let (filename, testsuite) = {
            let mut path: Vec<_> = path.components().collect();
            (path.pop().unwrap(), path.pop().unwrap())
        };

        // Parse the file name to obtain the test case information
        let filename_str = filename.as_os_str().to_str().unwrap();
        let case: ReferenceTestCase = filename_str.parse().unwrap();

        let mut img_path = base.clone();
        img_path.push(IMAGE_DIR);
        img_path.push(decoder);
        img_path.push(testsuite.as_os_str());
        img_path.push(case.orig_filename);

        // Load the test image
        let mut test_img = None;

        match case.kind {
            ReferenceTestKind::AnimatedFrame { frame: frame_num } => {
                let format = image::io::Reader::open(&img_path)
                    .unwrap()
                    .with_guessed_format()
                    .unwrap()
                    .format();

                #[cfg(feature = "gif")]
                if format == Some(image::ImageFormat::Gif) {
                    // Interpret the input file as an animation file
                    use image::AnimationDecoder;
                    let stream = io::BufReader::new(fs::File::open(&img_path).unwrap());
                    let decoder = match image::codecs::gif::GifDecoder::new(stream) {
                        Ok(decoder) => decoder,
                        Err(image::ImageError::Unsupported(_)) => return,
                        Err(err) => {
                            panic!("decoding of {:?} failed with: {}", img_path, err)
                        }
                    };

                    let mut frames = match decoder.into_frames().collect_frames() {
                        Ok(frames) => frames,
                        Err(image::ImageError::Unsupported(_)) => return,
                        Err(err) => {
                            panic!("collecting frames of {:?} failed with: {}", img_path, err)
                        }
                    };

                    // Select a single frame
                    let frame = frames.drain(frame_num..).next().unwrap();

                    // Convert the frame to a`RgbaImage`
                    test_img = Some(DynamicImage::from(frame.into_buffer()));
                }

                #[cfg(feature = "png")]
                if format == Some(image::ImageFormat::Png) {
                    // Interpret the input file as an animation file
                    use image::AnimationDecoder;
                    let stream = io::BufReader::new(fs::File::open(&img_path).unwrap());
                    let decoder = match image::codecs::png::PngDecoder::new(stream) {
                        Ok(decoder) => decoder.apng(),
                        Err(image::ImageError::Unsupported(_)) => return,
                        Err(err) => {
                            panic!("decoding of {:?} failed with: {}", img_path, err)
                        }
                    };

                    let mut frames = match decoder.into_frames().collect_frames() {
                        Ok(frames) => frames,
                        Err(image::ImageError::Unsupported(_)) => return,
                        Err(err) => {
                            panic!("collecting frames of {:?} failed with: {}", img_path, err)
                        }
                    };

                    // Select a single frame
                    let frame = frames.drain(frame_num..).next().unwrap();

                    // Convert the frame to a`RgbaImage`
                    test_img = Some(DynamicImage::from(frame.into_buffer()));
                }

                if test_img.is_none() {
                    println!("Skipping - GIF codec is not enabled");
                    return;
                }
            }

            ReferenceTestKind::SingleImage => {
                // Read the input file as a single image
                match image::open(&img_path) {
                    Ok(img) => test_img = Some(img),
                    // Do not fail on unsupported error
                    // This might happen because the testsuite contains unsupported images
                    // or because a specific decoder included via a feature.
                    Err(image::ImageError::Unsupported(_)) => return,
                    Err(err) => panic!("decoding of {:?} failed with: {}", img_path, err),
                };
            }
        }

        let test_img = match test_img.as_ref() {
            Some(img) => img,
            None => return,
        };

        let test_crc_actual = {
            let mut hasher = Crc32::new();
            match test_img {
                DynamicImage::ImageLuma8(_)
                | DynamicImage::ImageLumaA8(_)
                | DynamicImage::ImageRgb8(_)
                | DynamicImage::ImageRgba8(_) => hasher.update(test_img.as_bytes()),
                DynamicImage::ImageLuma16(_)
                | DynamicImage::ImageLumaA16(_)
                | DynamicImage::ImageRgb16(_)
                | DynamicImage::ImageRgba16(_) => {
                    for v in test_img.as_bytes().chunks(2) {
                        hasher.update(&u16::from_ne_bytes(v.try_into().unwrap()).to_le_bytes());
                    }
                }
                DynamicImage::ImageRgb32F(_) | DynamicImage::ImageRgba32F(_) => {
                    for v in test_img.as_bytes().chunks(4) {
                        hasher.update(&f32::from_ne_bytes(v.try_into().unwrap()).to_le_bytes());
                    }
                }
                _ => panic!("Unsupported image format"),
            }
            hasher.finalize()
        };

        if test_crc_actual != case.crc {
            panic!(
                "{}: The decoded image's hash does not match (expected = {:08x}, actual = {:08x}).",
                img_path.display(),
                case.crc,
                test_crc_actual
            );
        }

        if ref_img.as_bytes() != test_img.as_bytes() {
            panic!("Reference rendering does not match.");
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
        for c in path
            .components()
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
        let decoder =
            image::codecs::hdr::HdrDecoder::new(io::BufReader::new(fs::File::open(&path).unwrap()))
                .unwrap();
        let decoded = decoder.read_image_hdr().unwrap();
        let reference = image::codecs::hdr::read_raw_file(&ref_path).unwrap();
        assert_eq!(decoded, reference);
    }
}
