//! Compares the decoding results with reference renderings.
use std::collections::BTreeSet;
use std::fs;
use std::io;
use std::path::Path;
use std::path::PathBuf;
use std::str::FromStr;

use crc32fast::Hasher as Crc32;
use image::{DynamicImage, ImageFormat, ImageReader};
use libtest_mimic::{Arguments, Failed, Trial};
use walkdir::WalkDir;

fn main() {
    let mut trials = Vec::new();

    let image_dir = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("images");
    let reference_dir = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("reference");
    let output_dir = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("output");

    let mut images = BTreeSet::new();
    for entry in WalkDir::new(&image_dir) {
        let entry = entry.unwrap();
        if entry.file_type().is_file() {
            images.insert(entry.into_path());
        }
    }

    for entry in WalkDir::new(&reference_dir) {
        let entry = entry.unwrap();
        if !entry.file_type().is_file() || entry.path().extension() == Some("txt".as_ref()) {
            continue;
        }

        let relative_path = entry
            .path()
            .strip_prefix(&reference_dir)
            .unwrap()
            .to_path_buf();
        let Ok(case) = ReferenceTestCase::from_str(entry.file_name().to_str().unwrap()) else {
            trials.push(Trial::test(
                format!("reference_images {}", relative_path.display()),
                || Err("Malformed reference image filename".into()),
            ));
            continue;
        };
        let path = entry.into_path();

        let original_relative_path = relative_path.parent().unwrap().join(&case.orig_filename);
        let img_path = image_dir.join(&original_relative_path);

        images.remove(&img_path);

        let test_name = match case.kind {
            ReferenceTestKind::AnimatedFrame { frame } => format!(
                "reference_images {}[{}]",
                original_relative_path.display(),
                frame + 1
            ),
            ReferenceTestKind::SingleImage => {
                format!("reference_images {}", original_relative_path.display())
            }
        };

        let image_format = img_path
            .extension()
            .and_then(|ext| ImageFormat::from_extension(ext));
        let reference_format = relative_path
            .extension()
            .and_then(|ext| ImageFormat::from_extension(ext))
            .expect("reference image with unknown extension");

        if image_format.is_none() {
            trials.push(Trial::test(
                test_name,
                || Err("Unknown image format".into()),
            ));
            continue;
        }

        if !image_format.unwrap().reading_enabled() || !reference_format.reading_enabled() {
            trials.push(Trial::test(test_name, || Ok(())).with_ignored_flag(true));
            continue;
        }

        trials.push(Trial::test(test_name, move || -> Result<(), Failed> {
            // Load the test image
            let mut test_img = None;

            match case.kind {
                ReferenceTestKind::AnimatedFrame { frame: frame_num } => {
                    let format = ImageReader::open(&img_path)
                        .unwrap()
                        .with_guessed_format()
                        .unwrap()
                        .format();

                    #[cfg(feature = "gif")]
                    if format == Some(image::ImageFormat::Gif) {
                        // Interpret the input file as an animation file
                        use image::AnimationDecoder;
                        let stream = io::BufReader::new(fs::File::open(&img_path).unwrap());
                        let decoder = image::codecs::gif::GifDecoder::new(stream)?;
                        let mut frames = decoder.into_frames().collect_frames()?;

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
                        let decoder = image::codecs::png::PngDecoder::new(stream)?.apng()?;
                        let mut frames = decoder.into_frames().collect_frames()?;

                        // Select a single frame
                        let frame = frames.drain(frame_num..).next().unwrap();

                        // Convert the frame to a`RgbaImage`
                        test_img = Some(DynamicImage::from(frame.into_buffer()));
                    }

                    assert!(test_img.is_some());
                }

                ReferenceTestKind::SingleImage => {
                    // Read the input file as a single image
                    test_img = Some(image::open(&img_path)?);
                }
            }

            let Some(test_img) = test_img.as_mut() else {
                unreachable!()
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

            if reference_format == ImageFormat::Png && image_format == Some(ImageFormat::Tiff) {
                match test_img {
                    DynamicImage::ImageRgb32F(_) => {
                        *test_img = test_img.to_rgb16().into();
                    }
                    DynamicImage::ImageRgba32F(_) => {
                        *test_img = test_img.to_rgba16().into();
                    }
                    _ => {}
                }
            }

            if test_crc_actual != case.crc {
                if let Some(tmpdir) = std::env::var_os("TMPDIR") {
                    let filename =
                        format!("{}.{:08x}.{}", case.orig_filename, test_crc_actual, "png");
                    let filename = PathBuf::from(tmpdir).join(filename);
                    match test_img.save(&filename) {
                        Ok(()) => println!("New reference saved to: {}", filename.display()),
                        Err(e) => println!("Failed to save new reference: {e}"),
                    }
                }
                return Err(format!(
                    "The decoded image's hash does not match (expected = {:08x}, actual = {:08x})",
                    case.crc, test_crc_actual
                )
                .into());
            }

            let ref_img = image::open(&path)?;
            if ref_img.as_bytes() != test_img.as_bytes() {
                if let Some(tmpdir) = std::env::var_os("TMPDIR") {
                    let filename =
                        format!("{}.{:08x}.{}", case.orig_filename, test_crc_actual, "png");
                    let filename = PathBuf::from(tmpdir).join(filename);
                    match test_img.save(&filename) {
                        Ok(()) => println!("New reference saved to: {}", filename.display()),
                        Err(e) => println!("Failed to save new reference: {e}"),
                    }
                }
                return Err("Reference rendering does not match".into());
            };

            Ok(())
        }));
    }

    // for image_path in images {
    //     let Some(extension) = image_path.extension() else {
    //         continue;
    //     };
    //     let Some(format) = ImageFormat::from_extension(extension) else {
    //         continue;
    //     };

    //     let relative_path = image_path.strip_prefix(&image_dir).unwrap().to_path_buf();
    //     let directory = output_dir.join(relative_path.parent().unwrap()).to_owned();
    //     let test_name = format!("reference_images {}", relative_path.display());
    //     if !format.reading_enabled() {
    //         trials.push(Trial::test(test_name, || Ok(())).with_ignored_flag(true));
    //         continue;
    //     }

    //     trials.push(Trial::test(test_name, move || -> Result<(), Failed> {
    //         let img = image::open(&image_path)?;

    //         let mut crc = Crc32::new();
    //         crc.update(img.as_bytes());

    //         let use_tiff = matches!(img.color(), ColorType::Rgb32F | ColorType::Rgba32F);

    //         fs::create_dir_all(&directory).unwrap();

    //         let filename = format!(
    //             "{}.{:x}.{}",
    //             image_path.file_name().unwrap().to_str().unwrap(),
    //             crc.finalize(),
    //             if use_tiff { "tiff" } else { "png" }
    //         );
    //         img.save(directory.join(filename))?;

    //         // Ok(())
    //         Err("Missing reference file".into())
    //     }));
    // }

    let args = Arguments::from_args();
    libtest_mimic::run(&args, trials).exit();
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
