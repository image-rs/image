//! Compares the decoding results with reference renderings.
//!
//! This test harness automatically detects all reference images in
//! `tests/reference/...` and compares them to the associated file in
//! `tests/images/...`.

use std::fs;
use std::fs::File;
use std::io::{self, BufWriter};
use std::path::Path;
use std::str::FromStr;

use image::{ColorType, DynamicImage, ImageFormat};

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

        let test_name = match case.kind {
            ReferenceTestKind::AnimatedFrame { frame } => format!(
                "reference tests/images/{}[{}]",
                original_relative_path.display(),
                frame + 1
            ),
            ReferenceTestKind::SingleImage => {
                format!(
                    "reference tests/images/{}",
                    original_relative_path.display()
                )
            }
        };

        let image_format = img_path.extension().and_then(ImageFormat::from_extension);
        let reference_format = relative_path
            .extension()
            .and_then(ImageFormat::from_extension)
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

        let output_dir = output_dir.clone();
        trials.push(Trial::test(test_name, move || -> Result<(), Failed> {
            // Load the test image
            let mut test_img = match case.kind {
                ReferenceTestKind::SingleImage => {
                    // Read the input file as a single image
                    image::open(&img_path)?
                }
                ReferenceTestKind::AnimatedFrame { frame: frame_num } => {
                    // TODO: Once there's a generic API for animated images, switch to that instead.
                    match image_format {
                        #[cfg(feature = "gif")]
                        Some(image::ImageFormat::Gif) => {
                            // Interpret the input file as an animation file
                            let stream = io::BufReader::new(fs::File::open(&img_path).unwrap());
                            let decoder = image::codecs::gif::GifDecoder::new(stream)?;
                            let decoder = image::ImageReader::from_decoder(Box::new(decoder));
                            let mut frames = decoder.into_frames().collect_frames()?;

                            // Select a single frame
                            let frame = frames.drain(frame_num..).next().unwrap();

                            // Convert the frame to a`RgbaImage`
                            DynamicImage::from(frame.into_buffer())
                        }

                        #[cfg(feature = "png")]
                        Some(image::ImageFormat::Png) => {
                            // Interpret the input file as an animation file
                            let stream = io::BufReader::new(fs::File::open(&img_path).unwrap());
                            let decoder = image::codecs::png::PngDecoder::new(stream).apng()?;
                            let decoder = image::ImageReader::from_decoder(Box::new(decoder));
                            let mut frames = decoder.into_frames().collect_frames()?;

                            // Select a single frame
                            let frame = frames.drain(frame_num..).next().unwrap();

                            // Convert the frame to a`RgbaImage`
                            DynamicImage::from(frame.into_buffer())
                        }
                        _ => unreachable!(
                            "Format is unspported or disabled. Should have been detected earlier"
                        ),
                    }
                }
            };

            if reference_format == ImageFormat::Png && image_format == Some(ImageFormat::Tiff) {
                match test_img {
                    DynamicImage::ImageRgb32F(_) => {
                        test_img = test_img.to_rgb16().into();
                    }
                    DynamicImage::ImageRgba32F(_) => {
                        test_img = test_img.to_rgba16().into();
                    }
                    _ => {}
                }
            }

            let mut error: String = if image::open(&path)?.as_bytes() != test_img.as_bytes() {
                "Reference rendering does not match".into()
            } else {
                // The image exactly matches the reference. Success!
                return Ok(());
            };

            // The image doesn't match the reference. Save the decoded version to the
            // output directory for inspection.
            let ext = match test_img.color() {
                ColorType::Rgb32F | ColorType::Rgba32F => "tiff",
                _ => "png",
            };
            let output_filename = format!("{}.{ext}", case.orig_filename);
            let output_path = output_dir.join(output_filename);
            fs::create_dir_all(output_dir).unwrap();

            let ret = match test_img.color() {
                ColorType::Rgb32F | ColorType::Rgba32F => test_img.save(&output_path),
                #[cfg(feature = "png")]
                _ => {
                    use image::codecs::png::{CompressionType, FilterType, PngEncoder};
                    test_img.write_with_encoder(PngEncoder::new_with_quality(
                        BufWriter::new(File::create(&output_path).unwrap()),
                        CompressionType::Best,
                        FilterType::Adaptive,
                    ))
                }
                #[cfg(not(feature = "png"))]
                _ => unreachable!(),
            };

            match ret {
                Ok(()) => error.push_str(&format!(
                    "\n\n    New reference saved to: {}",
                    output_path.display()
                )),
                Err(e) => error.push_str(&format!("\n\n     Failed to save new reference: {e}")),
            }
            Err(error.into())
        }));
    }

    let args = Arguments::from_args();
    libtest_mimic::run(&args, trials).exit();
}

/// Describes a single test case of `check_references`.
struct ReferenceTestCase {
    orig_filename: String,
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
        let mut filename_parts: Vec<&str> = filename.split('.').collect();

        // Ignore the file extension
        filename_parts.pop();

        // Figure out if it's an animation. Uses the format `anim_<FRAME>`
        let mut kind = ReferenceTestKind::SingleImage;
        let last = *filename_parts.last().ok_or("missing metadata part")?;
        let meta = last.split('_').collect::<Vec<_>>();
        if meta.len() == 2 && meta[0] == "anim" {
            let frame: usize = meta[1].parse().map_err(|_| "malformed frame number")?;
            kind = ReferenceTestKind::AnimatedFrame {
                frame: frame.checked_sub(1).ok_or("frame number must be 1-based")?,
            };
            filename_parts.pop();
        }

        // The remaining part represents the original file name
        let orig_filename = filename_parts.join(".");

        Ok(Self {
            orig_filename,
            kind,
        })
    }
}
