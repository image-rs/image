use std::collections::HashSet;
use std::fs;
use std::path::{Path, PathBuf};

use image::{AnimationDecoder, ColorType, ImageFormat, RgbaImage};
use image::{DynamicImage, GenericImageView};
use walkdir::WalkDir;

/// Tests that all images in `tests/bad/` fail to decode.
#[test]
fn bad_images() {
    let images = iter_image_path_in(&test_dir().join("bad"));

    for image in images {
        let rel_image = image.strip_prefix(test_dir()).unwrap();
        match image::open(&image) {
            Ok(_) => {
                panic!("❌ Bad image decoded successfully: {}", rel_image.display());
            }
            Err(_) => {
                // expected
            }
        }
    }
}

/// Test decoding of all test images in `tests/images/` against reference images
/// (either PNG or TIFF) in `tests/reference/`. This will also write the decoded
/// images to `tests/output/` regardless of whether they match the reference, to
/// allow for inspection.
///
/// ## Bless mode
///
/// If the `BLESS` environment variable is set, the test will update the reference images
/// to match the newly decoded images. This is useful when adding new test images,
/// updating existing ones, or seeing the output of incorrectly decoded images.
///
/// ```sh
/// BLESS=1 cargo test
/// ```
/// ```powershell
/// $env:BLESS=1; cargo test; $env:BLESS=$null
/// ```
#[test]
#[cfg(all(feature = "png", feature = "tiff"))] // we need both formats to be able to open+save reference images
fn check_references() {
    let bless = std::env::var("BLESS").is_ok();

    let reference_images = std::sync::Mutex::new(vec![]);

    let check_image_reference =
        |case: &TestCase, image: DynamicImage| -> Result<(), Box<dyn std::error::Error>> {
            // track which reference images are used, to detect unused ones.
            reference_images
                .lock()
                .unwrap()
                .push(case.reference.clone());

            // save output for inspection
            save_image(&image, &case.output)?;

            // load reference
            let reference_path = ["png", "tiff"]
                .iter()
                .map(|ext| with_added_extensions(&case.reference, ext))
                .find(|p| p.exists());
            if bless && reference_path.is_none() {
                // create new reference in bless mode
                save_image(&image, &case.reference)?;
                return Ok(());
            }
            let reference_path = reference_path.ok_or("Missing reference image")?;

            let reference = image::open(&reference_path)
                .map_err(|e| format!("Failed to open reference image: {e}"))?;

            // compare to reference
            let cmp_result = compare_to_reference(&image, &reference);
            if bless && cmp_result.is_err() && fs::remove_file(&reference_path).is_ok() {
                // update reference in bless mode
                save_image(&image, &case.reference)?;
            }
            cmp_result?;

            Ok(())
        };

    let check_test_case = |case: &TestCase| -> Result<(), Box<dyn std::error::Error>> {
        // load image
        let image = image::open(&case.image).map_err(|e| format!("Failed to open image: {}", e))?;
        check_image_reference(case, image)
    };

    let check_test_case_animation = |case: &TestCase| -> Result<(), Box<dyn std::error::Error>> {
        // load frames
        let Some(frames) = decode_animation_frames(&case.image)? else {
            // images that aren't animations don't matter for this test.
            return Ok(());
        };

        for (index, frame) in frames.into_iter().enumerate() {
            let frame_num = index + 1;
            let frame = DynamicImage::from(frame);
            let frame_case = TestCase {
                image: case.image.clone(),
                output: with_added_extensions(&case.output, &format!("{frame_num:02}")),
                reference: with_added_extensions(&case.reference, &format!("{frame_num:02}")),
            };

            check_image_reference(&frame_case, frame)
                .map_err(|e| format!("Frame {frame_num}: {e}"))?;
        }

        Ok(())
    };

    let mut partial_format_support = false;
    let test_dir = test_dir();
    let mut errors = vec![];
    for test_case in list_test_cases() {
        let format = ImageFormat::from_path(&test_case.image);
        if let Ok(format) = format {
            if !format.reading_enabled() {
                partial_format_support = true;
                continue; // skip formats whose features aren't enabled.
            }
        }

        let rel_image = test_case.image.strip_prefix(&test_dir).unwrap();

        if let Err(e) = check_test_case(&test_case) {
            errors.push(format!("❌ {}\n     {e}", rel_image.display()));
        }
        if let Err(e) = check_test_case_animation(&test_case) {
            errors.push(format!("❌ (animation) {}\n     {e}", rel_image.display()));
        }
    }

    if !errors.is_empty() {
        panic!("Errors in references:\n{}", errors.join("\n"));
    }

    // if there are no errors, check for unused reference images
    if partial_format_support {
        println!("⚠️ Some formats were skipped due to missing features, so unused reference images won't be checked.");
        return;
    }
    let used_references: HashSet<PathBuf> =
        reference_images.into_inner().unwrap().into_iter().collect();
    let unused_references: Vec<PathBuf> = iter_image_path_in(&test_dir.join("reference"))
        .filter(|p| {
            // saving as an image adds the extension of the file format we save as (typically .png),
            // so we need to remove that extension to get back the original reference path.
            let ref_path = p.with_extension("");
            !used_references.contains(&ref_path)
        })
        .collect();
    if !unused_references.is_empty() {
        let mut list = String::new();
        for path in &unused_references {
            list += &format!("\n    {}", path.strip_prefix(&test_dir).unwrap().display());
        }
        panic!(
            "⚠️ Unused reference images ({}):{}\n",
            unused_references.len(),
            list
        );
    }
}

fn test_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests")
}

fn save_image(image: &DynamicImage, path: &Path) -> Result<(), Box<dyn std::error::Error>> {
    let format = match image.color() {
        ColorType::Rgb32F | ColorType::Rgba32F => ImageFormat::Tiff,
        _ => ImageFormat::Png,
    };

    if !format.writing_enabled() {
        // nothing we can do if the feature isn't enabled.
        return Ok(());
    }

    let output_path = with_added_extensions(path, format.extensions_str()[0]);
    fs::create_dir_all(output_path.parent().unwrap())?;
    image.save_with_format(&output_path, format)?;

    Ok(())
}

/// Returns a list of all test cases.
fn list_test_cases() -> Vec<TestCase> {
    let mut cases = Vec::new();

    let image_dir = test_dir().join("images");
    let reference_dir = test_dir().join("reference");
    let output_dir = test_dir().join("output");

    for image in iter_image_path_in(&image_dir) {
        let rel = image.strip_prefix(&image_dir).unwrap().to_path_buf();

        let output = output_dir.join(&rel);
        let reference = reference_dir.join(&rel);

        cases.push(TestCase {
            image,
            output,
            reference,
        });
    }

    cases
}

struct TestCase {
    /// Absolute path to the test image.
    image: PathBuf,
    /// Absolute path to the output image.
    output: PathBuf,
    /// Absolute path to the reference image.
    reference: PathBuf,
}

fn iter_image_path_in(dir: &Path) -> impl Iterator<Item = PathBuf> {
    WalkDir::new(dir)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| e.file_type().is_file())
        .map(|e| e.into_path())
        .filter(|p| p.extension().is_some_and(|ext| ext != "txt" && ext != "md"))
}

fn compare_to_reference(image: &DynamicImage, reference: &DynamicImage) -> Result<(), String> {
    if image.dimensions() != reference.dimensions() {
        return Err(format!(
            "Reference dimension mismatch: image is {:?}, reference is {:?}",
            image.dimensions(),
            reference.dimensions()
        ));
    }

    if image.color() == reference.color() {
        // same color type, just compare the raw bytes
        if image.as_bytes() == reference.as_bytes() {
            return Ok(());
        } else {
            return Err("Reference pixel data mismatch".to_string());
        }
    }

    // convert to a common color type
    #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
    enum Precision {
        U8,
        U16,
        F32,
    }
    impl Precision {
        fn of(color: ColorType) -> Self {
            match color {
                ColorType::L8 | ColorType::La8 | ColorType::Rgb8 | ColorType::Rgba8 => Self::U8,
                ColorType::L16 | ColorType::La16 | ColorType::Rgb16 | ColorType::Rgba16 => {
                    Self::U16
                }
                ColorType::Rgb32F | ColorType::Rgba32F | _ => Self::F32,
            }
        }
    }
    fn to_rgba_precision(image: &DynamicImage, precision: Precision) -> DynamicImage {
        match precision {
            Precision::U8 => image.to_rgba8().into(),
            Precision::U16 => image.to_rgba16().into(),
            Precision::F32 => image.to_rgba32f().into(),
        }
    }

    let image_precision = Precision::of(image.color());
    let reference_precision = Precision::of(reference.color());
    let common_precision = image_precision.max(reference_precision);

    let image = to_rgba_precision(image, common_precision);
    let reference = to_rgba_precision(reference, common_precision);
    assert_eq!(image.color(), reference.color());

    if image.as_bytes() == reference.as_bytes() {
        Ok(())
    } else {
        Err("Reference pixel data mismatch".to_string())
    }
}

// TODO: Use Path::with_added_extension once MSRV is 1.91.0
fn with_added_extensions(path: &Path, extension: &str) -> PathBuf {
    let mut new_path = path.to_owned();
    new_path.set_file_name(
        new_path.file_name().unwrap().to_string_lossy().to_string() + "." + extension,
    );
    new_path
}

fn decode_animation_frames(
    path: &Path,
) -> Result<Option<Vec<RgbaImage>>, Box<dyn std::error::Error>> {
    let format = ImageFormat::from_path(path)?;
    if !format.reading_enabled() {
        return Ok(None); // nothing we can do if the feature isn't enabled.
    }

    let reader = std::io::BufReader::new(std::fs::File::open(path)?);
    let frames: image::Frames<'_> = match format {
        #[cfg(feature = "gif")]
        ImageFormat::Gif => image::codecs::gif::GifDecoder::new(reader)?.into_frames(),
        #[cfg(feature = "png")]
        ImageFormat::Png => {
            let decoder = image::codecs::png::PngDecoder::new(reader)?;
            if !decoder.is_apng()? {
                return Ok(None);
            }
            decoder.apng()?.into_frames()
        }
        #[cfg(feature = "webp")]
        ImageFormat::WebP => {
            let decoder = image::codecs::webp::WebPDecoder::new(reader)?;
            if !decoder.has_animation() {
                return Ok(None);
            }
            decoder.into_frames()
        }
        _ => {
            return Ok(None); // format doesn't support animations
        }
    };
    let frames = frames.collect_frames()?;
    if frames.len() < 2 {
        return Ok(None); // not actually animated
    }
    Ok(Some(frames.into_iter().map(|f| f.into_buffer()).collect()))
}
