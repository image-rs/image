use std::error::Error;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::LazyLock;

use image::{AnimationDecoder, ColorType, ImageFormat, RgbaImage};
use image::{DynamicImage, GenericImageView};
use libtest_mimic::{Arguments, Trial};
use walkdir::WalkDir;

static BLESSED: LazyLock<bool> = LazyLock::new(|| std::env::var("BLESS").is_ok());
static TEST_DIR: LazyLock<PathBuf> =
    LazyLock::new(|| Path::new(env!("CARGO_MANIFEST_DIR")).join("tests"));

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
fn main() {
    let mut trials = Vec::new();

    for case in list_test_cases() {
        let rel_image = case.image.strip_prefix(&*TEST_DIR).unwrap();
        let test_name = format!("test reference {}", rel_image.display());

        // we need both PNG and TIFF to be able to run reference tests, since
        // reference images are stored in those formats.
        if !ImageFormat::Png.reading_enabled() || !ImageFormat::Tiff.reading_enabled() {
            trials.push(Trial::test(test_name, || Ok(())).with_ignored_flag(true));
            continue;
        }

        // if the format of the test image isn't enabled, skip
        let format = ImageFormat::from_path(&case.image);
        if format.is_ok_and(|f| !f.reading_enabled()) {
            trials.push(Trial::test(test_name, || Ok(())).with_ignored_flag(true));
            continue;
        }

        trials.push(Trial::test(test_name, move || {
            // load image and check against reference
            let image =
                image::open(&case.image).map_err(|e| format!("Failed to open image: {}", e))?;
            check_image_reference(&case, image)?;

            // if the image is an animation, check each frame against its reference as well
            if let Some(frames) = decode_animation_frames(&case.image)? {
                for (index, frame) in frames.into_iter().enumerate() {
                    let frame_num = index + 1;
                    let frame = DynamicImage::from(frame);
                    let frame_case = TestCase {
                        image: case.image.clone(),
                        output: with_added_extensions(&case.output, &format!("{frame_num:02}")),
                        reference: with_added_extensions(
                            &case.reference,
                            &format!("{frame_num:02}"),
                        ),
                    };

                    check_image_reference(&frame_case, frame)
                        .map_err(|e| format!("Frame {frame_num}: {e}"))?;
                }
            }

            Ok(())
        }))
    }

    let args = Arguments::from_args();
    libtest_mimic::run(&args, trials).exit();
}

fn check_image_reference(case: &TestCase, image: DynamicImage) -> Result<(), Box<dyn Error>> {
    // save output for inspection
    // save_image(&image, &case.output)?;

    // load reference
    let reference_path = ["png", "tiff"]
        .iter()
        .map(|ext| with_added_extensions(&case.reference, ext))
        .find(|p| p.exists());
    if *BLESSED && reference_path.is_none() {
        // create new reference in bless mode
        save_image(&image, &case.reference)?;
        return Ok(());
    }
    let reference_path = reference_path.ok_or("Missing reference image")?;

    let reference =
        image::open(&reference_path).map_err(|e| format!("Failed to open reference image: {e}"))?;

    // compare to reference
    let cmp_result = compare_to_reference(&image, &reference);
    if *BLESSED && cmp_result.is_err() && fs::remove_file(&reference_path).is_ok() {
        // update reference in bless mode
        save_image(&image, &case.reference)?;
        return Ok(());
    }
    cmp_result?;

    Ok(())
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

    let image_dir = TEST_DIR.join("images");
    let reference_dir = TEST_DIR.join("reference");
    let output_dir = TEST_DIR.join("output");

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
