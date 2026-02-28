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
static IMAGE_DIR: LazyLock<PathBuf> = LazyLock::new(|| TEST_DIR.join("images"));
static REFERENCE_DIR: LazyLock<PathBuf> = LazyLock::new(|| TEST_DIR.join("reference"));
static OUTPUT_DIR: LazyLock<PathBuf> = LazyLock::new(|| TEST_DIR.join("output"));

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

    for image_path in image_paths_in(&IMAGE_DIR) {
        let test_name = format!(
            "test reference {}",
            image_path.strip_prefix(&*TEST_DIR).unwrap().display()
        );

        // we need both PNG and TIFF to be able to run reference tests, since
        // reference images are stored in those formats.
        if !ImageFormat::Png.reading_enabled() || !ImageFormat::Tiff.reading_enabled() {
            trials.push(Trial::test(test_name, || Ok(())).with_ignored_flag(true));
            continue;
        }

        // fail if the test image is of an unknown format
        let Ok(format) = ImageFormat::from_path(&image_path) else {
            trials.push(Trial::test(
                test_name,
                || Err("Unknown image format".into()),
            ));
            continue;
        };

        // skip if the format of the test image isn't enabled
        if !format.reading_enabled() {
            trials.push(Trial::test(test_name, || Ok(())).with_ignored_flag(true));
            continue;
        }

        trials.push(Trial::test(test_name, move || {
            // load image and check against reference
            let image =
                image::open(&image_path).map_err(|e| format!("Failed to open image: {}", e))?;

            let reference_path = get_reference_path(&image_path, None, &image);
            check_image_reference(&reference_path, image)?;

            // if the image is an animation, check each frame against its reference as well
            if let Some(frames) = decode_animation_frames(&image_path)? {
                for (index, frame) in frames.into_iter().enumerate() {
                    let frame_num = index + 1;
                    let frame = DynamicImage::from(frame);

                    let reference_path = get_reference_path(&image_path, Some(frame_num), &frame);
                    check_image_reference(&reference_path, frame)
                        .map_err(|e| format!("Frame {frame_num}: {e}"))?;
                }
            }

            Ok(())
        }))
    }

    let args = Arguments::from_args();
    libtest_mimic::run(&args, trials).exit();
}

fn check_image_reference(reference_path: &Path, image: DynamicImage) -> Result<(), Box<dyn Error>> {
    // save output for inspection
    let output_path = OUTPUT_DIR.join(reference_path.strip_prefix(&*REFERENCE_DIR).unwrap());
    save_image(&image, &output_path)?;

    // check if reference exists
    if !reference_path.exists() {
        if *BLESSED {
            // create new reference in bless mode
            return save_image(&image, reference_path);
        }

        return Err("Missing reference image".into());
    }

    // load reference
    let reference =
        image::open(reference_path).map_err(|e| format!("Failed to open reference image: {e}"))?;

    // compare to reference
    let cmp_result = compare_to_reference(&image, &reference);
    if cmp_result.is_err() && *BLESSED {
        // update reference in bless mode
        return save_image(&image, reference_path);
    }
    cmp_result?;

    Ok(())
}

fn save_image(image: &DynamicImage, path: &Path) -> Result<(), Box<dyn Error>> {
    fs::create_dir_all(path.parent().unwrap())?;
    image.save(path)?;
    Ok(())
}

fn get_save_format(image: &DynamicImage) -> ImageFormat {
    match image.color() {
        ColorType::Rgb32F | ColorType::Rgba32F => ImageFormat::Tiff,
        _ => ImageFormat::Png,
    }
}

fn get_reference_path(
    image_path: &Path,
    frame_num: Option<usize>,
    image: &DynamicImage,
) -> PathBuf {
    // TODO: Use PathBuf::add_extension once MSRV is 1.91.0
    fn add_extension(path: &mut PathBuf, extension: &str) {
        path.set_file_name(
            path.file_name().unwrap().to_string_lossy().to_string() + "." + extension,
        );
    }

    let relative_path = image_path.strip_prefix(&*IMAGE_DIR).unwrap();
    let mut reference_path = REFERENCE_DIR.join(relative_path);

    if let Some(frame_num) = frame_num {
        add_extension(&mut reference_path, &format!("{frame_num:02}"));
    }
    add_extension(
        &mut reference_path,
        get_save_format(image).extensions_str()[0],
    );

    reference_path
}

fn image_paths_in(dir: &Path) -> impl Iterator<Item = PathBuf> {
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

    // fast path
    if image.color() == reference.color() && image.as_bytes() == reference.as_bytes() {
        return Ok(());
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
        Err("Mismatching pixel data".into())
    }
}

fn decode_animation_frames(path: &Path) -> Result<Option<Vec<RgbaImage>>, Box<dyn Error>> {
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
