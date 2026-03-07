//! Compares the decoding results with reference renderings.
//!
//! This test harness automatically detects all reference images in
//! `tests/images/...` and compares them to the associated file in
//! `tests/reference/...`.

use std::error::Error;
use std::fs::File;
use std::io::BufWriter;
use std::path::{Path, PathBuf};

use image::{ColorType, GenericImageView};
use image::{DynamicImage, ImageFormat};
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

    for entry in WalkDir::new(&image_dir) {
        let entry = entry.unwrap();
        if !entry.file_type().is_file()
            || entry.path().extension().is_none()
            || entry.path().extension() == Some("txt".as_ref())
        {
            continue;
        }

        let relative_path = entry.path().strip_prefix(&image_dir).unwrap().to_path_buf();
        let test_name = format!("image {}", relative_path.display());

        let image_path = entry.into_path();
        let output_base_path = output_dir.join(&relative_path);
        let reference_base_path = reference_dir.join(&relative_path);

        let Ok(image_format) = ImageFormat::from_path(&image_path) else {
            trials.push(Trial::test(
                test_name,
                || Err("Unknown image format".into()),
            ));
            continue;
        };

        // ignore if we can't read the test image
        if !image_format.reading_enabled() {
            trials.push(Trial::test(test_name, || Ok(())).with_ignored_flag(true));
            continue;
        }
        // ignore if we can't read the reference image
        if !ImageFormat::Png.reading_enabled() || !ImageFormat::Tiff.reading_enabled() {
            trials.push(Trial::test(test_name, || Ok(())).with_ignored_flag(true));
            continue;
        }
        // QOI is broken on big endian targets
        // See https://github.com/image-rs/image/issues/2808
        if image_format == ImageFormat::Qoi && cfg!(target_endian = "big") {
            trials.push(Trial::test(test_name, || Ok(())).with_ignored_flag(true));
            continue;
        }

        trials.push(Trial::test(test_name, move || -> Result<(), Failed> {
            // Load the test image
            let mut image = image::open(&image_path)?;

            if image_format == ImageFormat::Tiff
                && matches!(image.color(), ColorType::Rgb32F | ColorType::Rgba32F)
            {
                // The 32-bit image are stored as TIFF in references. So if we
                // were to do the same for TIFF test images, we would use the
                // same decoder for both test and references images, defeating
                // the purpose of this test. Instead, we'll convert 32-bit TIFF
                // test images to 16-bit and store them as PNG.
                image = match image.color() {
                    ColorType::Rgb32F => image.to_rgb16().into(),
                    ColorType::Rgba32F => image.to_rgba16().into(),
                    _ => unreachable!(),
                };
            }

            if let Err(e) = compare_to_reference(
                &image,
                &get_reference_path(&reference_base_path, None, &image),
            ) {
                // The image doesn't match the reference. Save the decoded version to the
                // output directory for inspection.
                let output_path = get_reference_path(&output_base_path, None, &image);
                save_image(&output_path, &image)?;
                return Err(e.into());
            }

            // Load animation
            if let Some(frames) = open_animation(&image_path)? {
                for (i, frame) in frames.into_iter().enumerate() {
                    let frame_number = i + 1;
                    if let Err(e) = compare_to_reference(
                        &frame,
                        &get_reference_path(&reference_base_path, Some(frame_number), &frame),
                    ) {
                        // The frame doesn't match the reference. Save the decoded version to the
                        // output directory for inspection.
                        let output_path =
                            get_reference_path(&output_base_path, Some(frame_number), &frame);
                        save_image(&output_path, &frame)?;
                        return Err(format!("Frame {frame_number}: {e}").into());
                    }
                }
            }

            Ok(())
        }));
    }

    let args = Arguments::from_args();
    libtest_mimic::run(&args, trials).exit();
}

fn get_reference_path(
    base_path: &Path,
    frame_number: Option<usize>,
    image: &DynamicImage,
) -> PathBuf {
    let mut suffix = String::from(".");
    if let Some(frame_number) = frame_number {
        suffix += &format!("anim_{frame_number:02}.");
    }

    let ext = match image.color() {
        ColorType::Rgb32F | ColorType::Rgba32F => "tiff",
        _ => "png",
    };
    suffix += ext;

    base_path.with_file_name(format!(
        "{}{}",
        base_path.file_name().unwrap().to_string_lossy(),
        suffix
    ))
}

fn compare_to_reference(image: &DynamicImage, reference_path: &Path) -> Result<(), Box<dyn Error>> {
    if !reference_path.exists() {
        return Err("Missing reference image".into());
    }

    // load reference
    let reference =
        image::open(reference_path).map_err(|e| format!("Failed to open reference image: {e}"))?;

    // compare
    if image.dimensions() != reference.dimensions() {
        return Err(format!(
            "Dimension mismatch: image {:?} vs reference {:?}",
            image.dimensions(),
            reference.dimensions()
        )
        .into());
    }
    if image.as_bytes() != reference.as_bytes() {
        return Err("Reference image does not match".into());
    }

    Ok(())
}

fn save_image(path: &Path, image: &DynamicImage) -> Result<(), Box<dyn Error>> {
    std::fs::create_dir_all(path.parent().unwrap())?;

    // special path for PNG to apply more compression
    #[cfg(feature = "png")]
    if path.extension() == Some("png".as_ref()) {
        use image::codecs::png::{CompressionType, FilterType, PngEncoder};
        image.write_with_encoder(PngEncoder::new_with_quality(
            BufWriter::new(File::create(path).unwrap()),
            CompressionType::Best,
            FilterType::Adaptive,
        ))?;
        return Ok(());
    }

    image.save(path)?;
    Ok(())
}

/// Returns `Some` with a list of animation frames if the file is an
/// animation, or `None` otherwise.
fn open_animation(path: &Path) -> Result<Option<Vec<DynamicImage>>, Box<dyn Error>> {
    use image::AnimationDecoder;

    let format = ImageFormat::from_path(path)?;
    assert!(
        format.reading_enabled(),
        "Format should have been detected earlier"
    );

    // TODO: Once there's a generic API for animated images, switch to that instead.
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
    Ok(Some(
        frames
            .into_iter()
            .map(|f| DynamicImage::from(f.into_buffer()))
            .collect(),
    ))
}
