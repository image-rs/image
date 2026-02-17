use std::fs::{self, File};
use std::io::{BufReader, Cursor};
use std::path::PathBuf;

#[cfg(feature = "webp")]
use image::{codecs::webp::WebPDecoder, ImageReader};

const BASE_PATH: [&str; 2] = [".", "tests"];
const IMAGE_DIR: &str = "images";
const REGRESSION_DIR: &str = "regression";

fn process_images<F>(dir: &str, input_decoder: Option<&str>, func: F)
where
    F: Fn(PathBuf),
{
    let base: PathBuf = BASE_PATH.iter().collect();
    let decoders = &[
        "tga", "tiff", "png", "gif", "bmp", "ico", "jpg", "hdr", "pbm", "webp", "exr",
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
            func(path);
        }
    }
}

#[test]
fn check_regressions() {
    process_images(REGRESSION_DIR, None, |path| {
        let _ = image::open(path);
    });
}

/// Check that the WEBP frames iterator returns the right amount of frames.
#[test]
#[cfg(feature = "webp")]
fn check_webp_frames_regressions() {
    let path: PathBuf = BASE_PATH
        .iter()
        .collect::<PathBuf>()
        .join(IMAGE_DIR)
        .join("webp/extended_images")
        .join("*.webp");
    let pattern = &*format!("{}", path.display());
    for path in glob::glob(pattern).unwrap().filter_map(Result::ok) {
        let bytes = fs::read(&path).unwrap();
        let cursor = Cursor::new(&bytes);
        let frame_count = image_webp::WebPDecoder::new(cursor.clone())
            .unwrap()
            .num_frames() as usize;
        let decoder = WebPDecoder::new(cursor).unwrap();
        let reader = ImageReader::from_decoder(Box::new(decoder));
        // The `take` guards against a potentially infinitely running iterator.
        // Since we take `frame_count + 1`, we can assume that the last iteration already returns `None`.
        // We then check that each frame has been decoded successfully.
        let decoded_frames_count = reader
            .into_frames()
            .take(frame_count + 1)
            .enumerate()
            .inspect(|(frame_index, frame_res)| {
                if let Err(e) = frame_res {
                    panic!("Error decoding {path:?} frame {}: {e:?}", frame_index + 1);
                }
            })
            .count();
        assert_eq!(frame_count, decoded_frames_count);
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
        // Manually reading the file so we can use load() instead of open()
        // We have to use load() so we can override the format
        let im_file = BufReader::new(File::open(path).unwrap());
        let im: Result<image::DynamicImage, image::ImageError> =
            image::load(im_file, image::ImageFormat::Bmp);
        assert!(im.is_err());
    }
}

/// Regression test for ICO files containing BMP V5 images with embedded ICC profiles.
/// The ICC profile offset in BITMAPV5HEADER is relative to the DIB header start, so it
/// should work correctly when embedded in ICO files (which don't have a BITMAPFILEHEADER).
/// This test verifies the image can be fully decoded without errors.
#[cfg(feature = "ico")]
#[test]
fn ico_bmp_v5_with_icc_profile() {
    let path = BASE_PATH
        .iter()
        .collect::<PathBuf>()
        .join(IMAGE_DIR)
        .join("ico/images")
        .join("bmp_v5_with_icc.ico");

    // Verify the image can be opened and decoded without errors
    let img = image::open(&path).expect("Failed to open ICO with BMP V5 ICC profile");

    // Verify the image has expected dimensions (127x64 from rgb24prof.bmp)
    assert_eq!(img.width(), 127);
    assert_eq!(img.height(), 64);
}

// Test that BMP bitmaps with extra `BI_BITFIELD` values are parsed correctly.
//
// The test data comes from a `CF_DIBV5` bitmap on the Windows clipboard. It is a screenshot
// of a Rust function in VSCode with syntax highlighting enabled (dark theme). When parsed correctly the entire left
// side of the image should be grey pixels (the VSCode editor background color). When it isn't read correctly,
// some of the yellow from the function's name highlighting on the right is shifted into this left region.
#[cfg(feature = "bmp")]
#[test]
fn bmp_bitfields() {
    let path = BASE_PATH
        .iter()
        .collect::<PathBuf>()
        .join(IMAGE_DIR)
        .join("bmp/raw")
        .join("windows_dibv5_dump.bin");
    let im_file = BufReader::new(File::open(path).unwrap());
    let decoder = image::codecs::bmp::BmpDecoder::new_without_file_header(im_file).unwrap();

    let im_buffer = image::DynamicImage::from_decoder(decoder)
        .unwrap()
        .into_rgba8()
        .into_raw();

    // All of the pixels in this range should be grey with no transparency.
    // Before parsing was fixed, there are pixels like `[220, 220, 175 255]` which are
    // a yellow-ish that shouldn't be there.
    assert!(im_buffer[19880..19960]
        .iter()
        .copied()
        .all(|b| b == 31 || b == 255));
}

#[test]
fn bad_gif_oom() {
    let data = &[
        71, 73, 70, 56, 55, 97, 0, 0, 0, 0, 0, 0, 0, 44, 255, 255, 219, 255, 172, 199, 199, 255,
        216, 255, 255, 0, 0, 48, 230, 2, 195, 195, 195, 195, 255, 239, 0,
    ];

    // The original code made a vec![0; very_large_number] which due to overcommit *does not* OOM.
    // It then exits normally with an EOF when reading.
    //
    // So instead we look for a limits error (or an unsupported error, for the case that we're
    // running these tests without gif being actually supported)
    let error = image::load_from_memory(data).unwrap_err();

    assert!(
        matches!(error, image::ImageError::Limits(_))
            | matches!(error, image::ImageError::Unsupported(_))
    );
}

#[test]
#[cfg(feature = "png")]
fn resizing_with_alpha() {
    use image::imageops::FilterType;
    use image::GenericImageView as _;

    let base: PathBuf = BASE_PATH.iter().collect();
    let image =
        image::ImageReader::open(base.join("regression/image/resize-with-alpha-original.png"))
            .unwrap()
            .decode()
            .unwrap();

    let (w, h) = image.dimensions();
    let mut resizable = image.clone();
    resizable.resize_exact(2 * w, 2 * h, FilterType::Nearest);
    resizable.resize_exact(w, h, FilterType::Nearest);

    assert_eq!(image, resizable);
}

/// NOTE: this test is a little experimental. There's no reason to assume that the values should be
/// exact but also we may want to know if they diverge. If that happens too often we should find a
/// better (more visual) way to test this. For instance, `imagemagick` disagrees
///
/// ```bash
/// magick convert regression/image/resize-with-alpha-original.png -filter CatmullRom -resize 50% \
///   regression/image/resize-with-alpha-original-half-size-imagemagick.png
/// ```
///
/// But also the output of magick appears noticeably darkened in the cursive `Image` part of the
/// test whereas ours is closer to the original color. Not sure what causes this (might be
/// difference in the color space used during interpolation) but ours seems good.
#[test]
#[cfg(feature = "png")]
fn resizing_with_catmul() {
    use image::imageops::FilterType;
    use image::GenericImageView as _;

    let base: PathBuf = BASE_PATH.iter().collect();
    let image =
        image::ImageReader::open(base.join("regression/image/resize-with-alpha-original.png"))
            .unwrap()
            .decode()
            .unwrap();

    let expected = image::ImageReader::open(
        base.join("regression/image/resize-with-alpha-original-half-size.png"),
    )
    .unwrap()
    .decode()
    .unwrap();

    let (w, h) = image.dimensions();
    let (nw, nh) = (w.div_ceil(2), h.div_ceil(2));
    let mut resizable = image.clone();
    resizable.resize_exact(nw, nh, FilterType::CatmullRom);
    assert_eq!(resizable, expected);
}
