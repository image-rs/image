use std::fs::{self, File};
use std::io::{BufReader, Cursor};
use std::path::PathBuf;

#[cfg(feature = "webp")]
use image::{codecs::webp::WebPDecoder, AnimationDecoder};

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
        // The `take` guards against a potentially infinitely running iterator.
        // Since we take `frame_count + 1`, we can assume that the last iteration already returns `None`.
        // We then check that each frame has been decoded successfully.
        let decoded_frames_count = decoder
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
        let im = image::load(im_file, image::ImageFormat::Bmp);
        assert!(im.is_err());
    }
}

#[test]
fn bad_gif_oom() {
    let data = [
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
