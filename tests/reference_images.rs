use std::collections::BTreeMap;
use std::error::Error;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::{LazyLock, Mutex};

use image::{AnimationDecoder, ColorType, ImageFormat, RgbaImage};
use image::{DynamicImage, GenericImageView};
use libtest_mimic::{Arguments, Trial};
use walkdir::WalkDir;

static BLESSED: LazyLock<bool> = LazyLock::new(|| std::env::var("BLESS").is_ok());

static TEST_DIR: LazyLock<PathBuf> =
    LazyLock::new(|| Path::new(env!("CARGO_MANIFEST_DIR")).join("tests"));
static IMAGE_DIR: LazyLock<PathBuf> = LazyLock::new(|| TEST_DIR.join("images"));
static REFERENCE_DIR: LazyLock<PathBuf> = LazyLock::new(|| TEST_DIR.join("reference"));

static HASH_PATH: LazyLock<PathBuf> = LazyLock::new(|| TEST_DIR.join("hashes.toml"));
static HASHES: LazyLock<HashFile> = LazyLock::new(|| HashFile::open(&HASH_PATH).unwrap());

/// Test decoding of all test images in `tests/images/` against their hashes in
/// `hashes.toml` and reference images (either PNG or TIFF) in `tests/reference/`.
///
/// ## Bless mode
///
/// If the `BLESS` environment variable is set, the test will create/update all
/// missing/mismatching hashes and reference images.
///
/// To add a new test image, simply add it to `tests/images/` and run the
/// following command:
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
            "reference {}",
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

        // QOI is currently broken on BE architectures, so we have to skip.
        // See https://github.com/image-rs/image/issues/2808 for details.
        // TODO: Remove this stupid hack once QOI is fixed.
        if format == ImageFormat::Qoi && cfg!(target_endian = "big") {
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
    let hash_key = reference_path
        .strip_prefix(&*REFERENCE_DIR)
        .unwrap()
        .with_extension("") // remove reference image extension
        .to_string_lossy()
        .replace(std::path::MAIN_SEPARATOR, "/"); // use UNIX separators

    // check hash
    let hash = HASHES.get(&hash_key);
    let image_hash = format!("{:x}", crc32fast::hash(image.as_bytes()));
    if hash != Some(&image_hash) {
        if !*BLESSED {
            return Err(
                "Decoded image changed!\nMissing or mismatching pixel data hash. Try running tests in blessed mode.".into(),
            );
        }
        edit_image_hash(&hash_key, &image)?;
    }

    // check reference image
    if !reference_path.exists() {
        // missing reference images are probably just ignored via .gitignore.
        // so just create them in blessed mode
        if *BLESSED {
            return save_image(&image, reference_path, true);
        }
        return Ok(());
    }

    // load reference
    let reference =
        image::open(reference_path).map_err(|e| format!("Failed to open reference image: {e}"))?;

    // compare to reference
    let cmp_result = compare_to_reference(&image, &reference);
    if cmp_result.is_err() && *BLESSED {
        // update reference in bless mode
        return save_image(&image, reference_path, true);
    }
    cmp_result?;

    Ok(())
}

fn save_image(image: &DynamicImage, path: &Path, optimize: bool) -> Result<(), Box<dyn Error>> {
    fs::create_dir_all(path.parent().unwrap())?;
    image.save(path)?;

    // optimize PNGs with oxipng to reduce the size of reference images in the repo
    if optimize && path.extension().is_some_and(|ext| ext == "png") {
        oxipng::optimize(
            &oxipng::InFile::Path(path.to_path_buf()),
            &oxipng::OutFile::from_path(path.to_path_buf()),
            &oxipng::Options::from_preset(4),
        )?;
    }

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

struct HashFile {
    data: BTreeMap<String, String>,
}
impl HashFile {
    fn open(path: &Path) -> Result<Self, Box<dyn Error>> {
        let contents = fs::read_to_string(path)?;
        let data = toml::from_str(&contents)?;
        Ok(Self { data })
    }
    fn save(&self, path: &Path) -> Result<(), Box<dyn Error>> {
        let contents = toml::to_string(&self.data)?;
        fs::write(path, contents)?;
        Ok(())
    }

    fn get(&self, key: &str) -> Option<&String> {
        self.data.get(key)
    }

    fn set(&mut self, key: String, value: String) {
        self.data.insert(key, value);
    }
}

static HASH_WRITE_LOCK: Mutex<()> = Mutex::new(());
fn edit_image_hash(key: &str, image: &DynamicImage) -> Result<(), Box<dyn Error>> {
    let hash = format!("{:x}", crc32fast::hash(image.as_bytes()));
    let guard = HASH_WRITE_LOCK.lock().unwrap();
    let mut hash_file = HashFile::open(&HASH_PATH)?;
    hash_file.set(key.to_string(), hash);
    hash_file.save(&HASH_PATH)?;
    std::mem::drop(guard);
    Ok(())
}
