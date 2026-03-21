use image::{DynamicImage, GenericImageView, GrayImage, RgbImage, RgbaImage};
use std::{
    path::{Path, PathBuf},
    sync::OnceLock,
};

use libtest_mimic::{Arguments, Trial};

fn main() -> std::process::ExitCode {
    let mut trials = Vec::new();

    static IMAGES: OnceLock<[Image; 3]> = OnceLock::new();
    let [bw_edge, noise, cat] = IMAGES.get_or_init(|| {
        [
            Image::open(&tests_dir().join("assets/bw-edge.png")),
            Image::open(&tests_dir().join("assets/noise.png")),
            Image::open(&tests_dir().join("assets/cat.png")),
        ]
    });

    // blur & fast_blur with various sigmas
    for image in [bw_edge, noise] {
        for sigma in [0.1, 0.5, 1.0, 1.5, 2.0, 5.0, 10.0, 50.0] {
            add_trial(
                &mut trials,
                format!("blur/{} blur sigma={sigma:.1}", image.name),
                move || {
                    image::imageops::blur_advanced(
                        &image.rgb,
                        image::imageops::GaussianBlurParameters::new_from_sigma(sigma),
                    )
                },
            );
            add_trial(
                &mut trials,
                format!("blur/{} fast_blur sigma={sigma:.1}", image.name),
                move || image::imageops::fast_blur(&image.rgb, sigma),
            );
        }
    }

    // huerotate with various angles
    for angle in [0, 30, 180, -45] {
        add_trial(&mut trials, format!("huerotate angle={angle}"), move || {
            image::imageops::huerotate(&cat.rgba, angle)
        });
    }
    add_trial(&mut trials, "huerotate grayscale", move || {
        image::imageops::huerotate(&cat.gray, 180)
    });

    // invert
    add_trial(&mut trials, "invert", move || {
        let mut img = cat.rgba.clone();
        image::imageops::invert(&mut img);
        img
    });

    // filter3x3
    add_trial(&mut trials, "filter3x3 laplace", move || {
        image::imageops::filter3x3(&cat.rgb, &[1.0, 1.0, 1.0, 1.0, -8.0, 1.0, 1.0, 1.0, 1.0])
    });
    add_trial(&mut trials, "filter3x3 box blur", move || {
        image::imageops::filter3x3(&cat.rgb, &[1.0 / 9.0; 9])
    });
    add_trial(&mut trials, "filter3x3 sharpen", move || {
        image::imageops::filter3x3(&cat.rgb, &[0.0, -0.5, 0.0, -0.5, 3.0, -0.5, 0.0, -0.5, 0.0])
    });

    let args = Arguments::from_args();
    libtest_mimic::run(&args, trials).exit_code()
}

fn tests_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests")
}

fn add_trial<I: Into<DynamicImage>>(
    trials: &mut Vec<Trial>,
    name: impl AsRef<str>,
    f: impl FnOnce() -> I + Send + 'static,
) {
    if !cfg!(feature = "png") {
        trials.push(Trial::test(name.as_ref(), move || Ok(())).with_ignored_flag(true));
        return;
    }

    let path = tests_dir()
        .join("imageops")
        .join(format!("{}.png", name.as_ref()));
    trials.push(Trial::test(name.as_ref(), move || {
        let image = f().into();
        compare_to_output(&path, image);
        Ok(())
    }));
}

struct Image {
    name: String,
    rgba: RgbaImage,
    rgb: RgbImage,
    gray: GrayImage,
}
impl Image {
    fn open(path: &Path) -> Self {
        let name = path.file_stem().unwrap().to_str().unwrap().to_string();

        let image = if cfg!(feature = "png") {
            image::open(path).unwrap()
        } else {
            DynamicImage::new_rgb8(8, 8)
        };

        Self {
            name,
            rgba: image.to_rgba8(),
            rgb: image.to_rgb8(),
            gray: image.to_luma8(),
        }
    }
}

fn compare_to_output(path: &Path, image: DynamicImage) {
    let name = path.file_stem().unwrap().to_str().unwrap();

    if !path.exists() {
        #[cfg(feature = "png")]
        save_png(path, image);
        panic!("Saved output for {name} to {path:?}. Please verify it is correct and commit it.");
    }

    let reference = image::open(path).unwrap();
    assert_eq!(
        image.dimensions(),
        reference.dimensions(),
        "Output dimensions differ for {name}"
    );
    assert_eq!(
        image.as_bytes(),
        reference.as_bytes(),
        "Output pixel data differs for {name}"
    );
}
#[cfg(feature = "png")]
fn save_png(path: &Path, image: DynamicImage) {
    use image::codecs::png::{CompressionType, FilterType, PngEncoder};

    std::fs::create_dir_all(path.parent().unwrap()).unwrap();

    image
        .write_with_encoder(PngEncoder::new_with_quality(
            std::io::BufWriter::new(std::fs::File::create(path).unwrap()),
            CompressionType::Best,
            FilterType::Adaptive,
        ))
        .unwrap();
}
