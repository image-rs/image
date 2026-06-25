use image::{ExtendedColorType, ImageEncoder};

const ALL_COLORS: &[ExtendedColorType] = &[
    ExtendedColorType::A8,
    ExtendedColorType::L1,
    ExtendedColorType::La1,
    ExtendedColorType::Rgb1,
    ExtendedColorType::Rgba1,
    ExtendedColorType::L2,
    ExtendedColorType::La2,
    ExtendedColorType::Rgb2,
    ExtendedColorType::Rgba2,
    ExtendedColorType::L4,
    ExtendedColorType::La4,
    ExtendedColorType::Rgb4,
    ExtendedColorType::Rgba4,
    ExtendedColorType::Rgb5x1,
    ExtendedColorType::L8,
    ExtendedColorType::La8,
    ExtendedColorType::Rgb8,
    ExtendedColorType::Rgba8,
    ExtendedColorType::L16,
    ExtendedColorType::La16,
    ExtendedColorType::Rgb16,
    ExtendedColorType::Rgba16,
    ExtendedColorType::Bgr8,
    ExtendedColorType::Bgra8,
    ExtendedColorType::L32F,
    ExtendedColorType::La32F,
    ExtendedColorType::Rgb32F,
    ExtendedColorType::Rgba32F,
    ExtendedColorType::Cmyk8,
    ExtendedColorType::Cmyk16,
    ExtendedColorType::YCbCr8,
];

/// Verify that encoders' reported supported colors match the colors they can actually encode.
fn verify_supported_colors<E: ImageEncoder>(f: impl Fn() -> E) {
    let reference = f();
    let Some(supported_colors) = reference.supported_colors() else {
        // If the encoder doesn't report supported colors, we can't verify anything
        return;
    };

    let width = 8;
    let height = 8;

    for &color in ALL_COLORS {
        let buf = vec![0_u8; color.buffer_size(width, height) as usize];
        let result = f().write_image(&buf, width, height, color);
        let actually_supported = result.is_ok();
        let claim_supported = supported_colors.contains(&color);

        if actually_supported && !claim_supported {
            panic!(
                "Encoder claimed to not support color {:?} but was able to encode it",
                color
            );
        } else if !actually_supported && claim_supported {
            panic!(
                "Encoder claimed to support color {:?} but failed to encode it: {:?}",
                color,
                result.err()
            );
        }
    }
}

fn writer() -> impl std::io::Write + std::io::Seek {
    std::io::Cursor::new(Vec::new())
}

#[cfg(feature = "avif")]
#[test]
fn test_encoder_avif() {
    verify_supported_colors(|| image::codecs::avif::AvifEncoder::new(writer()));
}

#[cfg(feature = "bmp")]
#[test]
fn test_encoder_bmp() {
    verify_supported_colors(|| image::codecs::bmp::BmpEncoder::new(writer()));
}

#[cfg(feature = "exr")]
#[test]
fn test_encoder_exr() {
    verify_supported_colors(|| image::codecs::openexr::OpenExrEncoder::new(writer()));
}

#[cfg(feature = "ff")]
#[test]
fn test_encoder_farbfeld() {
    verify_supported_colors(|| image::codecs::farbfeld::FarbfeldEncoder::new(writer()));
}

#[cfg(feature = "gif")]
#[test]
fn test_encoder_gif() {
    verify_supported_colors(|| image::codecs::gif::GifEncoder::new(writer()));
}

#[cfg(feature = "hdr")]
#[test]
fn test_encoder_hdr() {
    verify_supported_colors(|| image::codecs::hdr::HdrEncoder::new(writer()));
}

#[cfg(feature = "ico")]
#[test]
fn test_encoder_ico() {
    verify_supported_colors(|| image::codecs::ico::IcoEncoder::new(writer()));
}

#[cfg(feature = "jpeg")]
#[test]
fn test_encoder_jpeg() {
    verify_supported_colors(|| image::codecs::jpeg::JpegEncoder::new(writer()));
}

#[cfg(feature = "png")]
#[test]
fn test_encoder_png() {
    verify_supported_colors(|| image::codecs::png::PngEncoder::new(writer()));
}

#[cfg(feature = "pnm")]
#[test]
fn test_encoder_pnm() {
    verify_supported_colors(|| image::codecs::pnm::PnmEncoder::new(writer()));
}

#[cfg(feature = "qoi")]
#[test]
fn test_encoder_qoi() {
    verify_supported_colors(|| image::codecs::qoi::QoiEncoder::new(writer()));
}

#[cfg(feature = "tga")]
#[test]
fn test_encoder_tga() {
    verify_supported_colors(|| image::codecs::tga::TgaEncoder::new(writer()));
}

#[cfg(feature = "tiff")]
#[test]
fn test_encoder_tiff() {
    verify_supported_colors(|| image::codecs::tiff::TiffEncoder::new(writer()));
}

#[cfg(feature = "webp")]
#[test]
fn test_encoder_webp() {
    verify_supported_colors(|| image::codecs::webp::WebPEncoder::new_lossless(writer()));
}
