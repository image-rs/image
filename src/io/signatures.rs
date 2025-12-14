use std::sync::RwLock;

use crate::ImageFormat;

type Sig = (ImageFormat, &'static [u8], &'static [u8]);

static SIGNATURES: RwLock<Vec<Sig>> = RwLock::new(Vec::new());

fn write_signatures<R>(f: impl FnOnce(&mut Vec<Sig>) -> R) -> R {
    let mut registry_lock = SIGNATURES.write().unwrap();
    if registry_lock.is_empty() {
        registry_lock.extend(BUILTIN_MAGIC_BYTES.iter().copied());
    }
    f(&mut registry_lock)
}
fn read_signatures<R>(f: impl FnOnce(&[Sig]) -> R) -> R {
    let registry_lock = SIGNATURES.read().unwrap();
    // the happy path (and common path) is that the registry is already initialized
    if !registry_lock.is_empty() {
        f(registry_lock.as_slice())
    } else {
        drop(registry_lock);
        write_signatures(|registry| f(registry))
    }
}

const BUILTIN_MAGIC_BYTES: &[Sig] = &[
    (ImageFormat::Png, b"\x89PNG\r\n\x1a\n", b""),
    (ImageFormat::Jpeg, &[0xff, 0xd8, 0xff], b""),
    (ImageFormat::Gif, b"GIF89a", b""),
    (ImageFormat::Gif, b"GIF87a", b""),
    (
        ImageFormat::WebP,
        b"RIFF\0\0\0\0WEBP",
        b"\xFF\xFF\xFF\xFF\0\0\0\0",
    ),
    (ImageFormat::Tiff, b"MM\x00*", b""),
    (ImageFormat::Tiff, b"II*\x00", b""),
    (ImageFormat::Dds, b"DDS ", b""),
    (ImageFormat::Bmp, b"BM", b""),
    (ImageFormat::Ico, &[0, 0, 1, 0], b""),
    (ImageFormat::Hdr, b"#?RADIANCE", b""),
    (ImageFormat::Avif, b"\0\0\0\0ftypavif", b"\xFF\xFF\0\0"),
    (ImageFormat::OpenExr, &[0x76, 0x2f, 0x31, 0x01], b""), // = &exr::meta::magic_number::BYTES
    (ImageFormat::Qoi, b"qoif", b""),
    (ImageFormat::Pnm, b"P1", b""),
    (ImageFormat::Pnm, b"P2", b""),
    (ImageFormat::Pnm, b"P3", b""),
    (ImageFormat::Pnm, b"P4", b""),
    (ImageFormat::Pnm, b"P5", b""),
    (ImageFormat::Pnm, b"P6", b""),
    (ImageFormat::Pnm, b"P7", b""),
    (ImageFormat::Farbfeld, b"farbfeld", b""),
];

pub(crate) fn register_signature(sig: Sig) {
    write_signatures(move |signatures| signatures.push(sig));
}

pub(crate) fn guess_format_from_signature(buffer: &[u8]) -> Option<ImageFormat> {
    read_signatures(|signatures| {
        for (format, signature, mask) in signatures.iter().copied() {
            if mask.is_empty() {
                if buffer.starts_with(signature) {
                    return Some(format);
                }
            } else if buffer.len() >= signature.len()
                && buffer
                    .iter()
                    .zip(signature.iter())
                    .zip(mask.iter().chain(std::iter::repeat(&0xFF)))
                    .all(|((&byte, &sig), &mask)| byte & mask == sig)
            {
                return Some(format);
            }
        }

        None
    })
}
