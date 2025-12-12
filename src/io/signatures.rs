use std::sync::RwLock;

use crate::ImageFormat;

#[derive(Copy, Clone)]
pub(crate) struct Sig {
    format: ImageFormat,
    signature: &'static [u8],
    mask: &'static [u8],
}
impl Sig {
    const fn new(format: ImageFormat, signature: &'static [u8]) -> Self {
        Self::masked(format, signature, &[])
    }
    pub(crate) const fn masked(
        format: ImageFormat,
        signature: &'static [u8],
        mask: &'static [u8],
    ) -> Self {
        assert!(!signature.is_empty(), "Signature cannot be empty");

        Self {
            format,
            signature,
            mask,
        }
    }

    fn matches(&self, buffer: &[u8]) -> bool {
        if self.mask.is_empty() {
            buffer.starts_with(self.signature)
        } else {
            buffer.len() >= self.signature.len()
                && buffer
                    .iter()
                    .zip(self.signature.iter())
                    .zip(self.mask.iter().chain(std::iter::repeat(&0xFF)))
                    .all(|((&byte, &sig), &mask)| byte & mask == sig)
        }
    }
}

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
    Sig::new(ImageFormat::Png, b"\x89PNG\r\n\x1a\n"),
    Sig::new(ImageFormat::Jpeg, &[0xff, 0xd8, 0xff]),
    Sig::new(ImageFormat::Gif, b"GIF89a"),
    Sig::new(ImageFormat::Gif, b"GIF87a"),
    Sig::masked(
        ImageFormat::WebP,
        b"RIFF\0\0\0\0WEBP",
        b"\xFF\xFF\xFF\xFF\0\0\0\0",
    ),
    Sig::new(ImageFormat::Tiff, b"MM\x00*"),
    Sig::new(ImageFormat::Tiff, b"II*\x00"),
    Sig::new(ImageFormat::Dds, b"DDS "),
    Sig::new(ImageFormat::Bmp, b"BM"),
    Sig::new(ImageFormat::Ico, &[0, 0, 1, 0]),
    Sig::new(ImageFormat::Hdr, b"#?RADIANCE"),
    Sig::masked(ImageFormat::Avif, b"\0\0\0\0ftypavif", b"\xFF\xFF\0\0"),
    Sig::new(ImageFormat::OpenExr, &[0x76, 0x2f, 0x31, 0x01]), // = &exr::meta::magic_number::BYTES
    Sig::new(ImageFormat::Qoi, b"qoif"),
    Sig::new(ImageFormat::Pnm, b"P1"),
    Sig::new(ImageFormat::Pnm, b"P2"),
    Sig::new(ImageFormat::Pnm, b"P3"),
    Sig::new(ImageFormat::Pnm, b"P4"),
    Sig::new(ImageFormat::Pnm, b"P5"),
    Sig::new(ImageFormat::Pnm, b"P6"),
    Sig::new(ImageFormat::Pnm, b"P7"),
    Sig::new(ImageFormat::Farbfeld, b"farbfeld"),
];

pub(crate) fn register_signature(sig: Sig) {
    write_signatures(move |signatures| {
        signatures.push(sig);
    });
}

pub(crate) fn guess_format_from_signature(buffer: &[u8]) -> Option<ImageFormat> {
    read_signatures(|signatures| {
        for sig in signatures {
            if sig.matches(buffer) {
                return Some(sig.format);
            }
        }

        None
    })
}
