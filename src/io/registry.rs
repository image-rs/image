use std::{
    borrow::Cow,
    collections::HashMap,
    sync::{Arc, RwLock},
};

use crate::{hooks::GenericReader, ImageDecoder, ImageResult};

static REGISTRY: RwLock<Option<FormatRegistry>> = RwLock::new(None);

pub(crate) fn write_registry<R>(f: impl FnOnce(&mut FormatRegistry) -> R) -> R {
    let mut registry_lock = REGISTRY.write().unwrap();
    let registry = registry_lock.get_or_insert_with(create_initial_registry);
    f(registry)
}
pub(crate) fn read_registry<R>(f: impl FnOnce(&FormatRegistry) -> R) -> R {
    let registry_lock = REGISTRY.read().unwrap();
    // the happy path (and common path) is that the registry is already initialized
    if let Some(registry) = registry_lock.as_ref() {
        f(registry)
    } else {
        drop(registry_lock);
        write_registry(|registry| f(registry))
    }
}
fn create_initial_registry() -> FormatRegistry {
    let mut registry = FormatRegistry::empty();
    for spec in BUILTIN_FORMATS {
        registry
            .add_format(spec.clone())
            .expect("Cannot register builtin format");
    }
    registry
}

/// A valid index for the format registry.
///
/// Note that valid indices always remain valid even after new formats are added.
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub(crate) struct RegistryId {
    index: u8,
}
pub(crate) const MAX_REGISTRY_LEN: u16 = 256;

fn as_ascii_lower_case<'a>(s: &'a str) -> Cow<'a, str> {
    if s.bytes().all(|b| !b.is_ascii_uppercase()) {
        Cow::Borrowed(s)
    } else {
        Cow::Owned(s.to_ascii_lowercase())
    }
}

#[derive(Debug)]
pub(crate) enum RegistryError {
    AlreadyFull {
        #[allow(unused)] // used in error message
        max_capacity: u16,
    },
    EmptyExtensionNotAllowed,
}

pub(crate) struct FormatRegistry {
    formats: Vec<FormatSpec>,
    by_extension: HashMap<&'static str, RegistryId>,
    by_mime_type: HashMap<&'static str, RegistryId>,
}
impl FormatRegistry {
    fn empty() -> Self {
        Self {
            formats: Vec::new(),
            by_extension: HashMap::new(),
            by_mime_type: HashMap::new(),
        }
    }

    /// Add the given format and returns the assigned `RegistryId`, or `None` if the registry is full.
    pub(crate) fn add_format(&mut self, spec: FormatSpec) -> Result<RegistryId, RegistryError> {
        if self.formats.len() >= usize::from(MAX_REGISTRY_LEN) {
            return Err(RegistryError::AlreadyFull {
                max_capacity: MAX_REGISTRY_LEN,
            });
        }
        let id = RegistryId {
            index: self.formats.len() as u8,
        };

        for &ext in spec.all_extensions() {
            self.by_extension.insert(ext, id);
        }
        for &mime in spec.all_mime_type() {
            self.by_mime_type.insert(mime, id);
        }
        self.formats.push(spec);

        Ok(id)
    }
    pub(crate) fn add_extension_aliases(&mut self, id: RegistryId, aliases: &[&'static str]) {
        // always update mapping even if aliases already exist
        for &ext in aliases {
            self.by_extension.insert(ext, id);
        }

        // Add new extensions to the format spec, so users can query them
        let spec = &mut self.formats[id.index as usize];

        let mut new_extensions = spec.extensions.to_vec();
        for alias in aliases {
            // avoid duplicates
            if !new_extensions.contains(alias) {
                new_extensions.push(alias);
            }
        }
        if new_extensions.len() == spec.extensions.len() {
            // no need to change anything
            return;
        }

        // TODO: Avoid memory leaking by either:
        // 1. Changing the API of `ImageFormat::extensions_str` to no longer require `'static` lifetime. Would like need to be Arc.
        // 2. Reserve space for K extension up front and refuse to support more than K extensions.
        spec.extensions = Box::leak(new_extensions.into_boxed_slice());
    }
    pub(crate) fn add_mime_types(&mut self, id: RegistryId, mime_types: &[&'static str]) {
        // always update mapping even if aliases already exist
        for &mime in mime_types {
            self.by_mime_type.insert(mime, id);
        }

        // Add new extensions to the format spec, so users can query them
        let spec = &mut self.formats[id.index as usize];

        let format_mime_types = spec.mime_types.to_mut();
        for mime in mime_types {
            // avoid duplicates
            if !format_mime_types.contains(mime) {
                format_mime_types.push(mime);
            }
        }
    }

    pub(crate) fn get(&self, id: RegistryId) -> &FormatSpec {
        &self.formats[id.index as usize]
    }
    pub(crate) fn get_mut(&mut self, id: RegistryId) -> &mut FormatSpec {
        &mut self.formats[id.index as usize]
    }
    pub(crate) fn get_by_extension(&self, ext: &str) -> Option<RegistryId> {
        self.by_extension.get(&*as_ascii_lower_case(ext)).copied()
    }
    pub(crate) fn get_by_mime_type(&self, mime_type: &str) -> Option<RegistryId> {
        self.by_mime_type
            .get(&*as_ascii_lower_case(mime_type))
            .copied()
    }

    pub(crate) fn all(&self) -> impl Iterator<Item = RegistryId> {
        (0..self.formats.len()).map(|index| RegistryId { index: index as u8 })
    }
}

const CAN_READ: u8 = 1;
const CAN_WRITE: u8 = 2;
const CAN_READ_WRITE: u8 = CAN_READ | CAN_WRITE;
const CANNOT_READ_NOR_WRITE: u8 = 0;

pub(crate) type DecodingFn =
    dyn for<'a> Fn(GenericReader<'a>) -> ImageResult<Box<dyn ImageDecoder + 'a>> + Send + Sync;

#[derive(Clone)]
pub(crate) struct FormatSpec {
    extensions: &'static [&'static str],
    mime_types: Cow<'static, [&'static str]>,

    pub(crate) feature_enabled: bool,
    pub(crate) can_read: bool,
    pub(crate) can_write: bool,

    pub(crate) decoding_fn: Option<Arc<DecodingFn>>,
}
impl FormatSpec {
    const fn builtin(
        extensions: &'static [&'static str],
        mime_types: &'static [&'static str],
        feature: bool,
        ability: u8,
    ) -> Self {
        Self {
            extensions,
            mime_types: Cow::Borrowed(mime_types),

            feature_enabled: feature,
            can_read: ability & CAN_READ != 0,
            can_write: ability & CAN_WRITE != 0,

            decoding_fn: None,
        }
    }

    pub(crate) fn new_hook(
        extension: &'static str,
        hook: Arc<DecodingFn>,
    ) -> Result<Self, RegistryError> {
        if extension.is_empty() {
            return Err(RegistryError::EmptyExtensionNotAllowed);
        }

        // TODO: Avoid leaking memory. See TODO in `add_extension_aliases`.
        let extensions = Box::leak(Box::new([extension]));

        Ok(Self {
            extensions,
            mime_types: Cow::Borrowed(&[]),

            feature_enabled: true,
            can_read: true,
            can_write: false,

            decoding_fn: Some(hook),
        })
    }

    pub(crate) fn main_extension(&self) -> &'static str {
        self.extensions[0]
    }
    pub(crate) fn all_extensions(&self) -> &'static [&'static str] {
        self.extensions
    }
    pub(crate) fn main_mime_type(&self) -> Option<&'static str> {
        self.mime_types.first().copied()
    }
    pub(crate) fn all_mime_type(&self) -> &[&'static str] {
        &self.mime_types
    }
}

const BUILTIN_FORMATS: &[FormatSpec] = &[
    FormatSpec::builtin(
        &["png", "apng"],
        &[
            "image/png",
            // TODO: Add missing MIME type for APNG
            // "image/apng"
        ],
        cfg!(feature = "png"),
        CAN_READ_WRITE,
    ),
    FormatSpec::builtin(
        &["jpeg", "jpg", "jfif"],
        &["image/jpeg"],
        cfg!(feature = "jpeg"),
        CAN_READ_WRITE,
    ),
    FormatSpec::builtin(
        &["gif"],
        &["image/gif"],
        cfg!(feature = "gif"),
        CAN_READ_WRITE,
    ),
    FormatSpec::builtin(
        &["webp"],
        &["image/webp"],
        cfg!(feature = "webp"),
        CAN_READ_WRITE,
    ),
    FormatSpec::builtin(
        &["pnm", "pbm", "pgm", "ppm", "pam"],
        &[
            "image/x-portable-anymap",
            "image/x-portable-bitmap",
            "image/x-portable-graymap",
            "image/x-portable-pixmap",
        ],
        cfg!(feature = "pnm"),
        CAN_READ_WRITE,
    ),
    FormatSpec::builtin(
        &["tiff", "tif"],
        &[
            "image/tiff",
            // TODO: Wikipedia says image/tiff-fx works too
            // "tiff-fx",
        ],
        cfg!(feature = "tiff"),
        CAN_READ_WRITE,
    ),
    FormatSpec::builtin(
        &["tga"],
        &["image/x-targa", "image/x-tga"],
        cfg!(feature = "tga"),
        CAN_READ_WRITE,
    ),
    FormatSpec::builtin(
        &["dds"],
        &["image/vnd-ms.dds"],
        false,
        CANNOT_READ_NOR_WRITE,
    ),
    FormatSpec::builtin(
        &["bmp"],
        &["image/bmp"],
        cfg!(feature = "bmp"),
        CAN_READ_WRITE,
    ),
    FormatSpec::builtin(
        &["ico"],
        &["image/x-icon", "image/vnd.microsoft.icon"],
        cfg!(feature = "ico"),
        CAN_READ_WRITE,
    ),
    FormatSpec::builtin(
        &["hdr"],
        &["image/vnd.radiance"],
        cfg!(feature = "hdr"),
        CAN_READ_WRITE,
    ),
    FormatSpec::builtin(
        &["exr"],
        &["image/x-exr"],
        cfg!(feature = "exr"),
        CAN_READ_WRITE,
    ),
    FormatSpec::builtin(
        &["ff"],
        // TODO: This used to be application/octet-stream. Think about that.
        // Source: https://mime-type.com/file-extension/ff/
        &["image/x-farbfeld"],
        cfg!(feature = "ff"),
        CAN_READ_WRITE,
    ),
    FormatSpec::builtin(
        &["avif"],
        &["image/avif"],
        cfg!(feature = "avif"),
        CAN_READ_WRITE,
    ),
    FormatSpec::builtin(
        &["qoi"],
        // Qoi's MIME type is being worked on.
        // See: https://github.com/phoboslab/qoi/issues/167
        &["image/x-qoi"],
        cfg!(feature = "qoi"),
        CAN_READ_WRITE,
    ),
];

pub(crate) const PNG_ID: RegistryId = RegistryId { index: 0 };
pub(crate) const JPEG_ID: RegistryId = RegistryId { index: 1 };
pub(crate) const GIF_ID: RegistryId = RegistryId { index: 2 };
pub(crate) const WEBP_ID: RegistryId = RegistryId { index: 3 };
pub(crate) const PNM_ID: RegistryId = RegistryId { index: 4 };
pub(crate) const TIFF_ID: RegistryId = RegistryId { index: 5 };
pub(crate) const TGA_ID: RegistryId = RegistryId { index: 6 };
pub(crate) const DDS_ID: RegistryId = RegistryId { index: 7 };
pub(crate) const BMP_ID: RegistryId = RegistryId { index: 8 };
pub(crate) const ICO_ID: RegistryId = RegistryId { index: 9 };
pub(crate) const HDR_ID: RegistryId = RegistryId { index: 10 };
pub(crate) const EXR_ID: RegistryId = RegistryId { index: 11 };
pub(crate) const FARBFELD_ID: RegistryId = RegistryId { index: 12 };
pub(crate) const AVIF_ID: RegistryId = RegistryId { index: 13 };
pub(crate) const QOI_ID: RegistryId = RegistryId { index: 14 };
