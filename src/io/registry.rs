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
        registry.add_format(spec.clone());
    }
    registry
}

/// A valid index for the format registry.
///
/// Note that valid indices always remain valid even after new formats are added.
#[derive(Copy, Clone, PartialEq, Eq)]
pub(crate) struct RegistryId {
    index: u32,
}

pub(crate) struct FormatRegistry {
    formats: Vec<FormatSpec>,
    by_extension: HashMap<String, RegistryId>,
    by_mime_type: HashMap<String, RegistryId>,
    by_main_extension: HashMap<&'static str, RegistryId>,
}
impl FormatRegistry {
    fn empty() -> Self {
        Self {
            formats: Vec::new(),
            by_extension: HashMap::new(),
            by_mime_type: HashMap::new(),
            by_main_extension: HashMap::new(),
        }
    }

    /// Add the given format and returns the assigned `RegistryId`, or `Err` if the registry is full.
    pub(crate) fn add_format(&mut self, spec: FormatSpec) -> RegistryId {
        let id = RegistryId {
            index: u32::try_from(self.formats.len()).expect("Too many formats"),
        };

        let prev = self.by_main_extension.insert(spec.main_extension(), id);
        debug_assert!(prev.is_none(), "Main extensions must be unique");

        for &ext in spec.all_extensions() {
            self.by_extension.insert(ext.to_ascii_lowercase(), id);
        }
        for &mime in spec.all_mime_types() {
            // TODO: Lowercase this too?
            self.by_mime_type.insert(mime.to_owned(), id);
        }

        self.formats.push(spec);

        id
    }
    pub(crate) fn add_extension_aliases(&mut self, id: RegistryId, extension_aliases: &[&str]) {
        // Add new extensions to the format spec, so users can query them
        let spec = &mut self.formats[id.index as usize];

        let mut extension_list = spec.extensions.to_vec();
        for ext in extension_aliases {
            let ext = ext.to_lowercase();

            // always update mapping even if aliases already exist
            let prev = self.by_extension.insert(ext.clone(), id);

            // avoid duplicates
            if prev.is_none() || prev != Some(id) && !extension_list.contains(&ext.as_str()) {
                // The extension string will live for the remainder of the program's life,
                // so leaking it here is fine.
                extension_list.push(ext.leak());
            }
        }

        // TODO: Avoid memory leaking by either:
        // 1. Changing the API of `ImageFormat::extensions_str` to no longer require `'static` lifetime. Would like need to be Arc.
        // 2. Reserve space for K extension up front and refuse to support more than K extensions. Will likely require unsafe.
        spec.extensions = Box::leak(extension_list.into_boxed_slice());
    }
    pub(crate) fn add_mime_types(&mut self, id: RegistryId, mime_types: &[&'static str]) {
        let mime_list = self.formats[id.index as usize].mime_types.to_mut();

        for mime in mime_types {
            // always update mapping even if aliases already exist
            let prev = self.by_mime_type.insert(mime.to_string(), id);

            // avoid duplicates
            if prev.is_none() || prev != Some(id) && !mime_list.contains(mime) {
                mime_list.push(mime);
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
        let ext = ext.to_ascii_lowercase();
        // main extensions take precedence
        self.by_main_extension
            .get(ext.as_str())
            .or_else(|| self.by_extension.get(ext.as_str()))
            .copied()
    }
    pub(crate) fn get_by_main_extension(&self, main_extension: &str) -> Option<RegistryId> {
        self.by_main_extension.get(main_extension).copied()
    }
    pub(crate) fn get_by_mime_type(&self, mime_type: &str) -> Option<RegistryId> {
        self.by_mime_type
            .get(mime_type.to_ascii_lowercase().as_str())
            .copied()
    }

    pub(crate) fn all(&self) -> impl Iterator<Item = RegistryId> {
        (0..self.formats.len()).map(|index| RegistryId {
            index: index as u32,
        })
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

    pub(crate) feature_enabled_read: bool,
    pub(crate) feature_enabled_write: bool,
    pub(crate) can_read: bool,
    pub(crate) can_write: bool,

    pub(crate) decoding_fn: Option<Arc<DecodingFn>>,
}
impl FormatSpec {
    const fn builtin(
        extensions: &'static [&'static str],
        mime_types: &'static [&'static str],
        feature: [bool; 2],
        ability: u8,
    ) -> Self {
        Self {
            extensions,
            mime_types: Cow::Borrowed(mime_types),

            feature_enabled_read: feature[0],
            feature_enabled_write: feature[1],
            can_read: ability & CAN_READ != 0,
            can_write: ability & CAN_WRITE != 0,

            decoding_fn: None,
        }
    }
    pub(crate) fn empty(main_extension: &'static str) -> Self {
        // TODO: Avoid leaking memory. See TODO in `add_extension_aliases`.
        let extensions = Box::leak(Box::new([main_extension]));

        Self {
            extensions,
            mime_types: Cow::Borrowed(&[]),

            feature_enabled_read: true,
            feature_enabled_write: true,
            can_read: false,
            can_write: false,

            decoding_fn: None,
        }
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
    pub(crate) fn all_mime_types(&self) -> &[&'static str] {
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
        [cfg!(feature = "png"); 2],
        CAN_READ_WRITE,
    ),
    FormatSpec::builtin(
        &["jpeg", "jpg", "jfif"],
        &["image/jpeg"],
        [cfg!(feature = "jpeg"); 2],
        CAN_READ_WRITE,
    ),
    FormatSpec::builtin(
        &["gif"],
        &["image/gif"],
        [cfg!(feature = "gif"); 2],
        CAN_READ_WRITE,
    ),
    FormatSpec::builtin(
        &["webp"],
        &["image/webp"],
        [cfg!(feature = "webp"); 2],
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
        [cfg!(feature = "pnm"); 2],
        CAN_READ_WRITE,
    ),
    FormatSpec::builtin(
        &["tiff", "tif"],
        &[
            "image/tiff",
            // TODO: Wikipedia says image/tiff-fx works too
            // "tiff-fx",
        ],
        [cfg!(feature = "tiff"); 2],
        CAN_READ_WRITE,
    ),
    FormatSpec::builtin(
        &["tga"],
        &["image/x-targa", "image/x-tga"],
        [cfg!(feature = "tga"); 2],
        CAN_READ_WRITE,
    ),
    FormatSpec::builtin(
        &["dds"],
        &["image/vnd-ms.dds"],
        [false; 2],
        CANNOT_READ_NOR_WRITE,
    ),
    FormatSpec::builtin(
        &["bmp"],
        &["image/bmp"],
        [cfg!(feature = "bmp"); 2],
        CAN_READ_WRITE,
    ),
    FormatSpec::builtin(
        &["ico"],
        &["image/x-icon", "image/vnd.microsoft.icon"],
        [cfg!(feature = "ico"); 2],
        CAN_READ_WRITE,
    ),
    FormatSpec::builtin(
        &["hdr"],
        &["image/vnd.radiance"],
        [cfg!(feature = "hdr"); 2],
        CAN_READ_WRITE,
    ),
    FormatSpec::builtin(
        &["exr"],
        &["image/x-exr"],
        [cfg!(feature = "exr"); 2],
        CAN_READ_WRITE,
    ),
    FormatSpec::builtin(
        &["ff"],
        // TODO: This used to be application/octet-stream. Think about that.
        // Source: https://mime-type.com/file-extension/ff/
        &["image/x-farbfeld"],
        [cfg!(feature = "ff"); 2],
        CAN_READ_WRITE,
    ),
    FormatSpec::builtin(
        &["avif"],
        &["image/avif"],
        [cfg!(feature = "avif-native"), cfg!(feature = "avif")],
        CAN_READ_WRITE,
    ),
    FormatSpec::builtin(
        &["qoi"],
        // Qoi's MIME type is being worked on.
        // See: https://github.com/phoboslab/qoi/issues/167
        &["image/x-qoi"],
        [cfg!(feature = "qoi"); 2],
        CAN_READ_WRITE,
    ),
];

pub(crate) const PNG: RegistryId = RegistryId { index: 0 };
pub(crate) const JPEG: RegistryId = RegistryId { index: 1 };
pub(crate) const GIF: RegistryId = RegistryId { index: 2 };
pub(crate) const WEBP: RegistryId = RegistryId { index: 3 };
pub(crate) const PNM: RegistryId = RegistryId { index: 4 };
pub(crate) const TIFF: RegistryId = RegistryId { index: 5 };
pub(crate) const TGA: RegistryId = RegistryId { index: 6 };
pub(crate) const DDS: RegistryId = RegistryId { index: 7 };
pub(crate) const BMP: RegistryId = RegistryId { index: 8 };
pub(crate) const ICO: RegistryId = RegistryId { index: 9 };
pub(crate) const HDR: RegistryId = RegistryId { index: 10 };
pub(crate) const EXR: RegistryId = RegistryId { index: 11 };
pub(crate) const FARBFELD: RegistryId = RegistryId { index: 12 };
pub(crate) const AVIF: RegistryId = RegistryId { index: 13 };
pub(crate) const QOI: RegistryId = RegistryId { index: 14 };
