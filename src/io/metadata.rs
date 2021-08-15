use std::cell::RefCell;
use std::sync::{Arc, Mutex};

use crate::color::Color;

/// Collects some arbitrary metadata of an image.
///
/// Note that information collected here will, per default, appear opaque to the `image` library
/// itself. For example, the width and height of an image might be recorded as one set of values
/// that's completely different from the reported dimensions in the decoder. During decoding and
/// writing of images the library may ignore the color profile and exif data. You should always
/// recheck with the specific decoder if you require color accuracy beyond presuming sRGB. (This
/// may be improved upon in the future, if you have a concrete draft please open an issue).
#[derive(Clone, Default)]
pub struct MetadataContainer {
    /// The original width in pixels.
    pub width: u32,
    /// The original height in pixels.
    pub height: u32,
    /// The available color information.
    /// For example, an ICC profile characterizing color interpretation of the image input.
    pub color_profile: Option<Color>,
    /// Encoded EXIF data associated with the image.
    pub exif: Option<Vec<u8>>,
    _non_exhaustive: (),
}

/// Hints whether one specific metadatum is requested.
#[derive(Clone, Copy)]
pub enum DatumRequested {
    /// The decoder should skip the datum when encountered.
    Ignore,
    /// The decoder should decode, validate, and add the datum.
    Record,
}

/// Configure the caller needs of metadata.
///
/// Not every piece of metadata can be added for free. For example, reading EXIF profiles is
/// commonly associated with decoding of additional chunks, validation, and additional resource
/// usage. All of these can be opt-out by allowing such a customization. Note that, nevertheless,
/// the absence of metadata is not definitive proof of its absence in the original file. It might
/// be more recent than the decoder and not yet have support.
#[derive(Clone, Default)]
// We may want to add a non-copy field?
#[allow(missing_copy_implementations)]
pub struct RecorderConfig {
    /// Whether the decoder should try to provide color profile data.
    pub color_profile: DatumRequested,
    /// Whether the decoder should try to provide EXIF data.
    pub exif: DatumRequested,
    _non_exhaustive: (),
}

/// A buffer for the extra metadata produced by an image decoder.
///
/// There are two main ways of creation: Any consumer of the `ImageDecoder` interface can create
/// their own recorder to retrieve metadata from the decoder. A decoder wrapping another format
/// (i.e. jpeg-within-tiff) might create a recorder from a `SharedRecorder` to add additional data
/// to the outer recorder instead.
///
/// # Use
///
/// ```rust
/// # struct FakeDecoder;
/// # use image::ImageDecoder;
/// # impl ImageDecoder<'_> for FakeDecoder {
/// #   type Reader = std::io::Empty;
/// #   fn dimensions(&self) -> (u32, u32) { (0, 0) }
/// #   fn color_type(&self) -> image::ColorType { todo!() }
/// #   fn into_reader(self) -> image::ImageResult<std::io::Empty> { Ok(std::io::empty()) }
/// # }
/// use image::io::{MetadataContainer, Recorder};
///
/// let mut some_decoder = // ..
/// # FakeDecoder;
/// let mut recorder = Recorder::new();
/// some_decoder.metagram(&mut recorder);
///
/// if recorder.is_shared() {
///     // The decoder kept a shared clone, The complete metagram may not yet be available.
///     // It will likely add more to the metagram while decoding.
///     let mut _buffer = vec![0; some_decoder.total_bytes() as usize];
///     some_decoder.read_image(&mut _buffer);
/// }
///
/// let meta: MetadataContainer = recorder.to_result();
/// ```
pub struct Recorder {
    inner: RecorderInner,
    config: RecorderConfig,
}

enum RecorderInner {
    Owned(Box<RefCell<MetadataContainer>>),
    Shared(Arc<Mutex<MetadataContainer>>),
}

/// An owned handle to a `MetadataContainer`, that allows concurrent modification.
#[allow(missing_copy_implementations)]
pub struct SharedRecorder {
    inner: Arc<Mutex<MetadataContainer>>,
    config: RecorderConfig,
}

impl Recorder {
    /// Create a recorder recording into a new, empty metadata.
    pub fn new() -> Self {
        Recorder::default()
    }

    /// Get the current configuration.
    pub fn config(&self) -> &RecorderConfig {
        &self.config
    }

    /// Get a mutable reference to the current configuration.
    ///
    /// Note that the configuration is cloned when converting between `Recorder` and
    /// `SharedRecorder` but it is not _shared_ between instances. That is, changes only apply to
    /// this instance and not to clones.
    pub fn config_mut(&mut self) -> &mut RecorderConfig {
        &mut self.config
    }

    /// Create a record that already contains some data.
    pub fn with(meta: MetadataContainer) -> Self {
        Recorder {
            inner: RecorderInner::Owned(Box::new(RefCell::new(meta))),
            config: RecorderConfig::default(),
        }
    }

    /// Check if this recorder was shared.
    pub fn is_shared(&self) -> bool {
        match self.inner {
            RecorderInner::Owned(_) => false,
            RecorderInner::Shared(_) => true,
        }
    }

    /// Split the recorder such that it can be sent to a different thread.
    pub fn share(&mut self) -> SharedRecorder {
        let inner;
        match &self.inner {
            RecorderInner::Shared(arc) => {
                inner = Arc::clone(&arc);
            },
            RecorderInner::Owned(boxed) => {
                let meta = boxed.borrow().clone();
                let arc = Arc::new(Mutex::new(meta));
                inner = Arc::clone(&arc);
                self.inner = RecorderInner::Shared(arc);
            }
        };

        SharedRecorder {
            inner,
            config: self.config.clone(),
        }
    }

    /// Get a clone of the configured metadata.
    pub fn to_result(&self) -> MetadataContainer {
        self.inner.to_result()
    }
}

impl RecorderInner {
    fn with_mut(&self, function: impl FnOnce(&mut MetadataContainer)) {
        match self {
            RecorderInner::Owned(boxed) => {
                function(&mut boxed.borrow_mut())
            }
            RecorderInner::Shared(arc) => {
                function(&mut arc.lock().unwrap_or_else(|err| err.into_inner()))
            }
        }
    }

    fn to_result(&self) -> MetadataContainer {
        match self {
            RecorderInner::Owned(boxed) => boxed.borrow().clone(),
            RecorderInner::Shared(arc) => arc.lock().unwrap().clone(),
        }
    }
}

/// Setters for recording metadata.
///
/// Any change here should also be made for `SharedRecorder` unless it is specifically not possible
/// to perform on a shared, and locked struct.
impl Recorder {
    /// Add original dimensions.
    pub fn dimensions(&self, width: u32, height: u32) {
        self.inner.with_mut(|meta| meta.set_dimensions((width, height)));
    }

    /// Add a color profile.
    pub fn color(&self, color: Color) {
        self.inner.with_mut(|meta| meta.set_color(color))
    }

    /// Overwrite all EXIF data.
    pub fn exif(&self, data: Vec<u8>) {
        self.inner.with_mut(|meta| meta.set_exif(data))
    }

    /// Mutate the metadata container.
    ///
    /// This will make the changes visible to all other recorders sharing the container target. It
    /// is not specified what happens when `function` panics but there will be no undefined
    /// behavior.
    pub fn with_mut(&self, function: impl FnOnce(&mut MetadataContainer)) {
        self.inner.with_mut(function)
    }
}

impl SharedRecorder {
    /// Get the current configuration.
    pub fn config(&self) -> &RecorderConfig {
        &self.config
    }

    /// Get a mutable reference to the current configuration.
    ///
    /// Note that the configuration is cloned when converting between `Recorder` and
    /// `SharedRecorder` but it is not _shared_ between instances. That is, changes only apply to
    /// this instance and not to clones.
    pub fn config_mut(&mut self) -> &mut RecorderConfig {
        &mut self.config
    }

    /// Add original dimensions.
    pub fn dimensions(&self, width: u32, height: u32) {
        self.with_mut(|meta| meta.set_dimensions((width, height)));
    }

    /// Add a color profile.
    pub fn color(&self, color: Color) {
        self.with_mut(|meta| meta.set_color(color))
    }

    /// Overwrite all EXIF data.
    pub fn exif(&self, data: Vec<u8>) {
        self.with_mut(|meta| meta.set_exif(data))
    }

    /// Mutate the shared metadata container.
    ///
    /// This will make the changes visible to all other recorders sharing the container target. It
    /// is not specified what happens when `function` panics but there will be no undefined
    /// behavior.
    pub fn with_mut(&self, function: impl FnOnce(&mut MetadataContainer)) {
        // Regarding lock recovery: None of the inner methods should usually panic. The only
        // exception would be from allocation error while inserting a new exif tag or something.
        function(&mut self.inner.lock().unwrap_or_else(|err| err.into_inner()))
    }
}

/// Private implementation of metagram, existing for the purpose of make assignment available as
/// methods.
impl MetadataContainer {
    fn set_dimensions(&mut self, (width, height): (u32, u32)) {
        self.width = width;
        self.height = height;
    }

    fn set_color(&mut self, color: Color) {
        self.color_profile = Some(color);
    }

    fn set_exif(&mut self, exif: Vec<u8>) {
        self.exif = Some(exif);
    }
}

impl Default for Recorder {
    fn default() -> Self {
        Recorder {
            inner: RecorderInner::Owned(Default::default()),
            config: RecorderConfig::default(),
        }
    }
}

/// Convert a shared recorder into a non-thread safe variant.
/// The two will _still_ record to the same metadata collection but this new instances could be
/// used as a method argument to another `ImageDecoder` impl.
impl From<SharedRecorder> for Recorder {
    fn from(shared: SharedRecorder) -> Recorder {
        Recorder {
            inner: RecorderInner::Shared(shared.inner),
            config: shared.config,
        }
    }
}

impl Default for DatumRequested {
    fn default() -> Self {
        DatumRequested::Record
    }
}
