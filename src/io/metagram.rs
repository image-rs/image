use std::cell::RefCell;
use std::sync::{Arc, Mutex};
use std::collections::HashMap;

use crate::color;

// oder (Engram), Metagram, 
/// Collects some arbitrary meta data of an image.
#[derive(Clone, Default)]
pub struct Metagram {
    width: u32,
    height: u32,
    color: Option<color::ExtendedColorType>,
    color_profile_iccp: Vec<u8>,
    exif: Vec<u8>,
    comments: Vec<String>,
}

/// A buffer for the extra meta data produced by an image decoder.
pub struct Recorder {
    inner: RecorderInner,
}

enum RecorderInner {
    Owned(Box<RefCell<Metagram>>),
    Shared(Arc<Mutex<Metagram>>),
}

/// An owned handle to a `Metagram`, that allows concurrent modification.
#[allow(missing_copy_implementations)]
pub struct SharedRecorder {
    inner: Arc<Mutex<Metagram>>,
}

impl Metagram {
    pub fn dimensions(&mut self, width: u32, height: u32) {
        self.width = width;
        self.height = height;
    }

    pub fn color(&mut self, color: color::ExtendedColorType) {
        self.color = Some(color);
    }

    pub fn add_comment(&mut self, comment: String) {
        self.comments.push(comment)
    }

    pub fn set_exif(&mut self, data: Vec<u8>) {
        self.exif = data;
    }
}

impl Recorder {
    pub fn new() -> Self {
        Recorder::default()
    }

    pub fn with(meta: Metagram) -> Self {
        Recorder {
            inner: RecorderInner::Owned(Box::new(RefCell::new(meta))),
        }
    }

    pub fn dimensions(&self, width: u32, height: u32) {
        self.inner.with_mut(|meta| meta.dimensions(width, height))
    }

    pub fn color(&self, color: color::ExtendedColorType) {
        self.inner.with_mut(|meta| meta.color(color))
    }

    /// Add an additional comment.
    pub fn add_comment(&self, comment: String) {
        self.inner.with_mut(|meta| meta.add_comment(comment))
    }

    /// Overwrite all EXIF data.
    pub fn exif(&self, data: Vec<u8>) {
        self.inner.with_mut(|meta| meta.set_exif(data))
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
        }
    }

    /// Get a clone of the configured meta data.
    pub fn to_result(&self) -> Metagram {
        self.inner.to_result()
    }
}

impl RecorderInner {
    fn with_mut(&self, function: impl FnOnce(&mut Metagram)) {
        match self {
            RecorderInner::Owned(boxed) => {
                function(&mut boxed.borrow_mut())
            }
            RecorderInner::Shared(arc) => {
                function(&mut arc.lock().unwrap())
            }
        }
    }

    fn to_result(&self) -> Metagram {
        match self {
            RecorderInner::Owned(boxed) => boxed.borrow().clone(),
            RecorderInner::Shared(arc) => arc.lock().unwrap().clone(),
        }
    }
}

impl SharedRecorder {
    pub fn set_exif(&mut self, data: Vec<u8>) {
        self.with_mut(|meta| meta.set_exif(data))
    }

    fn with_mut(&self, function: impl FnOnce(&mut Metagram)) {
        // Regarding lock recovery: None of the inner methods should usually panic. The only
        // exception would be from allocation error while inserting a new exif tag or something.
        function(&mut self.inner.lock().unwrap_or_else(|err| err.into_inner()))
    }
}

impl Default for Recorder {
    fn default() -> Self {
        Recorder {
            inner: RecorderInner::Owned(Default::default()),
        }
    }
}

/// Convert a shared recorder into a non-thread safe variant.
/// The two will _still_ record to the same meta data collection but this new instances could be
/// used as a method argument to another `ImageDecoder` impl.
impl From<SharedRecorder> for Recorder {
    fn from(shared: SharedRecorder) -> Recorder {
        Recorder { inner: RecorderInner::Shared(shared.inner) }
    }
}
