use crate::error::ImageResult;

/// An implementation dependent iterator, reading the frames as requested
pub struct Stack<'a, I> {
    iterator: Box<dyn Iterator<Item = ImageResult<I>> + 'a>,
}

impl<'a, I> Stack<'a, I> {
    /// Creates a new `Stack` from an implementation specific iterator.
    #[must_use]
    pub fn new(iterator: Box<dyn Iterator<Item = ImageResult<I>> + 'a>) -> Self {
        Stack { iterator }
    }

    /// Steps through the iterator from the current frame until the end and pushes each frame into
    /// a `Vec`.
    /// If en error is encountered that error is returned instead.
    ///
    /// Note: This is equivalent to `Stack::collect::<ImageResult<Vec<Frame>>>()`
    pub fn collect_frames(self) -> ImageResult<Vec<I>> {
        self.collect()
    }
}

impl<I> Iterator for Stack<'_, I> {
    type Item = ImageResult<I>;

    fn next(&mut self) -> Option<ImageResult<I>> {
        self.iterator.next()
    }
}

/// A single image frame
pub struct Frame<I> {
    /// x offset
    left: u32,
    /// y offset
    top: u32,
    pub(crate) buffer: I,
}

impl<I> Clone for Frame<I>
where
    I: Clone,
{
    fn clone(&self) -> Self {
        Self {
            left: self.left,
            top: self.top,
            buffer: self.buffer.clone(),
        }
    }

    fn clone_from(&mut self, source: &Self) {
        self.left = source.left;
        self.top = source.top;
        self.buffer.clone_from(&source.buffer);
    }
}

impl<I> Frame<I> {
    /// Constructs a new frame without any delay.
    #[must_use]
    pub fn new(buffer: I) -> Frame<I> {
        Frame {
            left: 0,
            top: 0,
            buffer,
        }
    }

    /// Constructs a new frame
    #[must_use]
    pub fn from_parts(buffer: I, left: u32, top: u32) -> Frame<I> {
        Frame { left, top, buffer }
    }

    /// Returns the image buffer
    #[must_use]
    pub fn buffer(&self) -> &I {
        &self.buffer
    }

    /// Returns a mutable image buffer
    pub fn buffer_mut(&mut self) -> &mut I {
        &mut self.buffer
    }

    /// Returns the image buffer
    #[must_use]
    pub fn into_buffer(self) -> I {
        self.buffer
    }

    /// Returns the x offset
    #[must_use]
    pub fn left(&self) -> u32 {
        self.left
    }

    /// Returns the y offset
    #[must_use]
    pub fn top(&self) -> u32 {
        self.top
    }
}
