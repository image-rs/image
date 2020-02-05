use std::iter::Iterator;
use std::time::Duration;

use num_rational::Ratio;

use crate::buffer::RgbaImage;
use crate::error::ImageResult;

/// An implementation dependent iterator, reading the frames as requested
pub struct Frames<'a> {
    iterator: Box<dyn Iterator<Item = ImageResult<Frame>> + 'a>
}

impl<'a> Frames<'a> {
    /// Creates a new `Frames` from an implementation specific iterator.
    pub fn new(iterator: Box<dyn Iterator<Item = ImageResult<Frame>> + 'a>) -> Self {
        Frames { iterator }
    }

    /// Steps through the iterator from the current frame until the end and pushes each frame into
    /// a `Vec`.
    /// If en error is encountered that error is returned instead.
    ///
    /// Note: This is equivalent to `Frames::collect::<ImageResult<Vec<Frame>>>()`
    pub fn collect_frames(self) -> ImageResult<Vec<Frame>> {
        self.collect()
    }
}

impl<'a> Iterator for Frames<'a> {
    type Item = ImageResult<Frame>;
    fn next(&mut self) -> Option<ImageResult<Frame>> {
        self.iterator.next()
    }
}

/// A single animation frame
#[derive(Clone)]
pub struct Frame {
    /// Delay between the frames in milliseconds
    delay: Delay,
    /// x offset
    left: u32,
    /// y offset
    top: u32,
    buffer: RgbaImage,
}

/// The delay of a frame relative to the previous one.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd)]
pub struct Delay {
    ratio: Ratio<u32>,
}

impl Frame {
    /// Contructs a new frame without any delay.
    pub fn new(buffer: RgbaImage) -> Frame {
        Frame {
            delay: Delay::from_ratio(Ratio::from_integer(0)),
            left: 0,
            top: 0,
            buffer,
        }
    }

    /// Contructs a new frame
    pub fn from_parts(buffer: RgbaImage, left: u32, top: u32, delay: Delay) -> Frame {
        Frame {
            delay,
            left,
            top,
            buffer,
        }
    }

    /// Delay of this frame
    pub fn delay(&self) -> Delay {
        self.delay
    }

    /// Returns the image buffer
    pub fn buffer(&self) -> &RgbaImage {
        &self.buffer
    }

    /// Returns the image buffer
    pub fn into_buffer(self) -> RgbaImage {
        self.buffer
    }

    /// Returns the x offset
    pub fn left(&self) -> u32 {
        self.left
    }

    /// Returns the y offset
    pub fn top(&self) -> u32 {
        self.top
    }
}

impl Delay {
    /// Create a delay from a ratio of milliseconds.
    ///
    /// # Examples
    ///
    /// ```
    /// use image::Delay;
    /// let delay_10ms = Delay::from_num_denom_ms(10, 1);
    /// ```
    pub fn from_num_denom_ms(numerator: u32, denominator: u32) -> Self {
        Delay { ratio: Ratio::new_raw(numerator, denominator) }
    }

    /// The numerator and denominator of the delay in milliseconds.
    pub fn num_denom_ms(self) -> (u32, u32) {
        (*self.ratio.numer(), *self.ratio.denom())
    }

    pub(crate) fn from_ratio(ratio: Ratio<u32>) -> Self {
        Delay { ratio }
    }

    pub(crate) fn into_ratio(self) -> Ratio<u32> {
        self.ratio
    }
}

impl From<Delay> for Duration {
    fn from(delay: Delay) -> Self {
        let ratio = delay.into_ratio();
        let ms = ratio.to_integer();
        let rest = ratio.numer() % ratio.denom();
        let nanos = (u64::from(rest) * 1_000_000) / u64::from(*ratio.denom());
        Duration::from_millis(ms.into()) + Duration::from_nanos(nanos)
    }
}

#[cfg(test)]
mod tests {
    use super::{Delay, Duration, Ratio};

    #[test]
    fn simple() {
        let second = Delay::from_num_denom_ms(1000, 1);
        assert_eq!(Duration::from(second), Duration::from_secs(1));
    }

    #[test]
    fn fps_30() {
        let thirtieth = Delay::from_num_denom_ms(1000, 30);
        let duration = Duration::from(thirtieth);
        assert_eq!(duration.as_secs(), 0);
        assert_eq!(duration.subsec_millis(), 33);
        assert_eq!(duration.subsec_nanos(), 33_333_333);
    }
}
