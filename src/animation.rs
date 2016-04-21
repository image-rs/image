use num_rational::Ratio;

use buffer::RgbaImage;

/// Holds the frames of the animated image
pub struct Frames {
    frames: Vec<Frame>,
    current_frame: usize,
}

impl Frames {
    /// Contructs a new frame iterator
    pub fn new(frames: Vec<Frame>) -> Frames {
        Frames {
            frames: frames,
            current_frame: 0
        }
    }
}

/// A single animation frame
#[derive(Clone)]
pub struct Frame {
    /// Delay between the frames in s
    delay: Ratio<u16>,
    /// x offset
    left: u32,
    /// y offset
    top: u32,
    buffer: RgbaImage,
}

impl Frame {
    /// Contructs a new frame
    pub fn new(buffer: RgbaImage) -> Frame {
        Frame {
            delay: Ratio::from_integer(0),
            left: 0,
            top: 0,
            buffer: buffer
        }
    }

    /// Contructs a new frame
    pub fn from_parts(buffer: RgbaImage, left: u32, top: u32, delay: Ratio<u16>) -> Frame {
        Frame {
            delay: delay,
            left: left,
            top: top,
            buffer: buffer
        }
    }

    /// Delay of this frame
    pub fn delay(&self) -> Ratio<u16> {
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

impl<'a> Iterator for Frames {
    type Item = Frame;
    fn next(&mut self) -> Option<Frame> {
        let frame = self.current_frame;
        self.current_frame += 1;
        self.frames.get(frame).map(|v| v.clone())
    }
}
