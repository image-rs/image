use num::rational::Ratio;

use buffer::RgbaImage;

/// Hold the frames of the animated image
pub struct Frames {
    frames: Vec<Frame>,
    current_frame: uint,
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
#[deriving(Clone)]
pub struct Frame {
    /// Delay between the frames in s
    delay: Ratio<u16>,
    buffer: RgbaImage,
}

impl Frame {
    /// Contructs a new frame
    pub fn new(buffer: RgbaImage) -> Frame {
        Frame {
            delay: Ratio::from_integer(0),
            buffer: buffer
        }
    }
}

impl<'a> Iterator<Frame> for Frames {
    fn next(&mut self) -> Option<Frame> {
        let frame = self.current_frame;
        self.current_frame += 1;
        self.frames.get(frame).map(|v| v.clone())
    }
}