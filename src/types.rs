//! Common types shared between the encoder and decoder


#[derive(NumFromPrimitive, Debug, Clone, Copy)]
pub enum ColorType {
    Grayscale = 0,
    RGB = 2,
    Indexed = 3,
    GrayscaleAlpha = 4,
    RGBA = 6
}

impl ColorType {
    /// Returns the number of samples used per pixel of `ColorType`
    pub fn samples(&self) -> usize {
        use self::ColorType::*;
        match *self {
            Grayscale => 1,
            RGB => 3,
            Indexed => 1,
            GrayscaleAlpha => 2,
            RGBA => 4
        }
    }
}


/// Frame control information
#[derive(Debug)]
pub struct FrameControl;

/// Animation control information
#[derive(Debug)]
pub struct AnimationControl;

/// PNG info struct
#[derive(Debug)]
pub struct Info {
    pub width: u32,
    pub height: u32,
    pub bit_depth: u8,
    pub color_type: ColorType,
    pub interlaced: bool,
    pub frame_control: Option<FrameControl>,
    pub animation_control: Option<AnimationControl>
}

impl Default for Info {
    fn default() -> Info {
        Info {
            width: 0,
            height: 0,
            bit_depth: 0,
            color_type: ColorType::Grayscale,
            interlaced: false,
            frame_control: None,
            animation_control: None
        }
    }
}

impl Info {
    /// Size of the image
    pub fn size(&self) -> (u32, u32) {
        (self.width, self.height)
    }
    
    /// Returns true if the image is an APNG image.
    pub fn is_animated(&self) -> bool {
        self.frame_control.is_some() && self.animation_control.is_some()
    }
    
    /// Returns the frame control information of the image
    pub fn animation_control(&self) -> Option<&AnimationControl> {
        self.animation_control.as_ref()
    }
    
    /// Returns the frame control information of the current frame
    pub fn frame_control(&self) -> Option<&FrameControl> {
        self.frame_control.as_ref()
    }
    
    /// Returns the bytes per pixel
    pub fn bytes_per_pixel(&self) -> usize {
        self.color_type.samples() * ((self.bit_depth as usize + 7) >> 3)
    }
    
    /// Returns the number of bytes needed for one deinterlaced image
    pub fn raw_bytes(&self) -> usize {
        self.height as usize * self.raw_row_length()
    }
    
    /// Returns the number of bytes needed for one deinterlaced row 
    pub fn raw_row_length(&self) -> usize {
        self.width as usize
        * self.color_type.samples()
        * ((self.bit_depth as usize + 7) >> 3)
        + 1 // filter method
    }
}