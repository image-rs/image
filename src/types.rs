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
    pub trns: Option<Vec<u8>>,
    pub palette: Option<Vec<u8>>,
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
            palette: None,
            trns: None,
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
    pub fn raw_bytes_per_pixel(&self) -> usize {
        self.color_type.samples() * ((self.bit_depth as usize + 7) >> 3)
    }
    
    /// Returns the number of bytes needed for one deinterlaced image
    pub fn raw_bytes(&self) -> usize {
        self.height as usize * self.raw_row_length()
    }
    
    /// Returns the number of bytes needed for one deinterlaced row 
    pub fn raw_row_length(&self) -> usize {
        let bits = self.width as usize * self.color_type.samples() * self.bit_depth as usize;
        let extra = bits % 8;
        bits/8
        + match extra { 0 => 0, _ => 1 }
        + 1 // filter method
    }
}

/// Test
bitflags! {
    flags Transformations: u32 {
        /// No transformation
        const TRANSFORM_IDENTITY            = 0x0000, // read and write */
        /// Strip 16-bit samples to 8 bits
        const TRANSFORM_STRIP_16            = 0x0001, // read only */
        /// Discard the alpha channel
        const TRANSFORM_STRIP_ALPHA         = 0x0002, // read only */
        /// Expand 1, 2 and 4-bit samples to bytes
        const TRANSFORM_PACKING             = 0x0004, // read and write */
        /// Change order of packed pixels to LSB first
        const TRANSFORM_PACKSWAP            = 0x0008, // read and write */
        /// Expand paletted images to RGB, expand grayscale images of
        /// less than 8-bit depth to 8-bit depth, and expand tRNS chunks
        /// to alpha channels.
        const TRANSFORM_EXPAND              = 0x0010, // read only */
        /// Invert monochrome images
        const TRANSFORM_INVERT_MONO         = 0x0020, // read and write */
        /// Normalize pixels to the sBIT depth
        const TRANSFORM_SHIFT               = 0x0040, // read and write */
        /// Flip RGB to BGR, RGBA to BGRA
        const TRANSFORM_BGR                 = 0x0080, // read and write */
        /// Flip RGBA to ARGB or GA to AG
        const TRANSFORM_SWAP_ALPHA          = 0x0100, // read and write */
        /// Byte-swap 16-bit samples
        const TRANSFORM_SWAP_ENDIAN         = 0x0200, // read and write */
        /// Change alpha from opacity to transparency
        const TRANSFORM_INVERT_ALPHA        = 0x0400, // read and write */
        const TRANSFORM_STRIP_FILLER        = 0x0800, // write only */
        const TRANSFORM_STRIP_FILLER_BEFORE = 0x0800, // write only
        const TRANSFORM_STRIP_FILLER_AFTER  = 0x1000, // write only */
        const TRANSFORM_GRAY_TO_RGB         = 0x2000, // read only */ 
        const TRANSFORM_EXPAND_16           = 0x4000, // read only */ 
        const TRANSFORM_SCALE_16            = 0x8000, // read only */ 
    }
}


    