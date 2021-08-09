use std::io::Write;

use crate::ImageResult;


/// WebP Encoder
pub struct WebPEncoder<W: Write> {
    w: W,
}

impl<W: Write> WebPEncoder<W> {
    /// Creates a new encoder that writes its output to w
    pub fn new(w: W) -> WebPEncoder<W> {
        WebPEncoder {
            w,
        }
    }

    /// Encodes an image
    pub fn encode(&mut self, data: &[u8], width: u16, height: u16) -> ImageResult<()> {
        
        self.w.write(b"RIFF")?;
        self.w.write(&[0, 0, 10, 10])?;// file size?
        self.w.write(b"WEBP")?;

        todo!()
    }

    
}







