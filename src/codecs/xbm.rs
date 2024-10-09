//! Encoding of XBM images
//!
//! # Related Links
//!
//! - <https://www.x.org/releases/X11R7.7/doc/libX11/libX11/libX11.html#Manipulating_Bitmaps>

use std::io::Write;

use crate::{
    color::ExtendedColorType,
    error::{ImageError, ImageResult, UnsupportedError, UnsupportedErrorKind},
    image::{ImageEncoder, ImageFormat},
};

/// XBM encoder
pub struct XbmEncoder<W: Write> {
    writer: W,
}

impl<W: Write> XbmEncoder<W> {
    /// Creates a new encoder that writes its output to ```writer```.
    pub fn new(writer: W) -> XbmEncoder<W> {
        Self { writer }
    }

    /// Encodes the image `buf` that has dimensions `width` and `height`.
    ///
    /// # Panics
    ///
    /// Panics if `width * height != buf.len()` or `buf` contains pixels other
    /// than `0` or `1`.
    #[track_caller]
    pub fn encode(mut self, name: &str, buf: &[u8], width: u32, height: u32) -> ImageResult<()> {
        let expected_buffer_len = u64::from(width) * u64::from(height);
        assert_eq!(
            expected_buffer_len,
            buf.len() as u64,
            "Invalid buffer length: expected {expected_buffer_len} got {} for {width}x{height} image",
            buf.len(),
        );
        assert!(
            !buf.iter().any(|&p| p > 1),
            "image contained pixels other than `0` or `1`"
        );
        self.writer
            .write_all(format!("#define {name}_width {width}\n").as_bytes())?;
        self.writer
            .write_all(format!("#define {name}_height {height}\n").as_bytes())?;
        self.writer
            .write_all(format!("static unsigned char {name}_bits[] = {{\n").as_bytes())?;
        let mut pixels = Vec::with_capacity(12);
        for bits_per_line in buf.chunks(width as usize) {
            for bits in bits_per_line.chunks(8) {
                let mut byte = 0;
                for (i, bit) in bits.iter().enumerate() {
                    byte |= bit << (7 - i);
                }
                byte = byte.reverse_bits();
                pixels.push(byte);
                // line_width <= 80
                if pixels.len() == 12 {
                    let line = pixels
                        .iter()
                        .map(|p| format!("{p:#04x}"))
                        .collect::<Vec<_>>()
                        .join(", ");
                    self.writer.write_all(format!("    {line},\n").as_bytes())?;
                    pixels.clear();
                }
            }
        }
        if !pixels.is_empty() {
            let line = pixels
                .iter()
                .map(|p| format!("{p:#04x}"))
                .collect::<Vec<_>>()
                .join(", ");
            self.writer.write_all(format!("    {line},\n").as_bytes())?;
        }
        self.writer.write_all(b"};\n")?;
        Ok(())
    }
}

impl<W: Write> ImageEncoder for XbmEncoder<W> {
    #[track_caller]
    fn write_image(
        self,
        buf: &[u8],
        width: u32,
        height: u32,
        color_type: ExtendedColorType,
    ) -> ImageResult<()> {
        if color_type != ExtendedColorType::L1 {
            return Err(ImageError::Unsupported(
                UnsupportedError::from_format_and_kind(
                    ImageFormat::Xbm.into(),
                    UnsupportedErrorKind::Color(color_type),
                ),
            ));
        }
        self.encode("image", buf, width, height)
    }
}

#[cfg(test)]
mod tests {
    use std::str;

    use super::*;

    #[test]
    fn write() {
        let mut writer = [0; 132];
        // "B" (8x7)
        let buf = b"\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x01\x01\x01\x00\x00\x00\
\x00\x00\x01\x00\x00\x01\x00\x00\
\x00\x00\x01\x01\x01\x00\x00\x00\
\x00\x00\x01\x00\x00\x01\x00\x00\
\x00\x00\x01\x01\x01\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00";
        let expected = "#define image_width 8
#define image_height 7
static unsigned char image_bits[] = {
    0x00, 0x1c, 0x24, 0x1c, 0x24, 0x1c, 0x00,
};
";
        XbmEncoder::new(&mut writer[..])
            .write_image(buf, 8, 7, ExtendedColorType::L1)
            .unwrap();
        assert_eq!(str::from_utf8(&writer).unwrap(), expected);
    }

    #[test]
    fn write_16x14() {
        let mut writer = [0; 268];
        // "B" (16x14)
        let buf = b"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x01\x01\x01\x01\x01\x01\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x01\x01\x01\x01\x01\x01\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x01\x01\x00\x00\x00\x00\x01\x01\x00\x00\x00\x00\
\x00\x00\x00\x00\x01\x01\x00\x00\x00\x00\x01\x01\x00\x00\x00\x00\
\x00\x00\x00\x00\x01\x01\x01\x01\x01\x01\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x01\x01\x01\x01\x01\x01\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x01\x01\x00\x00\x00\x00\x01\x01\x00\x00\x00\x00\
\x00\x00\x00\x00\x01\x01\x00\x00\x00\x00\x01\x01\x00\x00\x00\x00\
\x00\x00\x00\x00\x01\x01\x01\x01\x01\x01\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x01\x01\x01\x01\x01\x01\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00";
        let expected = "#define image_width 16
#define image_height 14
static unsigned char image_bits[] = {
    0x00, 0x00, 0x00, 0x00, 0xf0, 0x03, 0xf0, 0x03, 0x30, 0x0c, 0x30, 0x0c,
    0xf0, 0x03, 0xf0, 0x03, 0x30, 0x0c, 0x30, 0x0c, 0xf0, 0x03, 0xf0, 0x03,
    0x00, 0x00, 0x00, 0x00,
};
";
        XbmEncoder::new(&mut writer[..])
            .write_image(buf, 16, 14, ExtendedColorType::L1)
            .unwrap();
        assert_eq!(str::from_utf8(&writer).unwrap(), expected);
    }

    #[test]
    fn write_width_7() {
        let mut writer = [0; 126];
        // "I" (7x6)
        let buf = b"\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x01\x01\x01\x00\x00\
\x00\x00\x00\x01\x00\x00\x00\
\x00\x00\x00\x01\x00\x00\x00\
\x00\x00\x01\x01\x01\x00\x00\
\x00\x00\x00\x00\x00\x00\x00";
        let expected = "#define image_width 7
#define image_height 6
static unsigned char image_bits[] = {
    0x00, 0x1c, 0x08, 0x08, 0x1c, 0x00,
};
";
        XbmEncoder::new(&mut writer[..])
            .write_image(buf, 7, 6, ExtendedColorType::L1)
            .unwrap();
        assert_eq!(str::from_utf8(&writer).unwrap(), expected);
    }

    #[test]
    fn write_width_14() {
        let mut writer = [0; 240];
        // "I" (14x12)
        let buf = b"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x01\x01\x01\x01\x01\x01\x00\x00\x00\x00\
\x00\x00\x00\x00\x01\x01\x01\x01\x01\x01\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x01\x01\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x01\x01\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x01\x01\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x01\x01\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x01\x01\x01\x01\x01\x01\x00\x00\x00\x00\
\x00\x00\x00\x00\x01\x01\x01\x01\x01\x01\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00";
        let expected = "#define image_width 14
#define image_height 12
static unsigned char image_bits[] = {
    0x00, 0x00, 0x00, 0x00, 0xf0, 0x03, 0xf0, 0x03, 0xc0, 0x00, 0xc0, 0x00,
    0xc0, 0x00, 0xc0, 0x00, 0xf0, 0x03, 0xf0, 0x03, 0x00, 0x00, 0x00, 0x00,
};
";
        XbmEncoder::new(&mut writer[..])
            .write_image(buf, 14, 12, ExtendedColorType::L1)
            .unwrap();
        assert_eq!(str::from_utf8(&writer).unwrap(), expected);
    }

    #[test]
    fn write_invalid_extended_color_type() {
        let mut writer = [];
        let buf = [0; 8];
        assert!(XbmEncoder::new(&mut writer[..])
            .write_image(&buf, 8, 1, ExtendedColorType::Rgba8)
            .is_err());
    }

    #[test]
    #[should_panic(expected = "Invalid buffer length: expected 32 got 8 for 16x2 image")]
    fn write_invalid_dimensions() {
        let mut writer = [];
        let buf = [0; 8];
        let _: ImageResult<()> =
            XbmEncoder::new(&mut writer[..]).write_image(&buf, 16, 2, ExtendedColorType::L1);
    }

    #[test]
    #[should_panic(expected = "image contained pixels other than `0` or `1`")]
    fn write_invalid_pixels() {
        let mut writer = [];
        let buf = b"\x00\x01\x02\x03\x03\x02\x01\x00";
        let _: ImageResult<()> =
            XbmEncoder::new(&mut writer[..]).write_image(buf, 8, 1, ExtendedColorType::L1);
    }
}
