//! Decoding of netpbm image formats (pbm, pgm, ppm and pam).
//!
//! The formats pbm, pgm and ppm are fully supported. The pam decoder recognizes the tuple types
//! `BLACKANDWHITE`, `GRAYSCALE` and `RGB` and explicitely recognizes but rejects their `_ALPHA`
//! variants for now as alpha color types are unsupported.
pub use self::decoder::PNMDecoder;
pub use self::encoder::PNMEncoder;
pub use self::header::{PNMHeader, PNMSubtype, SampleEncoding};
pub use self::header::{ArbitraryHeader, ArbitraryTuplType, BitmapHeader, GraymapHeader, PixmapHeader};
use self::autobreak::AutoBreak;
use self::header::HeaderRecord;

mod autobreak;
mod decoder;
mod encoder;
mod header;

#[cfg(test)]
mod tests {
    use super::*;
    use color::ColorType;
    use image::{DecodingResult, ImageDecoder};

    fn execute_roundtrip_default(buffer: &[u8], width: u32, height: u32, color: ColorType) {
        let mut encoded_buffer = Vec::new();

        {
            let mut encoder = PNMEncoder::new(&mut encoded_buffer);
            encoder.encode(buffer, width, height, color)
                .expect("Failed to encode the image buffer");
        }

        let (header, loaded_color, loaded_image) = {
            let mut decoder = PNMDecoder::new(&encoded_buffer[..]).unwrap();
            let colortype = decoder.colortype()
                .expect("Failed to decode color type");
            let image = decoder.read_image()
                .expect("Failed to decode the image");
            let (_, header) = decoder.into_inner();
            (header, colortype, image)
        };

        assert_eq!(header.width(), width);
        assert_eq!(header.height(), height);
        assert_eq!(loaded_color, color);
        match loaded_image {
            DecodingResult::U8(ref data) if data.as_slice() == buffer => (),
            _ => panic!("Loaded image buffer deviates from original! {:?}", loaded_image),
        }
    }

    fn execute_roundtrip_with_subtype(
        buffer: &[u8], width: u32, height: u32, color: ColorType, subtype: PNMSubtype) {
        let mut encoded_buffer = Vec::new();

        {
            let mut encoder = PNMEncoder::new(&mut encoded_buffer)
                .with_subtype(subtype);
            encoder.encode(buffer, width, height, color)
                .expect("Failed to encode the image buffer");
        }

        let (header, loaded_color, loaded_image) = {
            let mut decoder = PNMDecoder::new(&encoded_buffer[..]).unwrap();
            let colortype = decoder.colortype()
                .expect("Failed to decode color type");
            let image = decoder.read_image()
                .expect("Failed to decode the image");
            let (_, header) = decoder.into_inner();
            (header, colortype, image)
        };

        assert_eq!(header.width(), width);
        assert_eq!(header.height(), height);
        assert_eq!(header.subtype(), subtype);
        assert_eq!(loaded_color, color);
        match loaded_image {
            DecodingResult::U8(ref data) if data.as_slice() == buffer => (),
            _ => panic!("Loaded image buffer deviates from original! {:?}", loaded_image),
        }
    }

    fn execute_roundtrip_u16(buffer: &[u16], width: u32, height: u32, color: ColorType) {
        let mut encoded_buffer = Vec::new();

        {
            let mut encoder = PNMEncoder::new(&mut encoded_buffer);
            encoder.encode(buffer, width, height, color)
                .expect("Failed to encode the image buffer");
        }

        let (header, loaded_color, loaded_image) = {
            let mut decoder = PNMDecoder::new(&encoded_buffer[..]).unwrap();
            let colortype = decoder.colortype()
                .expect("Failed to decode color type");
            let image = decoder.read_image()
                .expect("Failed to decode the image");
            let (_, header) = decoder.into_inner();
            (header, colortype, image)
        };

        assert_eq!(header.width(), width);
        assert_eq!(header.height(), height);
        assert_eq!(loaded_color, color);
        match loaded_image {
            DecodingResult::U16(ref data) if data.as_slice() == buffer => (),
            _ => panic!("Loaded image buffer deviates from original! {:?}", loaded_image),
        }
    }

    #[test]
    fn roundtrip_rgb() {
        let buf: [u8; 27] = [
              0,   0,   0,
              0,   0, 255,
              0, 255,   0,
              0, 255, 255,
            255,   0,   0,
            255,   0, 255,
            255, 255,   0,
            255, 255, 255,
            255, 255, 255,
        ];
        execute_roundtrip_default(&buf, 3, 3, ColorType::RGB(8));
        execute_roundtrip_with_subtype(&buf, 3, 3, ColorType::RGB(8),
            PNMSubtype::ArbitraryMap);
        execute_roundtrip_with_subtype(&buf, 3, 3, ColorType::RGB(8),
            PNMSubtype::Pixmap(SampleEncoding::Binary));
        execute_roundtrip_with_subtype(&buf, 3, 3, ColorType::RGB(8),
            PNMSubtype::Pixmap(SampleEncoding::Ascii));
    }

    #[test]
    fn roundtrip_u16() {
        let buf: [u16; 6] = [
            0, 1, 0xFFFF, 0x1234, 0x3412, 0xBEAF,
        ];

        execute_roundtrip_u16(&buf, 6, 1, ColorType::Gray(16));
    }
}
