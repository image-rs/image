//! Encoding of portable pixmap Images

pub use self::encoder::PPMEncoder as PPMEncoder;
pub use self::decoder::PPMDecoder as PPMDecoder;

mod encoder;
mod decoder;

#[cfg(test)]
mod test {
    use color::ColorType;
    use image::{ImageDecoder, DecodingResult};

    #[test]
    fn test_roundtrip_ppm() {
        // 3x3 image that tries all the 0/255 RGB combinations
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

        let mut stream = Vec::<u8>::new();
        {
            let mut encoder = super::PPMEncoder::new(&mut stream);
            match encoder.encode(&buf, 3, 3, ColorType::RGB(8)) {
                Ok(_) => {},
                Err(_) => panic!("PPM encoder failed"),
            };
        }

        let mut decoder = match super::PPMDecoder::new(&stream[..]) {
            Ok(img) => img,
            Err(e) => panic!("PPM decoder failed with {}", e),
        };
        match decoder.read_image() {
            Ok(DecodingResult::U8(vec)) => {
                assert_eq!(&buf[..], &vec[..]);
            },
            r => {
                panic!("PPM: Got a strange image result {:?}", r);
            }
        }
    }
}
