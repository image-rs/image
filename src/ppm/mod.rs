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

    #[test]
    fn test_roundtrip_ppm_16bit() {
        // 3x3 image that tries all the 0/65535 RGB combinations plus a few more values
        // that check for big-endian conversion correctness
        let buf: [u16; 27] = [
                0,     0,     0,
                0,     0, 65535,
                0, 65535,     0,
                0, 65535, 65535,
            65535,     0,     0,
            65535,     0, 65535,
            65535, 65535,     0,
            65535, 65535, 65535,
             1000,  2000,  3000,
        ];
        let mut bytebuf = [0 as u8; 54];
        for (o, i) in bytebuf.chunks_mut(2).zip(buf.iter()) {
          o[0] = (i >> 8) as u8;
          o[1] = (i & 0xff) as u8;
        }

        let mut stream = Vec::<u8>::new();
        {
            let mut encoder = super::PPMEncoder::new(&mut stream);
            match encoder.encode(&bytebuf, 3, 3, ColorType::RGB(16)) {
                Ok(_) => {},
                Err(_) => panic!("PPM encoder failed"),
            };
        }

        let mut decoder = match super::PPMDecoder::new(&stream[..]) {
            Ok(img) => img,
            Err(e) => panic!("PPM decoder failed with {}", e),
        };
        match decoder.read_image() {
            Ok(DecodingResult::U16(vec)) => {
                assert_eq!(&buf[..], &vec[..]);
            },
            r => {
                panic!("PPM: Got a strange image result {:?}", r);
            }
        }
    }
}
