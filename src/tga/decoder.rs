use std::io;
use std::io::{Read, Seek};
use byteorder::{ReadBytesExt, LittleEndian};

use image::ImageError;
use image::ImageResult;
use image::ImageDecoder;
use image::DecodingResult;
use color::ColorType;

enum ImageType {
    NoImageData = 0,
    /// Uncompressed images
    RawColorMap = 1,
    RawTrueColor = 2,
    RawGrayScale = 3,
    /// Run length encoded images
    RunColorMap = 9,
    RunTrueColor = 10,
    RunGrayScale = 11,
    Unknown,
}

impl ImageType {
    /// Create a new image type from a u8
    fn new(img_type: u8) -> ImageType {
        match img_type {
            0  => ImageType::NoImageData,

            1  => ImageType::RawColorMap,
            2  => ImageType::RawTrueColor,
            3  => ImageType::RawGrayScale,

            9  => ImageType::RunColorMap,
            10 => ImageType::RunTrueColor,
            11 => ImageType::RunGrayScale,

            _  => ImageType::Unknown,
        }
    }

    /// Check if the image format uses colors as opposed to gray scale
    fn is_color(&self) -> bool {
        match *self {
            ImageType::RawColorMap  |
            ImageType::RawTrueColor |
            ImageType::RunTrueColor |
            ImageType::RunColorMap => true,
            _ => false,
        }
    }

    /// Does the image use a color map
    fn is_color_mapped(&self) -> bool {
        match *self {
            ImageType::RawColorMap |
            ImageType::RunColorMap => true,
            _ => false,
        }
    }

    /// Is the image run length encoded
    fn is_encoded(&self) -> bool {
        match *self {
            ImageType::RunColorMap |
            ImageType::RunTrueColor |
            ImageType::RunGrayScale => true,
            _ => false,
        }
    }
}

/// Header used by TGA image files
#[derive(Debug)]
struct Header {
    id_length: u8,         // length of ID string
    map_type: u8,          // color map type
    image_type: u8,        // image type code
    map_origin: u16,       // starting index of map
    map_length: u16,      // length of map
    map_entry_size: u8,    // size of map entries in bits
    x_origin: u16,         // x-origin of image
    y_origin: u16,         // y-origin of image
    image_width: u16,      // width of image
    image_height: u16,     // height of image
    pixel_depth: u8,       // bits per pixel
    image_desc: u8,        // image descriptor
}

impl Header {
    /// Create a header with all valuse set to zero
    fn new() -> Header {
        Header {
            id_length: 0,
            map_type: 0,
            image_type: 0,
            map_origin: 0,
            map_length: 0,
            map_entry_size: 0,
            x_origin: 0,
            y_origin: 0,
            image_width: 0,
            image_height: 0,
            pixel_depth: 0,
            image_desc: 0,
        }
    }

    /// Load the header with values from the reader
    fn from_reader(r: &mut Read) -> ImageResult<Header> {
        Ok(Header {
            id_length:         try!(r.read_u8()),
            map_type:          try!(r.read_u8()),
            image_type:        try!(r.read_u8()),
            map_origin:        try!(r.read_u16::<LittleEndian>()),
            map_length:        try!(r.read_u16::<LittleEndian>()),
            map_entry_size:    try!(r.read_u8()),
            x_origin:          try!(r.read_u16::<LittleEndian>()),
            y_origin:          try!(r.read_u16::<LittleEndian>()),
            image_width:       try!(r.read_u16::<LittleEndian>()),
            image_height:      try!(r.read_u16::<LittleEndian>()),
            pixel_depth:       try!(r.read_u8()),
            image_desc:        try!(r.read_u8()),
        })
    }
}

struct ColorMap {
    /// sizes in bytes
    start_offset: usize,
    entry_size: usize,
    bytes: Vec<u8>,
}

impl ColorMap {
    pub fn from_reader(r: &mut Read,
                       start_offset: u16,
                       num_entries: u16,
                       bits_per_entry: u8)
        -> ImageResult<ColorMap> {
            let bytes_per_entry = (bits_per_entry as usize + 7) / 8;

            let mut bytes = vec![0; bytes_per_entry * num_entries as usize];
            try!(r.read_exact(&mut bytes));

            Ok(ColorMap {
                entry_size: bytes_per_entry,
                start_offset: start_offset as usize,
                bytes: bytes,
            })
        }

    /// Get one entry from the color map
    pub fn get(&self, index: usize) -> &[u8] {
        let entry = self.start_offset + self.entry_size * index;
        &self.bytes[entry..entry + self.entry_size]
    }
}

/// The representation of a TGA decoder
pub struct TGADecoder<R> {
    r: R,

    width: usize,
    height: usize,
    bytes_per_pixel: usize,
    has_loaded_metadata: bool,

    image_type: ImageType,
    color_type: ColorType,

    header: Header,
    color_map: Option<ColorMap>,
}

impl<R: Read + Seek> TGADecoder<R> {
    /// Create a new decoder that decodes from the stream `r`
    pub fn new(r: R) -> TGADecoder<R> {
        TGADecoder {
            r: r,

            width: 0,
            height: 0,
            bytes_per_pixel: 0,
            has_loaded_metadata: false,

            image_type: ImageType::Unknown,
            color_type: ColorType::Gray(1),

            header: Header::new(),
            color_map: None,
        }
    }

    fn read_header(&mut self) -> ImageResult<()> {
        self.header = try!(Header::from_reader(&mut self.r));
        self.image_type = ImageType::new(self.header.image_type);
        self.width = self.header.image_width as usize;
        self.height = self.header.image_height as usize;
        self.bytes_per_pixel = (self.header.pixel_depth as usize + 7) / 8;
        Ok(())
    }

    fn read_metadata(&mut self) -> ImageResult<()> {
        if !self.has_loaded_metadata {
            try!(self.read_header());
            try!(self.read_image_id());
            try!(self.read_color_map());
            try!(self.read_color_information());
            self.has_loaded_metadata = true;
        }
        Ok(())
    }

    /// Loads the color information for the decoder
    ///
    /// To keep things simple, we won't handle bit depths that aren't divisible
    /// by 8 and are less than 32.
    fn read_color_information(&mut self) -> ImageResult<()> {
        if self.header.pixel_depth % 8 != 0 {
            return Err(ImageError::UnsupportedError("\
                Bit depth must be divisible by 8".to_string()));
        }
        if self.header.pixel_depth > 32 {
            return Err(ImageError::UnsupportedError("\
                Bit depth must be less than 32".to_string()));
        }

        let num_alpha_bits = self.header.image_desc & 0b1111;

        let other_channel_bits = if self.header.map_type != 0 {
            self.header.map_entry_size
        } else {
            self.header.pixel_depth - num_alpha_bits
        };
        let color = self.image_type.is_color();

        match (num_alpha_bits, other_channel_bits, color) {
            // really, the encoding is BGR and BGRA, this is fixed
            // up with `TGADecoder::reverse_encoding`.
            (8, 24, true) => self.color_type = ColorType::RGBA(8),
            (0, 24, true) => self.color_type = ColorType::RGB(8),
            (8, 8, false) => self.color_type = ColorType::GrayA(8),
            (0, 8, false) => self.color_type = ColorType::Gray(8),
            _ => return Err(ImageError::UnsupportedError(format!("\
                    Color format not supported. Bit depth: {}, Alpha bits: {}",
                    other_channel_bits, num_alpha_bits).to_string())),
        }
        Ok(())
    }

    /// Read the image id field
    ///
    /// We're not interested in this field, so this function skips it if it
    /// is present
    fn read_image_id(&mut self) -> ImageResult<()> {
        try!(self.r.seek(io::SeekFrom::Current(self.header.id_length as i64)));
        Ok(())
    }

    fn read_color_map(&mut self) -> ImageResult<()> {
        if self.header.map_type == 1 {
            self.color_map = Some(try!(
                ColorMap::from_reader(&mut self.r,
                                      self.header.map_origin,
                                      self.header.map_length,
                                      self.header.map_entry_size)));
        }
        Ok(())
    }

    /// Expands indices into its mapped color
    fn expand_color_map(&mut self, pixel_data: Vec<u8>) -> Vec<u8> {
        #[inline]
        fn bytes_to_index(bytes: &[u8]) -> usize {
            let mut result = 0usize;
            for byte in bytes.iter() {
                result = result << 8 | *byte as usize;
            }
            result
        }

        let bytes_per_entry = (self.header.map_entry_size as usize + 7) / 8;
        let mut result = Vec::with_capacity(self.width * self.height *
                                            bytes_per_entry);

        let color_map = match self.color_map {
            Some(ref color_map) => color_map,
            None => unreachable!(),
        };

        for chunk in pixel_data.chunks(self.bytes_per_pixel) {
            let index = bytes_to_index(chunk);
            result.extend(color_map.get(index).iter().map(|&c| c));
        }

        result
    }

    fn read_image_data(&mut self) -> ImageResult<Vec<u8>> {
        // read the pixels from the data region
        let mut pixel_data = if self.image_type.is_encoded() {
            try!(self.read_encoded_data())
        } else {
            let num_raw_bytes = self.width * self.height * self.bytes_per_pixel;
            let mut buf = vec![0; num_raw_bytes];
            try!(self.r.by_ref().read_exact(&mut buf));
            buf
        };

        // expand the indices using the color map if necessary
        if self.image_type.is_color_mapped() {
            pixel_data = self.expand_color_map(pixel_data)
        }

        self.reverse_encoding(&mut pixel_data);
        Ok(pixel_data)
    }

    /// Reads a run length encoded packet
    fn read_encoded_data(&mut self) -> ImageResult<Vec<u8>> {
        let num_bytes = self.width * self.height * self.bytes_per_pixel;
        let mut pixel_data = Vec::with_capacity(num_bytes);

        while pixel_data.len() < num_bytes {
            let run_packet = try!(self.r.read_u8());
            // If the highest bit in `run_packet` is set, then we repeat pixels
            //
            // Note: the TGA format adds 1 to both counts because having a count
            // of 0 would be pointless.
            if (run_packet & 0x80) != 0 {
                // high bit set, so we will repeat the data
                let repeat_count = ((run_packet & !0x80) + 1) as usize;
                let mut data = Vec::with_capacity(self.bytes_per_pixel);
                try!(self.r.by_ref().take(self.bytes_per_pixel as u64).read_to_end(&mut data));
                for _ in 0usize..repeat_count {
                    pixel_data.extend(data.iter().map(|&c| c));
                }
            } else {
                // not set, so `run_packet+1` is the number of non-encoded pixels
                let num_raw_bytes = (run_packet + 1) as usize * self.bytes_per_pixel;
                try!(self.r.by_ref().take(num_raw_bytes as u64).read_to_end(&mut pixel_data));
            }
        }

        Ok(pixel_data)
    }

    /// Reverse from BGR encoding to RGB encoding
    ///
    /// TGA files are stored in the BGRA encoding. This function swaps
    /// the blue and red bytes in the `pixels` array.
    fn reverse_encoding(&mut self, pixels: &mut [u8]) {
        // We only need to reverse the encoding of color images
        match self.color_type {
            ColorType::RGB(8) => {
                for chunk in pixels.chunks_mut(self.bytes_per_pixel) {
                    let r = chunk[0];
                    chunk[0] = chunk[2];
                    chunk[2] = r;
                }
            }
            ColorType::RGBA(8) => {
                for chunk in pixels.chunks_mut(self.bytes_per_pixel) {
                    let r = chunk[0];
                    chunk[0] = chunk[2];
                    chunk[2] = r;
                }
            }
            _ => { }
        }
    }
}

impl<R: Read + Seek> ImageDecoder for TGADecoder<R> {
    fn dimensions(&mut self) -> ImageResult<(u32, u32)> {
        try!(self.read_metadata());

        Ok((self.width as u32, self.height as u32))
    }

    fn colortype(&mut self) -> ImageResult<ColorType> {
        try!(self.read_metadata());

        Ok(self.color_type)
    }

    fn row_len(&mut self) -> ImageResult<usize> {
        try!(self.read_metadata());

        Ok(self.bytes_per_pixel * 8 * self.width)
    }

    fn read_scanline(&mut self, _buf: &mut [u8]) -> ImageResult<u32> {
        unimplemented!();
    }

    fn read_image(&mut self) -> ImageResult<DecodingResult> {
        try!(self.read_metadata());
        self.read_image_data().map(|v| DecodingResult::U8(v) )
    }
}
