use std::slice;
use std::io;

use image;
use image::ImageResult;
use image::ImageDecoder;
use color;

use super::lzw::LZWReader;

const IMAGEDESCRIPTOR: u8 = 0x2C;
const EXTENSION: u8 = 0x21;
const APPLICATION: u8 = 0xFF;
const GRAPHICCONTROL: u8 = 0xF9;
const COMMENT: u8 = 0xFE;
const TRAILER: u8 = 0x3B;

/// The Representation of a GIF decoder
pub struct GIFDecoder <R> {
    r: R,

    width: u16,
    height: u16,

    global_table: [(u8, u8, u8), ..256],
    local_table: Option<Vec<(u8, u8, u8)>>,

    delay: u16,
    image: Vec<u8>,

    global_backgroud_index: Option<u8>,
    local_transparent_index: Option<u8>,

    have_header: bool,
    decoded_rows: u32,
}

impl<R: Reader> GIFDecoder<R> {
    /// Create a new GIFDecoder from the Reader ```r```.
    /// This function takes ownership of the Reader.
    pub fn new(r: R) -> GIFDecoder<R> {
        GIFDecoder {
            r: r,

            width: 0,
            height: 0,

            global_table: [(0u8, 0u8, 0u8), ..256],
            local_table: None,

            delay: 0,
            image: Vec::new(),

            global_backgroud_index: None,
            local_transparent_index: None,

            have_header: false,
            decoded_rows: 0,
        }
    }

    ///Returns the display delay in 100th's of a second for the currently
    ///decoded image.
    pub fn delay(&mut self) -> ImageResult<u16> {

        let _ = try!(self.read_metadata());

        Ok(self.delay)
    }

    fn read_header(&mut self) -> ImageResult<()> {
        let signature = try!(self.r.read_exact(3));
        let version   = try!(self.r.read_exact(3));

        if signature.as_slice() != "GIF".as_bytes() {
            Err(image::ImageError::FormatError("GIF signature not found.".to_string()))
        } else if version.as_slice() != "87a".as_bytes() &&
                  version.as_slice() != "89a".as_bytes() {
            Err(image::ImageError::UnsupportedError(
                format!("GIF version {} is not supported.", version)
            ))
        } else {
            Ok(())
        }
    }

    fn read_block(&mut self) -> io::IoResult<Vec<u8>> {
        let size = try!(self.r.read_u8());
        Ok(try!(self.r.read_exact(size as uint)))
    }

    fn read_image_data(&mut self) -> ImageResult<Vec<u8>> {
        let minimum_code_size = try!(self.r.read_u8());

        if minimum_code_size > 8 {
            return Err(image::ImageError::FormatError(format!("Invalid code size {}.", minimum_code_size)))
        }

        let mut data = Vec::new();
        loop {
            let b = try!(self.read_block());

            if b.len() == 0 {
                break
            }

            data = data + b;
        }

        let m = io::MemReader::new(data);
        let mut lzw = LZWReader::new(m, minimum_code_size);
        let b = lzw.read_to_end().unwrap();

        Ok(b)
    }

    fn read_image_descriptor(&mut self) -> ImageResult<()> {
        let image_left   = try!(self.r.read_le_u16());
        let image_top    = try!(self.r.read_le_u16());
        let image_width  = try!(self.r.read_le_u16());
        let image_height = try!(self.r.read_le_u16());

        let fields = try!(self.r.read_u8());

        let local_table = fields & 80 != 0;
        let interlace   = fields & 40 != 0;
        let table_size  = fields & 7;

        if interlace {
            return Err(image::ImageError::UnsupportedError("Interlaced images are not supported.".to_string()))
        }

        if local_table {
            let n   = 1 << (table_size + 1) as uint;
            let buf = try!(self.r.read_exact(3 * n));
            let mut b = Vec::from_elem(n, (0u8, 0u8, 0u8));

            for (i, rgb) in buf.as_slice().chunks(3).enumerate() {
                b.as_mut_slice()[i] = (rgb[0], rgb[1], rgb[2]);
            }

            self.local_table = Some(b);
        }

        let indices = try!(self.read_image_data());

        {

            let trans_index = if self.local_transparent_index.is_some() {
                self.local_transparent_index
            } else {
                self.global_backgroud_index
            };

            let table = if self.local_table.is_some() {
                self.local_table.as_ref().unwrap().as_slice()
            } else {
                self.global_table.as_slice()
            };

            expand_image(
                table,
                indices.as_slice(),
                image_top as uint,
                image_left as uint,
                image_width as uint,
                image_height as uint,
                self.width as uint * 3,
                trans_index,
                self.image.as_mut_slice()
            );
        }

        self.local_table = None;
        self.local_transparent_index = None;

        Ok(())
    }

    fn read_extension(&mut self) -> ImageResult<()> {
        let identifier = try!(self.r.read_u8());

        match identifier {
            APPLICATION    => try!(self.read_application_extension()),
            GRAPHICCONTROL => try!(self.read_graphic_control_extension()),
            COMMENT 	   => try!(self.read_comment_extension()),
            _              => return Err(image::ImageError::UnsupportedError(
                                  format!("Identifier {} is not supported.", identifier))
                              )
        }

        Ok(())
    }

    fn read_comment_extension(&mut self) -> ImageResult<()> {
        loop {
            let b = try!(self.read_block());

            if b.len() == 0 {
                break
            }
        }

        Ok(())
    }

    fn read_graphic_control_extension(&mut self) -> ImageResult<()> {
        let size   = try!(self.r.read_u8());
        assert!(size == 4);

        let fields = try!(self.r.read_u8());
        self.delay = try!(self.r.read_le_u16());
        let trans  = try!(self.r.read_u8());

        if fields & 1 != 0 {
            self.local_transparent_index = Some(trans);
        }

        let _disposal = (fields & 0x1C) >> 2;
        let _term = try!(self.r.read_u8());

        Ok(())
    }

    fn read_application_extension(&mut self) -> ImageResult<()> {
        let size = try!(self.r.read_u8());
        let _ = try!(self.r.read_exact(size as uint));

        loop {
            let b = try!(self.read_block());

            if b.len() == 0 {
                break
            }
        }

        Ok(())
    }

    fn read_logical_screen_descriptor(&mut self) -> ImageResult<()> {
        self.width  = try!(self.r.read_le_u16());
        self.height = try!(self.r.read_le_u16());
        self.image  = Vec::from_elem(self.width as uint * self.height as uint * 3, 0u8);

        let fields = try!(self.r.read_u8());

        let global_table = fields & 0x80 != 0;

        let entries = if global_table {
            1 << ((fields & 7) + 1) as uint
        } else {
            0u
        };

        let b = try!(self.r.read_u8());
        if global_table {
            self.global_backgroud_index = Some(b);
        }

        let _aspect_ratio = try!(self.r.read_u8());

        let buf = try!(self.r.read_exact(3 * entries));

        for (i, rgb) in buf.as_slice().chunks(3).enumerate() {
            self.global_table.as_mut_slice()[i] = (rgb[0], rgb[1], rgb[2]);
        }

        Ok(())
    }

    fn read_metadata(&mut self) -> ImageResult<()> {
        if !self.have_header {
            let _ = try!(self.read_header());
            let _ = try!(self.read_logical_screen_descriptor());
            self.have_header = true;
        }

        Ok(())
    }
}

impl<R: Reader> ImageDecoder for GIFDecoder<R> {
    fn dimensions(&mut self) -> ImageResult<(u32, u32)> {
        let _ = try!(self.read_metadata());
        Ok((self.width as u32, self.height as u32))
    }

    fn colortype(&mut self) -> ImageResult<color::ColorType> {
        let _ = try!(self.read_metadata());
        Ok(color::ColorType::RGB(8))
    }

    fn row_len(&mut self) -> ImageResult<uint> {
        let _ = try!(self.read_metadata());
        Ok(3 * self.width as uint)
    }

    fn read_scanline(&mut self, buf: &mut [u8]) -> ImageResult<u32> {
        let _ = try!(self.read_metadata());
        if self.decoded_rows == self.height as u32 || self.image.len() == 0 {
            let _ = try!(self.read_image());
        }

        let rlen  = buf.len();
        let slice = self.image.slice(self.decoded_rows as uint * rlen,
        self.decoded_rows as uint * rlen + rlen);

        slice::bytes::copy_memory(buf, slice);
        self.decoded_rows += 1;

        Ok(self.decoded_rows)
    }

    fn read_image(&mut self) -> ImageResult<Vec<u8>> {
        let _ = try!(self.read_metadata());
        loop {
            let block = try!(self.r.read_u8());

            match block {
                EXTENSION => try!(self.read_extension()),
                IMAGEDESCRIPTOR => {
                    let _ = try!(self.read_image_descriptor());
                    return Ok(self.image.clone())
                }
                TRAILER => break,
                _       => return Err(image::ImageError::UnsupportedError(
                            format!("Block type {} is not supported.", block))
                        )
            }
        }

        Err(image::ImageError::ImageEnd)
    }
}

fn expand_image(palette: &[(u8, u8, u8)],
                indices: &[u8],
                y0: uint,
                x0: uint,
                width: uint,
                height: uint,
                stride: uint,
                trans_index: Option<u8>,
                image: &mut [u8]) {

    for y in range(0, height) {
        for x in range(0, width) {
            let index = indices[y * width + x];

            if trans_index == Some(index) {
                continue
            }

            let (r, g, b) = palette[index as uint];

            image[(y0 + y) * stride + x0 * 3 + x * 3 + 0] = r;
            image[(y0 + y) * stride + x0 * 3 + x * 3 + 1] = g;
            image[(y0 + y) * stride + x0 * 3 + x * 3 + 2] = b;
        }
    }
}
