//! Encoding of PPM Images
use std::io;
use std::io::Write;

use color::{ColorType, num_components};
use super::{HeaderRecord, PNMHeader, PNMSubtype, SampleEncoding};
use super::{ArbitraryHeader, ArbitraryTuplType, BitmapHeader, GraymapHeader, PixmapHeader};

enum HeaderStrategy {
    Dynamic,
    Subtype(PNMSubtype),
    Chosen(PNMHeader),
}

pub struct PNMEncoder<'a> {
    writer: &'a mut Write,
    header: HeaderStrategy,
}

enum TupleEncoding {
    PbmBits {
        width: u32,
        height: u32,
    },
    U8 {
        encoding: SampleEncoding,
        width: u32,
        height: u32,
        components: u32,
    },
}

impl<'a> PNMEncoder<'a> {
    /// Create new PNMEncoder from the `writer`.
    ///
    /// The encoded images will have some `pnm` format. If more control over the image type is
    /// required, use either one of `with_subtype` or `with_header`. For more information on the
    /// behaviour, see `with_dynamic_header`.
    pub fn new(writer: &'a mut Write) -> Self {
        PNMEncoder {
            writer,
            header: HeaderStrategy::Dynamic,
        }
    }

    /// Encode a specific pnm subtype image.
    ///
    /// The magic number and encoding type will be chosen as provided while the rest of the header
    /// data will be generated dynamically. Trying to encode incompatible images (e.g. encoding an
    /// RGB image as Graymap) will result in an error.
    ///
    /// This will overwrite the effect of earlier calls to `with_header` and `with_dynamic_header`.
    pub fn with_subtype(self, subtype: PNMSubtype) -> Self {
        PNMEncoder {
            writer: self.writer,
            header: HeaderStrategy::Subtype(subtype),
        }
    }

    /// Enforce the use of a chosen header.
    ///
    /// While this option gives the most control over the actual written data, the encoding process
    /// will error in case the header data and image parameters do not agree. It is the users
    /// obligation to ensure that the width and height are set accordingly, for example.
    ///
    /// Choose this option if you want a lossless decoding/encoding round trip.
    ///
    /// This will overwrite the effect of earlier calls to `with_subtype` and `with_dynamic_header`.
    pub fn with_header(self, header: PNMHeader) -> Self {
        PNMEncoder {
            writer: self.writer,
            header: HeaderStrategy::Chosen(header)
        }
    }

    /// Create the header dynamically for each image.
    ///
    /// This is the default option upon creation of the encoder. With this, most images should be
    /// encodable but the specific format chosen is out of the users control. The pnm subtype is
    /// chosen arbitrarily by the library.
    ///
    /// This will overwrite the effect of earlier calls to `with_subtype` and `with_header`.
    pub fn with_dynamic_header(self) -> Self {
        PNMEncoder {
            writer: self.writer,
            header: HeaderStrategy::Dynamic,
        }
    }

    pub fn encode(&mut self, image: &[u8], width: u32, height: u32, color: ColorType)
        -> io::Result<()>
    {
        check_buffer_dimensions(image, width, height, color)?;
        let tuple_encoding = self.write_header(image, width, height, color)?;
        self.write_image(tuple_encoding, image)
    }

    /// Write the header and report back how the pixels should be written.
    fn write_header(&mut self, image: &[u8], width: u32, height: u32, c: ColorType)
        -> io::Result<TupleEncoding>
    {
        match self.header {
            HeaderStrategy::Dynamic
                => self.write_dynamic_header(width, height, c),
            HeaderStrategy::Subtype(subtype)
                => self.write_subtyped_header(subtype, width, height, c),
            HeaderStrategy::Chosen(ref header)
                => self.write_chosen_header(header, width, height, c),
        }
    }

    /// Choose any valid pnm format that the image can be expressed in and write its header.
    ///
    /// Returns how the body should be written if successful.
    fn write_dynamic_header(&mut self, width: u32, height: u32, color: ColorType)
        -> io::Result<TupleEncoding>
    {
        let depth = num_components(color) as u32;
        let (maxval, tupltype) = match color {
            ColorType::Gray(1) => (1, ArbitraryTuplType::BlackAndWhite),
            ColorType::GrayA(1) => (1, ArbitraryTuplType::BlackAndWhiteAlpha),
            ColorType::Gray(8) => (255, ArbitraryTuplType::Grayscale),
            ColorType::GrayA(8) => (255, ArbitraryTuplType::GrayscaleAlpha),
            ColorType::RGB(8) => (255, ArbitraryTuplType::RBG),
            ColorType::RGBA(8) => (255, ArbitraryTuplType::RGBAlpha),
            _ => return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    &format!("Writing colour type {:?} is not supported", color)[..]))
        };

        self.write_arbitrary_header(&ArbitraryHeader {
            width,
            height,
            depth,
            maxval,
            tupltype: Some(tupltype),
        })?;

        Ok(TupleEncoding::U8 {
            encoding: SampleEncoding::Binary,
            width,
            height,
            components: depth,
        })
    }

    /// Try to encode the image with the chosen format, give its corresponding pixel encoding type.
    fn write_subtyped_header(
        &mut self,
        subtype: PNMSubtype,
        width: u32,
        height: u32,
        color: ColorType)
        -> io::Result<TupleEncoding>
    {
        match (subtype, color) {
            (PNMSubtype::ArbitraryMap, color) => self.write_dynamic_header(width, height, color),
            (PNMSubtype::Pixmap(encoding), ColorType::RGB(8)) => {
                self.write_pixmap_header(&PixmapHeader {
                    encoding,
                    width,
                    height,
                    maxval: 255,
                }).map(|()| TupleEncoding::U8 {
                    encoding,
                    width,
                    height,
                    components: 3, // num_components(RGB(8)) == 3
                })
            },
            (PNMSubtype::Graymap(encoding), ColorType::Gray(8)) => {
                self.write_graymap_header(&GraymapHeader {
                    encoding,
                    width,
                    height,
                    maxwhite: 255,
                }).map(|()| TupleEncoding::U8 {
                    encoding,
                    width,
                    height,
                    components: 1, // num_components(Gray(8)) == 1
                })
            },
            (PNMSubtype::Bitmap(encoding), ColorType::Gray(8))
            | (PNMSubtype::Bitmap(encoding), ColorType::Gray(1)) => {
                self.write_bitmap_header(&BitmapHeader {
                    encoding: encoding,
                    width,
                    height,
                })?;
                match encoding {
                    SampleEncoding::Binary => Ok(TupleEncoding::PbmBits {
                        width,
                        height,
                    }),
                    SampleEncoding::Ascii => Ok(TupleEncoding::U8 {
                        encoding: SampleEncoding::Ascii,
                        width,
                        height,
                        components: 1,
                    })
                }
            },
            (_, _) => Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "Color type can not be represented in the chosen format"))
        }
    }

    /// Try to encode the image with the chosen header, checking if values are correct.
    ///
    /// Returns how the body should be written if successful.
    fn write_chosen_header(
        &mut self,
        header: &PNMHeader,
        width: u32,
        height: u32,
        color: ColorType)
        -> io::Result<TupleEncoding>
    {
        let tupltype = check_chosen_header(&header, width, height, color)?;

        match header {
            &PNMHeader { encoded: Some(ref content), .. }
                => self.writer.write_all(content)?,
            &PNMHeader { decoded: HeaderRecord::Bitmap(ref header), .. }
                => self.write_bitmap_header(header)?,
            &PNMHeader { decoded: HeaderRecord::Graymap(ref header), .. }
                => self.write_graymap_header(header)?,
            &PNMHeader { decoded: HeaderRecord::Pixmap(ref header), .. }
                => self.write_pixmap_header(header)?,
            &PNMHeader { decoded: HeaderRecord::Arbitrary(ref header), .. }
                => self.write_arbitrary_header(header)?,
        };

        Ok(tupltype)
    }

    fn write_bitmap_header(&mut self, &BitmapHeader{ encoding, width, height }: &BitmapHeader)
        -> io::Result<()>
    {
        self.writer.write_all(PNMSubtype::Bitmap(encoding).magic_constant())?;
        write!(self.writer, "\n{} {}\n", width, height)
    }

    fn write_graymap_header(
        &mut self,
        &GraymapHeader{ encoding, width, height, maxwhite}: &GraymapHeader)
        -> io::Result<()>
    {
        self.writer.write_all(PNMSubtype::Graymap(encoding).magic_constant())?;
        write!(self.writer, "\n{} {} {}\n", width, height, maxwhite)
    }

    fn write_pixmap_header(
        &mut self,
        &PixmapHeader{ encoding, width, height, maxval}: &PixmapHeader)
        -> io::Result<()>
    {
        self.writer.write_all(PNMSubtype::Pixmap(encoding).magic_constant())?;
        write!(self.writer, "\n{} {} {}\n", width, height, maxval)
    }

    fn write_arbitrary_header(&mut self, &ArbitraryHeader{ width, height, depth, maxval, tupltype}: &ArbitraryHeader)
        -> io::Result<()>
    {
        let custom_fallback = String::new();
        let tupltype = match tupltype {
            None => "",
            Some(ArbitraryTuplType::BlackAndWhite) => "TUPLTYPE BLACKANDWHITE\n",
            Some(ArbitraryTuplType::BlackAndWhiteAlpha) => "TUPLTYPE BLACKANDWHITE_ALPHA\n",
            Some(ArbitraryTuplType::Grayscale) => "TUPLTYPE GRAYSCALE\n",
            Some(ArbitraryTuplType::GrayscaleAlpha) => "TUPLTYPE GRAYSCALE_ALPHA\n",
            Some(ArbitraryTuplType::RBG) => "TUPLTYPE RGB\n",
            Some(ArbitraryTuplType::RGBAlpha) => "TUPLTYPE RGB_ALPHA\n",
            Some(ArbitraryTuplType::Custom(ref custom)) => {
                custom_fallback = format!("TUPLTYPE {}", custom);
                &custom_fallback
            }
        };

        write!(self.writer, "P7\nWIDTH {}\nHEIGHT {}\nDEPTH {}\nMAXVAL {}\n{}ENDHDR",
            width, height, depth, maxval, tupltype)
    }

    fn write_image(&mut self, encoding: TupleEncoding, image: &[u8]) -> io::Result<()> {
        match encoding {
            TupleEncoding::U8 { encoding: SampleEncoding::Binary, .. }
                => self.writer.write_all(image),
            TupleEncoding::U8 { encoding: SampleEncoding::Ascii, .. } => {
                // Standard dictates to insert line breaks after 70 characters. Assumes that no
                // line breaks are written.
                struct AutoBreak<'a> {
                    wrapped: io::LineWriter<&'a mut Write>,
                    line_capacity: usize,
                    buffer: Vec<u8>,
                    needs_flush: bool,
                }

                impl<'a> Write for AutoBreak<'a> {
                    fn write(&mut self, buffer: &[u8]) -> io::Result<usize> {
                        if self.needs_flush {
                            self.flush()?;
                            assert!(self.buffer.len() == 0);
                        }

                        // Buffer may be longer than the capacity but there's no way around
                        if self.buffer.len() == 0 {
                            self.buffer.extend_from_slice(buffer);
                            return Ok(buffer.len());
                        }

                        if buffer.len() + self.buffer.len() > self.line_capacity {
                            self.needs_flush = true;
                            self.buffer.push(b'\n');
                            self.flush()?;
                            assert!(self.buffer.len() == 0);
                        }

                        self.buffer.extend_from_slice(buffer);
                        Ok(buffer.len())
                    }

                    fn flush(&mut self) -> io::Result<()> {
                        let written = self.wrapped.write(self.buffer.as_slice())?;

                        self.buffer.drain(0..written);
                        if self.buffer.len() > 0 {
                            return Err(io::Error::new(
                                io::ErrorKind::Interrupted,
                                "Line was not fully flushed"))
                        }

                        self.needs_flush = false;
                        self.wrapped.flush()
                    }
                }

                let auto_break_writer = AutoBreak {
                    wrapped: io::LineWriter::new(self.writer),
                    line_capacity: 70,
                    buffer: Vec::with_capacity(71),
                    needs_flush: false,
                };

                for value in image.iter() {
                    write!(auto_break_writer, "{} ", value)?;
                }
                Ok(())
            },
            TupleEncoding::PbmBits { width, height } => {
                /// These should have thrown errors previously.
                assert_eq!(width*height, image.len());
                assert!(image.len() > 0);

                let line_width = (width - 1)/8 + 1;

                // We'll be writing single bytes, so buffer
                let line_buffer = Vec::with_capacity(line_width as usize);

                for line in image.chunks(width as usize) {
                    for byte_bits in line.chunks(8) {
                        let byte = 0u8;
                        for i in 0..8 {
                            // Black pixels are encoded as 1s
                            if let Some(&0u8) = byte_bits.get(i) {
                                byte |= 1u8 << (7 - i)
                            }
                        }
                        line_buffer.push(byte)
                    }
                    self.writer.write_all(line_buffer.as_slice());
                    line_buffer.clear();
                }

                Ok(())
            },
            // _ => panic!()
        }

    }
}

fn check_buffer_dimensions(image: &[u8], width: u32, height: u32, color: ColorType)
    -> io::Result<()>
{
    let components = num_components(color);
    let width = width as usize;
    let height = height as usize;
    match Some(components)
        .and_then(|v| v.checked_mul(width))
        .and_then(|v| v.checked_mul(height)) {
        None => Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            &format!("Image dimensions invalid: {}x{}x{} (wxhxd)",
                width, height, components)[..])),
        Some(v) if v != image.len()
            => Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                &format!("Image buffer does not correspond to size and colour")[..])),
        Some(v) => Ok(())
    }
}

fn check_chosen_header(header: &PNMHeader, width: u32, height: u32, color: ColorType)
    -> io::Result<TupleEncoding>
{
    if header.width() != width || header.height() != height {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "Chosen header does not match Image dimensions"));
    }

    // FIXME check other conditions …
    // if header

    Ok(TupleEncoding::U8 {
        encoding: SampleEncoding::Binary,
        width,
        height,
        components: num_components(color) as u32,
    })
}
