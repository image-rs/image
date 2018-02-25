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

/// Encodes images to any of the `pnm` image formats.
pub struct PNMEncoder<'a> {
    writer: &'a mut Write,
    header: HeaderStrategy,
}

// Encapsulate the checking system in the type system.
struct CheckedImageBuffer<'a> {
    image: &'a [u8],
    width: u32,
    #[allow(unused)] // Part of the checked property but not accessed
    height: u32,
    #[allow(unused)] // Part of the checked property but not accessed
    color: ColorType,
}

// Check the header against the buffer. Each struct produces the next after a check.
struct UncheckedHeader<'a> {
    header: &'a PNMHeader,
}
struct CheckedDimensions<'a> {
    unchecked: UncheckedHeader<'a>,
    width: u32,
    height: u32,
}
struct CheckedHeaderColor<'a> {
    dimensions: CheckedDimensions<'a>,
    color: ColorType
}
struct CheckedHeader<'a> {
    color: CheckedHeaderColor<'a>,
    encoding: CheckedEncoding<'a>,
}

// After writing a CheckedHeader we get a checked encoding.
struct CheckedEncoding<'a> {
    encoding: TupleEncoding,
    image: CheckedImageBuffer<'a>,
}

enum TupleEncoding {
    PbmBits,
    U8 {
        encoding: SampleEncoding,
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

    /// Encode an image whose samples are represented as `u8`.
    ///
    /// Some `pnm` subtypes are incompatible with some color options, a chosen header most
    /// certainly with any deviation from the original decoded image.
    pub fn encode(&mut self, image: &[u8], width: u32, height: u32, color: ColorType)
        -> io::Result<()>
    {
        match self.header {
            HeaderStrategy::Dynamic
                => self.write_dynamic_header(image, width, height, color),
            HeaderStrategy::Subtype(subtype)
                => self.write_subtyped_header(subtype, image, width, height, color),
            HeaderStrategy::Chosen(ref header)
                => Self::write_with_header(&mut self.writer, header, image, width, height, color),
        }
    }

    /// Choose any valid pnm format that the image can be expressed in and write its header.
    ///
    /// Returns how the body should be written if successful.
    fn write_dynamic_header(&mut self, image: &[u8], width: u32, height: u32, color: ColorType)
        -> io::Result<()>
    {
        let depth = num_components(color) as u32;
        let (maxval, tupltype) = match color {
            ColorType::Gray(1) => (1, ArbitraryTuplType::BlackAndWhite),
            ColorType::GrayA(1) => (1, ArbitraryTuplType::BlackAndWhiteAlpha),
            ColorType::Gray(n @ 1...8) => (1 << n, ArbitraryTuplType::Grayscale),
            ColorType::GrayA(n @ 1...8) => (1 << n, ArbitraryTuplType::GrayscaleAlpha),
            ColorType::RGB(n @ 1...8) => (1 << n, ArbitraryTuplType::RBG),
            ColorType::RGBA(n @ 1...8) => (1 << n, ArbitraryTuplType::RGBAlpha),
            _ => return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    &format!("Encoding colour type {:?} is not supported", color)[..]))
        };

        let header = PNMHeader {
            decoded: HeaderRecord::Arbitrary(ArbitraryHeader {
                width,
                height,
                depth,
                maxval,
                tupltype: Some(tupltype),
            }),
            encoded: None,
        };

        Self::write_with_header(&mut self.writer, &header, image, width, height, color)
    }

    /// Try to encode the image with the chosen format, give its corresponding pixel encoding type.
    fn write_subtyped_header(
        &mut self,
        subtype: PNMSubtype,
        image: &[u8],
        width: u32,
        height: u32,
        color: ColorType)
        -> io::Result<()>
    {
        let header = match (subtype, color) {
            (PNMSubtype::ArbitraryMap, color)
                => return self.write_dynamic_header(image, width, height, color),
            (PNMSubtype::Pixmap(encoding), ColorType::RGB(8)) =>
                PNMHeader {
                    decoded: HeaderRecord::Pixmap(PixmapHeader {
                        encoding,
                        width,
                        height,
                        maxval: 255,
                    }),
                    encoded: None
                },
            (PNMSubtype::Graymap(encoding), ColorType::Gray(8)) =>
                PNMHeader {
                    decoded: HeaderRecord::Graymap(GraymapHeader {
                        encoding,
                        width,
                        height,
                        maxwhite: 255,
                    }),
                    encoded: None
                },
            (PNMSubtype::Bitmap(encoding), ColorType::Gray(8))
            | (PNMSubtype::Bitmap(encoding), ColorType::Gray(1)) =>
                PNMHeader {
                    decoded: HeaderRecord::Bitmap(BitmapHeader {
                        encoding,
                        width,
                        height,
                    }),
                    encoded: None
                },
            (_, _) => return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "Color type can not be represented in the chosen format"))
        };

        Self::write_with_header(&mut self.writer, &header, image, width, height, color)
    }

    /// Try to encode the image with the chosen header, checking if values are correct.
    ///
    /// Returns how the body should be written if successful.
    fn write_with_header(
        writer: &mut Write,
        header: &PNMHeader,
        image: &[u8],
        width: u32,
        height: u32,
        color: ColorType)
        -> io::Result<()>
    {
        let unchecked = UncheckedHeader {
            header,
        };

        unchecked.check_header_dimensions(width, height)?
            .check_header_color(color)?
            .check_sample_values(image)?
            .write_header(writer)?
            .write_image(writer)
    }
}

impl <'a> CheckedEncoding<'a> {
    fn write_image(self, writer: &mut Write) -> io::Result<()> {
        let CheckedEncoding {
            encoding,
            image: CheckedImageBuffer {
                image,
                width,
            ..},
        .. } = self;

        match encoding {
            TupleEncoding::U8 { encoding: SampleEncoding::Binary, .. }
                => writer.write_all(image),
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

                let mut auto_break_writer = AutoBreak {
                    wrapped: io::LineWriter::new(writer),
                    line_capacity: 70,
                    buffer: Vec::with_capacity(71),
                    needs_flush: false,
                };

                for value in image.iter() {
                    write!(auto_break_writer, "{} ", value)?;
                }
                Ok(())
            },
            TupleEncoding::PbmBits => {
                /// These should have thrown errors previously.
                assert!(image.len() > 0);
                assert!(image.len() % (width as usize) == 0);

                // The length of an encoded scanline
                let line_width = (width - 1)/8 + 1;

                // We'll be writing single bytes, so buffer
                let mut line_buffer = Vec::with_capacity(line_width as usize);

                for line in image.chunks(width as usize) {
                    for byte_bits in line.chunks(8) {
                        let mut byte = 0u8;
                        for i in 0..8 {
                            // Black pixels are encoded as 1s
                            if let Some(&0u8) = byte_bits.get(i) {
                                byte |= 1u8 << (7 - i)
                            }
                        }
                        line_buffer.push(byte)
                    }
                    writer.write_all(line_buffer.as_slice())?;
                    line_buffer.clear();
                }

                Ok(())
            },
        }
    }
}

impl<'a> CheckedImageBuffer<'a> {
    fn check(image: &'a [u8], width: u32, height: u32, color: ColorType)
        -> io::Result<CheckedImageBuffer<'a>>
    {
        let components = num_components(color);
        let uwidth = width as usize;
        let uheight = height as usize;
        match Some(components)
            .and_then(|v| v.checked_mul(uwidth))
            .and_then(|v| v.checked_mul(uheight)) {
            None => Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                &format!("Image dimensions invalid: {}×{}×{} (w×h×d)",
                    width, height, components)[..])),
            Some(v) if v == image.len()
                => Ok(CheckedImageBuffer {
                    image,
                    width,
                    height,
                    color,
                }),
            Some(_)
                => Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    &format!("Image buffer does not correspond to size and colour")[..])),
        }
    }
}

impl<'a> UncheckedHeader<'a> {
    fn check_header_dimensions(self, width: u32, height: u32)
        -> io::Result<CheckedDimensions<'a>>
    {
        if self.header.width() != width || self.header.height() != height {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "Chosen header does not match Image dimensions"));
        }

        Ok(CheckedDimensions {
            unchecked: self,
            width,
            height,
        })
    }
}

impl<'a> CheckedDimensions<'a> {
    // Check color compatibility with the header. This will only error when we are certain that
    // the comination is bogus (e.g. combining Pixmap and Palette) but allows uncertain
    // combinations (basically a ArbitraryTuplType::Custom with any color of fitting depth).
    fn check_header_color(self, color: ColorType)
        -> io::Result<CheckedHeaderColor<'a>>
    {
        let components = match color {
            ColorType::Gray(_) => 1,
            ColorType::GrayA(_) => 2,
            ColorType::Palette(_)
            | ColorType::RGB(_) => 3,
            ColorType::RGBA(_) => 4,
        };

        match self.unchecked.header {
            &PNMHeader { decoded: HeaderRecord::Bitmap(_), .. } => match color {
                ColorType::Gray(_) => (),
                _ => return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    "PBM format only support ColorType::Gray")),
            },
            &PNMHeader { decoded: HeaderRecord::Graymap(_), ..} => match color {
                ColorType::Gray(_) =>  (),
                _ => return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    "PGM format only support ColorType::Gray")),
            },
            &PNMHeader { decoded: HeaderRecord::Pixmap(_), .. } => match color {
                ColorType::RGB(_) =>  (),
                _ => return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    "PPM format only support ColorType::RGB")),
            },
            &PNMHeader { decoded: HeaderRecord::Arbitrary(
                ArbitraryHeader { depth, ref tupltype, .. }), ..} => match (tupltype, color) {
                (&None, _) if depth == components => (),
                (&Some(ArbitraryTuplType::BlackAndWhite), ColorType::Gray(_)) => (),
                (&Some(ArbitraryTuplType::BlackAndWhiteAlpha), ColorType::GrayA(_)) => (),
                (&Some(ArbitraryTuplType::Grayscale), ColorType::Gray(_)) => (),
                (&Some(ArbitraryTuplType::GrayscaleAlpha), ColorType::GrayA(_)) => (),
                (&Some(ArbitraryTuplType::RBG), ColorType::RGB(_)) => (),
                (&Some(ArbitraryTuplType::RGBAlpha), ColorType::RGBA(_)) => (),
                (&Some(ArbitraryTuplType::Custom(_)), _) if depth == components => (),
                _ => return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    "Invalid color type for selected PAM color type")),
            }
        }

        Ok(CheckedHeaderColor {
            dimensions: self,
            color
        })
    }
}

impl<'a> CheckedHeaderColor<'a> {
    fn check_sample_values(self, image: &'a [u8])
        -> io::Result<CheckedHeader<'a>>
    {
        let header_maxval = match &self.dimensions.unchecked.header.decoded {
            &HeaderRecord::Bitmap(_) => 1,
            &HeaderRecord::Graymap(GraymapHeader { maxwhite, .. }) => maxwhite,
            &HeaderRecord::Pixmap(PixmapHeader { maxval, .. }) => maxval,
            &HeaderRecord::Arbitrary(ArbitraryHeader { maxval, .. }) => maxval,
        };

        // We trust the image color bit count to be correct at least.
        let max_sample = match self.color {
            // Protects against overflows from shifting and gives a better error.
            ColorType::Gray(n) | ColorType::GrayA(n) | ColorType::Palette(n)
            | ColorType::RGB(n) | ColorType::RGBA(n) if n > 8
                => return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    "Encoding colors with a bit depth greater 8 not supported")),
            ColorType::Gray(n)
            | ColorType::GrayA(n)
            | ColorType::Palette(n)
            | ColorType::RGB(n)
            | ColorType::RGBA(n) => 1 << n,
        };

        // Avoid the performance heavy check if possible, e.g. if the header has been chosen by us.
        if header_maxval < max_sample && image.iter().any(|&val| u32::from(val) > header_maxval) {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "Sample value greater than allowed for chosen header"));
        }

        let encoding = match &self.dimensions.unchecked.header.decoded {
            &HeaderRecord::Bitmap(BitmapHeader {
                    encoding: SampleEncoding::Binary,
                .. }) => TupleEncoding::PbmBits,
            &HeaderRecord::Bitmap(BitmapHeader {
                    encoding: SampleEncoding::Ascii,
                .. }) => TupleEncoding::U8 { encoding: SampleEncoding::Ascii },
            &HeaderRecord::Graymap(GraymapHeader { encoding, .. })
            | &HeaderRecord::Pixmap(PixmapHeader { encoding, .. })
                => TupleEncoding::U8 { encoding },
            &HeaderRecord::Arbitrary(_)
                => TupleEncoding::U8 { encoding: SampleEncoding::Binary },
        };

        let image = CheckedImageBuffer::check(image, self.dimensions.width,
            self.dimensions.height, self.color)?;

        Ok(CheckedHeader {
            color: self,
            encoding: CheckedEncoding {
                encoding,
                image,
            }
        })
    }
}

impl<'a> CheckedHeader<'a> {
    fn write_header(self, writer: &mut Write) -> io::Result<CheckedEncoding<'a>> {

        let CheckedHeader {
            color: CheckedHeaderColor {
                dimensions: CheckedDimensions {
                    unchecked: UncheckedHeader{ header, .. },
                .. },
            .. },
            encoding,
        } = self;

        match header {
            &PNMHeader { encoded: Some(ref content), .. }
                => writer.write_all(content)?,
            &PNMHeader { decoded: HeaderRecord::Bitmap(ref header), .. }
                => Self::write_bitmap_header(writer, header)?,
            &PNMHeader { decoded: HeaderRecord::Graymap(ref header), .. }
                => Self::write_graymap_header(writer, header)?,
            &PNMHeader { decoded: HeaderRecord::Pixmap(ref header), .. }
                => Self::write_pixmap_header(writer, header)?,
            &PNMHeader { decoded: HeaderRecord::Arbitrary(ref header), .. }
                => Self::write_arbitrary_header(writer, header)?,
        }

        Ok(encoding)
    }

    fn write_bitmap_header(writer: &mut Write, &BitmapHeader{ encoding, width, height }: &BitmapHeader)
        -> io::Result<()>
    {
        writer.write_all(PNMSubtype::Bitmap(encoding).magic_constant())?;
        write!(writer, "\n{} {}\n", width, height)
    }

    fn write_graymap_header(
        writer: &mut Write,
        &GraymapHeader{ encoding, width, height, maxwhite}: &GraymapHeader)
        -> io::Result<()>
    {
        writer.write_all(PNMSubtype::Graymap(encoding).magic_constant())?;
        write!(writer, "\n{} {} {}\n", width, height, maxwhite)
    }

    fn write_pixmap_header(
        writer: &mut Write,
        &PixmapHeader{ encoding, width, height, maxval}: &PixmapHeader)
        -> io::Result<()>
    {
        writer.write_all(PNMSubtype::Pixmap(encoding).magic_constant())?;
        write!(writer, "\n{} {} {}\n", width, height, maxval)
    }

    fn write_arbitrary_header(
        writer: &mut Write,
        &ArbitraryHeader{ width, height, depth, maxval, ref tupltype}: &ArbitraryHeader)
        -> io::Result<()>
    {
        #[allow(unused_assignments)]
        // Declared here so its lifetime exceeds the matching. This is a trivial
        // constructor, no allocation takes place and in the custom case we must allocate
        // regardless due to borrow. Still, the warnings checker does pick this up :/
        // Could use std::borrow::Cow instead but that really doesn't achieve anything but
        // increasing type complexity.
        let mut custom_fallback = String::new();

        let tupltype = match tupltype {
            &None => "",
            &Some(ArbitraryTuplType::BlackAndWhite) => "TUPLTYPE BLACKANDWHITE\n",
            &Some(ArbitraryTuplType::BlackAndWhiteAlpha) => "TUPLTYPE BLACKANDWHITE_ALPHA\n",
            &Some(ArbitraryTuplType::Grayscale) => "TUPLTYPE GRAYSCALE\n",
            &Some(ArbitraryTuplType::GrayscaleAlpha) => "TUPLTYPE GRAYSCALE_ALPHA\n",
            &Some(ArbitraryTuplType::RBG) => "TUPLTYPE RGB\n",
            &Some(ArbitraryTuplType::RGBAlpha) => "TUPLTYPE RGB_ALPHA\n",
            &Some(ArbitraryTuplType::Custom(ref custom)) => {
                custom_fallback = format!("TUPLTYPE {}", custom);
                &custom_fallback
            }
        };

        write!(writer, "P7\nWIDTH {}\nHEIGHT {}\nDEPTH {}\nMAXVAL {}\n{}ENDHDR",
            width, height, depth, maxval, tupltype)
    }
}
