//! The main color aware buffer.
use core::ops::Deref;
use std::io::{Seek, Write};

use crate::{
    error::{DecodingError, ImageFormatHint, ParameterError},
    flat::SampleLayout,
    metadata::Orientation,
    traits::{EncodableLayout, PixelWithColorType},
    ColorType, DynamicImage, ExtendedColorType, FlatSamples, ImageBuffer, ImageDecoder, ImageError,
    ImageFormat, ImageResult, Luma, LumaA, Rgb, Rgba,
};

use image_canvas::layout::{CanvasLayout, SampleBits, SampleParts, Texel};
use image_canvas::Canvas;
use image_canvas::{color::Color, layout::Block};

/// Arbitrary image data in memory, with its associated information.
///
/// A byte buffer for raw image data, with various layout and color possibilities.
///
/// This container is designed to hold byte data suitably aligned for further processing, without
/// making too many choices on allowed layouts and representations. In contrast to `DynamicImage`,
/// we try to avoid making assumptions about the suitable layout or policy about the color
/// representations. The user is given control over these at the cost of some convenience.
///
/// This implies that image operations can not be total for all possible `Image` data, there are
/// always error cases of unknown or unsupported data. In particular, the design work for data and
/// conversion pipelines for [`Image`] is still ongoing.
///
/// Note that it is possible to convert most [`ImageBuffer`] as well as [`DynamicImage`] instances
/// into a canvas that represents the same pixels and color. See `From` implementations below.
pub struct Image {
    /// The pixel data container.
    canvas: Canvas,
    metadata: ImageMetadata,
}

#[derive(Default)]
#[non_exhaustive]
pub struct ImageMetadata {
    /// The viewing orientation associated with the in-memory representation of the image.
    pub viewing_orientation: Option<Orientation>,
    /// The viewing orientation as stored in the underlying file.
    pub initial_orientation: Option<Orientation>,
    /// The EXIF data block attached to the file, if any.
    pub exif: Option<Vec<u8>>,
}

/// The data layout of an [`Image`].
///
/// Apart from the simplistic cases of matrices of uniform channel arrays, this can internally also
/// represent complex cases such as pixels with mixed channel types, matrices of compressed blocks,
/// and planar layouts. The public constructors define the possibilities that are support.
#[derive(Clone, PartialEq)]
pub struct ImageLayout {
    inner: CanvasLayout,
}

/// Signals that the `ExtendedColorType` couldn't be made into a canvas texel.
///
/// The goal here is to incrementally increase the support for texels and *eventually* turn this
/// struct into a public one, whose only member is an uninhabited (`Never`) type—such that all
/// methods returning it as a result become infallible.
#[derive(Clone, Debug, PartialEq)]
// By design. Please, clippy, recognizing this pattern would be helpful.
#[allow(missing_copy_implementations)]
pub struct UnknownCanvasTexelError {
    _inner: (),
}

impl Image {
    /// Allocate an image canvas for a given color, and dimensions.
    ///
    /// # Panics
    ///
    /// If the layout is invalid for this platform, i.e. requires more than `isize::MAX` bytes to
    /// allocate. Also panics if the allocator fails.
    pub fn new(color: ColorType, w: u32, h: u32) -> Self {
        let texel = color_to_texel(color);
        let color = nominal_color(color);

        let mut layout = CanvasLayout::with_texel(&texel, w, h).expect("layout error");
        layout.set_color(color).expect("layout error");

        Image {
            canvas: Canvas::new(layout),
            metadata: ImageMetadata::default(),
        }
    }

    /// Allocate an image canvas with complex layout.
    ///
    /// This is a generalization of [`Self::new`] and the error case of an invalid layout is
    /// isolated, and can be properly handled. Furthermore, this provides control over the
    /// allocation by making its size available to the caller before it is performed.
    pub fn with_layout(layout: ImageLayout) -> Self {
        Image {
            canvas: Canvas::new(layout.inner),
            metadata: ImageMetadata::default(),
        }
    }

    /// Allocate a canvas for the result of an image decoder, then read the image into it.
    pub fn from_decoder(decoder: impl ImageDecoder) -> ImageResult<Self> {
        let layout = ImageLayout::from_decoder(&decoder)?;
        let mut canvas = Canvas::new(layout);

        // FIXME: here we should have the trait return metadata.
        decoder.read_image(canvas.as_bytes_mut())?;

        Ok(Image {
            canvas,
            metadata: ImageMetadata::default(),
        })
    }

    /// Read an image into the canvas.
    ///
    /// The allocated memory of the current canvas is reused.
    ///
    /// On success, returns an `Ok`-value. The layout and contents are changed to the new image. On
    /// failure, returns an appropriate `Err`-value and the contents of this canvas are not defined
    /// (but initialized). The layout may have changed.
    pub fn decode(&mut self, decoder: impl ImageDecoder) -> ImageResult<()> {
        let layout = ImageLayout::from_decoder(&decoder)?;
        self.canvas.set_layout(layout);

        decoder.read_image(self.canvas.as_bytes_mut())?;

        Ok(())
    }

    /// The width of this canvas, in represented pixels.
    pub fn width(&self) -> u32 {
        self.canvas.layout().width()
    }

    /// The height of this canvas, in represented pixels.
    pub fn height(&self) -> u32 {
        self.canvas.layout().height()
    }

    /// Query file, interpretation, and other extraneous information about the image.
    ///
    /// While many of the fields are public they must not be modified incoherently with the color
    /// information, color data, and each other. Therefore this just returns an immutable view.
    pub fn metadata(&self) -> &ImageMetadata {
        &self.metadata
    }

    /// Get a reference to the raw bytes making up this canvas.
    ///
    /// The interpretation will very much depend on the current layout. Note that primitive
    /// channels will be generally stored in native-endian order.
    ///
    /// This can be used to pass the bytes through FFI but prefer [`Self::as_flat_samples_u8`] and
    /// related methods for a typed descriptor that will check against the underlying type of
    /// channels.
    pub fn as_bytes(&self) -> &[u8] {
        self.canvas.as_bytes()
    }

    /// Get a mutable reference to the raw bytes making up this canvas.
    ///
    /// The interpretation will very much depend on the current layout. Note that primitive
    /// channels will be generally stored in native-endian order.
    ///
    /// This can be used to initialize bytes from FFI.
    pub fn as_bytes_mut(&mut self) -> &mut [u8] {
        self.canvas.as_bytes_mut()
    }

    /// Drop all layout information and turn into raw bytes.
    ///
    /// This will not be any less efficient than calling `as_bytes().to_owned()` but it *may* be
    /// possible to reuse the allocation in the future.
    ///
    /// However, note that it is **unsound** to take the underlying allocation as its layout has an
    /// alignment mismatch with the layout that `Vec<u8>` is expecting (it is generally allocated
    /// to a much higher alignment). Instead, a reallocation is required; the allocator might
    /// recycle the allocation which avoids the byte copy.
    pub fn into_bytes(self) -> Vec<u8> {
        // No better way in `image-texel = 0.2.0` but may be in the future, we're changing the
        // alignment of the layout.
        self.as_bytes().to_owned()
    }

    /// Get a reference to channels, without conversion.
    ///
    /// This returns `None` if the channels of the underlying layout are not unsigned bytes, or if
    /// the layout is not a single image plane.
    pub fn as_flat_samples_u8(&self) -> Option<FlatSamples<&[u8]>> {
        let channels = self.canvas.channels_u8()?;
        let spec = channels.layout().spec();
        Some(FlatSamples {
            samples: channels.into_slice(),
            layout: Self::sample_layout(spec),
            color_hint: Self::color_type(self.canvas.layout()),
        })
    }

    /// Get a reference to channels, without conversion.
    ///
    /// This returns `None` if the channels of the underlying layout are not unsigned shorts, or if
    /// the layout is not a single image plane.
    pub fn as_flat_samples_u16(&self) -> Option<FlatSamples<&[u16]>> {
        let channels = self.canvas.channels_u16()?;
        let spec = channels.layout().spec();
        Some(FlatSamples {
            samples: channels.into_slice(),
            layout: Self::sample_layout(spec),
            color_hint: Self::color_type(self.canvas.layout()),
        })
    }

    /// Get a reference to channels, without conversion.
    ///
    /// This returns `None` if the channels of the underlying layout are not 32-bit floating point
    /// numbers, or if the layout is not a single image plane.
    pub fn as_flat_samples_f32(&self) -> Option<FlatSamples<&[f32]>> {
        let channels = self.canvas.channels_f32()?;
        let spec = channels.layout().spec();
        Some(FlatSamples {
            samples: channels.into_slice(),
            layout: Self::sample_layout(spec),
            color_hint: Self::color_type(self.canvas.layout()),
        })
    }

    /// Convert the data into an `ImageBuffer`.
    ///
    /// Performs color conversion if necessary, such as rearranging color channel order,
    /// transforming pixels to the `sRGB` color space etc.
    ///
    /// The subpixel of the result type is constrained by a sealed, internal trait that is
    /// implemented for all primitive numeric types.
    ///
    /// This is essentially an optimized method compared to using an intermediate `DynamicImage`.
    pub fn to_buffer<P: PixelWithColorType>(&self) -> ImageResult<ImageBuffer<P, Vec<P::Subpixel>>>
    where
        [P::Subpixel]: EncodableLayout,
    {
        let (width, height) = (self.width(), self.height());
        let mut buffer = ImageBuffer::<P, Vec<P::Subpixel>>::new(width, height);

        let mut fallback_canvas;
        // FIXME(perf): can we wrap the output buffer into a proper layout and have the library
        // convert into the output buffer instead?
        let canvas = if false {
            /* self.inner.layout().texel() == texel */
            // FIXME: can select `self` if it's exactly the correct color type and layout. For now
            // we just note the possibility down in the type system.
            &self.canvas
        } else {
            let (texel, color) = texel_color_for::<P>();
            let layout = CanvasLayout::with_texel(&texel, width, height)
                .expect("Valid layout because buffer has one");

            fallback_canvas = Canvas::new(layout);
            fallback_canvas
                .set_color(color)
                .expect("Valid for rgb layout");

            self.canvas
                .convert(&mut fallback_canvas)
                .map_err(Self::conversion_error)?;
            &fallback_canvas
        };

        let destination = buffer.as_bytes_mut();
        let initializer = canvas.as_bytes();
        let len = destination.len().min(initializer.len());

        debug_assert!(
            destination.len() == initializer.len(),
            "The layout computation should not differ"
        );

        destination[..len].copy_from_slice(&initializer[..len]);

        Ok(buffer)
    }

    /// Convert this canvas into an enumeration of simple buffer types.
    ///
    /// If the underlying texels are any of the simple [`ColorType`] formats then this format is
    /// chosen as the output variant. Otherwise, prefers `Rgba<f32>` if an alpha channel is present
    /// and `Rgb<f32>` otherwise. The conversion _may_ be lossy.
    ///
    /// This entails a re-allocation! However, the actual runtime costs vary depending on the
    /// actual current image layout. For example, if a color conversion to `sRGB` is necessary then
    /// this is more expensive than a channel reordering.
    pub fn to_dynamic(&self) -> ImageResult<DynamicImage> {
        let layout = self.canvas.layout();
        Ok(match Self::color_type(layout) {
            Some(ColorType::L8) => self.to_buffer::<Luma<u8>>()?.into(),
            Some(ColorType::La8) => self.to_buffer::<LumaA<u8>>()?.into(),
            Some(ColorType::Rgb8) => self.to_buffer::<Rgb<u8>>()?.into(),
            Some(ColorType::Rgba8) => self.to_buffer::<Rgba<u8>>()?.into(),
            Some(ColorType::L16) => self.to_buffer::<Luma<u16>>()?.into(),
            Some(ColorType::La16) => self.to_buffer::<LumaA<u16>>()?.into(),
            Some(ColorType::Rgb16) => self.to_buffer::<Rgb<u16>>()?.into(),
            Some(ColorType::Rgba16) => self.to_buffer::<Rgba<u16>>()?.into(),
            Some(ColorType::Rgb32F) => self.to_buffer::<Rgb<f32>>()?.into(),
            Some(ColorType::Rgba32F) => self.to_buffer::<Rgba<f32>>()?.into(),
            None if !Self::has_alpha(layout) => self.to_buffer::<Rgb<f32>>()?.into(),
            None => self.to_buffer::<Rgba<f32>>()?.into(),
        })
    }

    fn sample_layout(spec: image_canvas::layout::ChannelSpec) -> SampleLayout {
        SampleLayout {
            channels: spec.channels,
            channel_stride: spec.channel_stride,
            width: spec.width,
            width_stride: spec.width_stride,
            height: spec.height,
            height_stride: spec.height_stride,
        }
    }

    fn color_type(layout: &CanvasLayout) -> Option<ColorType> {
        let texel = layout.texel();
        let color = layout.color()?;

        if !matches!(texel.block, Block::Pixel) {
            return None;
        };

        if let &Color::SRGB = color {
            // Recognized color type, no color management required.
            if let SampleBits::UInt8x3 = texel.bits {
                if let SampleParts::Rgb = texel.parts {
                    return Some(ColorType::Rgb8);
                }
            } else if let SampleBits::UInt8x4 = texel.bits {
                if let SampleParts::RgbA = texel.parts {
                    return Some(ColorType::Rgba8);
                }
            } else if let SampleBits::UInt16x3 = texel.bits {
                if let SampleParts::Rgb = texel.parts {
                    return Some(ColorType::Rgb16);
                }
            } else if let SampleBits::UInt16x4 = texel.bits {
                if let SampleParts::RgbA = texel.parts {
                    return Some(ColorType::Rgba16);
                }
            } else if let SampleBits::Float32x3 = texel.bits {
                if let SampleParts::Rgb = texel.parts {
                    return Some(ColorType::Rgb32F);
                }
            } else if let SampleBits::Float32x4 = texel.bits {
                if let SampleParts::RgbA = texel.parts {
                    return Some(ColorType::Rgba32F);
                }
            }
        } else if let &Color::BT709 = color {
            if let SampleBits::UInt8 = texel.bits {
                if let SampleParts::Luma = texel.parts {
                    return Some(ColorType::L8);
                }
            } else if let SampleBits::UInt8x2 = texel.bits {
                if let SampleParts::LumaA = texel.parts {
                    return Some(ColorType::La8);
                }
            } else if let SampleBits::UInt16 = texel.bits {
                if let SampleParts::Luma = texel.parts {
                    return Some(ColorType::L16);
                }
            } else if let SampleBits::UInt16x2 = texel.bits {
                if let SampleParts::LumaA = texel.parts {
                    return Some(ColorType::La16);
                }
            }
        }

        None
    }

    fn conversion_error(err: image_canvas::ConversionError) -> ImageError {
        ImageError::Parameter(ParameterError::from_err(
            "unsupported layout conversion",
            err,
        ))
    }

    fn has_alpha(layout: &CanvasLayout) -> bool {
        layout.texel().parts.has_alpha()
    }
}

impl ImageLayout {
    /// Construct a row-major matrix for a given color type.
    pub fn new(color: ColorType, w: u32, h: u32) -> Option<Self> {
        let texel = color_to_texel(color);
        let color = nominal_color(color);

        let mut layout = CanvasLayout::with_texel(&texel, w, h).ok()?;
        layout.set_color(color).ok()?;

        Some(ImageLayout { inner: layout })
    }

    fn from_decoder<'a>(decoder: &dyn ImageDecoder) -> ImageResult<CanvasLayout> {
        // FIXME: we can also handle ExtendedColorType...
        let texel = color_to_texel(decoder.color_type());
        let (width, height) = decoder.dimensions();

        // FIXME: What about other scanline sizes?
        CanvasLayout::with_texel(&texel, width, height).map_err(|layout| {
            let decoder = DecodingError::new(ImageFormatHint::Unknown, format!("{:?}", layout));
            ImageError::Decoding(decoder)
        })
    }

    /// Construct a row-major matrix for an extended color type, i.e. generic texels.
    pub fn with_extended(
        color: ExtendedColorType,
        w: u32,
        h: u32,
    ) -> Result<Self, UnknownCanvasTexelError> {
        fn inner(color: ExtendedColorType, w: u32, h: u32) -> Option<ImageLayout> {
            let texel = extended_color_to_texel(color)?;
            let mut layout = CanvasLayout::with_texel(&texel, w, h).ok()?;
            if let Some(color) = extended_nominal_color(color) {
                layout.set_color(color).ok()?;
            }

            Some(ImageLayout { inner: layout })
        }

        inner(color, w, h).ok_or(UnknownCanvasTexelError { _inner: () })
    }

    /// Returns the number of bytes required for this layout.
    pub fn byte_len(&self) -> usize {
        self.inner.byte_len()
    }
}

impl Image {
    /// Encode the image into the writer, using the specified format.
    ///
    /// Will internally perform a conversion with [`Self::to_dynamic`] and fails when the color
    /// space or other metadata can not be represented. This restriction may be lifted in the
    /// future for some format and color combinations.
    pub fn write_to<W: Write + Seek>(&self, w: &mut W, format: ImageFormat) -> ImageResult<()> {
        // FIXME(perf): should check if the byte layout is already correct. Also another use case
        // of this would be to write formats that are *not* encodable via a RGB-like format and
        // instead we want to find a way to encode those layouts to the `ImageEncoder` trait.
        self.to_dynamic()?.write_to(w, format)
    }
}

/// Allocate a canvas with the layout of the buffer.
///
/// This can't result in an invalid layout, as the `ImageBuffer` certifies that the layout fits
/// into the address space. However, this conversion may panic while performing an allocation.
///
/// ## Design note.
///
/// Converting an owned image buffer is not currently implemented, until agreeing upon a design.
/// There's two goals that are subtly at odds: We will want to utilize a zero-copy reallocation as
/// best as possible but this is not possible while being generic over the container in the same
/// manner as here.
///
/// However, for the near future we will only ever be able to reuse an allocation originating from
/// a standard `Vec<_>` (of the *global* allocator). We can only specialize on this by giving the
/// type explicitly on the impl, or by restricting the container such that `C: Any` is available.
/// Both cases may justify explicit methods as a superior alternative.
///
/// The first of these options would be consistent with supporting `DynamicImage`.
impl<P, C> From<&'_ ImageBuffer<P, C>> for Image
where
    P: PixelWithColorType,
    [P::Subpixel]: EncodableLayout,
    C: Deref<Target = [P::Subpixel]>,
{
    fn from(buf: &ImageBuffer<P, C>) -> Self {
        let (texel, color) = texel_color_for::<P>();
        let (width, height) = ImageBuffer::dimensions(&buf);

        let layout = CanvasLayout::with_texel(&texel, width, height)
            .expect("Valid layout because buffer has one");
        let mut canvas = Canvas::new(layout);

        let destination = canvas.as_bytes_mut();
        let initializer = buf.inner_pixels().as_bytes();
        let len = destination.len().min(initializer.len());

        debug_assert!(
            destination.len() == initializer.len(),
            "The layout computation should not differ"
        );

        destination[..len].copy_from_slice(&initializer[..len]);

        canvas
            .set_color(color)
            .expect("valid color for chosen texel samples");

        Image {
            canvas,
            metadata: ImageMetadata::default(),
        }
    }
}

/// NOTE: While not implemented, a suitably aligned image buffer might be reused. We will have to
/// wait for the allocator traits to be stabilized since we could then reallocate the existing
/// vector in these `DynamicImage` structs for our use which is opportunistically optimal when the
/// allocator allows it.
impl<P, C> From<ImageBuffer<P, C>> for Image
where
    P: PixelWithColorType,
    [P::Subpixel]: EncodableLayout,
    C: Deref<Target = [P::Subpixel]>,
{
    fn from(buf: ImageBuffer<P, C>) -> Self {
        Image::from(&buf)
    }
}

impl From<&'_ DynamicImage> for Image {
    fn from(image: &'_ DynamicImage) -> Self {
        image.to_image()
    }
}

/// Convert a dynamic image to a canvas.
impl From<DynamicImage> for Image {
    fn from(image: DynamicImage) -> Self {
        image.into_image()
    }
}

fn texel_color_for<P: PixelWithColorType>() -> (Texel, Color) {
    //  Note: if adding any non-sRGB compatible type, be careful.
    let texel = extended_color_to_texel(P::COLOR_TYPE)
        .expect("failed consistency between pixel type and canvas support");
    let color = extended_nominal_color(P::COLOR_TYPE)
        .expect("failed consistency between pixel type and canvas support");
    (texel, color)
}

fn color_to_texel(color: ColorType) -> Texel {
    extended_color_to_texel(color.into()).expect("All basic colors are supported")
}

fn nominal_color(color: ColorType) -> Color {
    use ColorType::*;
    match color {
        L8 | La8 | L16 | La16 => Color::BT709,
        Rgb8 | Rgba8 | Rgb16 | Rgba16 | Rgb32F | Rgba32F => Color::SRGB,
    }
}

fn extended_nominal_color(color: ExtendedColorType) -> Option<Color> {
    use ExtendedColorType as C;
    match color {
        C::L1 | C::La1 | C::L2 | C::La2 | C::L4 | C::La4 | C::L8 | C::La8 | C::L16 | C::La16 => {
            Some(Color::BT709)
        }
        C::A8
        | C::Rgb1
        | C::Rgb2
        | C::Rgb4
        | C::Rgb8
        | C::Rgb16
        | C::Rgba1
        | C::Rgba2
        | C::Rgba4
        | C::Rgba8
        | C::Rgba16
        | C::Bgr8
        | C::Bgra8
        | C::Rgb32F
        | C::Rgba32F => Some(Color::SRGB),
        C::Cmyk8 | C::Unknown(_) => return None,
    }
}

fn extended_color_to_texel(color: ExtendedColorType) -> Option<Texel> {
    use ExtendedColorType as ColorType;

    #[allow(unreachable_patterns)]
    Some(match color {
        ColorType::L8 => Texel::new_u8(SampleParts::Luma),
        ColorType::La8 => Texel::new_u8(SampleParts::LumaA),
        ColorType::Rgb8 => Texel::new_u8(SampleParts::Rgb),
        ColorType::Rgba8 => Texel::new_u8(SampleParts::RgbA),
        ColorType::L16 => Texel::new_u16(SampleParts::Luma),
        ColorType::La16 => Texel::new_u16(SampleParts::LumaA),
        ColorType::Rgb16 => Texel::new_u16(SampleParts::Rgb),
        ColorType::Rgba16 => Texel::new_u16(SampleParts::RgbA),
        ColorType::Rgb32F => Texel::new_f32(SampleParts::Rgb),
        ColorType::Rgba32F => Texel::new_f32(SampleParts::RgbA),

        ColorType::Bgr8 => Texel::new_u8(SampleParts::Bgr),
        ColorType::Bgra8 => Texel::new_u8(SampleParts::BgrA),

        ColorType::A8 => Texel::new_u8(SampleParts::A),

        // Layouts with non-byte colors but still pixel
        ColorType::Rgba2 => Texel {
            block: Block::Pixel,
            bits: SampleBits::UInt2x4,
            parts: SampleParts::RgbA,
        },
        ColorType::Rgba4 => Texel {
            block: Block::Pixel,
            bits: SampleBits::UInt4x4,
            parts: SampleParts::RgbA,
        },

        // Repeated pixel layouts
        ColorType::Rgb4 => Texel {
            block: Block::Pack1x2,
            bits: SampleBits::UInt4x6,
            parts: SampleParts::RgbA,
        },
        ColorType::L1 => Texel {
            block: Block::Pack1x8,
            bits: SampleBits::UInt1x8,
            parts: SampleParts::Luma,
        },
        ColorType::La1 => Texel {
            block: Block::Pack1x4,
            bits: SampleBits::UInt2x4,
            parts: SampleParts::LumaA,
        },
        ColorType::Rgba1 => Texel {
            block: Block::Pack1x2,
            bits: SampleBits::UInt1x8,
            parts: SampleParts::RgbA,
        },
        ColorType::L2 => Texel {
            block: Block::Pack1x4,
            bits: SampleBits::UInt2x4,
            parts: SampleParts::Luma,
        },
        ColorType::La2 => Texel {
            block: Block::Pack1x2,
            bits: SampleBits::UInt2x4,
            parts: SampleParts::LumaA,
        },
        ColorType::L4 => Texel {
            block: Block::Pack1x2,
            bits: SampleBits::UInt4x2,
            parts: SampleParts::Luma,
        },
        ColorType::La4 => Texel {
            block: Block::Pixel,
            bits: SampleBits::UInt4x2,
            parts: SampleParts::LumaA,
        },

        // Placeholder for non-RGBA formats (CMYK, YUV, Lab).
        // …

        // Placeholder for subsampled YUV formats
        // …

        // Placeholder for planar formats?
        // …

        // Placeholder for Block layouts (ASTC, BC, ETC/EAC, PVRTC)
        // …

        // Uncovered variants..
        //
        // Rgb1/2 require 1x4 blocks with 12 channels, not supported.
        ColorType::Rgb1
        | ColorType::Rgb2
        // Cmyk is not a color model we have defined channels for.
        | ColorType::Cmyk8
        // Obvious..
        | ColorType::Unknown(_) => return None,
    })
}

#[test]
fn test_conversions() {
    use crate::{buffer::ConvertBuffer, Rgb, RgbImage, Rgba};

    let buffer = RgbImage::from_fn(32, 32, |x, y| {
        if (x + y) % 2 == 0 {
            Rgb([0u8, 0, 0])
        } else {
            Rgb([255, 255, 255])
        }
    });

    let canvas = Image::from(&buffer);

    assert_eq!(canvas.to_buffer::<Rgb<u8>>().unwrap(), buffer.clone());
    assert_eq!(canvas.to_buffer::<Rgb<u16>>().unwrap(), buffer.convert());
    assert_eq!(canvas.to_buffer::<Rgba<f32>>().unwrap(), buffer.convert());

    assert!(canvas.to_dynamic().unwrap().as_rgb8().is_some());
}

#[test]
#[rustfmt::skip]
fn test_expansion_bits() {
    let layout = ImageLayout::with_extended(ExtendedColorType::L1, 6, 2)
        .expect("valid layout type");
    let mut buffer = Image::with_layout(layout);

    assert_eq!(buffer.as_bytes(), b"\x00\x00");
    buffer.as_bytes_mut().copy_from_slice(&[0b01101100 as u8, 0b10110111]);

    let image = buffer.to_dynamic().unwrap().into_luma8();
    assert_eq!(image.as_bytes(), vec![
        0x00, 0xff, 0xff, 0x00, 0xff, 0xff,
        0xff, 0x00, 0xff, 0xff, 0x00, 0xff,
    ]);
}
