use core::ops::Deref;

use crate::{
    error::{DecodingError, ImageFormatHint},
    flat::SampleLayout,
    traits::{EncodableLayout, PixelWithColorType},
    ColorType, DynamicImage, ExtendedColorType, FlatSamples, ImageBuffer, ImageDecoder, ImageError,
    ImageResult, Luma, LumaA, Rgb, Rgba,
};

use image_canvas::color::Color;
use image_canvas::layout::{CanvasLayout as InnerLayout, SampleParts, Texel};
use image_canvas::Canvas as Inner;

/// A byte buffer for various color and layout combinations.
///
/// Note that it is possible to convert most [`ImageBuffer`] as well as [`DynamicImage`] instances
/// into a canvas that represents the same pixels and color. See `From` implementations below.
pub struct Canvas {
    inner: Inner,
}

/// The layout of a [`Canvas`].
///
/// Apart from the simplistic cases of matrices of uniform channel arrays, this can internally also
/// represent complex cases such as pixels with mixed channel types, matrices of compressed blocks,
/// and planar layouts. The public constructors define the possibilities that are support.
#[derive(Clone, PartialEq)]
pub struct CanvasLayout {
    inner: InnerLayout,
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

impl Canvas {
    /// Allocate an image canvas for a given color, and dimensions.
    ///
    /// # Panics
    ///
    /// If the layout is invalid for this platform, i.e. requires more than `isize::MAX` bytes to
    /// allocate. Also panics if the allocator fails.
    pub fn new(color: ColorType, w: u32, h: u32) -> Self {
        let texel = color_to_texel(color);
        let layout = InnerLayout::with_texel(&texel, w, h).expect("layout error");

        Canvas {
            inner: Inner::new(layout),
        }
    }

    /// Allocate an image canvas with complex layout.
    ///
    /// This is a generalization of [`Self::new`] and the error case of an invalid layout is
    /// isolated, and can be properly handled. Furthermore, this provides control over the
    /// allocation by making its size available to the caller before it is performed.
    pub fn with_layout(layout: CanvasLayout) -> Self {
        Canvas {
            inner: Inner::new(layout.inner),
        }
    }

    /// Allocate a canvas for the result of an image decoder, then read the image into it.
    pub fn from_decoder<'a>(decoder: impl ImageDecoder<'a>) -> ImageResult<Self> {
        // FIXME: we can also handle ExtendedColorType...
        let texel = color_to_texel(decoder.color_type());
        let (width, height) = decoder.dimensions();

        // FIXME: What about other scanline sizes?
        let layout = InnerLayout::with_texel(&texel, width, height).map_err(|layout| {
            let decoder = DecodingError::new(ImageFormatHint::Unknown, format!("{:?}", layout));
            ImageError::Decoding(decoder)
        })?;

        let mut canvas = Inner::new(layout);
        decoder.read_image(canvas.as_bytes_mut())?;

        Ok(Canvas { inner: canvas })
    }

    /// The width of this canvas, in represented pixels.
    pub fn width(&self) -> u32 {
        self.inner.layout().width()
    }

    /// The height of this canvas, in represented pixels.
    pub fn height(&self) -> u32 {
        self.inner.layout().height()
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
        self.inner.as_bytes()
    }

    /// Get a mutable reference to the raw bytes making up this canvas.
    ///
    /// The interpretation will very much depend on the current layout. Note that primitive
    /// channels will be generally stored in native-endian order.
    ///
    /// This can be used to initialize bytes from FFI.
    pub fn as_bytes_mut(&mut self) -> &mut [u8] {
        self.inner.as_bytes_mut()
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
        let channels = self.inner.channels_u8()?;
        let spec = channels.layout().spec();
        Some(FlatSamples {
            samples: channels.into_slice(),
            layout: Self::sample_layout(spec),
            color_hint: Self::color_type(self.inner.layout()),
        })
    }

    /// Get a reference to channels, without conversion.
    ///
    /// This returns `None` if the channels of the underlying layout are not unsigned shorts, or if
    /// the layout is not a single image plane.
    pub fn as_flat_samples_u16(&self) -> Option<FlatSamples<&[u16]>> {
        let channels = self.inner.channels_u16()?;
        let spec = channels.layout().spec();
        Some(FlatSamples {
            samples: channels.into_slice(),
            layout: Self::sample_layout(spec),
            color_hint: Self::color_type(self.inner.layout()),
        })
    }

    /// Get a reference to channels, without conversion.
    ///
    /// This returns `None` if the channels of the underlying layout are not 32-bit floating point
    /// numbers, or if the layout is not a single image plane.
    pub fn as_flat_samples_f32(&self) -> Option<FlatSamples<&[f32]>> {
        let channels = self.inner.channels_f32()?;
        let spec = channels.layout().spec();
        Some(FlatSamples {
            samples: channels.into_slice(),
            layout: Self::sample_layout(spec),
            color_hint: Self::color_type(self.inner.layout()),
        })
    }

    /// Convert the data into an `ImageBuffer`.
    ///
    /// Performs color conversion if necessary, such as rearranging color channel order,
    /// transforming pixels to the `sRGB` color space etc.
    ///
    /// The subpixel of the result type is constrained by a sealed, internal trait that is
    /// implemented for all primitive numeric types.
    pub fn to_buffer<P: PixelWithColorType>(&self) -> ImageBuffer<P, Vec<P::Subpixel>>
    where
        [P::Subpixel]: EncodableLayout,
    {
        let (width, height) = (self.width(), self.height());
        let mut buffer = ImageBuffer::new(width, height);

        let mut fallback_canvas;
        let canvas = if false {
            /* self.inner.layout().texel() == texel */
            // FIXME: can select `self` if it's correct color type.
            &self.inner
        } else {
            let texel = color_to_texel(P::COLOR_TYPE);
            let layout = InnerLayout::with_texel(&texel, width, height)
                .expect("Valid layout because buffer has one");

            fallback_canvas = Inner::new(layout);
            fallback_canvas
                .set_color(Color::SRGB)
                .expect("Valid for rgb layout");
            self.inner.convert(&mut fallback_canvas);
            &fallback_canvas
        };

        let destination = buffer.inner_pixels_mut().as_bytes_mut();
        let initializer = canvas.as_bytes();
        let len = destination.len().min(initializer.len());

        debug_assert!(
            destination.len() == initializer.len(),
            "The layout computation should not differ"
        );

        destination[..len].copy_from_slice(&initializer[..len]);

        buffer
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
    pub fn to_dynamic(&self) -> DynamicImage {
        let layout = self.inner.layout();
        match Self::color_type(layout) {
            Some(ColorType::L8) => self.to_buffer::<Luma<u8>>().into(),
            Some(ColorType::La8) => self.to_buffer::<LumaA<u8>>().into(),
            Some(ColorType::Rgb8) => self.to_buffer::<Rgb<u8>>().into(),
            Some(ColorType::Rgba8) => self.to_buffer::<Rgba<u8>>().into(),
            Some(ColorType::L16) => self.to_buffer::<Luma<u16>>().into(),
            Some(ColorType::La16) => self.to_buffer::<LumaA<u16>>().into(),
            Some(ColorType::Rgb16) => self.to_buffer::<Rgb<u16>>().into(),
            Some(ColorType::Rgba16) => self.to_buffer::<Rgba<u16>>().into(),
            Some(ColorType::Rgb32F) => self.to_buffer::<Rgb<f32>>().into(),
            Some(ColorType::Rgba32F) => self.to_buffer::<Rgba<f32>>().into(),
            None if !Self::has_alpha(layout) => self.to_buffer::<Rgb<f32>>().into(),
            None => self.to_buffer::<Rgba<f32>>().into(),
        }
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

    fn color_type(layout: &InnerLayout) -> Option<ColorType> {
        // FIXME: check for pixel types.
        None
    }

    fn has_alpha(layout: &InnerLayout) -> bool {
        // FIXME: check channels.
        true
    }
}

impl CanvasLayout {
    /// Construct a row-major matrix for a given color type.
    pub fn new(color: ColorType, w: u32, h: u32) -> Option<Self> {
        let texel = color_to_texel(color);
        let layout = InnerLayout::with_texel(&texel, w, h).ok()?;
        Some(CanvasLayout { inner: layout })
    }

    /// Construct a row-major matrix for an extended color type, i.e. generic texels.
    pub fn with_extended(
        color: ExtendedColorType,
        w: u32,
        h: u32,
    ) -> Result<Self, UnknownCanvasTexelError> {
        fn inner(color: ExtendedColorType, w: u32, h: u32) -> Option<CanvasLayout> {
            let texel = extended_color_to_texel(color)?;
            let layout = InnerLayout::with_texel(&texel, w, h).ok()?;
            Some(CanvasLayout { inner: layout })
        }

        inner(color, w, h).ok_or(UnknownCanvasTexelError { _inner: () })
    }

    /// Returns the number of bytes required for this layout.
    pub fn byte_len(&self) -> usize {
        self.inner.byte_len()
    }
}

use crate::ImageOutputFormat;
use std::io::{Seek, Write};

impl Canvas {
    /// Encode the image into the writer, using the specified format.
    ///
    /// Will internally perform a conversion with [`Self::to_dynamic`].
    pub fn write_to<W: Write + Seek, F: Into<ImageOutputFormat>>(
        &self,
        w: &mut W,
        format: F,
    ) -> ImageResult<()> {
        // FIXME(perf): should check if the byte layout is already correct.
        self.to_dynamic().write_to(w, format)
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
impl<P, C> From<&'_ ImageBuffer<P, C>> for Canvas
where
    P: PixelWithColorType,
    [P::Subpixel]: EncodableLayout,
    C: Deref<Target = [P::Subpixel]>,
{
    fn from(buf: &ImageBuffer<P, C>) -> Self {
        //  Note: if adding any non-sRGB compatible type, be careful.
        let texel = color_to_texel(P::COLOR_TYPE);

        let (width, height) = ImageBuffer::dimensions(&buf);
        let layout = InnerLayout::with_texel(&texel, width, height)
            .expect("Valid layout because buffer has one");

        let mut canvas = Inner::new(layout);

        let destination = canvas.as_bytes_mut();
        let initializer = buf.inner_pixels().as_bytes();
        let len = destination.len().min(initializer.len());

        debug_assert!(
            destination.len() == initializer.len(),
            "The layout computation should not differ"
        );

        destination[..len].copy_from_slice(&initializer[..len]);

        canvas.set_color(Color::SRGB).expect("Valid for rgb layout");
        Canvas { inner: canvas }
    }
}

impl From<DynamicImage> for Canvas {
    fn from(image: DynamicImage) -> Self {
        image.to_canvas()
    }
}

fn color_to_texel(color: ColorType) -> Texel {
    match color {
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
        _ => return None,

        // For exposition, all other patterns that aren't covered.
        ColorType::Rgb4 => todo!(),
        ColorType::Rgba4 => todo!(),

        ColorType::L1 => todo!(),
        ColorType::La1 => todo!(),
        ColorType::Rgb1 => todo!(),
        ColorType::Rgba1 => todo!(),
        ColorType::L2 => todo!(),
        ColorType::La2 => todo!(),
        ColorType::Rgb2 => todo!(),
        ColorType::Rgba2 => todo!(),
        ColorType::L4 => todo!(),
        ColorType::La4 => todo!(),
        ColorType::Unknown(_) => return None,
    })
}

#[test]
fn test_conversions() {
    use crate::{Rgb, Rgba, RgbImage, buffer::ConvertBuffer};

    let buffer = RgbImage::from_fn(32, 32, |x, y| {
        if (x + y) % 2 == 0 {
            Rgb([0, 0, 0])
        } else {
            Rgb([255, 255, 255])
        }
    });

    let canvas = Canvas::from(&buffer);

    assert_eq!(canvas.to_buffer::<Rgb<u8>>(), buffer.clone());
    assert_eq!(canvas.to_buffer::<Rgb<u16>>(), buffer.convert());
    assert_eq!(canvas.to_buffer::<Rgba<f32>>(), buffer.convert());

    assert!(canvas.to_dynamic().as_rgb8().is_some());
}
