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

pub struct Canvas {
    inner: Inner,
}

#[derive(Clone, PartialEq)]
pub struct CanvasLayout {
    inner: InnerLayout,
}

impl Canvas {
    /// # Panics
    /// If the layout is invalid for this platform, i.e. requires more than `isize::MAX` bytes to
    /// allocate. Also panics if the allocator fails.
    pub fn new(color: ColorType, w: u32, h: u32) -> Self {
        let texel = color_to_texel(color);
        let layout = InnerLayout::with_texel(&texel, w, h).expect("layout error");

        Canvas {
            inner: Inner::new(layout),
        }
    }

    pub fn with_layout(layout: CanvasLayout) -> Self {
        Canvas {
            inner: Inner::new(layout.inner),
        }
    }

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

    pub fn width(&self) -> u32 {
        self.inner.layout().width()
    }

    pub fn height(&self) -> u32 {
        self.inner.layout().height()
    }

    pub fn as_bytes(&self) -> &[u8] {
        self.inner.as_bytes()
    }

    pub fn as_bytes_mut(&mut self) -> &mut [u8] {
        self.inner.as_bytes_mut()
    }

    pub fn into_bytes(self) -> Vec<u8> {
        // No better way, we're changing the alignment of the layout.
        self.as_bytes().to_owned()
    }

    pub fn as_flat_samples_u8(&self) -> Option<FlatSamples<&[u8]>> {
        todo!()
    }

    pub fn as_flat_samples_u16(&self) -> Option<FlatSamples<&[u16]>> {
        todo!()
    }

    pub fn as_flat_samples_i8(&self) -> Option<FlatSamples<&[i8]>> {
        todo!()
    }

    pub fn as_flat_samples_i16(&self) -> Option<FlatSamples<&[i16]>> {
        todo!()
    }

    pub fn as_flat_samples_f32(&self) -> Option<FlatSamples<&[f32]>> {
        todo!()
    }

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
            "The layout computation should do the same"
        );

        destination[..len].copy_from_slice(&initializer[..len]);

        buffer
    }

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

    fn sample_layout(layout: &InnerLayout) -> Option<SampleLayout> {
        let plane = layout.as_plane()?;
        None
    }

    fn color_type(_: &InnerLayout) -> Option<ColorType> {
        // FIXME: check for pixel types.
        None
    }

    fn has_alpha(_: &InnerLayout) -> bool {
        // FIXME: check channels.
        true
    }
}

impl CanvasLayout {
    pub fn new(color: ColorType, w: u32, h: u32) -> Option<Self> {
        let texel = color_to_texel(color);
        let layout = InnerLayout::with_texel(&texel, w, h).ok()?;
        Some(CanvasLayout { inner: layout })
    }

    pub fn with_extended(color: ExtendedColorType, w: u32, h: u32) -> Option<Self> {
        let texel = extended_color_to_texel(color)?;
        let layout = InnerLayout::with_texel(&texel, w, h).ok()?;
        Some(CanvasLayout { inner: layout })
    }
}

use crate::ImageOutputFormat;
use std::io::{Seek, Write};

impl Canvas {
    pub fn write_to<W: Write + Seek, F: Into<ImageOutputFormat>>(
        &self,
        w: &mut W,
        format: F,
    ) -> ImageResult<()> {
        // FIXME(perf): should check if the byte layout is already correct.
        self.to_dynamic().write_to(w, format)
    }
}

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
            "The layout computation should do the same"
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
