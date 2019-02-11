use std::mem;

use buffer::{Pixel, EnumeratePixelsMut};
use image::{GenericImage, GenericImageView};

/// Immutable pixel iterator
pub struct Pixels<'a, I: ?Sized + 'a> {
    image: &'a I,
    x: u32,
    y: u32,
    width: u32,
    height: u32,
}

impl<'a, I: GenericImageView + ?Sized + 'a> Pixels<'a, I> {
    /// Create an iterator over enumerated pixels.
    ///
    /// This is a generic method that will sequentially access pixels via the
    /// `GenericImageView::get_pixel` method. Other iterator methods on specialized types may have
    /// better performance.
    pub fn new(image: &'a I) -> Self {
        let (width, height) = image.dimensions();

        Pixels {
            image,
            x: 0,
            y: 0,
            width,
            height,
        }
    }
}

impl<'a, I: GenericImageView + ?Sized + 'a> Iterator for Pixels<'a, I> {
    type Item = (u32, u32, I::Pixel);

    fn next(&mut self) -> Option<(u32, u32, I::Pixel)> {
        if self.x >= self.width {
            self.x = 0;
            self.y += 1;
        }

        if self.y >= self.height {
            None
        } else {
            let pixel = self.image.get_pixel(self.x, self.y);
            let p = (self.x, self.y, pixel);

            self.x += 1;

            Some(p)
        }
    }
}

/// Mutable pixel iterator
pub struct PixelsMut<'a, I: GenericImage + ?Sized + 'a> {
    inner: PixelsMutImpl<'a, I>,
}

enum PixelsMutImpl<'a, I: GenericImage + ?Sized + 'a> {
    /// Provide pixels via one mutable reference on `I`.
    ///
    /// This subverts lifetime checks, and is highly unsafe.
    Unsafe(PixelsMutUnsafe<'a, I>),

    /// Iterator when the buffer is in compact format.
    Buffer(EnumeratePixelsMut<'a, I::Pixel>),

    /// Fallback when nothing else is available.
    Boxed(Box<Iterator<Item=(u32, u32, &'a mut I::Pixel)> + 'a>),
}

struct PixelsMutUnsafe<'a, I: ?Sized + 'a> {
    image: &'a mut I,
    x: u32,
    y: u32,
    width: u32,
    height: u32,
}

impl<'a, I: GenericImage + ?Sized + 'a> PixelsMut<'a, I> {
    /// Iterate mutable over the pixels of `I`.
    ///
    /// This is highly unsafe. It subverts lifetime checks by transmuting references to the pixels.
    /// Therefore, the caller of this function must ensure that *all* calls to `I::get_pixel_mut`
    /// with different parameters will surely return unique references. Moreover, actually defined
    /// behaviour implies that the image itself does not hold any direct reference to its storage
    /// but only references it via pointers.
    ///
    /// Use with utmost care! The long name is not by accident.
    pub unsafe fn simultaneous_pixels_mut_unchecked(image: &'a mut I) -> Self {
        PixelsMut {
            inner: PixelsMutImpl::Unsafe(PixelsMutUnsafe::new(image)),
        }
    }

    /// Convert an iterator over an image buffer.
    ///
    /// This does not enforce that the image iterated over actually holds the supplied buffer.
    /// Instead, it should be used by image types that can convert their internal buffer into an
    /// `ImageBuffer` temporarily, for example through `ImageBuffer::from_raw`.
    pub fn from_buffer(enumerate: EnumeratePixelsMut<'a, I::Pixel>) -> Self {
        PixelsMut {
            inner: PixelsMutImpl::Buffer(enumerate),
        }
    }

    /// Wrap a boxed iterator.
    ///
    /// This does not enforce that the image iterated over actually holds the supplied buffer. This
    /// should be used as a fallback when no library supplied image reference type can provide the
    /// necessary wrapping. Still, this is much safer than resorting to
    /// `simultaneous_pixels_mut_unchecked`. If your custom implementation has to use this method
    /// but you feel like it provides a generalizable pattern, feel free to request an additional
    /// internal stack based variant for it to avoid the allocation.
    pub fn boxed(iterator: Box<Iterator<Item=(u32, u32, &'a mut I::Pixel)> + 'a>) -> Self {
        PixelsMut {
            inner: PixelsMutImpl::Boxed(iterator.into()),
        }
    }

    /// Box an arbitrary iterator.
    ///
    /// This is a utility method simply forwarding to `boxed`.
    pub fn box_it<T>(iterator: T) -> Self 
        where T: Iterator<Item=(u32, u32, &'a mut I::Pixel)> + 'a
    {
        Self::boxed(Box::new(iterator))
    }
}

impl<'a, I: GenericImage + ?Sized + 'a> Iterator for PixelsMut<'a, I>
{
    type Item = (u32, u32, &'a mut I::Pixel);

    fn next(&mut self) -> Option<(u32, u32, &'a mut I::Pixel)> {
        match &mut self.inner {
            &mut PixelsMutImpl::Unsafe(ref mut inner) => inner.next(),
            &mut PixelsMutImpl::Buffer(ref mut buffer) => buffer.next(),
            &mut PixelsMutImpl::Boxed(ref mut boxed) => boxed.next(),
        }
    }
}

impl<'a, I: GenericImage + ?Sized + 'a> PixelsMutUnsafe<'a, I> {
    pub fn new(image: &'a mut I) -> Self {
        let (width, height) = image.dimensions();

        PixelsMutUnsafe {
            image,
            x: 0,
            y: 0,
            width,
            height,
        }
    }
}

impl<'a, I: GenericImage + ?Sized + 'a> Iterator for PixelsMutUnsafe<'a, I>
where
    I::Pixel: 'a,
    <I::Pixel as Pixel>::Subpixel: 'a,
{
    type Item = (u32, u32, &'a mut I::Pixel);

    fn next(&mut self) -> Option<(u32, u32, &'a mut I::Pixel)> {
        if self.x >= self.width {
            self.x = 0;
            self.y += 1;
        }

        if self.y >= self.height {
            None
        } else {
            let tmp = self.image.get_pixel_mut(self.x, self.y);

            // NOTE: This is the dangerous operation. It would require the signature 
            // `fn next(&'a mut self)` to be safe.  However, any implementor of `GenericImage` that
            // constructed this iterator has opted into this being safe, although it is very
            // unlikely true.
            let ptr = unsafe { mem::transmute(tmp) };

            let p = (self.x, self.y, ptr);

            self.x += 1;

            Some(p)
        }
    }
}
