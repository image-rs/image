use image::{GenericImage, GenericImageView, ImageBuffer, Pixel, Primitive};

/// Example showcasing a generic implementation of image concatenation.
///
/// The example images are coming from https://placeholder.com/
///
/// Run from the root of the repository with:
/// cargo run --release --example concat
fn main() {
    h_concat(&[
        image::open("examples/concat/200x300.png").unwrap(),
        image::open("examples/concat/300x300.png").unwrap(),
        image::open("examples/concat/400x300.png").unwrap(),
    ])
    .save("examples/concat/concatenated_900x300.png")
    .unwrap();
}

/// Concatenate horizontally images with the same pixel type.
fn h_concat<I, P, S>(images: &[I]) -> ImageBuffer<P, Vec<S>>
where
    I: GenericImageView<Pixel = P>,
    P: Pixel<Subpixel = S> + 'static,
    S: Primitive + 'static,
{
    // The final width is the sum of all images width.
    let img_width_out: u32 = images.iter().map(|im| im.width()).sum();

    // The final height is the maximum height from the input images.
    let img_height_out: u32 = images.iter().map(|im| im.height()).max().unwrap_or(0);

    // Initialize an image buffer with the appropriate size.
    let mut imgbuf = image::ImageBuffer::new(img_width_out, img_height_out);
    let mut accumulated_width = 0;

    // Copy each input image at the correct location in the output image.
    for img in images {
        imgbuf.copy_from(img, accumulated_width, 0).unwrap();
        accumulated_width += img.width();
    }

    imgbuf
}
