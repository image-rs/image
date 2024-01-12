//! Test enforcement of size and memory limits for animation decoding APIs.

use image::{io::Limits, AnimationDecoder, ImageDecoder, ImageResult};

#[cfg(feature = "gif")]
use image::codecs::gif::GifDecoder;

#[cfg(feature = "gif")]
fn gif_decode(data: &[u8], limits: Limits) -> ImageResult<()> {
    let mut decoder = GifDecoder::new(data).unwrap();
    decoder.set_limits(limits)?;
    {
        let frames = decoder.into_frames();
        for result in frames {
            result?;
        }
    }
    Ok(())
}

/// Checks that the function returned `ImageError::Limits`, panics otherwise
#[track_caller]
fn assert_limit_error(res: ImageResult<()>) {
    let err = res.expect_err("The input should have been rejected because it exceeds limits");
    match err {
        image::ImageError::Limits(_) => (), // all good
        _ => panic!("Decoding failed due to an error unrelated to limits"),
    }
}

/// Each frame is the size of the image,
/// so we can just output each raw GIF frame buffer as the final composited frame
/// with no additional scratch space
#[test]
#[cfg(feature = "gif")]
fn animated_full_frame_discard() {
    let data =
        std::fs::read("tests/images/gif/anim/large-gif-anim-full-frame-replace.gif").unwrap();

    let mut limits_dimensions_too_small = Limits::default();
    limits_dimensions_too_small.max_image_width = Some(500);
    limits_dimensions_too_small.max_image_height = Some(500);
    assert_limit_error(gif_decode(&data, limits_dimensions_too_small));

    let mut limits_memory_way_too_small = Limits::default();
    // Start with a ridiculously low memory allocation cap
    limits_memory_way_too_small.max_alloc = Some(5);
    assert_limit_error(gif_decode(&data, limits_memory_way_too_small));

    let mut limits_memory_too_small = Limits::default();
    // 1000 * 1000 * 4 would be the exact size of the buffer for one RGBA frame.
    // The decoder always peaks with at least two frames in memory at the same time.
    // Set the limit a little higher than 1 frame than that it doesn't run into trivial checks
    // for output frame size, and make it run into actual buffer allocation errors.
    limits_memory_too_small.max_alloc = Some(1000 * 1000 * 5);
    assert_limit_error(gif_decode(&data, limits_memory_too_small));

    let mut limits_just_enough = Limits::default();
    limits_just_enough.max_image_height = Some(1000);
    limits_just_enough.max_image_width = Some(1000);
    limits_just_enough.max_alloc = Some(1000 * 1000 * 4 * 2); // 4 for RGBA, 2 for 2 buffers kept in memory simultaneously

    gif_decode(&data, limits_just_enough)
        .expect("With these limits it should have decoded successfully");
}

/// The GIF frame does not cover the whole image, requiring additional scratch space
#[test]
#[cfg(feature = "gif")]
fn animated_frame_combine() {
    let data = std::fs::read("tests/images/gif/anim/large-gif-anim-combine.gif").unwrap();

    let mut limits_dimensions_too_small = Limits::default();
    limits_dimensions_too_small.max_image_width = Some(500);
    limits_dimensions_too_small.max_image_height = Some(500);
    assert_limit_error(gif_decode(&data, limits_dimensions_too_small));

    let mut limits_memory_way_too_small = Limits::default();
    // Start with a ridiculously low memory allocation cap
    limits_memory_way_too_small.max_alloc = Some(5);
    assert_limit_error(gif_decode(&data, limits_memory_way_too_small));

    let mut limits_memory_too_small = Limits::default();
    // 1000 * 1000 * 4 * would be the exact size of two buffers for an RGBA frame.
    // In this mode the decoder uses 2 full frames (accumulated result and the output frame)
    // plus the smaller frame size from the GIF format decoder that it composites onto the output frame.
    // So two full frames are not actually enough for decoding here.
    // Verify that this is caught.
    limits_memory_too_small.max_alloc = Some(1000 * 1000 * 4 * 2); // 4 for RGBA, 2 for 2 buffers kept in memory simultaneously
    assert_limit_error(gif_decode(&data, limits_memory_too_small));

    let mut limits_enough = Limits::default();
    limits_enough.max_image_height = Some(1000);
    limits_enough.max_image_width = Some(1000);
    limits_enough.max_alloc = Some(1000 * 1000 * 4 * 3); // 4 for RGBA, 2 for 2 buffers kept in memory simultaneously

    gif_decode(&data, limits_enough)
        .expect("With these limits it should have decoded successfully");
}
