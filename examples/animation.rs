//! Create a small animation of 3 spinning circles that change color over time.
//!
//! The animation will be saved as APNG and GIF.

use image::{metadata::LoopCount, Delay, Frame, Rgba, RgbaImage};

fn main() {
    let frames = 50;
    let duration = 1000; // ms
    let size = 64; // px

    let dist = 0.3; // distance of circles from center
    let radius = 0.2; // radius of circles
    let path_circle_exp = 1.0;

    let frame_at = |t: f32| -> RgbaImage {
        use std::f32::consts::TAU;
        let r = (t * TAU).sin() * 0.5 + 0.5;
        let g = ((t + 1.0 / 3.0) * TAU).sin() * 0.5 + 0.5;
        let b = ((t + 2.0 / 3.0) * TAU).sin() * 0.5 + 0.5;
        let [r, g, b] = [r, g, b].map(|x| x.powf(1.0 / 2.2)); // gamma correct

        let apply_exp = |x: f32| x.signum() * x.abs().powf(path_circle_exp);
        let circle = |t: f32| {
            [
                apply_exp((t * TAU).sin()) * dist,
                apply_exp((t * TAU).cos()) * dist,
            ]
        };
        let circles = [circle(t + 0.0), circle(t + 0.3333), circle(t + 0.6666)];

        RgbaImage::from_fn(size, size, |x, y| {
            let x = (x as f32 + 0.5) / size as f32 - 0.5;
            let y = (y as f32 + 0.5) / size as f32 - 0.5;

            let min_dist = circles
                .iter()
                .map(|[cx, cy]| ((x - cx).powi(2) + (y - cy).powi(2)).sqrt())
                .fold(f32::INFINITY, f32::min);

            let alpha = 255 - (((min_dist - radius) * size as f32 + 0.5) * 255.0) as u8;

            Rgba([
                (r * 255.0) as u8,
                (g * 255.0) as u8,
                (b * 255.0) as u8,
                alpha,
            ])
        })
    };

    let frames_buffers: Vec<Frame> = (0..frames)
        .map(|i| {
            let t = i as f32 / frames as f32;
            Frame::from_parts(
                frame_at(t),
                0,
                0,
                Delay::from_numer_denom_ms(duration, frames),
            )
        })
        .collect();

    #[cfg(feature = "png")]
    save_png("spinning.png", frames_buffers.clone());
    #[cfg(feature = "gif")]
    save_gif("spinning.gif", frames_buffers);
}

#[cfg(feature = "png")]
fn save_png(name: &str, frames: Vec<Frame>) {
    use image::codecs::png::{CompressionType, FilterType, PngEncoder};

    let mut encoded = Vec::new();
    let encoder = PngEncoder::new_with_quality(
        &mut encoded,
        CompressionType::Balanced,
        FilterType::default(),
    );

    encoder
        .encode_frames(LoopCount::Infinite, frames)
        .expect("Could not encode animation");

    std::fs::write(name, &encoded).expect("Could not write output file");
    println!("Created {name}");
}

#[cfg(feature = "gif")]
fn save_gif(name: &str, frames: Vec<Frame>) {
    use image::codecs::gif::{GifEncoder, Repeat};

    let mut encoded = Vec::new();
    let mut encoder = GifEncoder::new(&mut encoded);
    encoder
        .set_repeat(Repeat::Infinite)
        .expect("Could not set repeat");

    encoder
        .encode_frames(frames)
        .expect("Could not encode animation");
    drop(encoder);

    std::fs::write(name, &encoded).expect("Could not write output file");
    println!("Created {name}");
}
