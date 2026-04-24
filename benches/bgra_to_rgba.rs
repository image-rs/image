use std::time::Duration;

use criterion::{criterion_group, criterion_main, Criterion};
use image::{ImageBuffer, Rgb, RgbImage, Rgba, RgbaImage};

pub fn from_raw_bgra_old(width: u32, height: u32, container: Vec<u8>) -> Option<RgbaImage> {
    let mut img = RgbaImage::from_raw(width, height, container)?;
    for pix in img.pixels_mut() {
        pix.0[..3].reverse();
    }
    Some(img)
}
pub fn from_raw_bgra_newdefault(width: u32, height: u32, container: Vec<u8>) -> Option<RgbaImage> {
    let mut img = RgbaImage::from_raw(width, height, container)?;
    for pix in img.as_chunks_mut::<4>().0 {
        pix.swap(0, 2);
    }
    Some(img)
}

pub fn bench_bgra_to_rgba(c: &mut Criterion) {
    let width = 2048;
    let height = 2048;
    // Create src as a Rgba image, but later treat it as a Bgra buffer
    let src = ImageBuffer::from_pixel(width, height, Rgba([255u8, 0, 0, 255])).to_vec();
    let src_s = ImageBuffer::from_pixel(width, height, Rgb([255u8, 0, 0])).to_vec();
    let mut group = c.benchmark_group("bgra_to_rgba");
    group.measurement_time(Duration::from_secs(15));
    group.bench_function("from_raw_bgra (new+optimized)", |b| {
        b.iter_batched(
            || src.clone(),
            |input| RgbaImage::from_raw_bgra(width, height, input),
            criterion::BatchSize::LargeInput,
        );
    });
    group.bench_function("from_raw_bgra (old)", |b| {
        b.iter_batched(
            || src.clone(),
            |input| from_raw_bgra_old(width, height, input),
            criterion::BatchSize::LargeInput,
        );
    });
    group.bench_function("from_raw_bgra (new)", |b| {
        b.iter_batched(
            || src.clone(),
            |input| from_raw_bgra_newdefault(width, height, input),
            criterion::BatchSize::LargeInput,
        );
    });
    group.bench_function("from_raw_bgr (new)", |b| {
        b.iter_batched(
            || src_s.clone(),
            |input| RgbImage::from_raw_bgr(width, height, input),
            criterion::BatchSize::LargeInput,
        );
    });
}

criterion_group!(benches, bench_bgra_to_rgba);
criterion_main!(benches);
