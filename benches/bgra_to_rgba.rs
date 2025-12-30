use std::time::Duration;

use criterion::{criterion_group, criterion_main, Criterion};
use image::{ImageBuffer, Rgba, RgbaImage};

pub fn bench_bgra_to_rgba(c: &mut Criterion) {
    let width = 2048;
    let height = 2048;
    // Create src as a Rgba image, but later treat it as a Bgra buffer
    let src = ImageBuffer::from_pixel(2048, 2048, Rgba([255u8, 0, 0, 255])).to_vec();
    let mut group = c.benchmark_group("bgra_to_rgba");
    group.measurement_time(Duration::from_secs(15));
    group.bench_function("from_raw_bgra", |b| {
        b.iter_batched(
            || src.clone(),
            |input| RgbaImage::from_raw_bgra(width, height, input),
            criterion::BatchSize::LargeInput,
        );
    });
}

criterion_group!(benches, bench_bgra_to_rgba);
criterion_main!(benches);
