use criterion::{criterion_group, criterion_main, Criterion};
use image::imageops::fast_blur;
use image::{imageops::blur, ImageBuffer, Rgb};

pub fn bench_fast_blur(c: &mut Criterion) {
    let src = ImageBuffer::from_pixel(1024, 768, Rgb([255u8, 0, 0]));

    c.bench_function("fast blur: sigma 3.0", |b| {
        b.iter(|| fast_blur(&src, 3.0));
    });

    c.bench_function("fast blur: sigma 7.0", |b| {
        b.iter(|| fast_blur(&src, 7.0));
    });

    c.bench_function("fast blur: sigma 50.0", |b| {
        b.iter(|| fast_blur(&src, 50.0));
    });

    c.bench_function("gaussian blur: sigma 3.0", |b| {
        b.iter(|| blur(&src, 0, 3.0));
    });

    c.bench_function("gaussian blur: sigma 7.0", |b| {
        b.iter(|| blur(&src, 0, 7.0));
    });

    c.bench_function("gaussian blur: sigma 50.0", |b| {
        b.iter(|| blur(&src, 0, 50.0));
    });
}

criterion_group!(benches, bench_fast_blur);
criterion_main!(benches);
