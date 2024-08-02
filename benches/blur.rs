use criterion::{criterion_group, criterion_main, Criterion};
use image::{imageops::blur, ImageBuffer, Rgb};

pub fn bench_fast_blur(c: &mut Criterion) {
    let src = ImageBuffer::from_pixel(1024, 768, Rgb([255u8, 0, 0]));

    c.bench_function("blur", |b| {
        b.iter(|| blur(&src, 50.0));
    });
}

criterion_group!(benches, bench_fast_blur);
criterion_main!(benches);
