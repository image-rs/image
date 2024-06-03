use criterion::{black_box, criterion_group, criterion_main, Criterion};
use image::{GenericImage, ImageBuffer};
use pixeli::Rgba;

pub fn bench_copy_from(c: &mut Criterion) {
    let src = ImageBuffer::from_pixel(2048, 2048, Rgba{r:255u8, g:0, b:0, a:255});
    let mut dst = ImageBuffer::from_pixel(2048, 2048, Rgba{r:0u8, g:0, b:0, a:255});

    c.bench_function("copy_from", |b| {
        b.iter(|| dst.copy_from(black_box(&src), 0, 0))
    });
}

criterion_group!(benches, bench_copy_from);
criterion_main!(benches);
