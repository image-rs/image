use std::time::Duration;

use criterion::{criterion_group, criterion_main, Criterion};
use image::{
    buffer::ConvertBuffer,
    imageops::{self, FilterType},
    GrayImage, ImageBuffer, Rgb,
};

pub fn bench_imageops(c: &mut Criterion) {
    let src = ImageBuffer::from_fn(1920, 1080, |x, y| {
        // "random" pixel values
        Rgb([x as u8, (y / 8) as u8, ((x * 13 + y) % 256) as u8])
    });

    c.bench_function("filter3x3", |b| {
        b.iter(|| imageops::filter3x3(&src, &[1.0, 1.0, 1.0, 1.0, -8.0, 1.0, 1.0, 1.0, 1.0]));
    });

    c.bench_function("brighten", |b| {
        b.iter(|| imageops::brighten(&src, 100));
    });

    c.bench_function("contrast", |b| {
        b.iter(|| imageops::contrast(&src, 5.0));
    });

    c.bench_function("dither", |b| {
        let luma: GrayImage = src.convert();
        b.iter(|| {
            let mut luma = luma.clone();
            imageops::dither(&mut luma, &imageops::BiLevel)
        });
    });

    c.bench_function("flip_horizontal", |b| {
        b.iter(|| imageops::flip_horizontal(&src));
    });
    c.bench_function("flip_vertical", |b| {
        b.iter(|| imageops::flip_vertical(&src));
    });

    c.bench_function("grayscale", |b| {
        b.iter(|| imageops::grayscale(&src));
    });
    c.bench_function("grayscale_alpha", |b| {
        b.iter(|| imageops::grayscale_alpha(&src));
    });

    c.bench_function("huerotate", |b| {
        b.iter(|| imageops::huerotate(&src, 180));
    });

    c.bench_function("invert", |b| {
        b.iter(|| {
            let mut src = src.clone();
            imageops::invert(&mut src)
        });
    });

    c.bench_function("rotate90", |b| {
        b.iter(|| imageops::rotate90(&src));
    });
    c.bench_function("rotate180", |b| {
        b.iter(|| imageops::rotate180(&src));
    });
    c.bench_function("rotate270", |b| {
        b.iter(|| imageops::rotate270(&src));
    });

    c.bench_function("unsharpen", |b| {
        b.iter(|| imageops::unsharpen(&src, 2.0, 0.0));
    });
}

pub fn bench_resize(c: &mut Criterion) {
    let src = ImageBuffer::from_fn(1920, 1080, |x, y| {
        // "random" pixel values
        Rgb([x as u8, (y / 8) as u8, ((x * 13 + y) % 256) as u8])
    });

    let filters = [
        FilterType::Nearest,
        FilterType::Triangle,
        FilterType::CatmullRom,
        FilterType::Gaussian,
        FilterType::Lanczos3,
    ];

    for filter in filters {
        c.bench_function(&format!("resize 400x300 {:?}", filter), |b| {
            b.iter(|| imageops::resize(&src, 400, 300, filter));
        });
    }

    // resizing with large dimensions is slower, so take fewer samples
    let mut group = c.benchmark_group("large");
    let c = group.warm_up_time(Duration::from_secs(1)).sample_size(10);

    for filter in filters {
        c.bench_function(format!("resize 2000x2000 {:?}", filter), |b| {
            b.iter(|| imageops::resize(&src, 2000, 2000, filter));
        });
    }
}

criterion_group!(benches, bench_imageops, bench_resize);
criterion_main!(benches);
