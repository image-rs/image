extern crate criterion;

use criterion::{BenchmarkId, Criterion, criterion_group, criterion_main};
use image::{ColorType, bmp::BMPEncoder, jpeg::JPEGEncoder};

use std::fs::File;
use std::io::{BufWriter, Seek, SeekFrom};

trait Encoder {
    fn encode_raw(&self, into: &mut Vec<u8>, im: &[u8], dims: u32, color: ColorType);
    fn encode_bufvec(&self, into: &mut Vec<u8>, im: &[u8], dims: u32, color: ColorType);
    fn encode_file(&self, file: &File, im: &[u8], dims: u32, color: ColorType);
}

#[derive(Clone, Copy)]
struct BenchDef {
    with: &'static dyn Encoder,
    name: &'static str,
    colors: &'static [ColorType],
}

/// Benchmarks encoding a zeroed image.
///
/// For compressed formats this is surely not representative of encoding a normal image but it's a
/// start for benchmarking.
fn encode_zero_sizes(criterion: &mut Criterion, with: &dyn Encoder, name: &str, color: ColorType) {
    let counts = vec![100u32, 200, 400];

    let mut group = criterion.benchmark_group(format!("encode-{}-{:?}", name, color));
    for size in counts.iter().copied() {
        let bytes = size as usize * usize::from(color.bytes_per_pixel());
        let im = vec![0; bytes * bytes];

        group.bench_with_input(BenchmarkId::new("rawvec", size), &im, |b, image| {
            let mut v = vec![];
            with.encode_raw(&mut v, &im, size, color);
            b.iter(|| with.encode_raw(&mut v, image, size, color));
        });
        group.bench_with_input(BenchmarkId::new("bufvec", size), &im, |b, image| {
            let mut v = vec![];
            with.encode_raw(&mut v, &im, size, color);
            b.iter(|| with.encode_bufvec(&mut v, image, size, color));
        });
        group.bench_with_input(BenchmarkId::new("file", size), &im, |b, image| {
            let file = File::create("temp.bmp").unwrap();
            b.iter(|| with.encode_file(&file, image, size, color));
        });
    }
}

fn load_all(c: &mut Criterion) {
    const BENCH_DEFS: &'static [BenchDef] = &[
        BenchDef {
            with: &Bmp,
            name: "bmp",
            colors: &[ColorType::L8, ColorType::Rgb8, ColorType::Rgba8],
        },
        BenchDef {
            with: &Jpeg,
            name: "jpeg",
            colors: &[ColorType::L8, ColorType::Rgb8, ColorType::Rgba8],
        },
    ];

    for definition in BENCH_DEFS {
        for &color in definition.colors {
            encode_zero_sizes(c, definition.with, definition.name, color);
        }
    }
}

criterion_group!(benches, load_all);
criterion_main!(benches);

struct Bmp;

struct Jpeg;

impl Encoder for Bmp {
    fn encode_raw(&self, into: &mut Vec<u8>, im: &[u8], size: u32, color: ColorType) {
        into.clear();
        let mut x = BMPEncoder::new(into);
        x.encode(im, size, size, color).unwrap();
    }

    fn encode_bufvec(&self, into: &mut Vec<u8>, im: &[u8], size: u32, color: ColorType) {
        into.clear();
        let mut buf = BufWriter::new(into);
        let mut x = BMPEncoder::new(&mut buf);
        x.encode(im, size, size, color).unwrap();
    }

    fn encode_file(&self, mut file: &File, im: &[u8], size: u32, color: ColorType) {
        file.seek(SeekFrom::Start(0)).unwrap();
        let mut buf = BufWriter::new(file);
        let mut x = BMPEncoder::new(&mut buf);
        x.encode(im, size, size, color).unwrap();
    }
}

impl Encoder for Jpeg {
    fn encode_raw(&self, into: &mut Vec<u8>, im: &[u8], size: u32, color: ColorType) {
        into.clear();
        let mut x = JPEGEncoder::new(into);
        x.encode(im, size, size, color).unwrap();
    }

    fn encode_bufvec(&self, into: &mut Vec<u8>, im: &[u8], size: u32, color: ColorType) {
        into.clear();
        let mut buf = BufWriter::new(into);
        let mut x = JPEGEncoder::new(&mut buf);
        x.encode(im, size, size, color).unwrap();
    }

    fn encode_file(&self, mut file: &File, im: &[u8], size: u32, color: ColorType) {
        file.seek(SeekFrom::Start(0)).unwrap();
        let mut buf = BufWriter::new(file);
        let mut x = JPEGEncoder::new(&mut buf);
        x.encode(im, size, size, color).unwrap();
    }
}
