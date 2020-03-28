extern crate criterion;

use criterion::{BenchmarkId, Criterion, criterion_group, criterion_main};
use image::{ColorType, bmp::BMPEncoder};

use std::fs::File;
use std::io::{BufWriter, Seek, SeekFrom};

fn encode_gray_test(criterion: &mut Criterion) {
    let counts = vec![100u32, 200, 400];

    let mut group = criterion.benchmark_group("encode_gray");
    for size in counts.iter().copied() {
        let bytes = size as usize;
        let im = vec![0; bytes * bytes];
        group.bench_with_input(BenchmarkId::new("rawvec", size), &im, |b, image| {
            let mut v = vec![];
            encode_raw(&mut v, &im, size, ColorType::L8);
            b.iter(|| encode_raw(&mut v, image, size, ColorType::L8));
        });
        group.bench_with_input(BenchmarkId::new("bufvec", size), &im, |b, image| {
            let mut v = vec![];
            encode_raw(&mut v, &im, size, ColorType::L8);
            b.iter(|| encode_bufvec(&mut v, image, size, ColorType::L8));
        });
        group.bench_with_input(BenchmarkId::new("file", size), &im, |b, image| {
            let file = File::create("temp.bmp").unwrap();
            b.iter(|| encode_file(&file, image, size, ColorType::L8));
        });
    }
}

criterion_group!(benches, encode_gray_test);
criterion_main!(benches);

fn encode_raw(into: &mut Vec<u8>, im: &[u8], size: u32, color: ColorType) {
    into.clear();
    let mut x = BMPEncoder::new(into);
    x.encode(im, size, size, color).unwrap();
}

fn encode_bufvec(into: &mut Vec<u8>, im: &[u8], size: u32, color: ColorType) {
    into.clear();
    let mut buf = BufWriter::new(into);
    let mut x = BMPEncoder::new(&mut buf);
    x.encode(im, size, size, color).unwrap();
}

fn encode_file(mut file: &File, im: &[u8], size: u32, color: ColorType) {
    file.seek(SeekFrom::Start(0)).unwrap();
    let mut buf = BufWriter::new(file);
    let mut x = BMPEncoder::new(&mut buf);
    x.encode(im, size, size, color).unwrap();
}
