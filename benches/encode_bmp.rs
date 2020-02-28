extern crate criterion;

use criterion::{Bencher, Criterion, ParameterizedBenchmark, criterion_group, criterion_main};
use image::{ColorType, bmp::BMPEncoder};

use std::fs::File;
use std::io::BufWriter;

fn encode_gray_test(criterion: &mut Criterion) {
    let counts = vec![100usize, 200, 400, 800, 1200];
    fn raw_vec(b: &mut Bencher, s: &usize) {
        let s = *s;
        let im = vec![0; s * s];
        let mut v = vec![0; s * s / 4 + 4096 + s * 4];
        b.iter(|| {
            v.clear();
            let mut x = BMPEncoder::new(&mut v);
            x.encode(&im, (s / 2) as u32, (s / 2) as u32, ColorType::L8).unwrap();
        }
        )
    }

    fn buf_vec(b: &mut Bencher, s: &usize) {
        let s = *s;
        let im = vec![0; s * s];
        let mut v = vec![0; s * s / 4 + 4096 + s * 4];
        b.iter(|| {
            v.clear();
            let mut buf = BufWriter::new(&mut v);
            let mut x = BMPEncoder::new(&mut buf);
            x.encode(&im, (s / 2) as u32, (s / 2) as u32, ColorType::L8).unwrap();
        }
        )
    }

    fn buf_file(b: &mut Bencher, s: &usize) {
        let s = *s;
        let im = vec![0; s * s];
        b.iter(|| {
            let mut f = File::open("temp.bmp").unwrap();
            let mut buf = BufWriter::new(&mut f);
            let mut x = BMPEncoder::new(&mut buf);
            x.encode(&im, (s / 2) as u32, (s / 2) as u32, ColorType::L8).unwrap();
        }
        )
    }

    criterion.bench("encode_gray",
                    ParameterizedBenchmark::new("raw_vec", raw_vec, counts)
                        .with_function("buf_vec", buf_vec)
                        .with_function("buf_file", buf_file));
}

fn encode_rgb_test(criterion: &mut Criterion) {
    let counts = vec![100usize, 200, 400, 800, 1200];
    fn raw_vec(b: &mut Bencher, s: &usize) {
        let s = *s;
        let im = vec![0; s * s];
        let mut v = vec![0; s * s / 4 + 4096 + s * 4];
        b.iter(|| {
            v.clear();
            let mut x = BMPEncoder::new(&mut v);
            x.encode(&im, (s / 2) as u32, (s / 2) as u32, ColorType::Rgb8).unwrap();
        }
        )
    }

    fn buf_vec(b: &mut Bencher, s: &usize) {
        let s = *s;
        let im = vec![0; s * s];
        let mut v = vec![0; s * s / 4 + 4096 + s * 4];
        b.iter(|| {
            v.clear();
            let mut buf = BufWriter::new(&mut v);
            let mut x = BMPEncoder::new(&mut buf);
            x.encode(&im, (s / 2) as u32, (s / 2) as u32, ColorType::Rgb8).unwrap();
        }
        )
    }

    fn buf_file(b: &mut Bencher, s: &usize) {
        let s = *s;
        let im = vec![0; s * s];
        b.iter(|| {
            let mut f = File::open("temp.bmp").unwrap();
            let mut buf = BufWriter::new(&mut f);
            let mut x = BMPEncoder::new(&mut buf);
            x.encode(&im, (s / 2) as u32, (s / 2) as u32, ColorType::Rgb8).unwrap();
        }
        )
    }

    criterion.bench("encode_rgb",
                    ParameterizedBenchmark::new("raw_vec", raw_vec, counts)
                        .with_function("buf_vec", buf_vec)
                        .with_function("buf_file", buf_file));
}

fn encode_rgba_test(criterion: &mut Criterion) {
    let counts = vec![100usize, 200, 400, 800, 1200];
    fn raw_vec(b: &mut Bencher, s: &usize) {
        let s = *s;
        let im = vec![0; s * s];
        let mut v = vec![0; s * s / 4 + 4096 + s * 4];
        b.iter(|| {
            v.clear();
            let mut x = BMPEncoder::new(&mut v);
            x.encode(&im, (s / 2) as u32, (s / 2) as u32, ColorType::Rgba8).unwrap();
        }
        )
    }

    fn buf_vec(b: &mut Bencher, s: &usize) {
        let s = *s;
        let im = vec![0; s * s];
        let mut v = vec![0; s * s / 4 + 4096 + s * 4];
        b.iter(|| {
            v.clear();
            let mut buf = BufWriter::new(&mut v);
            let mut x = BMPEncoder::new(&mut buf);
            x.encode(&im, (s / 2) as u32, (s / 2) as u32, ColorType::Rgba8).unwrap();
        }
        )
    }

    fn buf_file(b: &mut Bencher, s: &usize) {
        let s = *s;
        let im = vec![0; s * s];
        b.iter(|| {
            let mut f = File::open("temp.bmp").unwrap();
            let mut buf = BufWriter::new(&mut f);
            let mut x = BMPEncoder::new(&mut buf);
            x.encode(&im, (s / 2) as u32, (s / 2) as u32, ColorType::Rgba8).unwrap();
        }
        )
    }

    criterion.bench("encode_rgba",
                    ParameterizedBenchmark::new("raw_vec", raw_vec, counts)
                        .with_function("buf_vec", buf_vec)
                        .with_function("buf_file", buf_file));
}

criterion_group!(benches, encode_gray_test);
criterion_main!(benches);

