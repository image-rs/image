use std::{fs, path};

use image::ImageFormat;
use criterion::{Criterion, criterion_group, criterion_main};

#[derive(Clone, Copy)]
struct BenchDef {
    dir: &'static [&'static str],
    files: &'static [&'static str],
    format: ImageFormat,
}

fn bench_load(c: &mut Criterion, def: &BenchDef) {
    let group_name = format!("load-{:?}", def.format);
    let mut group = c.benchmark_group(&group_name);
    let paths = IMAGE_DIR.iter().chain(def.dir);

    for file in def.files {
        let fn_name = format!("{}-{}", &group_name, file);
        let path: path::PathBuf = paths.clone().chain(Some(file)).collect();
        let buf = fs::read(path).unwrap();
        group.bench_function(fn_name, |b| b.iter(|| {
            image::load_from_memory_with_format(&buf, def.format).unwrap();
        }));
    }
}

fn load_all(c: &mut Criterion) {
    const BENCH_DEFS: &'static [BenchDef] = &[
        BMP,
    ];

    for bench in BENCH_DEFS {
        bench_load(c, bench);
    }
}

criterion_group!(benches, load_all);
criterion_main!(benches);

const IMAGE_DIR: [&'static str; 3] = [".", "tests", "images"];

const BMP: BenchDef = BenchDef {
    dir: &["bmp", "images"],
    files: &[
        "Core_1_Bit.bmp",
        "Core_4_Bit.bmp",
        "Core_8_Bit.bmp",
        "rgb16.bmp",
        "rgb24.bmp",
        "rgb32.bmp",
        "pal4rle.bmp",
        "pal8rle.bmp",
        "rgb16-565.bmp",
        "rgb32bf.bmp",
    ],
    format: ImageFormat::Bmp,
};
