use std::fs;

use criterion::{criterion_group, criterion_main, Criterion, Throughput};
use png::Decoder;

#[path = "../src/test_utils.rs"]
mod test_utils;

fn load_all(c: &mut Criterion) {
    for entry in fs::read_dir("tests/benches/").unwrap().flatten() {
        match entry.path().extension() {
            Some(st) if st == "png" => {}
            _ => continue,
        }

        let data = fs::read(entry.path()).unwrap();
        bench_file(c, data, entry.file_name().into_string().unwrap());
    }

    bench_noncompressed_png(c, 8);
    bench_noncompressed_png(c, 128);
}

criterion_group!(benches, load_all);
criterion_main!(benches);

fn bench_noncompressed_png(c: &mut Criterion, width: u32) {
    let mut data = Vec::new();
    test_utils::write_noncompressed_png(&mut data, width);
    bench_file(
        c,
        data,
        format!("generated-png:noncompressed-{width}x{width}.png"),
    );
}

fn bench_file(c: &mut Criterion, data: Vec<u8>, name: String) {
    let mut group = c.benchmark_group("decode");
    if data.len() > 100000 {
        group.sample_size(20);
    }

    let decoder = Decoder::new(&*data);
    let mut reader = decoder.read_info().unwrap();
    let mut image = vec![0; reader.output_buffer_size()];
    let info = reader.next_frame(&mut image).unwrap();

    group.throughput(Throughput::Bytes(info.buffer_size() as u64));
    group.bench_with_input(name, &data, |b, data| {
        b.iter(|| {
            let decoder = Decoder::new(data.as_slice());
            let mut decoder = decoder.read_info().unwrap();
            decoder.next_frame(&mut image).unwrap();
        })
    });
}
