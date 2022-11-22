use std::{env, fs, path::PathBuf};

use png::Decoder;

fn main() {
    let mut total_uncompressed = 0;
    let mut total_compressed = 0;
    let mut total_pixels = 0;
    let mut total_encode_time = 0;
    let mut total_decode_time = 0;

    println!(
        "{:45} Ratio             Encode                    Decode",
        "Directory"
    );
    println!(
        "{:45}-------     --------------------      --------------------",
        "---------"
    );

    let mut pending = vec![PathBuf::from(env::args().nth(1).unwrap_or(".".to_string()))];
    while let Some(directory) = pending.pop() {
        let mut dir_uncompressed = 0;
        let mut dir_compressed = 0;
        let mut dir_pixels = 0;
        let mut dir_encode_time = 0;
        let mut dir_decode_time = 0;

        for entry in fs::read_dir(&directory).unwrap().flatten() {
            if entry.file_type().unwrap().is_dir() {
                pending.push(entry.path());
                continue;
            }

            match entry.path().extension() {
                Some(st) if st == "png" => {}
                _ => continue,
            }

            // Parse
            let data = fs::read(entry.path()).unwrap();
            let mut decoder = Decoder::new(&*data);
            if decoder.read_header_info().ok().map(|h| h.color_type)
                == Some(png::ColorType::Indexed)
            {
                decoder.set_transformations(png::Transformations::EXPAND);
            }
            let mut reader = match decoder.read_info() {
                Ok(reader) => reader,
                Err(_) => continue,
            };
            let mut image = vec![0; reader.output_buffer_size()];
            let info = match reader.next_frame(&mut image) {
                Ok(info) => info,
                Err(_) => continue,
            };
            let (width, height) = (info.width, info.height);
            let color_type = info.color_type;
            let bit_depth = info.bit_depth;

            // Re-encode
            let start = std::time::Instant::now();
            let mut reencoded = Vec::new();
            let mut encoder = png::Encoder::new(&mut reencoded, width, height);
            encoder.set_color(color_type);
            encoder.set_depth(bit_depth);
            encoder.set_compression(png::Compression::Fast);
            encoder.set_filter(png::FilterType::Paeth);
            let mut encoder = encoder.write_header().unwrap();
            encoder.write_image_data(&image).unwrap();
            encoder.finish().unwrap();
            let elapsed = start.elapsed().as_nanos() as u64;

            let start2 = std::time::Instant::now();
            let mut reader = Decoder::new(&*reencoded).read_info().unwrap();
            let mut image2 = vec![0; reader.output_buffer_size()];
            reader.next_frame(&mut image2).unwrap();
            let elapsed2 = start2.elapsed().as_nanos() as u64;

            assert_eq!(image, image2);

            // Stats
            dir_uncompressed += image.len();
            dir_compressed += reencoded.len();
            dir_pixels += (width * height) as u64;
            dir_encode_time += elapsed;
            dir_decode_time += elapsed2;
        }
        if dir_uncompressed > 0 {
            println!(
                "{:45}{:6.2}%{:8} mps {:6.2} GiB/s {:8} mps {:6.2} GiB/s",
                directory.display(),
                100.0 * dir_compressed as f64 / dir_uncompressed as f64,
                dir_pixels * 1000 / dir_encode_time,
                dir_uncompressed as f64 / (dir_encode_time as f64 * 1e-9 * (1 << 30) as f64),
                dir_pixels * 1000 / dir_decode_time,
                dir_uncompressed as f64 / (dir_decode_time as f64 * 1e-9 * (1 << 30) as f64)
            );
        }

        total_uncompressed += dir_uncompressed;
        total_compressed += dir_compressed;
        total_pixels += dir_pixels;
        total_encode_time += dir_encode_time;
        total_decode_time += dir_decode_time;
    }

    println!();
    println!(
        "{:44}{:7.3}%{:8} mps {:6.2} GiB/s {:8} mps {:6.2} GiB/s",
        "Total",
        100.0 * total_compressed as f64 / total_uncompressed as f64,
        total_pixels * 1000 / total_encode_time,
        total_uncompressed as f64 / (total_encode_time as f64 * 1e-9 * (1 << 30) as f64),
        total_pixels * 1000 / total_decode_time,
        total_uncompressed as f64 / (total_decode_time as f64 * 1e-9 * (1 << 30) as f64)
    );
}
