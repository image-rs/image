use image::imageops::FilterType;
use image::ImageFormat;
use std::fmt;
use std::fs::File;
use std::time::{Duration, Instant};

struct Elapsed(Duration);

impl Elapsed {
    fn from(start: &Instant) -> Self {
        Elapsed(start.elapsed())
    }
}

impl fmt::Display for Elapsed {
    fn fmt(&self, out: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match (self.0.as_secs(), self.0.subsec_nanos()) {
            (0, n) if n < 1000 => write!(out, "{} ns", n),
            (0, n) if n < 1_000_000 => write!(out, "{} µs", n / 1000),
            (0, n) => write!(out, "{} ms", n / 1_000_000),
            (s, n) if s < 10 => write!(out, "{}.{:02} s", s, n / 10_000_000),
            (s, _) => write!(out, "{} s", s),
        }
    }
}

fn main() {
    let tiny = image::open("examples/scaleup/tinycross.png").unwrap();
    for &(name, filter) in [
        ("near", FilterType::Nearest),
        ("tri", FilterType::Triangle),
        ("xcmr", FilterType::CatmullRom),
        ("ygauss", FilterType::Gaussian),
        ("zlcz2", FilterType::Lanczos3),
    ]
    .iter()
    {
        let timer = Instant::now();
        let scaled = tiny.resize(32, 32, filter);
        println!("Scaled by {} in {}", name, Elapsed::from(&timer));
        let mut output = File::create(&format!("up2-{}.png", name)).unwrap();
        scaled.write_to(&mut output, ImageFormat::Png).unwrap();

        let timer = Instant::now();
        let scaled = tiny.resize(48, 48, filter);
        println!("Scaled by {} in {}", name, Elapsed::from(&timer));
        let mut output = File::create(&format!("up3-{}.png", name)).unwrap();
        scaled.write_to(&mut output, ImageFormat::Png).unwrap();
    }
}
