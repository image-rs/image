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
            (0, n) if n < 1000 => write!(out, "{n} ns"),
            (0, n) if n < 1_000_000 => write!(out, "{} µs", n / 1000),
            (0, n) => write!(out, "{} ms", n / 1_000_000),
            (s, n) if s < 10 => write!(out, "{}.{:02} s", s, n / 10_000_000),
            (s, _) => write!(out, "{s} s"),
        }
    }
}

fn main() {
    let mut tiny = image::open("examples/scaleup/tinycross.png").unwrap();
    for &(name, filter) in &[
        ("near", FilterType::Nearest),
        ("tri", FilterType::Triangle),
        ("xcmr", FilterType::CatmullRom),
        ("ygauss", FilterType::Gaussian),
        ("zlcz2", FilterType::Lanczos3),
    ] {
        let mut tiny1 = tiny.clone();
        let timer = Instant::now();
        tiny1.resize_to_fit(32, 32, filter);
        println!("Scaled by {} in {}", name, Elapsed::from(&timer));
        let mut output = File::create(format!("up2-{name}.png")).unwrap();
        tiny1.write_to(&mut output, ImageFormat::Png).unwrap();

        let timer = Instant::now();
        tiny.resize_to_fit(48, 48, filter);
        println!("Scaled by {} in {}", name, Elapsed::from(&timer));
        let mut output = File::create(format!("up3-{name}.png")).unwrap();
        tiny.write_to(&mut output, ImageFormat::Png).unwrap();
    }
}
