extern crate crc32fast;
extern crate glob;
extern crate png;

use std::collections::BTreeMap;
use std::fs::File;
use std::path::{Component, Path, PathBuf};
use std::io::BufReader;
use std::io::prelude::*;

use crc32fast::Hasher as Crc32;

const BASE_PATH: [&'static str; 2] = [".", "tests"];

fn process_images<F>(results_path: &str, func: F)
where F: Fn(PathBuf) -> Result<u32, png::DecodingError> {
    let base: PathBuf = BASE_PATH.iter().collect();
    let test_suites = &["pngsuite", "pngsuite-extra", "bugfixes"];
    let mut results = BTreeMap::new();
    let mut expected_failures = 0;
    for suite in test_suites {
        let mut path = base.clone();
        path.push(suite);
        path.push("*.png");

        let pattern = &*format!("{}", path.display());
        for path in glob::glob(pattern).unwrap().filter_map(Result::ok) {
            print!("{}: ", path.display());
            match func(path.clone()) {
                Ok(crc) => {
                    results.insert(format!("{}", path.display()), format!("{}", crc));
                    println!("{}", crc)
                },
                Err(_) if path.file_name().unwrap().to_str().unwrap().starts_with("x") => {
                    expected_failures += 1;
                    println!("Expected failure")
                },   
                err => panic!("{:?}", err)
            }
        }
    }
    let mut path = base.clone();
    path.push(results_path);
    let mut ref_results = BTreeMap::new();
    let mut failures = 0;
    for line in BufReader::new(File::open(path).unwrap()).lines() {
        let line = line.unwrap();
        let parts: Vec<_> = line.split(": ").collect();
        if parts[1] == "Expected failure" {
            failures += 1;
        } else {
            let current_path = format!("{}", normalize_path(Path::new(&parts[0])).display());
            ref_results.insert(current_path, parts[1].to_string());
        }
    }
    assert_eq!(expected_failures, failures);
    for (path, crc) in results.iter() {
        assert_eq!(
            ref_results.get(path).expect(&format!("reference for {} is missing", path)), 
            crc,
            "{}", path
        )
    }
}

#[test]
fn render_images() {
    process_images("results.txt",|path| {
        let decoder = png::Decoder::new(File::open(path)?);
        let (info, mut reader) = decoder.read_info()?;
        let mut img_data = vec![0; info.buffer_size()];
        reader.next_frame(&mut img_data)?;
        // First sanity check:
        assert_eq!(
            img_data.len(), 
            info.width as usize
            * info.height as usize
            * info.color_type.samples()
            * info.bit_depth as usize/8
        );
        let mut crc = Crc32::new();
        crc.update(&img_data);
        Ok(crc.finalize())
    })
}

#[test]
fn render_images_identity() {
    process_images("results_identity.txt", |path| {
        let mut decoder = png::Decoder::new(File::open(&path)?);
        decoder.set_transformations(png::Transformations::IDENTITY);

        let (info, mut reader) = decoder.read_info()?;
        let mut img_data = vec![0; info.buffer_size()];
        reader.next_frame(&mut img_data)?;
		let bits = info.width as usize
                * info.height as usize
                * info.color_type.samples()
                * info.bit_depth as usize;
        // First sanity check:
        assert_eq!(
            img_data.len() * 8,
            bits + 7 & !7,
			"path: {} info: {:?} bits: {}", path.display(), info, bits
        );
        let mut crc = Crc32::new();
        crc.update(&img_data);
        Ok(crc.finalize())
    });
}

// until rust standardizes path normalization, see https://github.com/rust-lang/rfcs/issues/2208
fn normalize_path(path: &Path) -> PathBuf {
    let mut components = path.components().peekable();
    let mut ret = if let Some(c @ Component::Prefix(..)) = components.peek().cloned() {
        components.next();
        PathBuf::from(c.as_os_str())
    } else {
        PathBuf::new()
    };

    for component in components {
        match component {
            Component::Prefix(..) => unreachable!(),
            Component::RootDir => {
                ret.push(component.as_os_str());
            }
            Component::CurDir => {}
            Component::ParentDir => {
                ret.pop();
            }
            Component::Normal(c) => {
                ret.push(c);
            }
        }
    }
    ret
}
