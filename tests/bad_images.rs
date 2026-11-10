use std::path::{Path, PathBuf};

use walkdir::WalkDir;

/// Tests that all images in `tests/bad/` fail to decode.
#[test]
fn bad_images() {
    let test_dir = std::path::absolute("./bad").unwrap();
    let images = image_paths_in(&test_dir);

    for image in images {
        let rel_image = image.strip_prefix(&test_dir).unwrap();
        match image::open(&image) {
            Ok(_) => {
                panic!("❌ Bad image decoded successfully: {}", rel_image.display());
            }
            Err(_) => {
                // expected
            }
        }
    }
}

fn image_paths_in(dir: &Path) -> impl Iterator<Item = PathBuf> {
    WalkDir::new(dir)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| e.file_type().is_file())
        .map(|e| e.into_path())
        .filter(|p| p.extension().is_some_and(|ext| ext != "txt" && ext != "md"))
}
