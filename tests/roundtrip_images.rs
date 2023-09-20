use std::path::PathBuf;
use image::{DynamicImage, ImageFormat, ImageResult, ImageError};
use std::io::Cursor;
use image::io::Reader;

const BASE_PATH: [&str; 2] = [".", "tests"];


#[test]
fn roundtrip() {
    for_each_image_file_in_repository(|path| {
        println!("round trip test at path {:?}", path);

        let format_and_image: ImageResult<(ImageFormat, DynamicImage)> = Reader::open(&path)
            .map_err(|err| err.into())
            .and_then(|reader| {
                let reader = reader.with_guessed_format()?;
                let format = reader.format();
                let content = reader.decode()?;

                // the image has been decoded, so the format is guaranteed to be detected
                Ok((format.unwrap(), content))
            });

        if let Ok((format, decoded)) = format_and_image {
            if format.can_write() {
                let mut byte_buffer = Vec::new();
                match decoded.write_to(&mut Cursor::new(&mut byte_buffer), format) {
                    Ok(_) => {
                        let re_decoded = image::load(Cursor::new(byte_buffer.as_slice()), format)
                            .expect("roundtrip re-decoding failed");

                        assert_eq!(decoded, re_decoded); // TODO tolerance for float images
                    }

                    Err(ImageError::Unsupported(error)) =>
                        println!("reading was successful, but the encoder does not support this image ({:?})", error),

                    Err(_) => panic!("cannot write image even though format says so"),
                }
            } else {
                println!("Skipping read-only format {:?}", format);
            }
        } else {
            println!("Skipping invalid image at path {:?}", &path);
        }
    })
}


fn for_each_image_file_in_repository(process_image: impl Fn(PathBuf)) {
    let base: PathBuf = BASE_PATH.iter().collect();

    let extensions = &[
        "tga", "tiff", "png", "gif", "bmp", "ico",
        "jpg", "hdr", "pbm", "webp", "exr"
    ];

    for extension in extensions {
        let mut path = base.clone();
        path.push("**");
        path.push("*.".to_string() + extension);
        let pattern = &*format!("{}", path.display());
        for path in glob::glob(pattern).unwrap().filter_map(Result::ok) {
            process_image(path)
        }
    }
}