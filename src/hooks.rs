//! This module provides a way to register decoding hooks for image formats not directly supported
//! by this crate.

use std::{ffi::OsStr, sync::Arc};

use crate::{
    io::{registry, signatures},
    ImageFormat,
};

pub use crate::io::generic::GenericReader;

/// A function to produce an [`ImageDecoder`] for a given image format.
pub type DecodingHook = Box<registry::DecodingFn>;

/// Register a new decoding hook or returns false if one already exists for the given format.
///
/// TODO: Talk about how this interacts with builtin formats.
pub fn register_decoding_hook(extension: &str, hook: DecodingHook) -> Option<ImageFormat> {
    let extension = extension.to_ascii_lowercase();
    // TODO: This currently also resolves aliases. Should it?
    let format = ImageFormat::from_extension(&extension);
    let hook = Arc::from(hook);

    registry::write_registry(move |reg| {
        if let Some(format) = format {
            let spec = reg.get_mut(format.id());
            if spec.decoding_fn.is_some() {
                return None;
            }
            spec.decoding_fn = Some(hook);
            spec.can_read = true;
            Some(format)
        } else {
            // Once registered, the extension string will live for the remainder of the program's
            // life, so leaking it here is fine.
            let id = reg.add_format(registry::FormatSpec::new_hook(extension.leak(), hook));
            Some(ImageFormat::from_id(id))
        }
    })
}

/// Returns whether a decoding hook has been registered for the given format.
pub fn decoding_hook_registered(extension: &OsStr) -> bool {
    // TODO: Same as in register_decoding_hook. This currently also resolves aliases. Should it?
    let Some(format) = ImageFormat::from_extension(extension) else {
        return false;
    };
    registry::read_registry(|reg| reg.get(format.id()).decoding_fn.is_some())
}

/// Adds the given extensions as extension aliases to the given format.
///
/// If an extension is already registered for the format, no duplicate entries in the extension list
/// of the format will be created, but it will overwrite any existing mapping for format detection.
///
/// Extension aliases are not allowed to be empty or contain ASCII uppercase characters.
///
/// ## Examples
///
/// Suppose two formats "foo" and "bar" are registered with extensions "foo" and "bar" respectively.
/// Additionally, both formats support the extension "baz".
///
/// ```no_run
/// # use image::{ImageFormat, hooks::register_format_extensions};
/// let foo = ImageFormat::from_extension("foo").unwrap();
/// let bar = ImageFormat::from_extension("bar").unwrap();
/// register_format_extensions(foo, &["baz"]);
/// register_format_extensions(bar, &["baz"]);
/// assert_eq!(ImageFormat::from_extension("baz"), Some(bar));
/// ```
///
/// Since "bar" was registered last with the "baz" extension, it takes precedence in format detection.
pub fn register_format_extensions(format: ImageFormat, extensions: &[&'static str]) {
    registry::write_registry(|reg| {
        reg.add_extension_aliases(format.id(), extensions);
    });
}

/// Adds the given MIME types to the list associated with the given format extension.
///
/// The first registered MIME type is considered the main MIME type of the format and will be
/// returned by [ImageFormat::to_mime_type]. All others are considered aliases and will be used to
/// identify the format during MIME type based detection.
///
/// If an MIME types is already registered for the format, no duplicate entries in the MIME type
/// list of the format will be created, but it will overwrite any existing mapping for format
/// detection.
///
/// Registering `application/octet-stream` is **not** recommended.
///
/// ## Examples
///
/// ```no_run
/// # use image::{ImageFormat, hooks::register_format_mime_types};
/// let example = ImageFormat::from_extension("example").unwrap();
/// register_format_mime_types(example, &["image/example", "image/x-example"]);
/// assert_eq!(example.to_mime_type(), "image/example");
/// ```
pub fn register_format_mime_types(format: ImageFormat, mime_types: &[&'static str]) {
    registry::write_registry(|reg| {
        reg.add_mime_types(format.id(), mime_types);
    });
}

/// Registers a format detection hook.
///
/// The signature field holds the magic bytes from the start of the file that must be matched to
/// detect the format. The mask field is optional and can be used to specify which bytes in the
/// signature should be ignored during the detection.
///
/// # Examples
///
/// ## Using the mask to ignore some bytes
///
/// ```
/// # use image::{ImageFormat, hooks::register_format_detection_hook};
/// // WebP signature is 'riff' followed by 4 bytes of length and then by 'webp'.
/// // This requires a mask to ignore the length.
/// register_format_detection_hook(ImageFormat::WebP,
///      &[b'r', b'i', b'f', b'f', 0, 0, 0, 0, b'w', b'e', b'b', b'p'],
/// Some(&[0xff, 0xff, 0xff, 0xff, 0, 0, 0, 0, 0xff, 0xff, 0xff, 0xff]),
/// );
/// ```
///
/// ## Multiple signatures
///
/// ```no_run
/// # use image::{ImageFormat, hooks::register_format_detection_hook};
/// // JPEG XL has two different signatures: https://en.wikipedia.org/wiki/JPEG_XL
/// // This function should be called twice to register them both.
/// let jxl = ImageFormat::from_extension("jxl").unwrap();
/// register_format_detection_hook(jxl, &[0xff, 0x0a], None);
/// register_format_detection_hook(jxl,
///      &[0x00, 0x00, 0x00, 0x0c, 0x4a, 0x58, 0x4c, 0x20, 0x0d, 0x0a, 0x87, 0x0a], None,
/// );
/// ```
///
pub fn register_format_detection_hook(
    format: ImageFormat,
    signature: &'static [u8],
    mask: Option<&'static [u8]>,
) {
    signatures::register_signature((format, signature, mask.unwrap_or(&[])));
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        io::signatures::guess_format_from_signature, ColorType, DynamicImage, ImageDecoder,
        ImageReader, ImageResult,
    };
    use std::io::Cursor;

    const MOCK_HOOK_EXTENSION: &str = "MOCKHOOK";

    const MOCK_IMAGE_OUTPUT: [u8; 9] = [255, 0, 0, 0, 255, 0, 0, 0, 255];
    struct MockDecoder {}
    impl ImageDecoder for MockDecoder {
        fn dimensions(&self) -> (u32, u32) {
            ((&MOCK_IMAGE_OUTPUT.len() / 3) as u32, 1)
        }
        fn color_type(&self) -> ColorType {
            ColorType::Rgb8
        }
        fn read_image(self, buf: &mut [u8]) -> ImageResult<()> {
            buf[..MOCK_IMAGE_OUTPUT.len()].copy_from_slice(&MOCK_IMAGE_OUTPUT);
            Ok(())
        }
        fn read_image_boxed(self: Box<Self>, buf: &mut [u8]) -> ImageResult<()> {
            (*self).read_image(buf)
        }
    }
    fn is_mock_decoder_output(image: DynamicImage) -> bool {
        image.as_rgb8().unwrap().as_raw() == &MOCK_IMAGE_OUTPUT
    }
    fn register_mock_decoder() -> ImageFormat {
        register_decoding_hook(
            MOCK_HOOK_EXTENSION,
            Box::new(|_| Ok(Box::new(MockDecoder {}))),
        )
        .or(ImageFormat::from_extension(MOCK_HOOK_EXTENSION))
        .unwrap()
    }

    #[test]
    fn decoding_hook() {
        let mock = register_mock_decoder();

        assert!(decoding_hook_registered(OsStr::new(MOCK_HOOK_EXTENSION)));
        assert_eq!(ImageFormat::from_extension(MOCK_HOOK_EXTENSION), Some(mock));
        assert!(ImageFormat::all().any(|f| f == mock));
        assert!(mock.can_read());

        let image = ImageReader::open("tests/images/hook/extension.MoCkHoOk")
            .unwrap()
            .decode()
            .unwrap();

        assert!(is_mock_decoder_output(image));
    }

    #[test]
    fn detection_hook() {
        let mock = register_mock_decoder();

        register_format_detection_hook(
            mock,
            &[b'H', b'E', b'A', b'D', 0, 0, 0, 0, b'M', b'O', b'C', b'K'],
            Some(&[0xff, 0xff, 0xff, 0xff, 0, 0, 0, 0, 0xff, 0xff, 0xff, 0xff]),
        );

        const TEST_INPUT_IMAGE: [u8; 16] = [
            b'H', b'E', b'A', b'D', b'J', b'U', b'N', b'K', b'M', b'O', b'C', b'K', b'm', b'o',
            b'r', b'e',
        ];
        assert_eq!(guess_format_from_signature(&TEST_INPUT_IMAGE), Some(mock));

        let image = ImageReader::new(Cursor::new(TEST_INPUT_IMAGE))
            .with_guessed_format()
            .unwrap()
            .decode()
            .unwrap();

        assert!(is_mock_decoder_output(image));
    }
}
