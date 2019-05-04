//! Compares the decoding results with reference renderings.

use std::fs;
use std::io;
use std::path::PathBuf;
use std::u32;

extern crate glob;
extern crate image;

const BASE_PATH: [&str; 2] = [".", "tests"];
const IMAGE_DIR: &str = "images";
const OUTPUT_DIR: &str = "output";
const REFERENCE_DIR: &str = "reference";

fn process_images<F>(dir: &str, input_decoder: Option<&str>, func: F)
where
    F: Fn(&PathBuf, PathBuf, &str),
{
    let base: PathBuf = BASE_PATH.iter().collect();
    let decoders = &["tga", "tiff", "png", "gif", "bmp", "ico", "jpg", "hdr", "pbm"];
    for decoder in decoders {
        let mut path = base.clone();
        path.push(dir);
        path.push(decoder);
        path.push("**");
        path.push(
            "*.".to_string() + match input_decoder {
                Some(val) => val,
                None => decoder,
            },
        );
        let pattern = &*format!("{}", path.display());
        for path in glob::glob(pattern).unwrap().filter_map(Result::ok) {
            func(&base, path, decoder)
        }
    }
}

#[cfg(feature = "png")]
#[test]
fn render_images() {
    process_images(IMAGE_DIR, None, |base, path, decoder| {
        println!("render_images {}", path.display());
        let img = match image::open(&path) {
            Ok(img) => img.to_rgba(),
            // Do not fail on unsupported error
            // This might happen because the testsuite contains unsupported images
            // or because a specific decoder included via a feature.
            Err(image::ImageError::UnsupportedError(e)) => {
                println!("UNSUPPORTED {}: {}", path.display(), e);
                return;
            }
            Err(err) => panic!(format!("decoding of {:?} failed with: {}", path, err)),
        };
        let mut crc = Crc32::new();
        crc.update(&*img);

        let (filename, testsuite) = {
            let mut path: Vec<_> = path.components().collect();
            (path.pop().unwrap(), path.pop().unwrap())
        };
        let mut out_path = base.clone();

        out_path.push(OUTPUT_DIR);
        out_path.push(decoder);
        out_path.push(testsuite.as_os_str());
        fs::create_dir_all(&out_path).unwrap();
        out_path.push(format!(
            "{}.{}.{}",
            filename.as_os_str().to_str().unwrap(),
            format!("{:x}", crc.checksum()),
            "png"
        ));
        img.save(out_path).unwrap();
    })
}

#[test]
fn check_references() {
    process_images(REFERENCE_DIR, Some("png"), |base, path, decoder| {
        let ref_img = match image::open(&path) {
            Ok(img) => img.to_rgba(),
            // Do not fail on unsupported error
            // This might happen because the testsuite contains unsupported images
            // or because a specific decoder included via a feature.
            Err(image::ImageError::UnsupportedError(_)) => return,
            Err(err) => panic!(format!("{}", err)),
        };

        let (filename, testsuite) = {
            let mut path: Vec<_> = path.components().collect();
            (path.pop().unwrap(), path.pop().unwrap())
        };
        let mut img_path = base.clone();
        img_path.push(IMAGE_DIR);
        img_path.push(decoder);
        img_path.push(testsuite.as_os_str());
        img_path.push(
            filename
                .as_os_str()
                .to_str()
                .unwrap()
                .split('.')
                .take(2)
                .collect::<Vec<_>>()
                .join("."),
        );
        let ref_crc = u32::from_str_radix(
            filename
                .as_os_str()
                .to_str()
                .unwrap()
                .split('.')
                .nth(2)
                .unwrap(),
            16,
        ).unwrap();
        let test_img = match image::open(&img_path) {
            Ok(img) => img.to_rgba(),
            // Do not fail on unsupported error
            // This might happen because the testsuite contains unsupported images
            // or because a specific decoder included via a feature.
            Err(image::ImageError::UnsupportedError(_)) => return,
            Err(err) => panic!(format!("decoding of {:?} failed with: {}", path, err)),
        };
        let mut test_crc = Crc32::new();
        test_crc.update(&*test_img);
        if *ref_img != *test_img || test_crc.checksum() != ref_crc {
            panic!(
                "Reference rendering does not match for image at {:?}.",
                img_path
            )
        }
    })
}

#[cfg(feature = "hdr")]
#[test]
fn check_hdr_references() {
    let mut ref_path: PathBuf = BASE_PATH.iter().collect();
    ref_path.push(REFERENCE_DIR);
    ref_path.push("hdr");
    let mut path: PathBuf = BASE_PATH.iter().collect();
    path.push(IMAGE_DIR);
    path.push("hdr");
    path.push("*");
    path.push("*.hdr");
    let pattern = &*format!("{}", path.display());
    for path in glob::glob(pattern).unwrap().filter_map(Result::ok) {
        use std::path::Component::Normal;
        let mut ref_path = ref_path.clone();
        // append 2 last components of image path to reference path
        for c in path.components()
            .rev()
            .take(2)
            .collect::<Vec<_>>()
            .iter()
            .rev()
        {
            match *c {
                Normal(name) => ref_path.push(name),
                _ => panic!(),
            }
        }
        ref_path.set_extension("raw");
        println!("{}", ref_path.display());
        println!("{}", path.display());
        let decoder = image::hdr::HDRDecoder::new(io::BufReader::new(
            fs::File::open(&path).unwrap(),
        )).unwrap();
        let decoded = decoder.read_image_hdr().unwrap();
        let reference = image::hdr::read_raw_file(&ref_path).unwrap();
        assert_eq!(decoded, reference);
    }
}

/// Check that BMP files with large values could cause OOM issues are rejected.
///
/// The images are postfixed with `bad_bmp` to not be loaded by the other test.
#[test]
fn bad_bmps() {
    let path: PathBuf = BASE_PATH
        .iter()
        .collect::<PathBuf>()
        .join(IMAGE_DIR)
        .join("bmp/images")
        .join("*.bad_bmp");

    let pattern = &*format!("{}", path.display());
    for path in glob::glob(pattern).unwrap().filter_map(Result::ok) {
        let im = image::open(path);
        assert!(im.is_err());
    }
}

#[cfg_attr(rustfmt, rustfmt_skip)]
const CRC_TABLE: [u32; 256] = [
    0x0000_0000, 0x7707_3096, 0xee0e_612c, 0x9909_51ba, 0x076d_c419,
    0x706a_f48f, 0xe963_a535, 0x9e64_95a3, 0x0edb_8832, 0x79dc_b8a4,
    0xe0d5_e91e, 0x97d2_d988, 0x09b6_4c2b, 0x7eb1_7cbd, 0xe7b8_2d07,
    0x90bf_1d91, 0x1db7_1064, 0x6ab0_20f2, 0xf3b9_7148, 0x84be_41de,
    0x1ada_d47d, 0x6ddd_e4eb, 0xf4d4_b551, 0x83d3_85c7, 0x136c_9856,
    0x646b_a8c0, 0xfd62_f97a, 0x8a65_c9ec, 0x1401_5c4f, 0x6306_6cd9,
    0xfa0f_3d63, 0x8d08_0df5, 0x3b6e_20c8, 0x4c69_105e, 0xd560_41e4,
    0xa267_7172, 0x3c03_e4d1, 0x4b04_d447, 0xd20d_85fd, 0xa50a_b56b,
    0x35b5_a8fa, 0x42b2_986c, 0xdbbb_c9d6, 0xacbc_f940, 0x32d8_6ce3,
    0x45df_5c75, 0xdcd6_0dcf, 0xabd1_3d59, 0x26d9_30ac, 0x51de_003a,
    0xc8d7_5180, 0xbfd0_6116, 0x21b4_f4b5, 0x56b3_c423, 0xcfba_9599,
    0xb8bd_a50f, 0x2802_b89e, 0x5f05_8808, 0xc60c_d9b2, 0xb10b_e924,
    0x2f6f_7c87, 0x5868_4c11, 0xc161_1dab, 0xb666_2d3d, 0x76dc_4190,
    0x01db_7106, 0x98d2_20bc, 0xefd5_102a, 0x71b1_8589, 0x06b6_b51f,
    0x9fbf_e4a5, 0xe8b8_d433, 0x7807_c9a2, 0x0f00_f934, 0x9609_a88e,
    0xe10e_9818, 0x7f6a_0dbb, 0x086d_3d2d, 0x9164_6c97, 0xe663_5c01,
    0x6b6b_51f4, 0x1c6c_6162, 0x8565_30d8, 0xf262_004e, 0x6c06_95ed,
    0x1b01_a57b, 0x8208_f4c1, 0xf50f_c457, 0x65b0_d9c6, 0x12b7_e950,
    0x8bbe_b8ea, 0xfcb9_887c, 0x62dd_1ddf, 0x15da_2d49, 0x8cd3_7cf3,
    0xfbd4_4c65, 0x4db2_6158, 0x3ab5_51ce, 0xa3bc_0074, 0xd4bb_30e2,
    0x4adf_a541, 0x3dd8_95d7, 0xa4d1_c46d, 0xd3d6_f4fb, 0x4369_e96a,
    0x346e_d9fc, 0xad67_8846, 0xda60_b8d0, 0x4404_2d73, 0x3303_1de5,
    0xaa0a_4c5f, 0xdd0d_7cc9, 0x5005_713c, 0x2702_41aa, 0xbe0b_1010,
    0xc90c_2086, 0x5768_b525, 0x206f_85b3, 0xb966_d409, 0xce61_e49f,
    0x5ede_f90e, 0x29d9_c998, 0xb0d0_9822, 0xc7d7_a8b4, 0x59b3_3d17,
    0x2eb4_0d81, 0xb7bd_5c3b, 0xc0ba_6cad, 0xedb8_8320, 0x9abf_b3b6,
    0x03b6_e20c, 0x74b1_d29a, 0xead5_4739, 0x9dd2_77af, 0x04db_2615,
    0x73dc_1683, 0xe363_0b12, 0x9464_3b84, 0x0d6d_6a3e, 0x7a6a_5aa8,
    0xe40e_cf0b, 0x9309_ff9d, 0x0a00_ae27, 0x7d07_9eb1, 0xf00f_9344,
    0x8708_a3d2, 0x1e01_f268, 0x6906_c2fe, 0xf762_575d, 0x8065_67cb,
    0x196c_3671, 0x6e6b_06e7, 0xfed4_1b76, 0x89d3_2be0, 0x10da_7a5a,
    0x67dd_4acc, 0xf9b9_df6f, 0x8ebe_eff9, 0x17b7_be43, 0x60b0_8ed5,
    0xd6d6_a3e8, 0xa1d1_937e, 0x38d8_c2c4, 0x4fdf_f252, 0xd1bb_67f1,
    0xa6bc_5767, 0x3fb5_06dd, 0x48b2_364b, 0xd80d_2bda, 0xaf0a_1b4c,
    0x3603_4af6, 0x4104_7a60, 0xdf60_efc3, 0xa867_df55, 0x316e_8eef,
    0x4669_be79, 0xcb61_b38c, 0xbc66_831a, 0x256f_d2a0, 0x5268_e236,
    0xcc0c_7795, 0xbb0b_4703, 0x2202_16b9, 0x5505_262f, 0xc5ba_3bbe,
    0xb2bd_0b28, 0x2bb4_5a92, 0x5cb3_6a04, 0xc2d7_ffa7, 0xb5d0_cf31,
    0x2cd9_9e8b, 0x5bde_ae1d, 0x9b64_c2b0, 0xec63_f226, 0x756a_a39c,
    0x026d_930a, 0x9c09_06a9, 0xeb0e_363f, 0x7207_6785, 0x0500_5713,
    0x95bf_4a82, 0xe2b8_7a14, 0x7bb1_2bae, 0x0cb6_1b38, 0x92d2_8e9b,
    0xe5d5_be0d, 0x7cdc_efb7, 0x0bdb_df21, 0x86d3_d2d4, 0xf1d4_e242,
    0x68dd_b3f8, 0x1fda_836e, 0x81be_16cd, 0xf6b9_265b, 0x6fb0_77e1,
    0x18b7_4777, 0x8808_5ae6, 0xff0f_6a70, 0x6606_3bca, 0x1101_0b5c,
    0x8f65_9eff, 0xf862_ae69, 0x616b_ffd3, 0x166c_cf45, 0xa00a_e278,
    0xd70d_d2ee, 0x4e04_8354, 0x3903_b3c2, 0xa767_2661, 0xd060_16f7,
    0x4969_474d, 0x3e6e_77db, 0xaed1_6a4a, 0xd9d6_5adc, 0x40df_0b66,
    0x37d8_3bf0, 0xa9bc_ae53, 0xdebb_9ec5, 0x47b2_cf7f, 0x30b5_ffe9,
    0xbdbd_f21c, 0xcaba_c28a, 0x53b3_9330, 0x24b4_a3a6, 0xbad0_3605,
    0xcdd7_0693, 0x54de_5729, 0x23d9_67bf, 0xb366_7a2e, 0xc461_4ab8,
    0x5d68_1b02, 0x2a6f_2b94, 0xb40b_be37, 0xc30c_8ea1, 0x5a05_df1b,
    0x2d02_ef8d,
];

/// An Implementation of the Crc-32 checksum
/// This is copied from `png::hash` such that it does not have to be exposed in the public interface
pub struct Crc32 {
    crc: u32,
}

impl Default for Crc32 {
    fn default() -> Self {
        Self::new()
    }
}

impl Crc32 {
    /// Create a new hasher.
    pub fn new() -> Crc32 {
        Crc32 { crc: 0xFFFF_FFFF }
    }

    /// Update the internal hasher with the bytes from ```buf```
    pub fn update(&mut self, buf: &[u8]) {
        for &byte in buf {
            let a = (self.crc ^ u32::from(byte)) & 0xFF;
            let b = self.crc >> 8;

            self.crc = CRC_TABLE[a as usize] ^ b;
        }
    }

    /// Return the computed hash.
    pub fn checksum(&self) -> u32 {
        self.crc ^ 0xFFFF_FFFF
    }

    /// Reset this hasher to its initial state.
    pub fn reset(&mut self) {
        self.crc = 0xFFFF_FFFF;
    }
}
