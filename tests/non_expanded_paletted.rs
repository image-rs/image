#[test]
fn non_expanded_paletted() {
    use std::fs::File;
    use png::{Decoder, Transformations};

    // transformations omitting EXPAND will produce indexed bytes for paletted images
    let transformations = Transformations::IDENTITY;

    // any image in pngsuite ending with "3pXX" is a paletted image
    let mut decoder = Decoder::new(File::open("tests/pngsuite/basi3p01.png").unwrap());
    decoder.set_transformations(transformations);

    let (_, mut reader) = decoder.read_info().unwrap();
    let mut buf = vec![0u8; reader.output_buffer_size()];
    reader.next_frame(&mut buf).unwrap();

    let palette = reader.info().palette.as_ref().unwrap();

    let idx_palette = |i: usize| {
        let start= (buf[i / 8] >> (i % 8) & 1) as usize * 3;
        &palette[start .. start + 3]
    };

    assert_eq!(idx_palette(0), &[238, 255, 34]);
    assert_eq!(idx_palette(31), &[34, 102, 255]);
    assert_eq!(idx_palette(32 * 31 + 1), &[34, 102, 255]);
    assert_eq!(idx_palette(32 * 32 - 1), &[238, 255, 34]);
}