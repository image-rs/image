use byteorder::WriteBytesExt;
use std::io::Write;

/// Generates a store-only, non-compressed image:
///
/// * `00` compression mode (i.e.`BTYPE` = `00` = no compression) is used
/// * No filter is applied to the image rows
///
/// Currently the image always has the following properties:
///
/// * Single `IDAT` chunk
/// * Zlib chunks of maximum possible size
/// * 8-bit RGBA
///
/// These images are somewhat artificial, but may be useful for benchmarking performance of parts
/// outside of `fdeflate` crate and/or the `unfilter` function (e.g. these images were originally
/// used to evaluate changes to minimize copying of image pixels between various buffers - see
/// [this
/// discussion](https://github.com/image-rs/image-png/discussions/416#discussioncomment-7436871)
/// for more details).
pub fn write_noncompressed_png(w: &mut impl Write, width: u32) {
    write_png_sig(w);
    write_ihdr(w, width);
    write_noncompressed_idat(w, width);
    write_iend(w);
}

fn write_png_sig(w: &mut impl Write) {
    const SIG: [u8; 8] = [137, 80, 78, 71, 13, 10, 26, 10];
    w.write_all(&SIG).unwrap();
}

fn write_chunk(w: &mut impl Write, chunk_type: &[u8], data: &[u8]) {
    let crc = {
        let input = chunk_type
            .iter()
            .copied()
            .chain(data.iter().copied())
            .collect::<Vec<_>>();
        crc32fast::hash(input.as_slice())
    };
    w.write_u32::<byteorder::BigEndian>(data.len() as u32)
        .unwrap();
    w.write_all(chunk_type).unwrap();
    w.write_all(data).unwrap();
    w.write_u32::<byteorder::BigEndian>(crc).unwrap();
}

fn write_ihdr(w: &mut impl Write, width: u32) {
    let mut data = Vec::new();
    data.write_u32::<byteorder::BigEndian>(width).unwrap();
    data.write_u32::<byteorder::BigEndian>(width).unwrap(); // height
    data.write_u8(8).unwrap(); // bit depth = always 8-bits per channel
    data.write_u8(6).unwrap(); // color type = color + alpha
    data.write_u8(0).unwrap(); // compression method (0 is the only allowed value)
    data.write_u8(0).unwrap(); // filter method (0 is the only allowed value)
    data.write_u8(0).unwrap(); // interlace method = no interlacing
    write_chunk(w, b"IHDR", &data);
}

fn write_noncompressed_idat(w: &mut impl Write, width: u32) {
    // Generate arbitrary test pixels.
    let image_pixels = {
        let mut row = Vec::new();
        row.write_u8(0).unwrap(); // filter = no filter

        let row_pixels = (0..width)
            .map(|i| {
                let color: u8 = (i * 255 / width) as u8;
                let alpha: u8 = 0xff;
                [color, 255 - color, color / 2, alpha]
            })
            .flatten();
        row.extend(row_pixels);

        std::iter::repeat(row)
            .take(width as usize)
            .flatten()
            .collect::<Vec<_>>()
    };

    let mut zlib_data = Vec::new();
    let mut store_only_compressor =
        fdeflate::StoredOnlyCompressor::new(std::io::Cursor::new(&mut zlib_data)).unwrap();
    store_only_compressor.write_data(&image_pixels).unwrap();
    store_only_compressor.finish().unwrap();

    write_chunk(w, b"IDAT", &zlib_data);
}

fn write_iend(w: &mut impl Write) {
    write_chunk(w, b"IEND", &[]);
}
