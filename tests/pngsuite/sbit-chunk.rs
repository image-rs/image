// For reading and opening files
use png::text_metadata::{ITXtChunk, ZTXtChunk};
use std::env;
use std::fs::File;
use std::io::BufWriter;

fn main() {
    let path = env::args()
        .nth(1)
        .expect("Expected a filename to output to.");
    let file = File::create(path).unwrap();
    let w = &mut BufWriter::new(file);

    let mut encoder = png::Encoder::new(w, 200, 100); // Width is 2 pixels and height is 1.
    encoder.set_color(png::ColorType::Indexed);
    encoder.set_depth(png::BitDepth::Eight);
    encoder.set_sbit(png::SBIT::Indexed(5, 6, 5));
    // Adding text chunks to the header
    encoder
        .add_text_chunk(
            "Testing tEXt".to_string(),
            "This is a tEXt chunk that will appear before the IDAT chunks.".to_string(),
        )
        .unwrap();
    encoder
        .add_ztxt_chunk(
            "Testing zTXt".to_string(),
            "This is a zTXt chunk that is compressed in the png file.".to_string(),
        )
        .unwrap();
    encoder
        .add_itxt_chunk(
            "Testing iTXt".to_string(),
            "iTXt chunks support all of UTF8. Example: हिंदी.".to_string(),
        )
        .unwrap();

    let mut writer = encoder.write_header().unwrap();

    let data = vec!(0; 60000); // An array containing a RGBA sequence. First pixel is red and second pixel is black.
    // data.extend(vec![3; 2500]);
    // data.extend(vec![7; 2500]);
    // data.extend(vec![15; 2500]);
    // data.extend(vec![31; 2500]);
    // data.extend(vec!(63; 2500));
    // data.extend(vec!(127; 2500));
    // data.extend(vec!(255; 2500));
    writer.write_image_data(&data).unwrap(); // Save

    // We can add a tEXt/zTXt/iTXt at any point before the encoder is dropped from scope. These chunks will be at the end of the png file.
    let tail_ztxt_chunk = ZTXtChunk::new(
        "Comment".to_string(),
        "A zTXt chunk after the image data.".to_string(),
    );
    writer.write_text_chunk(&tail_ztxt_chunk).unwrap();

    // The fields of the text chunk are public, so they can be mutated before being written to the file.
    let mut tail_itxt_chunk = ITXtChunk::new("Author".to_string(), "सायंतन खान".to_string());
    tail_itxt_chunk.compressed = true;
    tail_itxt_chunk.language_tag = "hi".to_string();
    tail_itxt_chunk.translated_keyword = "लेखक".to_string();
    writer.write_text_chunk(&tail_itxt_chunk).unwrap();
}
