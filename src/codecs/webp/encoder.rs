use byteorder::{LittleEndian, WriteBytesExt};

use std::io::Write;

use crate::ImageResult;

use super::vp8::encoder::BoolEncoder;


/// WebP Encoder
pub struct WebPEncoder<W: Write> {
    w: W,
    bool_encoder: BoolEncoder,
}

impl<W: Write> WebPEncoder<W> {
    /// Creates a new encoder that writes its output to w
    pub fn new(w: W) -> WebPEncoder<W> {
        WebPEncoder {
            w,
            bool_encoder: BoolEncoder::new(),
        }
    }

    /// Encodes an image
    pub fn encode(&mut self, data: &[u8], width: u16, height: u16) -> ImageResult<()> {
        
        self.write_frame_header(width, height)?;

        todo!()
    }

    fn write_frame_header(&mut self, width: u16, height: u16) -> ImageResult<()> {

        //9.1
        let keyframe = true;
        let version = 0;
        let show_frame = true;
        let first_partition_size = 100;
        
        let value = first_partition_size << 5 | (u32::from(show_frame) << 4) | version << 1 | u32::from(keyframe);
        self.w.write_u24::<LittleEndian>(value)?;

        if keyframe {

            self.w.write(&[0x9d, 0x01, 0x2a])?;

            self.w.write_u16::<LittleEndian>(width)?;
            self.w.write_u16::<LittleEndian>(height)?;


            //9.2
            let color_space = false;
            let pixel_clamping = true;

            self.bool_encoder.add_bool(color_space, 128u8);
            self.bool_encoder.add_bool(pixel_clamping, 128u8);

        }

        //9.3
        let segments_enabled = false;
        self.bool_encoder.add_bool(segments_enabled, 128u8);

        if segments_enabled {
            self.write_segment_updates();
        }

        //9.4
        let filter_type = false;
        self.bool_encoder.add_bool(filter_type, 128u8);

        let filter_level = 16u8;
        self.bool_encoder.add_literal(6, filter_level);

        let sharpness_level = 0u8;
        self.bool_encoder.add_literal(3, sharpness_level);

        //9.5
        



        Ok(())
    }


    //unfinished
    fn write_segment_updates(&mut self) {
        let segments_update_map = false;

        self.bool_encoder.add_bool(segments_update_map, 128u8);

        let update_segment_feature_data = false;
        self.bool_encoder.add_bool(update_segment_feature_data, 128u8);

        if update_segment_feature_data {

        }

        if segments_update_map {

        }
    }
}







