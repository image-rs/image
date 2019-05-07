//! An implementation of the VP8 Video Codec
//!
//! This module contains a partial implementation of the
//! VP8 video format as defined in RFC-6386.
//!
//! It decodes Keyframes only sans Loop Filtering.
//! VP8 is the underpinning of the Webp image format
//!
//! # Related Links
//! * [rfc-6386](http://tools.ietf.org/html/rfc6386) - The VP8 Data Format and Decoding Guide
//! * [VP8.pdf](http://static.googleusercontent.com/media/research.google.com/en//pubs/archive/37073.pdf) - An overview of
//! of the VP8 format
//!

use byteorder::{LittleEndian, ReadBytesExt};
use std::default::Default;
use std::cmp;
use std::io::Read;

use super::transform;
use ::{ImageError, ImageResult};

use math::utils::clamp;

mod constants;
mod bool_coder;
mod predict;

use self::constants::*;
use self::bool_coder::BoolReader;

// Prediction mode enum
#[repr(i8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum LumaMode {
    /// Predict DC using row above and column to the left.
    DC = DC_PRED,

    /// Predict rows using row above.
    V = V_PRED,

    /// Predict columns using column to the left.
    H = H_PRED,

    /// Propagate second differences.
    TM = TM_PRED,

    /// Each Y subblock is independently predicted.
    B = B_PRED,
}

impl LumaMode {
    fn from_i8(val: i8) -> Option<Self> {
        Some(match val {
            DC_PRED => LumaMode::DC,
            V_PRED => LumaMode::V,
            H_PRED => LumaMode::H,
            TM_PRED => LumaMode::TM,
            B_PRED => LumaMode::B,
            _ => return None,
        })
    }

    fn into_intra(self) -> Option<IntraMode> {
        Some(match self {
            LumaMode::DC => IntraMode::DC,
            LumaMode::V => IntraMode::VE,
            LumaMode::H => IntraMode::HE,
            LumaMode::TM => IntraMode::TM,
            LumaMode::B => return None,
        })
    }
}

impl Default for LumaMode {
    fn default() -> Self {
        LumaMode::DC
    }
}

#[repr(i8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ChromaMode {
    /// Predict DC using row above and column to the left.
    DC = DC_PRED,

    /// Predict rows using row above.
    V = V_PRED,

    /// Predict columns using column to the left.
    H = H_PRED,

    /// Propagate second differences.
    TM = TM_PRED,
}

impl ChromaMode {
    fn from_i8(val: i8) -> Option<Self> {
        Some(match val {
            DC_PRED => ChromaMode::DC,
            V_PRED => ChromaMode::V,
            H_PRED => ChromaMode::H,
            TM_PRED => ChromaMode::TM,
            _ => return None,
        })
    }
}

impl Default for ChromaMode {
    fn default() -> Self {
        ChromaMode::DC
    }
}

#[repr(i8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum IntraMode {
    DC = B_DC_PRED,
    TM = B_TM_PRED,
    VE = B_VE_PRED,
    HE = B_HE_PRED,
    LD = B_LD_PRED,
    RD = B_RD_PRED,
    VR = B_VR_PRED,
    VL = B_VL_PRED,
    HD = B_HD_PRED,
    HU = B_HU_PRED,
}

impl IntraMode {
    fn from_i8(val: i8) -> Option<Self> {
        Some(match val {
            B_DC_PRED => IntraMode::DC,
            B_TM_PRED => IntraMode::TM,
            B_VE_PRED => IntraMode::VE,
            B_HE_PRED => IntraMode::HE,
            B_LD_PRED => IntraMode::LD,
            B_RD_PRED => IntraMode::RD,
            B_VR_PRED => IntraMode::VR,
            B_VL_PRED => IntraMode::VL,
            B_HD_PRED => IntraMode::HD,
            B_HU_PRED => IntraMode::HU,
            _ => return None,
        })
    }
}

impl Default for IntraMode {
    fn default() -> Self {
        IntraMode::DC
    }
}

type Prob = u8;

#[derive(Default, Clone, Copy)]
struct MacroBlock {
    bpred: [IntraMode; 16],
    complexity: [u8; 9],
    luma_mode: LumaMode,
    chroma_mode: ChromaMode,
    segmentid: u8,
}

/// A Representation of the last decoded video frame
#[derive(Default, Debug, Clone)]
pub struct Frame {
    /// The width of the luma plane
    pub width: u16,

    /// The height of the luma plane
    pub height: u16,

    /// The luma plane of the frame
    pub ybuf: Vec<u8>,

    pub ubuf: Vec<u8>,

    pub vbuf: Vec<u8>,

    /// Indicates whether this frame is a keyframe
    pub keyframe: bool,

    version: u8,

    /// Indicates whether this frame is intended for display
    pub for_display: bool,

    // Section 9.2
    /// The pixel type of the frame as defined by Section 9.2
    /// of the VP8 Specification
    pub pixel_type: u8,

    // Section 9.4 and 15
    filter: u8,
    filter_level: u8,
    sharpness_level: u8,
}

#[derive(Clone, Copy, Default)]
struct Segment {
    ydc: i16,
    yac: i16,

    y2dc: i16,
    y2ac: i16,

    uvdc: i16,
    uvac: i16,

    delta_values: bool,

    quantizer_level: i8,
    loopfilter_level: i8,
}

/// VP8 Decoder
///
/// Only decodes keyframes
pub struct VP8Decoder<R> {
    r: R,
    b: BoolReader,

    mbwidth: u16,
    mbheight: u16,

    frame: Frame,

    segments_enabled: bool,
    segments_update_map: bool,
    segment: [Segment; MAX_SEGMENTS],

    partitions: [BoolReader; 8],
    num_partitions: u8,

    segment_tree_probs: [Prob; 3],
    token_probs: Box<TokenProbTables>,

    // Section 9.10
    prob_intra: Prob,

    // Section 9.11
    prob_skip_false: Option<Prob>,

    top: Vec<MacroBlock>,
    left: MacroBlock,

    top_border: [Vec<u8>; 3],
    left_border: [Vec<u8>; 3],
}

impl<R: Read> VP8Decoder<R> {
    /// Create a new decoder.
    /// The reader must present a raw vp8 bitstream to the decoder
    pub fn new(r: R) -> VP8Decoder<R> {
        let f = Frame::default();
        let s = Segment::default();
        let m = MacroBlock::default();

        VP8Decoder {
            r,
            b: BoolReader::new(),

            mbwidth: 0,
            mbheight: 0,

            frame: f,
            segments_enabled: false,
            segments_update_map: false,
            segment: [s; MAX_SEGMENTS],

            partitions: [
                BoolReader::new(),
                BoolReader::new(),
                BoolReader::new(),
                BoolReader::new(),
                BoolReader::new(),
                BoolReader::new(),
                BoolReader::new(),
                BoolReader::new(),
            ],

            num_partitions: 1,

            segment_tree_probs: [255u8; 3],
            token_probs: Box::new(COEFF_PROBS),

            // Section 9.10
            prob_intra: 0u8,

            // Section 9.11
            prob_skip_false: None,

            top: Vec::new(),
            left: m,

            top_border: [Vec::new(), Vec::new(), Vec::new()],
            left_border: [Vec::new(), Vec::new(), Vec::new()],
        }
    }

    fn update_token_probabilities(&mut self) {
        for (i, is) in COEFF_UPDATE_PROBS.iter().enumerate() {
            for (j, js) in is.iter().enumerate() {
                for (k, ks) in js.iter().enumerate() {
                    for (t, prob) in ks.iter().enumerate().take(NUM_DCT_TOKENS - 1) {
                        if self.b.read_bool(*prob) {
                            let v = self.b.read_literal(8);
                            self.token_probs[i][j][k][t] = v;
                        }
                    }
                }
            }
        }
    }

    fn init_partitions(&mut self, n: usize) -> ImageResult<()> {
        if n > 1 {
            let mut sizes = vec![0; 3 * n - 3];
            self.r.read_exact(sizes.as_mut_slice())?;

            for (i, s) in sizes.chunks(3).enumerate() {
                let size = {s}.read_u24::<LittleEndian>()
                    .expect("Reading from &[u8] can't fail and the chunk is complete");

                let mut buf = vec![0; size as usize];
                self.r.read_exact(buf.as_mut_slice())?;

                self.partitions[i].init(buf)?;
            }
        }

        let mut buf = Vec::new();
        self.r.read_to_end(&mut buf)?;
        self.partitions[n - 1].init(buf)?;

        Ok(())
    }

    fn read_quantization_indices(&mut self) {
        fn dc_quant(index: i32) -> i16 {
            DC_QUANT[clamp(index, 0, 127) as usize]
        }

        fn ac_quant(index: i32) -> i16 {
            AC_QUANT[clamp(index, 0, 127) as usize]
        }

        let yac_abs = self.b.read_literal(7);
        let ydc_delta = if self.b.read_flag() {
            self.b.read_magnitude_and_sign(4)
        } else {
            0
        };

        let y2dc_delta = if self.b.read_flag() {
            self.b.read_magnitude_and_sign(4)
        } else {
            0
        };

        let y2ac_delta = if self.b.read_flag() {
            self.b.read_magnitude_and_sign(4)
        } else {
            0
        };

        let uvdc_delta = if self.b.read_flag() {
            self.b.read_magnitude_and_sign(4)
        } else {
            0
        };

        let uvac_delta = if self.b.read_flag() {
            self.b.read_magnitude_and_sign(4)
        } else {
            0
        };

        let n = if self.segments_enabled {
            MAX_SEGMENTS
        } else {
            1
        };
        for i in 0usize..n {
            let base = i32::from(if !self.segment[i].delta_values {
                i16::from(self.segment[i].quantizer_level)
            } else {
                i16::from(self.segment[i].quantizer_level) + i16::from(yac_abs)
            });

            self.segment[i].ydc = dc_quant(base + ydc_delta);
            self.segment[i].yac = ac_quant(base);

            self.segment[i].y2dc = dc_quant(base + y2dc_delta) * 2;
            // The intermediate result (max`284*155`) can be larger than the `i16` range.
            self.segment[i].y2ac = (i32::from(ac_quant(base + y2ac_delta)) * 155 / 100) as i16;

            self.segment[i].uvdc = dc_quant(base + uvdc_delta);
            self.segment[i].uvac = ac_quant(base + uvac_delta);

            if self.segment[i].y2ac < 8 {
                self.segment[i].y2ac = 8;
            }

            if self.segment[i].uvdc > 132 {
                self.segment[i].uvdc = 132;
            }
        }
    }

    fn read_loop_filter_adjustments(&mut self) {
        if self.b.read_flag() {
            for _i in 0usize..4 {
                let ref_frame_delta_update_flag = self.b.read_flag();

                let _delta = if ref_frame_delta_update_flag {
                    self.b.read_magnitude_and_sign(6)
                } else {
                    0i32
                };
            }

            for _i in 0usize..4 {
                let mb_mode_delta_update_flag = self.b.read_flag();

                let _delta = if mb_mode_delta_update_flag {
                    self.b.read_magnitude_and_sign(6)
                } else {
                    0i32
                };
            }
        }
    }

    fn read_segment_updates(&mut self) {
        // Section 9.3
        self.segments_update_map = self.b.read_flag();
        let update_segment_feature_data = self.b.read_flag();

        if update_segment_feature_data {
            let segment_feature_mode = self.b.read_flag();

            for i in 0usize..MAX_SEGMENTS {
                self.segment[i].delta_values = !segment_feature_mode;
            }

            for i in 0usize..MAX_SEGMENTS {
                let update = self.b.read_flag();

                self.segment[i].quantizer_level = if update {
                    self.b.read_magnitude_and_sign(7)
                } else {
                    0i32
                } as i8;
            }

            for i in 0usize..MAX_SEGMENTS {
                let update = self.b.read_flag();

                self.segment[i].loopfilter_level = if update {
                    self.b.read_magnitude_and_sign(6)
                } else {
                    0i32
                } as i8;
            }
        }

        if self.segments_update_map {
            for i in 0usize..3 {
                let update = self.b.read_flag();

                self.segment_tree_probs[i] = if update { self.b.read_literal(8) } else { 255 };
            }
        }
    }

    fn read_frame_header(&mut self) -> ImageResult<()> {
        let tag = self.r.read_u24::<LittleEndian>()?;

        self.frame.keyframe = tag & 1 == 0;
        self.frame.version = ((tag >> 1) & 7) as u8;
        self.frame.for_display = (tag >> 4) & 1 != 0;

        let first_partition_size = tag >> 5;

        if self.frame.keyframe {
            let mut tag = [0u8; 3];
            self.r.read_exact(&mut tag)?;

            if tag != [0x9d, 0x01, 0x2a] {
                return Err(ImageError::FormatError(
                    format!("Invalid magic bytes {:?} for vp8", tag)))
            }

            let w = self.r.read_u16::<LittleEndian>()?;
            let h = self.r.read_u16::<LittleEndian>()?;

            self.frame.width = w & 0x3FFF;
            self.frame.height = h & 0x3FFF;

            self.top = init_top_macroblocks(self.frame.width as usize);
            // Almost always the first macro block, except when non exists (i.e. `width == 0`)
            self.left = self.top.get(0).cloned()
                .unwrap_or_else(MacroBlock::default);

            self.mbwidth = (self.frame.width + 15) / 16;
            self.mbheight = (self.frame.height + 15) / 16;

            self.frame.ybuf = vec![0u8; self.frame.width as usize * self.frame.height as usize];
            self.frame.ubuf = vec![0u8; self.frame.width as usize * self.frame.height as usize];
            self.frame.vbuf = vec![0u8; self.frame.width as usize * self.frame.height as usize];

            self.top_border = [
                vec![127u8; self.frame.width as usize + 4 + 16],
                vec![127u8; self.frame.width as usize / 2 + 4 + 8],
                vec![127u8; self.frame.width as usize / 2 + 4 + 8],
            ];

            self.left_border = [
                vec![129u8; 1 + 16],
                vec![129u8; 1 + 8],
                vec![129u8; 1 + 8],
            ];
        }

        let mut buf = vec![0; first_partition_size as usize];
        self.r.read_exact(&mut buf)?;

        // initialise binary decoder
        self.b.init(buf)?;

        if self.frame.keyframe {
            let color_space = self.b.read_literal(1);
            self.frame.pixel_type = self.b.read_literal(1);

            if color_space != 0 {
                return Err(ImageError::FormatError(
                    format!("Only YUV color space is specified.")))
            }
        }

        self.segments_enabled = self.b.read_flag();
        if self.segments_enabled {
            self.read_segment_updates();
        }

        self.frame.filter = self.b.read_literal(1);
        self.frame.filter_level = self.b.read_literal(6);
        self.frame.sharpness_level = self.b.read_literal(3);

        let lf_adjust_enable = self.b.read_flag();
        if lf_adjust_enable {
            self.read_loop_filter_adjustments();
        }

        self.num_partitions = (1usize << self.b.read_literal(2) as usize) as u8;
        let num_partitions = self.num_partitions as usize;
        self.init_partitions(num_partitions)?;

        self.read_quantization_indices();

        if !self.frame.keyframe {
            // 9.7 refresh golden frame and altref frame
            return Err(ImageError::UnsupportedError(
                "Frames that are not keyframes are not supported".into()))
            // FIXME: support this?
        } else {
            // Refresh entropy probs ?????
            let _ = self.b.read_literal(1);
        }

        self.update_token_probabilities();

        let mb_no_skip_coeff = self.b.read_literal(1);
        self.prob_skip_false = if mb_no_skip_coeff == 1 {
            Some(self.b.read_literal(8))
        } else {
            None
        };

        if !self.frame.keyframe {
            // 9.10 remaining frame data
            self.prob_intra = 0;

            return Err(ImageError::UnsupportedError(
                "Frames that are not keyframes are not supported".into()))
            // FIXME: support this?
        } else {
            // Reset motion vectors
        }

        Ok(())
    }

    fn read_macroblock_header(&mut self, mbx: usize) -> ImageResult<(bool, MacroBlock)> {
        let mut mb = MacroBlock::default();

        mb.segmentid = if self.segments_enabled && self.segments_update_map {
            self.b
                .read_with_tree(&SEGMENT_ID_TREE, &self.segment_tree_probs, 0) as u8
        } else {
            0
        };

        let skip_coeff = if self.prob_skip_false.is_some() {
            self.b.read_bool(*self.prob_skip_false.as_ref().unwrap())
        } else {
            false
        };

        let inter_predicted = if !self.frame.keyframe {
            self.b.read_bool(self.prob_intra)
        } else {
            false
        };

        if inter_predicted {
            return Err(ImageError::UnsupportedError(
                "VP8 inter prediction is not implemented yet".into()));
        }

        if self.frame.keyframe {
            // intra prediction
            let luma = self.b
                .read_with_tree(&KEYFRAME_YMODE_TREE, &KEYFRAME_YMODE_PROBS, 0);
            mb.luma_mode = LumaMode::from_i8(luma)
                .ok_or_else(|| ImageError::FormatError(
                    format!("Invalid luma prediction mode {}", luma))
                )?;

            match mb.luma_mode.into_intra() {
                // `LumaMode::B` - This is predicted individually
                None => {
                    for y in 0usize..4 {
                        for x in 0usize..4 {
                            let top = self.top[mbx].bpred[12 + x];
                            let left = self.left.bpred[y];
                            let intra = self.b.read_with_tree(
                                &KEYFRAME_BPRED_MODE_TREE,
                                &KEYFRAME_BPRED_MODE_PROBS[top as usize][left as usize],
                                0,
                            );
                            let bmode = IntraMode::from_i8(intra)
                                .ok_or_else(|| ImageError::FormatError(
                                    format!("Invalid intra prediction mode {}", intra))
                                )?;
                            mb.bpred[x + y * 4] = bmode;

                            self.top[mbx].bpred[12 + x] = bmode;
                            self.left.bpred[y] = bmode;
                        }
                    }
                },
                Some(mode) =>  {
                    for i in 0usize..4 {
                        mb.bpred[12 + i] = mode;
                        self.left.bpred[i] = mode;
                    }
                }
            }

            let chroma = self.b
                .read_with_tree(&KEYFRAME_UV_MODE_TREE, &KEYFRAME_UV_MODE_PROBS, 0);
            mb.chroma_mode = ChromaMode::from_i8(chroma)
                .ok_or_else(|| ImageError::FormatError(
                    format!("Invalid chroma prediction mode {}", chroma))
                )?;
        }

        self.top[mbx].chroma_mode = mb.chroma_mode;
        self.top[mbx].luma_mode = mb.luma_mode;
        self.top[mbx].bpred = mb.bpred;

        Ok((skip_coeff, mb))
    }

    fn intra_predict(&mut self, mbx: usize, mby: usize, mb: &MacroBlock, resdata: &[i32]) {
        let stride = 1usize + 16 + 4;
        let w = self.frame.width as usize;
        let mw = self.mbwidth as usize;
        let mut ws = create_border(mbx, mby, mw, &self.top_border[0], &self.left_border[0]);

        match mb.luma_mode {
            LumaMode::V => predict::vpred(&mut ws, 16, 1, 1, stride),
            LumaMode::H => predict::hpred(&mut ws, 16, 1, 1, stride),
            LumaMode::TM => predict::tmpred(&mut ws, 16, 1, 1, stride),
            LumaMode::DC => predict::dcpred(&mut ws, 16, stride, mby != 0, mbx != 0),
            LumaMode::B => predict::pred4x4(&mut ws, stride, &mb.bpred, resdata),
        }

        if mb.luma_mode != LumaMode::B {
            for y in 0usize..4 {
                for x in 0usize..4 {
                    let i = x + y * 4;
                    let rb = &resdata[i * 16..i * 16 + 16];
                    let y0 = 1 + y * 4;
                    let x0 = 1 + x * 4;

                    predict::add_residue(&mut ws, rb, y0, x0, stride);
                }
            }
        }

        self.left_border[0][0] = ws[16];

        for i in 0usize..16 {
            self.top_border[0][mbx * 16 + i] = ws[16 * stride + 1 + i];
            self.left_border[0][i + 1] = ws[(i + 1) * stride + 16];
        }


        let mut v_x;
        {
            let v_a = &self.top_border[1][mbx*8..mbx*8+8];
            let v_l = &self.left_border[1][1..9];
            let v_p = self.left_border[1][0];

            v_x = match mb.chroma_mode {
                ChromaMode::V => predict::chroma_v(v_a),
                ChromaMode::H => predict::chroma_h(v_l),
                ChromaMode::DC => predict::chroma_dc(v_a, v_l, mby != 0, mbx != 0),
                ChromaMode::TM => predict::chroma_tm(v_a, v_l, v_p),
            };

            for y0 in 0..2 {
                for x0 in 0..2 {
                    let i = x0 + y0 * 2 + 16;
                    let rb = &resdata[i * 16..i * 16 + 16];
        
                    predict::add_residue_chroma(&mut v_x, rb, y0*4, x0*4);
                }
            }
        }

        self.left_border[1][0] = self.top_border[1][mbx*8+7];
        for i in 0..8 {
            self.top_border[1][mbx*8+i] = v_x[7][i];
            self.left_border[1][1 + i] = v_x[i][7];
        }

        let mut u_x;
        {
            let u_a = &self.top_border[2][mbx*8..mbx*8+8];
            let u_l = &self.left_border[2][1..9];
            let u_p = self.left_border[2][0];

            u_x = match mb.chroma_mode {
                ChromaMode::V => predict::chroma_v(u_a),
                ChromaMode::H => predict::chroma_h(u_l),
                ChromaMode::DC => predict::chroma_dc(u_a, u_l, mby != 0, mbx != 0),
                ChromaMode::TM => predict::chroma_tm(u_a, u_l, u_p),
            };

            for y0 in 0..2 {
                for x0 in 0..2 {
                    let i = x0 + y0 * 2 + 20;
                    let rb = &resdata[i * 16..i * 16 + 16];
        
                    predict::add_residue_chroma(&mut u_x, rb, y0*4, x0*4);
                }
            }
        }

        self.left_border[2][0] = self.top_border[2][mbx*8+7];
        for i in 0..8 {
            self.top_border[2][mbx*8+i] = u_x[7][i];
            self.left_border[2][1 + i] = u_x[i][7];
        }

        // Length is the remainder to the border, but maximally the current chunk.
        let ylength = cmp::min(self.frame.height as usize - mby*16, 16);
        let xlength = cmp::min(self.frame.width as usize - mbx*16, 16);

        for y in 0usize..ylength {
            for x in 0usize..xlength {
                self.frame.ybuf[(mby * 16 + y) * w + mbx * 16 + x] = ws[(1 + y) * stride + 1 + x];
                self.frame.ubuf[(mby * 16 + y) * w + mbx * 16 + x] = u_x[y/2][x/2];
                self.frame.vbuf[(mby * 16 + y) * w + mbx * 16 + x] = v_x[y/2][x/2];
            }
        }
    }

    fn read_coefficients(
        &mut self,
        block: &mut [i32],
        p: usize,
        plane: usize,
        complexity: usize,
        dcq: i16,
        acq: i16,
    ) -> bool {
        let first = if plane == 0 { 1usize } else { 0usize };
        let probs = &self.token_probs[plane];
        let tree = &DCT_TOKEN_TREE;

        let mut complexity = complexity;
        let mut has_coefficients = false;
        let mut skip = false;

        for i in first..16usize {
            let table = &probs[COEFF_BANDS[i] as usize][complexity];

            let token = if !skip {
                self.partitions[p].read_with_tree(tree, table, 0)
            } else {
                self.partitions[p].read_with_tree(tree, table, 2)
            };

            let mut abs_value = i32::from(match token {
                DCT_EOB => break,

                DCT_0 => {
                    skip = true;
                    has_coefficients = true;
                    complexity = 0;
                    continue;
                }

                literal @ DCT_1...DCT_4 => i16::from(literal),

                category @ DCT_CAT1...DCT_CAT6 => {
                    let t = PROB_DCT_CAT[(category - DCT_CAT1) as usize];

                    let mut extra = 0i16;
                    let mut j = 0;

                    while t[j] > 0 {
                        extra = extra + extra + self.partitions[p].read_bool(t[j]) as i16;
                        j += 1;
                    }

                    i16::from(DCT_CAT_BASE[(category - DCT_CAT1) as usize]) + extra
                }

                c => panic!(format!("unknown token: {}", c)),
            });

            skip = false;

            complexity = if abs_value == 0 {
                0
            } else if abs_value == 1 {
                1
            } else {
                2
            };

            if self.partitions[p].read_bool(128) {
                abs_value = -abs_value;
            }

            block[ZIGZAG[i] as usize] =
                abs_value * i32::from(if ZIGZAG[i] > 0 { acq } else { dcq });

            has_coefficients = true;
        }

        has_coefficients
    }

    fn read_residual_data(&mut self, mb: &MacroBlock, mbx: usize, p: usize) -> [i32; 384] {
        let sindex = mb.segmentid as usize;
        let mut blocks = [0i32; 384];
        let mut plane = if mb.luma_mode == LumaMode::B { 3 } else { 1 };

        if plane == 1 {
            let complexity = self.top[mbx].complexity[0] + self.left.complexity[0];
            let mut block = [0i32; 16];
            let dcq = self.segment[sindex].y2dc;
            let acq = self.segment[sindex].y2ac;
            let n = self.read_coefficients(&mut block, p, plane, complexity as usize, dcq, acq);

            self.left.complexity[0] = if n { 1 } else { 0 };
            self.top[mbx].complexity[0] = if n { 1 } else { 0 };

            transform::iwht4x4(&mut block);

            for k in 0usize..16 {
                blocks[16 * k] = block[k];
            }

            plane = 0;
        }

        for y in 0usize..4 {
            let mut left = self.left.complexity[y + 1];
            for x in 0usize..4 {
                let i = x + y * 4;
                let block = &mut blocks[i * 16..i * 16 + 16];

                let complexity = self.top[mbx].complexity[x + 1] + left;
                let dcq = self.segment[sindex].ydc;
                let acq = self.segment[sindex].yac;

                let n = self.read_coefficients(block, p, plane, complexity as usize, dcq, acq);

                if block[0] != 0 || n {
                    transform::idct4x4(block);
                }

                left = if n { 1 } else { 0 };
                self.top[mbx].complexity[x + 1] = if n { 1 } else { 0 };
            }

            self.left.complexity[y + 1] = left;
        }

        plane = 2;

        for &j in &[5usize, 7usize] {
            for y in 0usize..2 {
                let mut left = self.left.complexity[y + j];

                for x in 0usize..2 {
                    let i = x + y * 2 + if j == 5 { 16 } else { 20 };
                    let block = &mut blocks[i * 16..i * 16 + 16];

                    let complexity = self.top[mbx].complexity[x + j] + left;
                    let dcq = self.segment[sindex].uvdc;
                    let acq = self.segment[sindex].uvac;

                    let n = self.read_coefficients(block, p, plane, complexity as usize, dcq, acq);
                    if block[0] != 0 || n {
                        transform::idct4x4(block);
                    }

                    left = if n { 1 } else { 0 };
                    self.top[mbx].complexity[x + j] = if n { 1 } else { 0 };
                }

                self.left.complexity[y + j] = left;
            }
        }

        blocks
    }

    /// Decodes the current frame and returns a reference to it
    pub fn decode_frame(&mut self) -> ImageResult<&Frame> {
        self.read_frame_header()?;

        for mby in 0..self.mbheight as usize {
            let p = mby % self.num_partitions as usize;
            self.left = MacroBlock::default();

            for mbx in 0..self.mbwidth as usize {
                let (skip, mb) = self.read_macroblock_header(mbx)?;
                let blocks = if !skip {
                    self.read_residual_data(&mb, mbx, p)
                } else {
                    if mb.luma_mode != LumaMode::B {
                        self.left.complexity[0] = 0;
                        self.top[mbx].complexity[0] = 0;
                    }

                    for i in 1usize..9 {
                        self.left.complexity[i] = 0;
                        self.top[mbx].complexity[i] = 0;
                    }

                    [0i32; 384]
                };

                self.intra_predict(mbx, mby, &mb, &blocks);
            }

            self.left_border = [
                vec![129u8; 1 + 16],
                vec![129u8; 1 + 8],
                vec![129u8; 1 + 8],
            ];
        }

        Ok(&self.frame)
    }
}


fn init_top_macroblocks(width: usize) -> Vec<MacroBlock> {
    let mb_width = (width + 15) / 16;

    let mb = MacroBlock {
        // Section 11.3 #3
        bpred: [IntraMode::DC; 16],
        luma_mode: LumaMode::DC,
        .. MacroBlock::default()
    };

    vec![mb; mb_width]
}

fn create_border(mbx: usize, mby: usize, mbw: usize, top: &[u8], left: &[u8]) -> [u8; 357] {
    let stride = 1usize + 16 + 4;
    let mut ws = [0u8; (1 + 16) * (1 + 16 + 4)];

    // A
    {
        let above = &mut ws[1..stride];
        if mby == 0 {
            for above in above.iter_mut() {
                *above = 127;
            }
        } else {
            for i in 0usize..16 {
                above[i] = top[mbx * 16 + i];
            }

            if mbx == mbw - 1 {
                for above in above.iter_mut().skip(16) {
                    *above = top[mbx * 16 + 15];
                }
            } else {
                for i in 16usize..above.len() {
                    above[i] = top[mbx * 16 + i];
                }
            }
        }
    }

    for i in 17usize..stride {
        ws[4 * stride + i] = ws[i];
        ws[8 * stride + i] = ws[i];
        ws[12 * stride + i] = ws[i];
    }

    // L
    if mbx == 0 {
        for i in 0usize..16 {
            ws[(i + 1) * stride] = 129;
        }
    } else {
        for i in 0usize..16 {
            ws[(i + 1) * stride] = left[i + 1];
        }
    }

    // P
    ws[0] = if mby == 0 {
        127
    } else if mbx == 0 {
        129
    } else {
        left[0]
    };

    ws
}

