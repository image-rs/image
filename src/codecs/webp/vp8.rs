//! An implementation of the VP8 Video Codec
//!
//! This module contains a partial implementation of the
//! VP8 video format as defined in RFC-6386.
//!
//! It decodes Keyframes only.
//! VP8 is the underpinning of the WebP image format
//!
//! # Related Links
//! * [rfc-6386](http://tools.ietf.org/html/rfc6386) - The VP8 Data Format and Decoding Guide
//! * [VP8.pdf](http://static.googleusercontent.com/media/research.google.com/en//pubs/archive/37073.pdf) - An overview of
//! of the VP8 format
//!

use byteorder::{LittleEndian, ReadBytesExt};
use std::convert::TryInto;
use std::default::Default;
use std::io::Read;
use std::{cmp, error, fmt};

use super::loop_filter;
use super::transform;
use crate::error::{
    DecodingError, ImageError, ImageResult, UnsupportedError, UnsupportedErrorKind,
};
use crate::image::ImageFormat;

use crate::utils::clamp;

const MAX_SEGMENTS: usize = 4;
const NUM_DCT_TOKENS: usize = 12;

// Prediction modes
const DC_PRED: i8 = 0;
const V_PRED: i8 = 1;
const H_PRED: i8 = 2;
const TM_PRED: i8 = 3;
const B_PRED: i8 = 4;

const B_DC_PRED: i8 = 0;
const B_TM_PRED: i8 = 1;
const B_VE_PRED: i8 = 2;
const B_HE_PRED: i8 = 3;
const B_LD_PRED: i8 = 4;
const B_RD_PRED: i8 = 5;
const B_VR_PRED: i8 = 6;
const B_VL_PRED: i8 = 7;
const B_HD_PRED: i8 = 8;
const B_HU_PRED: i8 = 9;

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

type Prob = u8;

static SEGMENT_ID_TREE: [i8; 6] = [2, 4, -0, -1, -2, -3];

// Section 11.2
// Tree for determining the keyframe luma intra prediction modes:
static KEYFRAME_YMODE_TREE: [i8; 8] = [-B_PRED, 2, 4, 6, -DC_PRED, -V_PRED, -H_PRED, -TM_PRED];

// Default probabilities for decoding the keyframe luma modes
static KEYFRAME_YMODE_PROBS: [Prob; 4] = [145, 156, 163, 128];

// Tree for determining the keyframe B_PRED mode:
static KEYFRAME_BPRED_MODE_TREE: [i8; 18] = [
    -B_DC_PRED, 2, -B_TM_PRED, 4, -B_VE_PRED, 6, 8, 12, -B_HE_PRED, 10, -B_RD_PRED, -B_VR_PRED,
    -B_LD_PRED, 14, -B_VL_PRED, 16, -B_HD_PRED, -B_HU_PRED,
];

// Probabilities for the BPRED_MODE_TREE
static KEYFRAME_BPRED_MODE_PROBS: [[[u8; 9]; 10]; 10] = [
    [
        [231, 120, 48, 89, 115, 113, 120, 152, 112],
        [152, 179, 64, 126, 170, 118, 46, 70, 95],
        [175, 69, 143, 80, 85, 82, 72, 155, 103],
        [56, 58, 10, 171, 218, 189, 17, 13, 152],
        [144, 71, 10, 38, 171, 213, 144, 34, 26],
        [114, 26, 17, 163, 44, 195, 21, 10, 173],
        [121, 24, 80, 195, 26, 62, 44, 64, 85],
        [170, 46, 55, 19, 136, 160, 33, 206, 71],
        [63, 20, 8, 114, 114, 208, 12, 9, 226],
        [81, 40, 11, 96, 182, 84, 29, 16, 36],
    ],
    [
        [134, 183, 89, 137, 98, 101, 106, 165, 148],
        [72, 187, 100, 130, 157, 111, 32, 75, 80],
        [66, 102, 167, 99, 74, 62, 40, 234, 128],
        [41, 53, 9, 178, 241, 141, 26, 8, 107],
        [104, 79, 12, 27, 217, 255, 87, 17, 7],
        [74, 43, 26, 146, 73, 166, 49, 23, 157],
        [65, 38, 105, 160, 51, 52, 31, 115, 128],
        [87, 68, 71, 44, 114, 51, 15, 186, 23],
        [47, 41, 14, 110, 182, 183, 21, 17, 194],
        [66, 45, 25, 102, 197, 189, 23, 18, 22],
    ],
    [
        [88, 88, 147, 150, 42, 46, 45, 196, 205],
        [43, 97, 183, 117, 85, 38, 35, 179, 61],
        [39, 53, 200, 87, 26, 21, 43, 232, 171],
        [56, 34, 51, 104, 114, 102, 29, 93, 77],
        [107, 54, 32, 26, 51, 1, 81, 43, 31],
        [39, 28, 85, 171, 58, 165, 90, 98, 64],
        [34, 22, 116, 206, 23, 34, 43, 166, 73],
        [68, 25, 106, 22, 64, 171, 36, 225, 114],
        [34, 19, 21, 102, 132, 188, 16, 76, 124],
        [62, 18, 78, 95, 85, 57, 50, 48, 51],
    ],
    [
        [193, 101, 35, 159, 215, 111, 89, 46, 111],
        [60, 148, 31, 172, 219, 228, 21, 18, 111],
        [112, 113, 77, 85, 179, 255, 38, 120, 114],
        [40, 42, 1, 196, 245, 209, 10, 25, 109],
        [100, 80, 8, 43, 154, 1, 51, 26, 71],
        [88, 43, 29, 140, 166, 213, 37, 43, 154],
        [61, 63, 30, 155, 67, 45, 68, 1, 209],
        [142, 78, 78, 16, 255, 128, 34, 197, 171],
        [41, 40, 5, 102, 211, 183, 4, 1, 221],
        [51, 50, 17, 168, 209, 192, 23, 25, 82],
    ],
    [
        [125, 98, 42, 88, 104, 85, 117, 175, 82],
        [95, 84, 53, 89, 128, 100, 113, 101, 45],
        [75, 79, 123, 47, 51, 128, 81, 171, 1],
        [57, 17, 5, 71, 102, 57, 53, 41, 49],
        [115, 21, 2, 10, 102, 255, 166, 23, 6],
        [38, 33, 13, 121, 57, 73, 26, 1, 85],
        [41, 10, 67, 138, 77, 110, 90, 47, 114],
        [101, 29, 16, 10, 85, 128, 101, 196, 26],
        [57, 18, 10, 102, 102, 213, 34, 20, 43],
        [117, 20, 15, 36, 163, 128, 68, 1, 26],
    ],
    [
        [138, 31, 36, 171, 27, 166, 38, 44, 229],
        [67, 87, 58, 169, 82, 115, 26, 59, 179],
        [63, 59, 90, 180, 59, 166, 93, 73, 154],
        [40, 40, 21, 116, 143, 209, 34, 39, 175],
        [57, 46, 22, 24, 128, 1, 54, 17, 37],
        [47, 15, 16, 183, 34, 223, 49, 45, 183],
        [46, 17, 33, 183, 6, 98, 15, 32, 183],
        [65, 32, 73, 115, 28, 128, 23, 128, 205],
        [40, 3, 9, 115, 51, 192, 18, 6, 223],
        [87, 37, 9, 115, 59, 77, 64, 21, 47],
    ],
    [
        [104, 55, 44, 218, 9, 54, 53, 130, 226],
        [64, 90, 70, 205, 40, 41, 23, 26, 57],
        [54, 57, 112, 184, 5, 41, 38, 166, 213],
        [30, 34, 26, 133, 152, 116, 10, 32, 134],
        [75, 32, 12, 51, 192, 255, 160, 43, 51],
        [39, 19, 53, 221, 26, 114, 32, 73, 255],
        [31, 9, 65, 234, 2, 15, 1, 118, 73],
        [88, 31, 35, 67, 102, 85, 55, 186, 85],
        [56, 21, 23, 111, 59, 205, 45, 37, 192],
        [55, 38, 70, 124, 73, 102, 1, 34, 98],
    ],
    [
        [102, 61, 71, 37, 34, 53, 31, 243, 192],
        [69, 60, 71, 38, 73, 119, 28, 222, 37],
        [68, 45, 128, 34, 1, 47, 11, 245, 171],
        [62, 17, 19, 70, 146, 85, 55, 62, 70],
        [75, 15, 9, 9, 64, 255, 184, 119, 16],
        [37, 43, 37, 154, 100, 163, 85, 160, 1],
        [63, 9, 92, 136, 28, 64, 32, 201, 85],
        [86, 6, 28, 5, 64, 255, 25, 248, 1],
        [56, 8, 17, 132, 137, 255, 55, 116, 128],
        [58, 15, 20, 82, 135, 57, 26, 121, 40],
    ],
    [
        [164, 50, 31, 137, 154, 133, 25, 35, 218],
        [51, 103, 44, 131, 131, 123, 31, 6, 158],
        [86, 40, 64, 135, 148, 224, 45, 183, 128],
        [22, 26, 17, 131, 240, 154, 14, 1, 209],
        [83, 12, 13, 54, 192, 255, 68, 47, 28],
        [45, 16, 21, 91, 64, 222, 7, 1, 197],
        [56, 21, 39, 155, 60, 138, 23, 102, 213],
        [85, 26, 85, 85, 128, 128, 32, 146, 171],
        [18, 11, 7, 63, 144, 171, 4, 4, 246],
        [35, 27, 10, 146, 174, 171, 12, 26, 128],
    ],
    [
        [190, 80, 35, 99, 180, 80, 126, 54, 45],
        [85, 126, 47, 87, 176, 51, 41, 20, 32],
        [101, 75, 128, 139, 118, 146, 116, 128, 85],
        [56, 41, 15, 176, 236, 85, 37, 9, 62],
        [146, 36, 19, 30, 171, 255, 97, 27, 20],
        [71, 30, 17, 119, 118, 255, 17, 18, 138],
        [101, 38, 60, 138, 55, 70, 43, 26, 142],
        [138, 45, 61, 62, 219, 1, 81, 188, 64],
        [32, 41, 20, 117, 151, 142, 20, 21, 163],
        [112, 19, 12, 61, 195, 128, 48, 4, 24],
    ],
];

// Section 11.4 Tree for determining macroblock the chroma mode
static KEYFRAME_UV_MODE_TREE: [i8; 6] = [-DC_PRED, 2, -V_PRED, 4, -H_PRED, -TM_PRED];

// Probabilities for determining macroblock mode
static KEYFRAME_UV_MODE_PROBS: [Prob; 3] = [142, 114, 183];

// Section 13.4
type TokenProbTables = [[[[Prob; NUM_DCT_TOKENS - 1]; 3]; 8]; 4];

// Probabilities that a token's probability will be updated
static COEFF_UPDATE_PROBS: TokenProbTables = [
    [
        [
            [255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255],
            [255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255],
            [255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255],
        ],
        [
            [176, 246, 255, 255, 255, 255, 255, 255, 255, 255, 255],
            [223, 241, 252, 255, 255, 255, 255, 255, 255, 255, 255],
            [249, 253, 253, 255, 255, 255, 255, 255, 255, 255, 255],
        ],
        [
            [255, 244, 252, 255, 255, 255, 255, 255, 255, 255, 255],
            [234, 254, 254, 255, 255, 255, 255, 255, 255, 255, 255],
            [253, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255],
        ],
        [
            [255, 246, 254, 255, 255, 255, 255, 255, 255, 255, 255],
            [239, 253, 254, 255, 255, 255, 255, 255, 255, 255, 255],
            [254, 255, 254, 255, 255, 255, 255, 255, 255, 255, 255],
        ],
        [
            [255, 248, 254, 255, 255, 255, 255, 255, 255, 255, 255],
            [251, 255, 254, 255, 255, 255, 255, 255, 255, 255, 255],
            [255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255],
        ],
        [
            [255, 253, 254, 255, 255, 255, 255, 255, 255, 255, 255],
            [251, 254, 254, 255, 255, 255, 255, 255, 255, 255, 255],
            [254, 255, 254, 255, 255, 255, 255, 255, 255, 255, 255],
        ],
        [
            [255, 254, 253, 255, 254, 255, 255, 255, 255, 255, 255],
            [250, 255, 254, 255, 254, 255, 255, 255, 255, 255, 255],
            [254, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255],
        ],
        [
            [255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255],
            [255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255],
            [255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255],
        ],
    ],
    [
        [
            [217, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255],
            [225, 252, 241, 253, 255, 255, 254, 255, 255, 255, 255],
            [234, 250, 241, 250, 253, 255, 253, 254, 255, 255, 255],
        ],
        [
            [255, 254, 255, 255, 255, 255, 255, 255, 255, 255, 255],
            [223, 254, 254, 255, 255, 255, 255, 255, 255, 255, 255],
            [238, 253, 254, 254, 255, 255, 255, 255, 255, 255, 255],
        ],
        [
            [255, 248, 254, 255, 255, 255, 255, 255, 255, 255, 255],
            [249, 254, 255, 255, 255, 255, 255, 255, 255, 255, 255],
            [255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255],
        ],
        [
            [255, 253, 255, 255, 255, 255, 255, 255, 255, 255, 255],
            [247, 254, 255, 255, 255, 255, 255, 255, 255, 255, 255],
            [255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255],
        ],
        [
            [255, 253, 254, 255, 255, 255, 255, 255, 255, 255, 255],
            [252, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255],
            [255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255],
        ],
        [
            [255, 254, 254, 255, 255, 255, 255, 255, 255, 255, 255],
            [253, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255],
            [255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255],
        ],
        [
            [255, 254, 253, 255, 255, 255, 255, 255, 255, 255, 255],
            [250, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255],
            [254, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255],
        ],
        [
            [255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255],
            [255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255],
            [255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255],
        ],
    ],
    [
        [
            [186, 251, 250, 255, 255, 255, 255, 255, 255, 255, 255],
            [234, 251, 244, 254, 255, 255, 255, 255, 255, 255, 255],
            [251, 251, 243, 253, 254, 255, 254, 255, 255, 255, 255],
        ],
        [
            [255, 253, 254, 255, 255, 255, 255, 255, 255, 255, 255],
            [236, 253, 254, 255, 255, 255, 255, 255, 255, 255, 255],
            [251, 253, 253, 254, 254, 255, 255, 255, 255, 255, 255],
        ],
        [
            [255, 254, 254, 255, 255, 255, 255, 255, 255, 255, 255],
            [254, 254, 254, 255, 255, 255, 255, 255, 255, 255, 255],
            [255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255],
        ],
        [
            [255, 254, 255, 255, 255, 255, 255, 255, 255, 255, 255],
            [254, 254, 255, 255, 255, 255, 255, 255, 255, 255, 255],
            [254, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255],
        ],
        [
            [255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255],
            [254, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255],
            [255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255],
        ],
        [
            [255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255],
            [255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255],
            [255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255],
        ],
        [
            [255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255],
            [255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255],
            [255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255],
        ],
        [
            [255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255],
            [255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255],
            [255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255],
        ],
    ],
    [
        [
            [248, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255],
            [250, 254, 252, 254, 255, 255, 255, 255, 255, 255, 255],
            [248, 254, 249, 253, 255, 255, 255, 255, 255, 255, 255],
        ],
        [
            [255, 253, 253, 255, 255, 255, 255, 255, 255, 255, 255],
            [246, 253, 253, 255, 255, 255, 255, 255, 255, 255, 255],
            [252, 254, 251, 254, 254, 255, 255, 255, 255, 255, 255],
        ],
        [
            [255, 254, 252, 255, 255, 255, 255, 255, 255, 255, 255],
            [248, 254, 253, 255, 255, 255, 255, 255, 255, 255, 255],
            [253, 255, 254, 254, 255, 255, 255, 255, 255, 255, 255],
        ],
        [
            [255, 251, 254, 255, 255, 255, 255, 255, 255, 255, 255],
            [245, 251, 254, 255, 255, 255, 255, 255, 255, 255, 255],
            [253, 253, 254, 255, 255, 255, 255, 255, 255, 255, 255],
        ],
        [
            [255, 251, 253, 255, 255, 255, 255, 255, 255, 255, 255],
            [252, 253, 254, 255, 255, 255, 255, 255, 255, 255, 255],
            [255, 254, 255, 255, 255, 255, 255, 255, 255, 255, 255],
        ],
        [
            [255, 252, 255, 255, 255, 255, 255, 255, 255, 255, 255],
            [249, 255, 254, 255, 255, 255, 255, 255, 255, 255, 255],
            [255, 255, 254, 255, 255, 255, 255, 255, 255, 255, 255],
        ],
        [
            [255, 255, 253, 255, 255, 255, 255, 255, 255, 255, 255],
            [250, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255],
            [255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255],
        ],
        [
            [255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255],
            [254, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255],
            [255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255],
        ],
    ],
];

// Section 13.5
// Default Probabilities for tokens
static COEFF_PROBS: TokenProbTables = [
    [
        [
            [128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128],
            [128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128],
            [128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128],
        ],
        [
            [253, 136, 254, 255, 228, 219, 128, 128, 128, 128, 128],
            [189, 129, 242, 255, 227, 213, 255, 219, 128, 128, 128],
            [106, 126, 227, 252, 214, 209, 255, 255, 128, 128, 128],
        ],
        [
            [1, 98, 248, 255, 236, 226, 255, 255, 128, 128, 128],
            [181, 133, 238, 254, 221, 234, 255, 154, 128, 128, 128],
            [78, 134, 202, 247, 198, 180, 255, 219, 128, 128, 128],
        ],
        [
            [1, 185, 249, 255, 243, 255, 128, 128, 128, 128, 128],
            [184, 150, 247, 255, 236, 224, 128, 128, 128, 128, 128],
            [77, 110, 216, 255, 236, 230, 128, 128, 128, 128, 128],
        ],
        [
            [1, 101, 251, 255, 241, 255, 128, 128, 128, 128, 128],
            [170, 139, 241, 252, 236, 209, 255, 255, 128, 128, 128],
            [37, 116, 196, 243, 228, 255, 255, 255, 128, 128, 128],
        ],
        [
            [1, 204, 254, 255, 245, 255, 128, 128, 128, 128, 128],
            [207, 160, 250, 255, 238, 128, 128, 128, 128, 128, 128],
            [102, 103, 231, 255, 211, 171, 128, 128, 128, 128, 128],
        ],
        [
            [1, 152, 252, 255, 240, 255, 128, 128, 128, 128, 128],
            [177, 135, 243, 255, 234, 225, 128, 128, 128, 128, 128],
            [80, 129, 211, 255, 194, 224, 128, 128, 128, 128, 128],
        ],
        [
            [1, 1, 255, 128, 128, 128, 128, 128, 128, 128, 128],
            [246, 1, 255, 128, 128, 128, 128, 128, 128, 128, 128],
            [255, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128],
        ],
    ],
    [
        [
            [198, 35, 237, 223, 193, 187, 162, 160, 145, 155, 62],
            [131, 45, 198, 221, 172, 176, 220, 157, 252, 221, 1],
            [68, 47, 146, 208, 149, 167, 221, 162, 255, 223, 128],
        ],
        [
            [1, 149, 241, 255, 221, 224, 255, 255, 128, 128, 128],
            [184, 141, 234, 253, 222, 220, 255, 199, 128, 128, 128],
            [81, 99, 181, 242, 176, 190, 249, 202, 255, 255, 128],
        ],
        [
            [1, 129, 232, 253, 214, 197, 242, 196, 255, 255, 128],
            [99, 121, 210, 250, 201, 198, 255, 202, 128, 128, 128],
            [23, 91, 163, 242, 170, 187, 247, 210, 255, 255, 128],
        ],
        [
            [1, 200, 246, 255, 234, 255, 128, 128, 128, 128, 128],
            [109, 178, 241, 255, 231, 245, 255, 255, 128, 128, 128],
            [44, 130, 201, 253, 205, 192, 255, 255, 128, 128, 128],
        ],
        [
            [1, 132, 239, 251, 219, 209, 255, 165, 128, 128, 128],
            [94, 136, 225, 251, 218, 190, 255, 255, 128, 128, 128],
            [22, 100, 174, 245, 186, 161, 255, 199, 128, 128, 128],
        ],
        [
            [1, 182, 249, 255, 232, 235, 128, 128, 128, 128, 128],
            [124, 143, 241, 255, 227, 234, 128, 128, 128, 128, 128],
            [35, 77, 181, 251, 193, 211, 255, 205, 128, 128, 128],
        ],
        [
            [1, 157, 247, 255, 236, 231, 255, 255, 128, 128, 128],
            [121, 141, 235, 255, 225, 227, 255, 255, 128, 128, 128],
            [45, 99, 188, 251, 195, 217, 255, 224, 128, 128, 128],
        ],
        [
            [1, 1, 251, 255, 213, 255, 128, 128, 128, 128, 128],
            [203, 1, 248, 255, 255, 128, 128, 128, 128, 128, 128],
            [137, 1, 177, 255, 224, 255, 128, 128, 128, 128, 128],
        ],
    ],
    [
        [
            [253, 9, 248, 251, 207, 208, 255, 192, 128, 128, 128],
            [175, 13, 224, 243, 193, 185, 249, 198, 255, 255, 128],
            [73, 17, 171, 221, 161, 179, 236, 167, 255, 234, 128],
        ],
        [
            [1, 95, 247, 253, 212, 183, 255, 255, 128, 128, 128],
            [239, 90, 244, 250, 211, 209, 255, 255, 128, 128, 128],
            [155, 77, 195, 248, 188, 195, 255, 255, 128, 128, 128],
        ],
        [
            [1, 24, 239, 251, 218, 219, 255, 205, 128, 128, 128],
            [201, 51, 219, 255, 196, 186, 128, 128, 128, 128, 128],
            [69, 46, 190, 239, 201, 218, 255, 228, 128, 128, 128],
        ],
        [
            [1, 191, 251, 255, 255, 128, 128, 128, 128, 128, 128],
            [223, 165, 249, 255, 213, 255, 128, 128, 128, 128, 128],
            [141, 124, 248, 255, 255, 128, 128, 128, 128, 128, 128],
        ],
        [
            [1, 16, 248, 255, 255, 128, 128, 128, 128, 128, 128],
            [190, 36, 230, 255, 236, 255, 128, 128, 128, 128, 128],
            [149, 1, 255, 128, 128, 128, 128, 128, 128, 128, 128],
        ],
        [
            [1, 226, 255, 128, 128, 128, 128, 128, 128, 128, 128],
            [247, 192, 255, 128, 128, 128, 128, 128, 128, 128, 128],
            [240, 128, 255, 128, 128, 128, 128, 128, 128, 128, 128],
        ],
        [
            [1, 134, 252, 255, 255, 128, 128, 128, 128, 128, 128],
            [213, 62, 250, 255, 255, 128, 128, 128, 128, 128, 128],
            [55, 93, 255, 128, 128, 128, 128, 128, 128, 128, 128],
        ],
        [
            [128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128],
            [128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128],
            [128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128],
        ],
    ],
    [
        [
            [202, 24, 213, 235, 186, 191, 220, 160, 240, 175, 255],
            [126, 38, 182, 232, 169, 184, 228, 174, 255, 187, 128],
            [61, 46, 138, 219, 151, 178, 240, 170, 255, 216, 128],
        ],
        [
            [1, 112, 230, 250, 199, 191, 247, 159, 255, 255, 128],
            [166, 109, 228, 252, 211, 215, 255, 174, 128, 128, 128],
            [39, 77, 162, 232, 172, 180, 245, 178, 255, 255, 128],
        ],
        [
            [1, 52, 220, 246, 198, 199, 249, 220, 255, 255, 128],
            [124, 74, 191, 243, 183, 193, 250, 221, 255, 255, 128],
            [24, 71, 130, 219, 154, 170, 243, 182, 255, 255, 128],
        ],
        [
            [1, 182, 225, 249, 219, 240, 255, 224, 128, 128, 128],
            [149, 150, 226, 252, 216, 205, 255, 171, 128, 128, 128],
            [28, 108, 170, 242, 183, 194, 254, 223, 255, 255, 128],
        ],
        [
            [1, 81, 230, 252, 204, 203, 255, 192, 128, 128, 128],
            [123, 102, 209, 247, 188, 196, 255, 233, 128, 128, 128],
            [20, 95, 153, 243, 164, 173, 255, 203, 128, 128, 128],
        ],
        [
            [1, 222, 248, 255, 216, 213, 128, 128, 128, 128, 128],
            [168, 175, 246, 252, 235, 205, 255, 255, 128, 128, 128],
            [47, 116, 215, 255, 211, 212, 255, 255, 128, 128, 128],
        ],
        [
            [1, 121, 236, 253, 212, 214, 255, 255, 128, 128, 128],
            [141, 84, 213, 252, 201, 202, 255, 219, 128, 128, 128],
            [42, 80, 160, 240, 162, 185, 255, 205, 128, 128, 128],
        ],
        [
            [1, 1, 255, 128, 128, 128, 128, 128, 128, 128, 128],
            [244, 1, 255, 128, 128, 128, 128, 128, 128, 128, 128],
            [238, 1, 255, 128, 128, 128, 128, 128, 128, 128, 128],
        ],
    ],
];

// DCT Tokens
const DCT_0: i8 = 0;
const DCT_1: i8 = 1;
const DCT_2: i8 = 2;
const DCT_3: i8 = 3;
const DCT_4: i8 = 4;
const DCT_CAT1: i8 = 5;
const DCT_CAT2: i8 = 6;
const DCT_CAT3: i8 = 7;
const DCT_CAT4: i8 = 8;
const DCT_CAT5: i8 = 9;
const DCT_CAT6: i8 = 10;
const DCT_EOB: i8 = 11;

static DCT_TOKEN_TREE: [i8; 22] = [
    -DCT_EOB, 2, -DCT_0, 4, -DCT_1, 6, 8, 12, -DCT_2, 10, -DCT_3, -DCT_4, 14, 16, -DCT_CAT1,
    -DCT_CAT2, 18, 20, -DCT_CAT3, -DCT_CAT4, -DCT_CAT5, -DCT_CAT6,
];

static PROB_DCT_CAT: [[Prob; 12]; 6] = [
    [159, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [165, 145, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [173, 148, 140, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [176, 155, 140, 135, 0, 0, 0, 0, 0, 0, 0, 0],
    [180, 157, 141, 134, 130, 0, 0, 0, 0, 0, 0, 0],
    [254, 254, 243, 230, 196, 177, 153, 140, 133, 130, 129, 0],
];

static DCT_CAT_BASE: [u8; 6] = [5, 7, 11, 19, 35, 67];
static COEFF_BANDS: [u8; 16] = [0, 1, 2, 3, 6, 4, 5, 6, 6, 6, 6, 6, 6, 6, 6, 7];

#[rustfmt::skip]
static DC_QUANT: [i16; 128] = [
      4,   5,   6,   7,   8,   9,  10,  10,
     11,  12,  13,  14,  15,  16,  17,  17,
     18,  19,  20,  20,  21,  21,  22,  22,
     23,  23,  24,  25,  25,  26,  27,  28,
     29,  30,  31,  32,  33,  34,  35,  36,
     37,  37,  38,  39,  40,  41,  42,  43,
     44,  45,  46,  46,  47,  48,  49,  50,
     51,  52,  53,  54,  55,  56,  57,  58,
     59,  60,  61,  62,  63,  64,  65,  66,
     67,  68,  69,  70,  71,  72,  73,  74,
     75,  76,  76,  77,  78,  79,  80,  81,
     82,  83,  84,  85,  86,  87,  88,  89,
     91,  93,  95,  96,  98, 100, 101, 102,
    104, 106, 108, 110, 112, 114, 116, 118,
    122, 124, 126, 128, 130, 132, 134, 136,
    138, 140, 143, 145, 148, 151, 154, 157,
];

#[rustfmt::skip]
static AC_QUANT: [i16; 128] = [
      4,   5,   6,   7,   8,    9,  10,  11,
      12,  13,  14,  15,  16,  17,  18,  19,
      20,  21,  22,  23,  24,  25,  26,  27,
      28,  29,  30,  31,  32,  33,  34,  35,
      36,  37,  38,  39,  40,  41,  42,  43,
      44,  45,  46,  47,  48,  49,  50,  51,
      52,  53,  54,  55,  56,  57,  58,  60,
      62,  64,  66,  68,  70,  72,  74,  76,
      78,  80,  82,  84,  86,  88,  90,  92,
      94,  96,  98, 100, 102, 104, 106, 108,
     110, 112, 114, 116, 119, 122, 125, 128,
     131, 134, 137, 140, 143, 146, 149, 152,
     155, 158, 161, 164, 167, 170, 173, 177,
     181, 185, 189, 193, 197, 201, 205, 209,
     213, 217, 221, 225, 229, 234, 239, 245,
     249, 254, 259, 264, 269, 274, 279, 284,
];

static ZIGZAG: [u8; 16] = [0, 1, 4, 8, 5, 2, 3, 6, 9, 12, 13, 10, 7, 11, 14, 15];

/// All errors that can occur when attempting to parse a VP8 codec inside WebP
#[derive(Debug, Clone, Copy)]
enum DecoderError {
    /// VP8's `[0x9D, 0x01, 0x2A]` magic not found or invalid
    Vp8MagicInvalid([u8; 3]),

    /// Decoder initialisation wasn't provided with enough data
    NotEnoughInitData,

    /// At time of writing, only the YUV colour-space encoded as `0` is specified
    ColorSpaceInvalid(u8),
    /// LUMA prediction mode was not recognised
    LumaPredictionModeInvalid(i8),
    /// Intra-prediction mode was not recognised
    IntraPredictionModeInvalid(i8),
    /// Chroma prediction mode was not recognised
    ChromaPredictionModeInvalid(i8),
}

impl fmt::Display for DecoderError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DecoderError::Vp8MagicInvalid(tag) => f.write_fmt(format_args!(
                "Invalid VP8 magic: [{:#04X?}, {:#04X?}, {:#04X?}]",
                tag[0], tag[1], tag[2]
            )),

            DecoderError::NotEnoughInitData => {
                f.write_str("Expected at least 2 bytes of VP8 decoder initialization data")
            }

            DecoderError::ColorSpaceInvalid(cs) => {
                f.write_fmt(format_args!("Invalid non-YUV VP8 color space {}", cs))
            }
            DecoderError::LumaPredictionModeInvalid(pm) => {
                f.write_fmt(format_args!("Invalid VP8 LUMA prediction mode {}", pm))
            }
            DecoderError::IntraPredictionModeInvalid(i) => {
                f.write_fmt(format_args!("Invalid VP8 intra-prediction mode {}", i))
            }
            DecoderError::ChromaPredictionModeInvalid(c) => {
                f.write_fmt(format_args!("Invalid VP8 chroma prediction mode {}", c))
            }
        }
    }
}

impl From<DecoderError> for ImageError {
    fn from(e: DecoderError) -> ImageError {
        ImageError::Decoding(DecodingError::new(ImageFormat::WebP.into(), e))
    }
}

impl error::Error for DecoderError {}

struct BoolReader {
    buf: Vec<u8>,
    index: usize,

    range: u32,
    value: u32,
    bit_count: u8,
}

impl BoolReader {
    pub(crate) fn new() -> BoolReader {
        BoolReader {
            buf: Vec::new(),
            range: 0,
            value: 0,
            bit_count: 0,
            index: 0,
        }
    }

    pub(crate) fn init(&mut self, buf: Vec<u8>) -> ImageResult<()> {
        if buf.len() < 2 {
            return Err(DecoderError::NotEnoughInitData.into());
        }

        self.buf = buf;
        // Direct access safe, since length has just been validated.
        self.value = (u32::from(self.buf[0]) << 8) | u32::from(self.buf[1]);
        self.index = 2;
        self.range = 255;
        self.bit_count = 0;

        Ok(())
    }

    pub(crate) fn read_bool(&mut self, probability: u8) -> bool {
        let split = 1 + (((self.range - 1) * u32::from(probability)) >> 8);
        let bigsplit = split << 8;

        let retval = if self.value >= bigsplit {
            self.range -= split;
            self.value -= bigsplit;
            true
        } else {
            self.range = split;
            false
        };

        while self.range < 128 {
            self.value <<= 1;
            self.range <<= 1;
            self.bit_count += 1;

            if self.bit_count == 8 {
                self.bit_count = 0;

                // If no more bits are available, just don't do anything.
                // This strategy is suggested in the reference implementation of RFC6386 (p.135)
                if self.index < self.buf.len() {
                    self.value |= u32::from(self.buf[self.index]);
                    self.index += 1;
                }
            }
        }

        retval
    }

    pub(crate) fn read_literal(&mut self, n: u8) -> u8 {
        let mut v = 0u8;
        let mut n = n;

        while n != 0 {
            v = (v << 1) + self.read_bool(128u8) as u8;
            n -= 1;
        }

        v
    }

    pub(crate) fn read_magnitude_and_sign(&mut self, n: u8) -> i32 {
        let magnitude = self.read_literal(n);
        let sign = self.read_literal(1);

        if sign == 1 {
            -i32::from(magnitude)
        } else {
            i32::from(magnitude)
        }
    }

    pub(crate) fn read_with_tree(&mut self, tree: &[i8], probs: &[Prob], start: isize) -> i8 {
        let mut index = start;

        loop {
            let a = self.read_bool(probs[index as usize >> 1]);
            let b = index + a as isize;
            index = tree[b as usize] as isize;

            if index <= 0 {
                break;
            }
        }

        -index as i8
    }

    pub(crate) fn read_flag(&mut self) -> bool {
        0 != self.read_literal(1)
    }
}

#[derive(Default, Clone, Copy)]
struct MacroBlock {
    bpred: [IntraMode; 16],
    complexity: [u8; 9],
    luma_mode: LumaMode,
    chroma_mode: ChromaMode,
    segmentid: u8,
    coeffs_skipped: bool,
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

    /// The blue plane of the frame
    pub ubuf: Vec<u8>,

    /// The red plane of the frame
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
    filter_type: bool, //if true uses simple filter // if false uses normal filter
    filter_level: u8,
    sharpness_level: u8,
}

impl Frame {
    /// Chroma plane is half the size of the Luma plane
    fn chroma_width(&self) -> u16 {
        (self.width + 1) / 2
    }

    fn chroma_height(&self) -> u16 {
        (self.height + 1) / 2
    }

    /// Fills an rgb buffer with the image
    pub(crate) fn fill_rgb(&self, buf: &mut [u8]) {
        for (index, rgb_chunk) in (0..self.ybuf.len()).zip(buf.chunks_exact_mut(3)) {
            let y = index / self.width as usize;
            let x = index % self.width as usize;
            let chroma_index = self.chroma_width() as usize * (y / 2) + x / 2;

            Frame::fill_single(
                self.ybuf[index],
                self.ubuf[chroma_index],
                self.vbuf[chroma_index],
                rgb_chunk,
            );
        }
    }

    /// Fills an rgba buffer by skipping the alpha values
    pub(crate) fn fill_rgba(&self, buf: &mut [u8]) {
        for (index, rgba_chunk) in (0..self.ybuf.len()).zip(buf.chunks_exact_mut(4)) {
            let y = index / self.width as usize;
            let x = index % self.width as usize;
            let chroma_index = self.chroma_width() as usize * (y / 2) + x / 2;

            Frame::fill_single(
                self.ybuf[index],
                self.ubuf[chroma_index],
                self.vbuf[chroma_index],
                rgba_chunk,
            );
        }
    }

    /// Conversion values from https://docs.microsoft.com/en-us/windows/win32/medfound/recommended-8-bit-yuv-formats-for-video-rendering#converting-8-bit-yuv-to-rgb888
    fn fill_single(y: u8, u: u8, v: u8, rgb: &mut [u8]) {
        let c: i32 = i32::from(y) - 16;
        let d: i32 = i32::from(u) - 128;
        let e: i32 = i32::from(v) - 128;

        let r: u8 = clamp((298 * c + 409 * e + 128) >> 8, 0, 255)
            .try_into()
            .unwrap();
        let g: u8 = clamp((298 * c - 100 * d - 208 * e + 128) >> 8, 0, 255)
            .try_into()
            .unwrap();
        let b: u8 = clamp((298 * c + 516 * d + 128) >> 8, 0, 255)
            .try_into()
            .unwrap();

        rgb[0] = r;
        rgb[1] = g;
        rgb[2] = b;
    }

    /// Gets the buffer size
    pub fn get_buf_size(&self) -> usize {
        self.ybuf.len() * 3
    }
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
pub struct Vp8Decoder<R> {
    r: R,
    b: BoolReader,

    mbwidth: u16,
    mbheight: u16,
    macroblocks: Vec<MacroBlock>,

    frame: Frame,

    segments_enabled: bool,
    segments_update_map: bool,
    segment: [Segment; MAX_SEGMENTS],

    ref_delta: [i32; 4],
    mode_delta: [i32; 4],

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

    top_border: Vec<u8>,
    left_border: Vec<u8>,
}

impl<R: Read> Vp8Decoder<R> {
    /// Create a new decoder.
    /// The reader must present a raw vp8 bitstream to the decoder
    pub fn new(r: R) -> Vp8Decoder<R> {
        let f = Frame::default();
        let s = Segment::default();
        let m = MacroBlock::default();

        Vp8Decoder {
            r,
            b: BoolReader::new(),

            mbwidth: 0,
            mbheight: 0,
            macroblocks: Vec::new(),

            frame: f,
            segments_enabled: false,
            segments_update_map: false,
            segment: [s; MAX_SEGMENTS],

            ref_delta: [0; 4],
            mode_delta: [0; 4],

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

            top_border: Vec::new(),
            left_border: Vec::new(),
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
                let size = { s }
                    .read_u24::<LittleEndian>()
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
            for i in 0usize..4 {
                let ref_frame_delta_update_flag = self.b.read_flag();

                self.ref_delta[i] = if ref_frame_delta_update_flag {
                    self.b.read_magnitude_and_sign(6)
                } else {
                    0i32
                };
            }

            for i in 0usize..4 {
                let mb_mode_delta_update_flag = self.b.read_flag();

                self.mode_delta[i] = if mb_mode_delta_update_flag {
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
                return Err(DecoderError::Vp8MagicInvalid(tag).into());
            }

            let w = self.r.read_u16::<LittleEndian>()?;
            let h = self.r.read_u16::<LittleEndian>()?;

            self.frame.width = w & 0x3FFF;
            self.frame.height = h & 0x3FFF;

            self.top = init_top_macroblocks(self.frame.width as usize);
            // Almost always the first macro block, except when non exists (i.e. `width == 0`)
            self.left = self.top.get(0).cloned().unwrap_or_default();

            self.mbwidth = (self.frame.width + 15) / 16;
            self.mbheight = (self.frame.height + 15) / 16;

            self.frame.ybuf = vec![0u8; self.frame.width as usize * self.frame.height as usize];
            self.frame.ubuf =
                vec![0u8; self.frame.chroma_width() as usize * self.frame.chroma_height() as usize];
            self.frame.vbuf =
                vec![0u8; self.frame.chroma_width() as usize * self.frame.chroma_height() as usize];

            self.top_border = vec![127u8; self.frame.width as usize + 4 + 16];
            self.left_border = vec![129u8; 1 + 16];
        }

        let mut buf = vec![0; first_partition_size as usize];
        self.r.read_exact(&mut buf)?;

        // initialise binary decoder
        self.b.init(buf)?;

        if self.frame.keyframe {
            let color_space = self.b.read_literal(1);
            self.frame.pixel_type = self.b.read_literal(1);

            if color_space != 0 {
                return Err(DecoderError::ColorSpaceInvalid(color_space).into());
            }
        }

        self.segments_enabled = self.b.read_flag();
        if self.segments_enabled {
            self.read_segment_updates();
        }

        self.frame.filter_type = self.b.read_flag();
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
            // FIXME: support this?
            return Err(ImageError::Unsupported(
                UnsupportedError::from_format_and_kind(
                    ImageFormat::WebP.into(),
                    UnsupportedErrorKind::GenericFeature("Non-keyframe frames".to_owned()),
                ),
            ));
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

            // FIXME: support this?
            return Err(ImageError::Unsupported(
                UnsupportedError::from_format_and_kind(
                    ImageFormat::WebP.into(),
                    UnsupportedErrorKind::GenericFeature("Non-keyframe frames".to_owned()),
                ),
            ));
        } else {
            // Reset motion vectors
        }

        Ok(())
    }

    fn read_macroblock_header(&mut self, mbx: usize) -> ImageResult<MacroBlock> {
        let mut mb = MacroBlock::default();

        if self.segments_enabled && self.segments_update_map {
            mb.segmentid = self
                .b
                .read_with_tree(&SEGMENT_ID_TREE, &self.segment_tree_probs, 0)
                as u8;
        };

        mb.coeffs_skipped = if self.prob_skip_false.is_some() {
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
            return Err(ImageError::Unsupported(
                UnsupportedError::from_format_and_kind(
                    ImageFormat::WebP.into(),
                    UnsupportedErrorKind::GenericFeature("VP8 inter-prediction".to_owned()),
                ),
            ));
        }

        if self.frame.keyframe {
            // intra prediction
            let luma = self
                .b
                .read_with_tree(&KEYFRAME_YMODE_TREE, &KEYFRAME_YMODE_PROBS, 0);
            mb.luma_mode =
                LumaMode::from_i8(luma).ok_or(DecoderError::LumaPredictionModeInvalid(luma))?;

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
                                .ok_or(DecoderError::IntraPredictionModeInvalid(intra))?;
                            mb.bpred[x + y * 4] = bmode;

                            self.top[mbx].bpred[12 + x] = bmode;
                            self.left.bpred[y] = bmode;
                        }
                    }
                }
                Some(mode) => {
                    for i in 0usize..4 {
                        mb.bpred[12 + i] = mode;
                        self.left.bpred[i] = mode;
                    }
                }
            }

            let chroma = self
                .b
                .read_with_tree(&KEYFRAME_UV_MODE_TREE, &KEYFRAME_UV_MODE_PROBS, 0);
            mb.chroma_mode = ChromaMode::from_i8(chroma)
                .ok_or(DecoderError::ChromaPredictionModeInvalid(chroma))?;
        }

        self.top[mbx].chroma_mode = mb.chroma_mode;
        self.top[mbx].luma_mode = mb.luma_mode;
        self.top[mbx].bpred = mb.bpred;

        Ok(mb)
    }

    fn intra_predict_luma(&mut self, mbx: usize, mby: usize, mb: &MacroBlock, resdata: &[i32]) {
        let stride = 1usize + 16 + 4;
        let w = self.frame.width as usize;
        let mw = self.mbwidth as usize;
        let mut ws = create_border_luma(mbx, mby, mw, &self.top_border, &self.left_border);

        match mb.luma_mode {
            LumaMode::V => predict_vpred(&mut ws, 16, 1, 1, stride),
            LumaMode::H => predict_hpred(&mut ws, 16, 1, 1, stride),
            LumaMode::TM => predict_tmpred(&mut ws, 16, 1, 1, stride),
            LumaMode::DC => predict_dcpred(&mut ws, 16, stride, mby != 0, mbx != 0),
            LumaMode::B => predict_4x4(&mut ws, stride, &mb.bpred, resdata),
        }

        if mb.luma_mode != LumaMode::B {
            for y in 0usize..4 {
                for x in 0usize..4 {
                    let i = x + y * 4;
                    // Create a reference to a [i32; 16] array for add_residue (slices of size 16 do not work).
                    let rb: &[i32; 16] = resdata[i * 16..][..16].try_into().unwrap();
                    let y0 = 1 + y * 4;
                    let x0 = 1 + x * 4;

                    add_residue(&mut ws, rb, y0, x0, stride);
                }
            }
        }

        self.left_border[0] = ws[16];

        for i in 0usize..16 {
            self.top_border[mbx * 16 + i] = ws[16 * stride + 1 + i];
            self.left_border[i + 1] = ws[(i + 1) * stride + 16];
        }

        // Length is the remainder to the border, but maximally the current chunk.
        let ylength = cmp::min(self.frame.height as usize - mby * 16, 16);
        let xlength = cmp::min(self.frame.width as usize - mbx * 16, 16);

        for y in 0usize..ylength {
            for x in 0usize..xlength {
                self.frame.ybuf[(mby * 16 + y) * w + mbx * 16 + x] = ws[(1 + y) * stride + 1 + x];
            }
        }
    }

    fn intra_predict_chroma(&mut self, mbx: usize, mby: usize, mb: &MacroBlock, resdata: &[i32]) {
        let stride = 1usize + 8;

        let w = self.frame.chroma_width() as usize;

        //8x8 with left top border of 1
        let mut uws = [0u8; (8 + 1) * (8 + 1)];
        let mut vws = [0u8; (8 + 1) * (8 + 1)];

        let ylength = cmp::min(self.frame.chroma_height() as usize - mby * 8, 8);
        let xlength = cmp::min(self.frame.chroma_width() as usize - mbx * 8, 8);

        //left border
        for y in 0usize..8 {
            let (uy, vy) = if mbx == 0 || y >= ylength {
                (129, 129)
            } else {
                let index = (mby * 8 + y) * w + ((mbx - 1) * 8 + 7);
                (self.frame.ubuf[index], self.frame.vbuf[index])
            };

            uws[(y + 1) * stride] = uy;
            vws[(y + 1) * stride] = vy;
        }
        //top border
        for x in 0usize..8 {
            let (ux, vx) = if mby == 0 || x >= xlength {
                (127, 127)
            } else {
                let index = ((mby - 1) * 8 + 7) * w + (mbx * 8 + x);
                (self.frame.ubuf[index], self.frame.vbuf[index])
            };

            uws[x + 1] = ux;
            vws[x + 1] = vx;
        }

        //top left point
        let (u1, v1) = if mby == 0 {
            (127, 127)
        } else if mbx == 0 {
            (129, 129)
        } else {
            let index = ((mby - 1) * 8 + 7) * w + (mbx - 1) * 8 + 7;
            if index >= self.frame.ubuf.len() {
                (127, 127)
            } else {
                (self.frame.ubuf[index], self.frame.vbuf[index])
            }
        };

        uws[0] = u1;
        vws[0] = v1;

        match mb.chroma_mode {
            ChromaMode::DC => {
                predict_dcpred(&mut uws, 8, stride, mby != 0, mbx != 0);
                predict_dcpred(&mut vws, 8, stride, mby != 0, mbx != 0);
            }
            ChromaMode::V => {
                predict_vpred(&mut uws, 8, 1, 1, stride);
                predict_vpred(&mut vws, 8, 1, 1, stride);
            }
            ChromaMode::H => {
                predict_hpred(&mut uws, 8, 1, 1, stride);
                predict_hpred(&mut vws, 8, 1, 1, stride);
            }
            ChromaMode::TM => {
                predict_tmpred(&mut uws, 8, 1, 1, stride);
                predict_tmpred(&mut vws, 8, 1, 1, stride);
            }
        }

        for y in 0usize..2 {
            for x in 0usize..2 {
                let i = x + y * 2;
                let urb: &[i32; 16] = resdata[16 * 16 + i * 16..][..16].try_into().unwrap();

                let y0 = 1 + y * 4;
                let x0 = 1 + x * 4;
                add_residue(&mut uws, urb, y0, x0, stride);

                let vrb: &[i32; 16] = resdata[20 * 16 + i * 16..][..16].try_into().unwrap();

                add_residue(&mut vws, vrb, y0, x0, stride);
            }
        }

        for y in 0usize..ylength {
            for x in 0usize..xlength {
                self.frame.ubuf[(mby * 8 + y) * w + mbx * 8 + x] = uws[(1 + y) * stride + 1 + x];
                self.frame.vbuf[(mby * 8 + y) * w + mbx * 8 + x] = vws[(1 + y) * stride + 1 + x];
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

                literal @ DCT_1..=DCT_4 => i16::from(literal),

                category @ DCT_CAT1..=DCT_CAT6 => {
                    let t = PROB_DCT_CAT[(category - DCT_CAT1) as usize];

                    let mut extra = 0i16;
                    let mut j = 0;

                    while t[j] > 0 {
                        extra = extra + extra + self.partitions[p].read_bool(t[j]) as i16;
                        j += 1;
                    }

                    i16::from(DCT_CAT_BASE[(category - DCT_CAT1) as usize]) + extra
                }

                c => panic!("unknown token: {}", c),
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

    /// Does loop filtering on the macroblock
    fn loop_filter(&mut self, mbx: usize, mby: usize, mb: &MacroBlock) {
        let luma_w = self.frame.width as usize;
        let luma_h = self.frame.height as usize;
        let chroma_w = self.frame.chroma_width() as usize;
        let chroma_h = self.frame.chroma_height() as usize;

        let (filter_level, interior_limit, hev_threshold) = self.calculate_filter_parameters(mb);

        if filter_level > 0 {
            let mbedge_limit = (filter_level + 2) * 2 + interior_limit;
            let sub_bedge_limit = (filter_level * 2) + interior_limit;

            let luma_ylength = cmp::min(luma_h - 16 * mby, 16);
            let luma_xlength = cmp::min(luma_w - 16 * mbx, 16);

            let chroma_ylength = cmp::min(chroma_h - 8 * mby, 8);
            let chroma_xlength = cmp::min(chroma_w - 8 * mbx, 8);

            //filter across left of macroblock
            if mbx > 0 {
                //simple loop filtering
                if self.frame.filter_type {
                    if luma_xlength >= 2 {
                        for y in 0usize..luma_ylength {
                            let y0 = mby * 16 + y;
                            let x0 = mbx * 16;

                            loop_filter::simple_segment(
                                mbedge_limit,
                                &mut self.frame.ybuf[..],
                                y0 * luma_w + x0,
                                1,
                            );
                        }
                    }
                } else {
                    if luma_xlength >= 4 {
                        for y in 0usize..luma_ylength {
                            let y0 = mby * 16 + y;
                            let x0 = mbx * 16;

                            loop_filter::macroblock_filter(
                                hev_threshold,
                                interior_limit,
                                mbedge_limit,
                                &mut self.frame.ybuf[..],
                                y0 * luma_w + x0,
                                1,
                            );
                        }
                    }

                    if chroma_xlength >= 4 {
                        for y in 0usize..chroma_ylength {
                            let y0 = mby * 8 + y;
                            let x0 = mbx * 8;

                            loop_filter::macroblock_filter(
                                hev_threshold,
                                interior_limit,
                                mbedge_limit,
                                &mut self.frame.ubuf[..],
                                y0 * chroma_w + x0,
                                1,
                            );
                            loop_filter::macroblock_filter(
                                hev_threshold,
                                interior_limit,
                                mbedge_limit,
                                &mut self.frame.vbuf[..],
                                y0 * chroma_w + x0,
                                1,
                            );
                        }
                    }
                }
            }

            //filter across vertical subblocks in macroblock
            if mb.luma_mode == LumaMode::B || !mb.coeffs_skipped {
                if self.frame.filter_type {
                    for x in (4usize..luma_xlength - 1).step_by(4) {
                        for y in 0..luma_ylength {
                            let y0 = mby * 16 + y;
                            let x0 = mbx * 16 + x;

                            loop_filter::simple_segment(
                                sub_bedge_limit,
                                &mut self.frame.ybuf[..],
                                y0 * luma_w + x0,
                                1,
                            );
                        }
                    }
                } else {
                    if luma_xlength > 3 {
                        for x in (4usize..luma_xlength - 3).step_by(4) {
                            for y in 0..luma_ylength {
                                let y0 = mby * 16 + y;
                                let x0 = mbx * 16 + x;

                                loop_filter::subblock_filter(
                                    hev_threshold,
                                    interior_limit,
                                    sub_bedge_limit,
                                    &mut self.frame.ybuf[..],
                                    y0 * luma_w + x0,
                                    1,
                                );
                            }
                        }
                    }

                    if chroma_xlength == 8 {
                        for y in 0usize..chroma_ylength {
                            let y0 = mby * 8 + y;
                            let x0 = mbx * 8 + 4;

                            loop_filter::subblock_filter(
                                hev_threshold,
                                interior_limit,
                                sub_bedge_limit,
                                &mut self.frame.ubuf[..],
                                y0 * chroma_w + x0,
                                1,
                            );

                            loop_filter::subblock_filter(
                                hev_threshold,
                                interior_limit,
                                sub_bedge_limit,
                                &mut self.frame.vbuf[..],
                                y0 * chroma_w + x0,
                                1,
                            );
                        }
                    }
                }
            }

            //filter across top of macroblock
            if mby > 0 {
                if self.frame.filter_type {
                    if luma_ylength >= 2 {
                        for x in 0usize..luma_xlength {
                            let y0 = mby * 16;
                            let x0 = mbx * 16 + x;

                            loop_filter::simple_segment(
                                mbedge_limit,
                                &mut self.frame.ybuf[..],
                                y0 * luma_w + x0,
                                luma_w,
                            );
                        }
                    }
                } else {
                    //if bottom macroblock, can only filter if there is 3 pixels below
                    if luma_ylength >= 4 {
                        for x in 0usize..luma_xlength {
                            let y0 = mby * 16;
                            let x0 = mbx * 16 + x;

                            loop_filter::macroblock_filter(
                                hev_threshold,
                                interior_limit,
                                mbedge_limit,
                                &mut self.frame.ybuf[..],
                                y0 * luma_w + x0,
                                luma_w,
                            );
                        }
                    }

                    if chroma_ylength >= 4 {
                        for x in 0usize..chroma_xlength {
                            let y0 = mby * 8;
                            let x0 = mbx * 8 + x;

                            loop_filter::macroblock_filter(
                                hev_threshold,
                                interior_limit,
                                mbedge_limit,
                                &mut self.frame.ubuf[..],
                                y0 * chroma_w + x0,
                                chroma_w,
                            );
                            loop_filter::macroblock_filter(
                                hev_threshold,
                                interior_limit,
                                mbedge_limit,
                                &mut self.frame.vbuf[..],
                                y0 * chroma_w + x0,
                                chroma_w,
                            );
                        }
                    }
                }
            }

            //filter across horizontal subblock edges within the macroblock
            if mb.luma_mode == LumaMode::B || !mb.coeffs_skipped {
                if self.frame.filter_type {
                    for y in (4usize..luma_ylength - 1).step_by(4) {
                        for x in 0..luma_xlength {
                            let y0 = mby * 16 + y;
                            let x0 = mbx * 16 + x;

                            loop_filter::simple_segment(
                                sub_bedge_limit,
                                &mut self.frame.ybuf[..],
                                y0 * luma_w + x0,
                                luma_w,
                            );
                        }
                    }
                } else {
                    if luma_ylength > 3 {
                        for y in (4usize..luma_ylength - 3).step_by(4) {
                            for x in 0..luma_xlength {
                                let y0 = mby * 16 + y;
                                let x0 = mbx * 16 + x;

                                loop_filter::subblock_filter(
                                    hev_threshold,
                                    interior_limit,
                                    sub_bedge_limit,
                                    &mut self.frame.ybuf[..],
                                    y0 * luma_w + x0,
                                    luma_w,
                                );
                            }
                        }
                    }

                    if chroma_ylength == 8 {
                        for x in 0..chroma_xlength {
                            let y0 = mby * 8 + 4;
                            let x0 = mbx * 8 + x;

                            loop_filter::subblock_filter(
                                hev_threshold,
                                interior_limit,
                                sub_bedge_limit,
                                &mut self.frame.ubuf[..],
                                y0 * chroma_w + x0,
                                chroma_w,
                            );

                            loop_filter::subblock_filter(
                                hev_threshold,
                                interior_limit,
                                sub_bedge_limit,
                                &mut self.frame.vbuf[..],
                                y0 * chroma_w + x0,
                                chroma_w,
                            );
                        }
                    }
                }
            }
        }
    }

    //return values are the filter level, interior limit and hev threshold
    fn calculate_filter_parameters(&self, macroblock: &MacroBlock) -> (u8, u8, u8) {
        let segment = self.segment[macroblock.segmentid as usize];
        let mut filter_level = self.frame.filter_level as i32;

        if self.segments_enabled {
            if segment.delta_values {
                filter_level += i32::from(segment.loopfilter_level);
            } else {
                filter_level = i32::from(segment.loopfilter_level);
            }
        }

        filter_level = clamp(filter_level, 0, 63);

        if macroblock.luma_mode == LumaMode::B {
            filter_level += self.mode_delta[0];
        }

        let filter_level = clamp(filter_level, 0, 63) as u8;

        //interior limit
        let mut interior_limit = filter_level;

        if self.frame.sharpness_level > 0 {
            interior_limit >>= if self.frame.sharpness_level > 4 { 2 } else { 1 };

            if interior_limit > 9 - self.frame.sharpness_level {
                interior_limit = 9 - self.frame.sharpness_level;
            }
        }

        if interior_limit == 0 {
            interior_limit = 1;
        }

        //high edge variance threshold
        let mut hev_threshold = 0;

        #[allow(clippy::collapsible_else_if)]
        if self.frame.keyframe {
            if filter_level >= 40 {
                hev_threshold = 2;
            } else {
                hev_threshold = 1;
            }
        } else {
            if filter_level >= 40 {
                hev_threshold = 3;
            } else if filter_level >= 20 {
                hev_threshold = 2;
            } else if filter_level >= 15 {
                hev_threshold = 1;
            }
        }

        (filter_level, interior_limit, hev_threshold)
    }

    /// Decodes the current frame
    pub fn decode_frame(&mut self) -> ImageResult<&Frame> {
        self.read_frame_header()?;

        for mby in 0..self.mbheight as usize {
            let p = mby % self.num_partitions as usize;
            self.left = MacroBlock::default();

            for mbx in 0..self.mbwidth as usize {
                let mb = self.read_macroblock_header(mbx)?;
                let blocks = if !mb.coeffs_skipped {
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

                self.intra_predict_luma(mbx, mby, &mb, &blocks);
                self.intra_predict_chroma(mbx, mby, &mb, &blocks);

                self.macroblocks.push(mb);
            }

            self.left_border = vec![129u8; 1 + 16];
        }

        //do loop filtering
        for mby in 0..self.mbheight as usize {
            for mbx in 0..self.mbwidth as usize {
                let mb = self.macroblocks[mby * self.mbwidth as usize + mbx];
                self.loop_filter(mbx, mby, &mb);
            }
        }

        Ok(&self.frame)
    }
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

fn init_top_macroblocks(width: usize) -> Vec<MacroBlock> {
    let mb_width = (width + 15) / 16;

    let mb = MacroBlock {
        // Section 11.3 #3
        bpred: [IntraMode::DC; 16],
        luma_mode: LumaMode::DC,
        ..MacroBlock::default()
    };

    vec![mb; mb_width]
}

fn create_border_luma(mbx: usize, mby: usize, mbw: usize, top: &[u8], left: &[u8]) -> [u8; 357] {
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

fn avg3(left: u8, this: u8, right: u8) -> u8 {
    let avg = (u16::from(left) + 2 * u16::from(this) + u16::from(right) + 2) >> 2;
    avg as u8
}

fn avg2(this: u8, right: u8) -> u8 {
    let avg = (u16::from(this) + u16::from(right) + 1) >> 1;
    avg as u8
}

// Only 16 elements from rblock are used to add residue, so it is restricted to 16 elements
// to enable SIMD and other optimizations.
fn add_residue(pblock: &mut [u8], rblock: &[i32; 16], y0: usize, x0: usize, stride: usize) {
    let mut pos = y0 * stride + x0;
    for row in rblock.chunks(4) {
        for (p, &a) in pblock[pos..pos + 4].iter_mut().zip(row.iter()) {
            *p = clamp(a + i32::from(*p), 0, 255) as u8;
        }
        pos += stride;
    }
}

fn predict_4x4(ws: &mut [u8], stride: usize, modes: &[IntraMode], resdata: &[i32]) {
    for sby in 0usize..4 {
        for sbx in 0usize..4 {
            let i = sbx + sby * 4;
            let y0 = sby * 4 + 1;
            let x0 = sbx * 4 + 1;

            match modes[i] {
                IntraMode::TM => predict_tmpred(ws, 4, x0, y0, stride),
                IntraMode::VE => predict_bvepred(ws, x0, y0, stride),
                IntraMode::HE => predict_bhepred(ws, x0, y0, stride),
                IntraMode::DC => predict_bdcpred(ws, x0, y0, stride),
                IntraMode::LD => predict_bldpred(ws, x0, y0, stride),
                IntraMode::RD => predict_brdpred(ws, x0, y0, stride),
                IntraMode::VR => predict_bvrpred(ws, x0, y0, stride),
                IntraMode::VL => predict_bvlpred(ws, x0, y0, stride),
                IntraMode::HD => predict_bhdpred(ws, x0, y0, stride),
                IntraMode::HU => predict_bhupred(ws, x0, y0, stride),
            }

            let rb: &[i32; 16] = resdata[i * 16..][..16].try_into().unwrap();
            add_residue(ws, rb, y0, x0, stride);
        }
    }
}

fn predict_vpred(a: &mut [u8], size: usize, x0: usize, y0: usize, stride: usize) {
    for y in 0usize..size {
        for x in 0usize..size {
            a[(x + x0) + stride * (y + y0)] = a[(x + x0) + stride * (y0 + y - 1)];
        }
    }
}

fn predict_hpred(a: &mut [u8], size: usize, x0: usize, y0: usize, stride: usize) {
    for y in 0usize..size {
        for x in 0usize..size {
            a[(x + x0) + stride * (y + y0)] = a[(x + x0 - 1) + stride * (y0 + y)];
        }
    }
}

fn predict_dcpred(a: &mut [u8], size: usize, stride: usize, above: bool, left: bool) {
    let mut sum = 0;
    let mut shf = if size == 8 { 2 } else { 3 };

    if left {
        for y in 0usize..size {
            sum += u32::from(a[(y + 1) * stride]);
        }

        shf += 1;
    }

    if above {
        for x in 0usize..size {
            sum += u32::from(a[x + 1]);
        }

        shf += 1;
    }

    let dcval = if !left && !above {
        128
    } else {
        (sum + (1 << (shf - 1))) >> shf
    };

    for y in 0usize..size {
        for x in 0usize..size {
            a[(x + 1) + stride * (y + 1)] = dcval as u8;
        }
    }
}

fn predict_tmpred(a: &mut [u8], size: usize, x0: usize, y0: usize, stride: usize) {
    for y in 0usize..size {
        for x in 0usize..size {
            let pred = i32::from(a[(y0 + y) * stride + x0 - 1])
                + i32::from(a[(y0 - 1) * stride + x0 + x])
                - i32::from(a[(y0 - 1) * stride + x0 - 1]);

            a[(x + x0) + stride * (y + y0)] = clamp(pred, 0, 255) as u8;
        }
    }
}

fn predict_bdcpred(a: &mut [u8], x0: usize, y0: usize, stride: usize) {
    let mut v = 4;
    for i in 0usize..4 {
        v += u32::from(a[(y0 + i) * stride + x0 - 1]) + u32::from(a[(y0 - 1) * stride + x0 + i]);
    }

    v >>= 3;
    for y in 0usize..4 {
        for x in 0usize..4 {
            a[x + x0 + stride * (y + y0)] = v as u8;
        }
    }
}

fn topleft_pixel(a: &[u8], x0: usize, y0: usize, stride: usize) -> u8 {
    a[(y0 - 1) * stride + x0 - 1]
}

fn top_pixels(a: &[u8], x0: usize, y0: usize, stride: usize) -> (u8, u8, u8, u8, u8, u8, u8, u8) {
    let pos = (y0 - 1) * stride + x0;
    let a_slice = &a[pos..pos + 8];
    let a0 = a_slice[0];
    let a1 = a_slice[1];
    let a2 = a_slice[2];
    let a3 = a_slice[3];
    let a4 = a_slice[4];
    let a5 = a_slice[5];
    let a6 = a_slice[6];
    let a7 = a_slice[7];

    (a0, a1, a2, a3, a4, a5, a6, a7)
}

fn left_pixels(a: &[u8], x0: usize, y0: usize, stride: usize) -> (u8, u8, u8, u8) {
    let l0 = a[y0 * stride + x0 - 1];
    let l1 = a[(y0 + 1) * stride + x0 - 1];
    let l2 = a[(y0 + 2) * stride + x0 - 1];
    let l3 = a[(y0 + 3) * stride + x0 - 1];

    (l0, l1, l2, l3)
}

fn edge_pixels(
    a: &[u8],
    x0: usize,
    y0: usize,
    stride: usize,
) -> (u8, u8, u8, u8, u8, u8, u8, u8, u8) {
    let pos = (y0 - 1) * stride + x0 - 1;
    let a_slice = &a[pos..=pos + 4];
    let e0 = a[pos + 4 * stride];
    let e1 = a[pos + 3 * stride];
    let e2 = a[pos + 2 * stride];
    let e3 = a[pos + stride];
    let e4 = a_slice[0];
    let e5 = a_slice[1];
    let e6 = a_slice[2];
    let e7 = a_slice[3];
    let e8 = a_slice[4];

    (e0, e1, e2, e3, e4, e5, e6, e7, e8)
}

fn predict_bvepred(a: &mut [u8], x0: usize, y0: usize, stride: usize) {
    let p = topleft_pixel(a, x0, y0, stride);
    let (a0, a1, a2, a3, a4, _, _, _) = top_pixels(a, x0, y0, stride);
    let avg_1 = avg3(p, a0, a1);
    let avg_2 = avg3(a0, a1, a2);
    let avg_3 = avg3(a1, a2, a3);
    let avg_4 = avg3(a2, a3, a4);

    let avg = [avg_1, avg_2, avg_3, avg_4];

    let mut pos = y0 * stride + x0;
    for _ in 0..4 {
        a[pos..=pos + 3].copy_from_slice(&avg);
        pos += stride;
    }
}

fn predict_bhepred(a: &mut [u8], x0: usize, y0: usize, stride: usize) {
    let p = topleft_pixel(a, x0, y0, stride);
    let (l0, l1, l2, l3) = left_pixels(a, x0, y0, stride);

    let avgs = [
        avg3(p, l0, l1),
        avg3(l0, l1, l2),
        avg3(l1, l2, l3),
        avg3(l2, l3, l3),
    ];

    let mut pos = y0 * stride + x0;
    for &avg in avgs.iter() {
        for a_p in a[pos..=pos + 3].iter_mut() {
            *a_p = avg;
        }
        pos += stride;
    }
}

fn predict_bldpred(a: &mut [u8], x0: usize, y0: usize, stride: usize) {
    let (a0, a1, a2, a3, a4, a5, a6, a7) = top_pixels(a, x0, y0, stride);

    let avgs = [
        avg3(a0, a1, a2),
        avg3(a1, a2, a3),
        avg3(a2, a3, a4),
        avg3(a3, a4, a5),
        avg3(a4, a5, a6),
        avg3(a5, a6, a7),
        avg3(a6, a7, a7),
    ];

    let mut pos = y0 * stride + x0;

    for i in 0..4 {
        a[pos..=pos + 3].copy_from_slice(&avgs[i..=i + 3]);
        pos += stride;
    }
}

fn predict_brdpred(a: &mut [u8], x0: usize, y0: usize, stride: usize) {
    let (e0, e1, e2, e3, e4, e5, e6, e7, e8) = edge_pixels(a, x0, y0, stride);

    let avgs = [
        avg3(e0, e1, e2),
        avg3(e1, e2, e3),
        avg3(e2, e3, e4),
        avg3(e3, e4, e5),
        avg3(e4, e5, e6),
        avg3(e5, e6, e7),
        avg3(e6, e7, e8),
    ];
    let mut pos = y0 * stride + x0;

    for i in 0..4 {
        a[pos..=pos + 3].copy_from_slice(&avgs[3 - i..7 - i]);
        pos += stride;
    }
}

fn predict_bvrpred(a: &mut [u8], x0: usize, y0: usize, stride: usize) {
    let (_, e1, e2, e3, e4, e5, e6, e7, e8) = edge_pixels(a, x0, y0, stride);

    a[(y0 + 3) * stride + x0] = avg3(e1, e2, e3);
    a[(y0 + 2) * stride + x0] = avg3(e2, e3, e4);
    a[(y0 + 3) * stride + x0 + 1] = avg3(e3, e4, e5);
    a[(y0 + 1) * stride + x0] = avg3(e3, e4, e5);
    a[(y0 + 2) * stride + x0 + 1] = avg2(e4, e5);
    a[y0 * stride + x0] = avg2(e4, e5);
    a[(y0 + 3) * stride + x0 + 2] = avg3(e4, e5, e6);
    a[(y0 + 1) * stride + x0 + 1] = avg3(e4, e5, e6);
    a[(y0 + 2) * stride + x0 + 2] = avg2(e5, e6);
    a[y0 * stride + x0 + 1] = avg2(e5, e6);
    a[(y0 + 3) * stride + x0 + 3] = avg3(e5, e6, e7);
    a[(y0 + 1) * stride + x0 + 2] = avg3(e5, e6, e7);
    a[(y0 + 2) * stride + x0 + 3] = avg2(e6, e7);
    a[y0 * stride + x0 + 2] = avg2(e6, e7);
    a[(y0 + 1) * stride + x0 + 3] = avg3(e6, e7, e8);
    a[y0 * stride + x0 + 3] = avg2(e7, e8);
}

fn predict_bvlpred(a: &mut [u8], x0: usize, y0: usize, stride: usize) {
    let (a0, a1, a2, a3, a4, a5, a6, a7) = top_pixels(a, x0, y0, stride);

    a[y0 * stride + x0] = avg2(a0, a1);
    a[(y0 + 1) * stride + x0] = avg3(a0, a1, a2);
    a[(y0 + 2) * stride + x0] = avg2(a1, a2);
    a[y0 * stride + x0 + 1] = avg2(a1, a2);
    a[(y0 + 1) * stride + x0 + 1] = avg3(a1, a2, a3);
    a[(y0 + 3) * stride + x0] = avg3(a1, a2, a3);
    a[(y0 + 2) * stride + x0 + 1] = avg2(a2, a3);
    a[y0 * stride + x0 + 2] = avg2(a2, a3);
    a[(y0 + 3) * stride + x0 + 1] = avg3(a2, a3, a4);
    a[(y0 + 1) * stride + x0 + 2] = avg3(a2, a3, a4);
    a[(y0 + 2) * stride + x0 + 2] = avg2(a3, a4);
    a[y0 * stride + x0 + 3] = avg2(a3, a4);
    a[(y0 + 3) * stride + x0 + 2] = avg3(a3, a4, a5);
    a[(y0 + 1) * stride + x0 + 3] = avg3(a3, a4, a5);
    a[(y0 + 2) * stride + x0 + 3] = avg3(a4, a5, a6);
    a[(y0 + 3) * stride + x0 + 3] = avg3(a5, a6, a7);
}

fn predict_bhdpred(a: &mut [u8], x0: usize, y0: usize, stride: usize) {
    let (e0, e1, e2, e3, e4, e5, e6, e7, _) = edge_pixels(a, x0, y0, stride);

    a[(y0 + 3) * stride + x0] = avg2(e0, e1);
    a[(y0 + 3) * stride + x0 + 1] = avg3(e0, e1, e2);
    a[(y0 + 2) * stride + x0] = avg2(e1, e2);
    a[(y0 + 3) * stride + x0 + 2] = avg2(e1, e2);
    a[(y0 + 2) * stride + x0 + 1] = avg3(e1, e2, e3);
    a[(y0 + 3) * stride + x0 + 3] = avg3(e1, e2, e3);
    a[(y0 + 2) * stride + x0 + 2] = avg2(e2, e3);
    a[(y0 + 1) * stride + x0] = avg2(e2, e3);
    a[(y0 + 2) * stride + x0 + 3] = avg3(e2, e3, e4);
    a[(y0 + 1) * stride + x0 + 1] = avg3(e2, e3, e4);
    a[(y0 + 1) * stride + x0 + 2] = avg2(e3, e4);
    a[y0 * stride + x0] = avg2(e3, e4);
    a[(y0 + 1) * stride + x0 + 3] = avg3(e3, e4, e5);
    a[y0 * stride + x0 + 1] = avg3(e3, e4, e5);
    a[y0 * stride + x0 + 2] = avg3(e4, e5, e6);
    a[y0 * stride + x0 + 3] = avg3(e5, e6, e7);
}

fn predict_bhupred(a: &mut [u8], x0: usize, y0: usize, stride: usize) {
    let (l0, l1, l2, l3) = left_pixels(a, x0, y0, stride);

    a[y0 * stride + x0] = avg2(l0, l1);
    a[y0 * stride + x0 + 1] = avg3(l0, l1, l2);
    a[y0 * stride + x0 + 2] = avg2(l1, l2);
    a[(y0 + 1) * stride + x0] = avg2(l1, l2);
    a[y0 * stride + x0 + 3] = avg3(l1, l2, l3);
    a[(y0 + 1) * stride + x0 + 1] = avg3(l1, l2, l3);
    a[(y0 + 1) * stride + x0 + 2] = avg2(l2, l3);
    a[(y0 + 2) * stride + x0] = avg2(l2, l3);
    a[(y0 + 1) * stride + x0 + 3] = avg3(l2, l3, l3);
    a[(y0 + 2) * stride + x0 + 1] = avg3(l2, l3, l3);
    a[(y0 + 2) * stride + x0 + 2] = l3;
    a[(y0 + 2) * stride + x0 + 3] = l3;
    a[(y0 + 3) * stride + x0] = l3;
    a[(y0 + 3) * stride + x0 + 1] = l3;
    a[(y0 + 3) * stride + x0 + 2] = l3;
    a[(y0 + 3) * stride + x0 + 3] = l3;
}

#[cfg(test)]
mod test {

    #[cfg(feature = "benchmarks")]
    extern crate test;
    use super::{
        add_residue, avg2, avg3, edge_pixels, predict_bhepred, predict_bldpred, predict_brdpred,
        predict_bvepred, top_pixels,
    };
    #[cfg(feature = "benchmarks")]
    use super::{predict_4x4, IntraMode};
    #[cfg(feature = "benchmarks")]
    use test::{black_box, Bencher};

    #[cfg(feature = "benchmarks")]
    const W: usize = 256;
    #[cfg(feature = "benchmarks")]
    const H: usize = 256;

    #[cfg(feature = "benchmarks")]
    fn make_sample_image() -> Vec<u8> {
        let mut v = Vec::with_capacity((W * H * 4) as usize);
        for c in 0u8..=255 {
            for k in 0u8..=255 {
                v.push(c);
                v.push(0);
                v.push(0);
                v.push(k);
            }
        }
        v
    }

    #[cfg(feature = "benchmarks")]
    #[bench]
    fn bench_predict_4x4(b: &mut Bencher) {
        let mut v = black_box(make_sample_image());

        let res_data = vec![1i32; W * H * 4];
        let modes = [
            IntraMode::TM,
            IntraMode::VE,
            IntraMode::HE,
            IntraMode::DC,
            IntraMode::LD,
            IntraMode::RD,
            IntraMode::VR,
            IntraMode::VL,
            IntraMode::HD,
            IntraMode::HU,
            IntraMode::TM,
            IntraMode::VE,
            IntraMode::HE,
            IntraMode::DC,
            IntraMode::LD,
            IntraMode::RD,
        ];

        b.iter(|| {
            black_box(predict_4x4(&mut v, W * 2, &modes, &res_data));
        });
    }

    #[cfg(feature = "benchmarks")]
    #[bench]
    fn bench_predict_bvepred(b: &mut Bencher) {
        let mut v = make_sample_image();

        b.iter(|| {
            predict_bvepred(black_box(&mut v), 5, 5, W * 2);
        });
    }

    #[cfg(feature = "benchmarks")]
    #[bench]
    fn bench_predict_bldpred(b: &mut Bencher) {
        let mut v = black_box(make_sample_image());

        b.iter(|| {
            black_box(predict_bldpred(black_box(&mut v), 5, 5, W * 2));
        });
    }

    #[cfg(feature = "benchmarks")]
    #[bench]
    fn bench_predict_brdpred(b: &mut Bencher) {
        let mut v = black_box(make_sample_image());

        b.iter(|| {
            black_box(predict_brdpred(black_box(&mut v), 5, 5, W * 2));
        });
    }

    #[cfg(feature = "benchmarks")]
    #[bench]
    fn bench_predict_bhepred(b: &mut Bencher) {
        let mut v = black_box(make_sample_image());

        b.iter(|| {
            black_box(predict_bhepred(black_box(&mut v), 5, 5, W * 2));
        });
    }

    #[cfg(feature = "benchmarks")]
    #[bench]
    fn bench_top_pixels(b: &mut Bencher) {
        let v = black_box(make_sample_image());

        b.iter(|| {
            black_box(top_pixels(black_box(&v), 5, 5, W * 2));
        });
    }

    #[cfg(feature = "benchmarks")]
    #[bench]
    fn bench_edge_pixels(b: &mut Bencher) {
        let v = black_box(make_sample_image());

        b.iter(|| {
            black_box(edge_pixels(black_box(&v), 5, 5, W * 2));
        });
    }

    #[test]
    fn test_avg2() {
        for i in 0u8..=255 {
            for j in 0u8..=255 {
                let ceil_avg = ((i as f32) + (j as f32)) / 2.0;
                let ceil_avg = ceil_avg.ceil() as u8;
                assert_eq!(
                    ceil_avg,
                    avg2(i, j),
                    "avg2({}, {}), expected {}, got {}.",
                    i,
                    j,
                    ceil_avg,
                    avg2(i, j)
                );
            }
        }
    }

    #[test]
    fn test_avg2_specific() {
        assert_eq!(
            255,
            avg2(255, 255),
            "avg2(255, 255), expected 255, got {}.",
            avg2(255, 255)
        );
        assert_eq!(1, avg2(1, 1), "avg2(1, 1), expected 1, got {}.", avg2(1, 1));
        assert_eq!(2, avg2(2, 1), "avg2(2, 1), expected 2, got {}.", avg2(2, 1));
    }

    #[test]
    fn test_avg3() {
        for i in 0u8..=255 {
            for j in 0u8..=255 {
                for k in 0u8..=255 {
                    let floor_avg = ((i as f32) + 2.0 * (j as f32) + { k as f32 } + 2.0) / 4.0;
                    let floor_avg = floor_avg.floor() as u8;
                    assert_eq!(
                        floor_avg,
                        avg3(i, j, k),
                        "avg3({}, {}, {}), expected {}, got {}.",
                        i,
                        j,
                        k,
                        floor_avg,
                        avg3(i, j, k)
                    );
                }
            }
        }
    }

    #[test]
    fn test_edge_pixels() {
        #[rustfmt::skip]
        let im = vec![5, 6, 7, 8, 9,
                      4, 0, 0, 0, 0,
                      3, 0, 0, 0, 0,
                      2, 0, 0, 0, 0,
                      1, 0, 0, 0, 0];
        let (e0, e1, e2, e3, e4, e5, e6, e7, e8) = edge_pixels(&im, 1, 1, 5);
        assert_eq!(e0, 1);
        assert_eq!(e1, 2);
        assert_eq!(e2, 3);
        assert_eq!(e3, 4);
        assert_eq!(e4, 5);
        assert_eq!(e5, 6);
        assert_eq!(e6, 7);
        assert_eq!(e7, 8);
        assert_eq!(e8, 9);
    }

    #[test]
    fn test_top_pixels() {
        #[rustfmt::skip]
        let im = vec![1, 2, 3, 4, 5, 6, 7, 8,
                                0, 0, 0, 0, 0, 0, 0, 0,
                                0, 0, 0, 0, 0, 0, 0, 0,
                                0, 0, 0, 0, 0, 0, 0, 0,
                                0, 0, 0, 0, 0, 0, 0, 0,
                                0, 0, 0, 0, 0, 0, 0, 0,
                                0, 0, 0, 0, 0, 0, 0, 0,
                                0, 0, 0, 0, 0, 0, 0, 0];
        let (e0, e1, e2, e3, e4, e5, e6, e7) = top_pixels(&im, 0, 1, 8);
        assert_eq!(e0, 1);
        assert_eq!(e1, 2);
        assert_eq!(e2, 3);
        assert_eq!(e3, 4);
        assert_eq!(e4, 5);
        assert_eq!(e5, 6);
        assert_eq!(e6, 7);
        assert_eq!(e7, 8);
    }

    #[test]
    fn test_add_residue() {
        let mut pblock = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16];
        let rblock = [
            -1, -2, -3, -4, 250, 249, 248, 250, -10, -18, -192, -17, -3, 15, 18, 9,
        ];
        let expected: [u8; 16] = [0, 0, 0, 0, 255, 255, 255, 255, 0, 0, 0, 0, 10, 29, 33, 25];

        add_residue(&mut pblock, &rblock, 0, 0, 4);

        for (&e, &i) in expected.iter().zip(&pblock) {
            assert_eq!(e, i);
        }
    }

    #[test]
    fn test_predict_bhepred() {
        #[rustfmt::skip]
        let expected: Vec<u8> = vec![5, 0, 0, 0, 0,
              4, 4, 4, 4, 4,
              3, 3, 3, 3, 3,
              2, 2, 2, 2, 2,
              1, 1, 1, 1, 1];

        #[rustfmt::skip]
        let mut im = vec![5, 0, 0, 0, 0,
                      4, 0, 0, 0, 0,
                      3, 0, 0, 0, 0,
                      2, 0, 0, 0, 0,
                      1, 0, 0, 0, 0];
        predict_bhepred(&mut im, 1, 1, 5);
        for (&e, i) in expected.iter().zip(im) {
            assert_eq!(e, i);
        }
    }

    #[test]
    fn test_predict_brdpred() {
        #[rustfmt::skip]
        let expected: Vec<u8> = vec![5, 6, 7, 8, 9,
              4, 5, 6, 7, 8,
              3, 4, 5, 6, 7,
              2, 3, 4, 5, 6,
              1, 2, 3, 4, 5];

        #[rustfmt::skip]
        let mut im = vec![5, 6, 7, 8, 9,
                      4, 0, 0, 0, 0,
                      3, 0, 0, 0, 0,
                      2, 0, 0, 0, 0,
                      1, 0, 0, 0, 0];
        predict_brdpred(&mut im, 1, 1, 5);
        for (&e, i) in expected.iter().zip(im) {
            assert_eq!(e, i);
        }
    }

    #[test]
    fn test_predict_bldpred() {
        #[rustfmt::skip]
        let mut im: Vec<u8> = vec![1, 2, 3, 4, 5, 6, 7, 8,
                                   0, 0, 0, 0, 0, 0, 0, 0,
                                   0, 0, 0, 0, 0, 0, 0, 0,
                                   0, 0, 0, 0, 0, 0, 0, 0,
                                   0, 0, 0, 0, 0, 0, 0, 0,
                                   0, 0, 0, 0, 0, 0, 0, 0,
                                   0, 0, 0, 0, 0, 0, 0, 0,
                                   0, 0, 0, 0, 0, 0, 0, 0,
                                   0, 0, 0, 0, 0, 0, 0, 0];
        let avg_1 = 2u8;
        let avg_2 = 3u8;
        let avg_3 = 4u8;
        let avg_4 = 5u8;
        let avg_5 = 6u8;
        let avg_6 = 7u8;
        let avg_7 = 8u8;

        predict_bldpred(&mut im, 0, 1, 8);

        assert_eq!(im[8], avg_1);
        assert_eq!(im[9], avg_2);
        assert_eq!(im[10], avg_3);
        assert_eq!(im[11], avg_4);
        assert_eq!(im[16], avg_2);
        assert_eq!(im[17], avg_3);
        assert_eq!(im[18], avg_4);
        assert_eq!(im[19], avg_5);
        assert_eq!(im[24], avg_3);
        assert_eq!(im[25], avg_4);
        assert_eq!(im[26], avg_5);
        assert_eq!(im[27], avg_6);
        assert_eq!(im[32], avg_4);
        assert_eq!(im[33], avg_5);
        assert_eq!(im[34], avg_6);
        assert_eq!(im[35], avg_7);
    }

    #[test]
    fn test_predict_bvepred() {
        #[rustfmt::skip]
        let mut im: Vec<u8> = vec![1, 2, 3, 4, 5, 6, 7, 8, 9,
                                   0, 0, 0, 0, 0, 0, 0, 0, 0,
                                   0, 0, 0, 0, 0, 0, 0, 0, 0,
                                   0, 0, 0, 0, 0, 0, 0, 0, 0,
                                   0, 0, 0, 0, 0, 0, 0, 0, 0,
                                   0, 0, 0, 0, 0, 0, 0, 0, 0,
                                   0, 0, 0, 0, 0, 0, 0, 0, 0,
                                   0, 0, 0, 0, 0, 0, 0, 0, 0,
                                   0, 0, 0, 0, 0, 0, 0, 0, 0];
        let avg_1 = 2u8;
        let avg_2 = 3u8;
        let avg_3 = 4u8;
        let avg_4 = 5u8;

        predict_bvepred(&mut im, 1, 1, 9);

        assert_eq!(im[10], avg_1);
        assert_eq!(im[11], avg_2);
        assert_eq!(im[12], avg_3);
        assert_eq!(im[13], avg_4);
        assert_eq!(im[19], avg_1);
        assert_eq!(im[20], avg_2);
        assert_eq!(im[21], avg_3);
        assert_eq!(im[22], avg_4);
        assert_eq!(im[28], avg_1);
        assert_eq!(im[29], avg_2);
        assert_eq!(im[30], avg_3);
        assert_eq!(im[31], avg_4);
        assert_eq!(im[37], avg_1);
        assert_eq!(im[38], avg_2);
        assert_eq!(im[39], avg_3);
        assert_eq!(im[40], avg_4);
    }
}
