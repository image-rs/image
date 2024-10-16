use std::fmt;

static LOOKUP_TABLE_3_BIT_TO_8_BIT: [u8; 8] = [0, 36, 73, 109, 146, 182, 219, 255];
static LOOKUP_TABLE_4_BIT_TO_8_BIT: [u8; 16] = [
    0, 17, 34, 51, 68, 85, 102, 119, 136, 153, 170, 187, 204, 221, 238, 255,
];
static LOOKUP_TABLE_5_BIT_TO_8_BIT: [u8; 32] = [
    0, 8, 16, 25, 33, 41, 49, 58, 66, 74, 82, 90, 99, 107, 115, 123, 132, 140, 148, 156, 165, 173,
    181, 189, 197, 206, 214, 222, 230, 239, 247, 255,
];
static LOOKUP_TABLE_6_BIT_TO_8_BIT: [u8; 64] = [
    0, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 45, 49, 53, 57, 61, 65, 69, 73, 77, 81, 85, 89, 93,
    97, 101, 105, 109, 113, 117, 121, 125, 130, 134, 138, 142, 146, 150, 154, 158, 162, 166, 170,
    174, 178, 182, 186, 190, 194, 198, 202, 206, 210, 215, 219, 223, 227, 231, 235, 239, 243, 247,
    251, 255,
];

/// All errors that can occur when working with packed bitmaps
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum BitfieldError {
    /// Bitfield (of the specified width – 16- or 32-bit) masks not present
    MasksMissing(u32),

    /// The bitfield mask interleaves set and unset bits
    MaskNonContiguous,
    /// Bitfield mask invalid (e.g. too long for specified type)
    MaskInvalid,
    /// Bitfield (of the specified width – 16- or 32-bit) mask not present
    MaskMissing(u32),
}

impl fmt::Display for BitfieldError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BitfieldError::MasksMissing(bb) => {
                f.write_fmt(format_args!("Missing {bb}-bit bitfield masks"))
            }
            BitfieldError::MaskNonContiguous => f.write_str("Non-contiguous bitfield mask"),
            BitfieldError::MaskInvalid => f.write_str("Invalid bitfield mask"),
            BitfieldError::MaskMissing(bb) => {
                f.write_fmt(format_args!("Missing {bb}-bit bitfield mask"))
            }
        }
    }
}

#[derive(PartialEq, Eq)]
pub(crate) struct Bitfield {
    pub(crate) shift: u32,
    pub(crate) len: u32,
}

impl Bitfield {
    fn from_mask(mask: u32, max_len: u32) -> Result<Bitfield, BitfieldError> {
        if mask == 0 {
            return Ok(Bitfield { shift: 0, len: 0 });
        }
        let mut shift = mask.trailing_zeros();
        let mut len = (!(mask >> shift)).trailing_zeros();
        if len != mask.count_ones() {
            return Err(BitfieldError::MaskNonContiguous);
        }
        if len + shift > max_len {
            return Err(BitfieldError::MaskInvalid);
        }
        if len > 8 {
            shift += len - 8;
            len = 8;
        }
        Ok(Bitfield { shift, len })
    }

    pub(crate) fn read(&self, data: u32) -> u8 {
        let data = data >> self.shift;
        match self.len {
            1 => ((data & 0b1) * 0xff) as u8,
            2 => ((data & 0b11) * 0x55) as u8,
            3 => LOOKUP_TABLE_3_BIT_TO_8_BIT[(data & 0b00_0111) as usize],
            4 => LOOKUP_TABLE_4_BIT_TO_8_BIT[(data & 0b00_1111) as usize],
            5 => LOOKUP_TABLE_5_BIT_TO_8_BIT[(data & 0b01_1111) as usize],
            6 => LOOKUP_TABLE_6_BIT_TO_8_BIT[(data & 0b11_1111) as usize],
            7 => ((data & 0x7f) << 1 | (data & 0x7f) >> 6) as u8,
            8 => (data & 0xff) as u8,
            _ => panic!(),
        }
    }
}

#[derive(PartialEq, Eq)]
pub(crate) struct Bitfields {
    pub(crate) r: Bitfield,
    pub(crate) g: Bitfield,
    pub(crate) b: Bitfield,
    pub(crate) a: Bitfield,
}

impl Bitfields {
    pub(crate) fn from_mask(
        r_mask: u32,
        g_mask: u32,
        b_mask: u32,
        a_mask: u32,
        max_len: u32,
    ) -> Result<Bitfields, BitfieldError> {
        let bitfields = Bitfields {
            r: Bitfield::from_mask(r_mask, max_len)?,
            g: Bitfield::from_mask(g_mask, max_len)?,
            b: Bitfield::from_mask(b_mask, max_len)?,
            a: Bitfield::from_mask(a_mask, max_len)?,
        };
        if bitfields.r.len == 0 || bitfields.g.len == 0 || bitfields.b.len == 0 {
            return Err(BitfieldError::MaskMissing(max_len));
        }
        Ok(bitfields)
    }
}
