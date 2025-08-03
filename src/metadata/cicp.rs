use std::sync::Arc;

/// CICP (coding independent code points) defines the colorimetric interpretation of rgb-ish color
/// components.
use crate::{
    error::{UnsupportedError, UnsupportedErrorKind},
    traits::{
        private::{LayoutWithColor, SealedPixelWithColorType},
        PixelWithColorType,
    },
    Primitive,
};

/// Reference: <https://www.itu.int/rec/T-REC-H.273-202407-I/en> (V4)
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct Cicp {
    /// Defines the exact color of red, green, blue primary colors.
    pub primaries: CicpColorPrimaries,
    /// The electro-optical transfer function (EOTF) that maps color components to linear values.
    pub transfer: CicpTransferFunction,
    /// A matrix between linear values and primary color representation.
    ///
    /// For an RGB space this is the identity matrix.
    pub matrix: CicpMatrixCoefficients,
    /// Whether the color components use all bits of the encoded values, or have headroom.
    ///
    /// You'll want to use [`CicpVideoFullRangeFlag::FullRange`] for most cases except if you need
    /// to emulate quite old TV settings. Some systems ignore this setting. `image` errors when
    /// trying to create a non-full-range transform.
    pub full_range: CicpVideoFullRangeFlag,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub(crate) struct CicpRgb {
    pub(crate) primaries: CicpColorPrimaries,
    pub(crate) transfer: CicpTransferFunction,
}

/// Defines the exact color of red, green, blue primary colors.
///
/// Each set defines the CIE 1931 XYZ (2°) color space coordinates of the primary colors and an
/// illuminant/whitepoint under which those colors are viewed.
///
/// Refer to Rec H.273 Table 2.
#[repr(u8)]
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
#[non_exhaustive]
pub enum CicpColorPrimaries {
    /// ITU-R BT.709-6
    SRgb = 1,
    /// ITU-R BT.470-6 System M
    RgbM = 4,
    /// ITU-R BT.470-6 System B, G
    RgbB = 5,
    /// SMPTE 170M
    /// functionally equivalent to 7
    Bt601 = 6,
    /// SMPTE 240M
    /// functionally equivalent to 6
    Rgb240m = 7,
    /// Generic film (colour filters using Illuminant C)
    GenericFilm = 8,
    /// Rec. ITU-R BT.2020-2
    /// Rec. ITU-R BT.2100-2
    Rgb2020 = 9,
    /// SMPTE ST 428-1
    ///
    /// (CIE 1931 XYZ as in ISO/CIE 11664-1)
    Xyz = 10,
    /// SMPTE RP 431-2
    SmpteRp431 = 11,
    /// SMPTE EG 432-1 (aka. DCI P3)
    SmpteRp432 = 12,
    /// Corresponds to value 22 but
    ///
    /// > No corresponding industry specification identified
    ///
    /// But moxcms identifies it as EBU Tech 3213-E: <https://tech.ebu.ch/docs/tech/tech3213.pdf>
    ///
    /// However, there are some differences in the second digit of red's CIE 1931 and the precision
    /// is only 2 digits whereas CICP names three; so unsure if this is fully accurate as the
    /// actual source material.
    Industry22 = 22,
}

impl CicpColorPrimaries {
    fn to_moxcms(self) -> moxcms::CicpColorPrimaries {
        use moxcms::CicpColorPrimaries as M;

        match self {
            CicpColorPrimaries::SRgb => M::Bt709,
            CicpColorPrimaries::RgbM => M::Bt470M,
            CicpColorPrimaries::RgbB => M::Bt470Bg,
            CicpColorPrimaries::Bt601 => M::Bt601,
            CicpColorPrimaries::Rgb240m => M::Smpte240,
            CicpColorPrimaries::GenericFilm => M::GenericFilm,
            CicpColorPrimaries::Rgb2020 => M::Bt2020,
            CicpColorPrimaries::Xyz => M::Xyz,
            CicpColorPrimaries::SmpteRp431 => M::Smpte431,
            CicpColorPrimaries::SmpteRp432 => M::Smpte432,
            CicpColorPrimaries::Industry22 => M::Ebu3213,
        }
    }
}

/// The transfer characteristics, expressing relation between encoded values and linear color
/// values.
///
/// Refer to Rec H.273 Table 3.
#[repr(u8)]
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
#[non_exhaustive]
pub enum CicpTransferFunction {
    /// Rec. ITU-R BT.709-6
    /// Rec. ITU-R BT.1361-0 conventional
    /// (functionally the same as the values 6, 14 and 15)
    Bt709 = 1,
    /// Rec. ITU-R BT.470-6 System M (historical)
    /// United States National Television System Committee 1953 Recommendation for transmission standards for color television
    /// United States Federal Communications Commission (2003) Title 47 Code of Federal Regulations 73.682 (a) (20)
    /// Rec. ITU-R BT.1700-0 625 PAL and 625 SECAM
    ///
    /// Assumed gamma of 2.2
    Bt470M = 4,
    /// Rec. ITU-R BT.470-6 System B, G (historical)
    Bt470BG = 5,
    /// Rec. ITU-R BT.601-7 525 or 625
    /// Rec. ITU-R BT.1358-1 525 or 625 (historical)
    /// Rec. ITU-R BT.1700-0 NTSC
    /// SMPTE ST 170 (functionally the same as the values 1, 14 and 15)
    Bt601 = 6,
    /// SMPTE ST 240
    Smpte240m = 7,
    /// Linear transfer characteristics
    Linear = 8,
    /// Logarithmic transfer characteristic (100:1 range)
    Log100 = 9,
    /// Logarithmic transfer characteristic (100 * Sqrt( 10 ) : 1 range)
    LogSqrt = 10,
    /// IEC 61966-2-4
    Iec61966_2_4 = 11,
    /// Rec. ITU-R BT.1361-0 extended colour gamut system (historical)
    Bt1361 = 12,
    /// IEC 61966-2-1 sRGB (with MatrixCoefficients equal to 0)
    /// IEC 61966-2-1 sYCC (with MatrixCoefficients equal to 5)
    SRgb = 13,
    /// Rec. ITU-R BT.2020-2 (10-bit system)
    /// (functionally the same as the values 1, 6 and 15)
    Bt2020_10bit = 14,
    /// Rec. ITU-R BT.2020-2 (12-bit system)
    /// (functionally the same as the values 1, 6 and 14)
    Bt2020_12bit = 15,
    /// SMPTE ST 2084 for 10-, 12-, 14- and 16-bit systems
    /// Rec. ITU-R BT.2100-2 perceptual quantization (PQ) system
    Smpte2084 = 16,
    /// SMPTE ST 428-1
    Smpte428 = 17,
    /// ARIB STD-B67
    /// Rec. ITU-R BT.2100-2 hybrid log- gamma (HLG) system
    Bt2100Hlg = 18,
}

impl CicpTransferFunction {
    fn to_moxcms(self) -> moxcms::TransferCharacteristics {
        use moxcms::TransferCharacteristics as T;

        match self {
            CicpTransferFunction::Bt709 => T::Bt709,
            CicpTransferFunction::Bt470M => T::Bt470M,
            CicpTransferFunction::Bt470BG => T::Bt470Bg,
            CicpTransferFunction::Bt601 => T::Bt601,
            CicpTransferFunction::Smpte240m => T::Smpte240,
            CicpTransferFunction::Linear => T::Linear,
            CicpTransferFunction::Log100 => T::Log100,
            CicpTransferFunction::LogSqrt => T::Log100sqrt10,
            CicpTransferFunction::Iec61966_2_4 => T::Iec61966,
            CicpTransferFunction::Bt1361 => T::Bt1361,
            CicpTransferFunction::SRgb => T::Srgb,
            CicpTransferFunction::Bt2020_10bit => T::Bt202010bit,
            CicpTransferFunction::Bt2020_12bit => T::Bt202012bit,
            CicpTransferFunction::Smpte2084 => T::Smpte2084,
            CicpTransferFunction::Smpte428 => T::Smpte428,
            CicpTransferFunction::Bt2100Hlg => T::Hlg,
        }
    }
}

///
/// Refer to Rec H.273 Table 4.
#[repr(u8)]
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
#[non_exhaustive]
pub enum CicpMatrixCoefficients {
    /// The identity matrix.
    /// Typically used for GBR (often referred to as RGB); however, may also be used for YZX (often referred to as XYZ);
    /// IEC 61966-2-1 sRGB
    /// SMPTE ST 428-1
    Identity = 0,
    /// Rec. ITU-R BT.709-6
    /// Rec. ITU-R BT.1361-0 conventional colour gamut system and extended colour gamut system (historical)
    /// IEC 61966-2-4 xvYCC709
    /// SMPTE RP 177 Annex B
    Bt709 = 1,
    /// United States Federal Communications Commission (2003) Title 47 Code of Federal Regulations 73.682 (a) (20)
    UsFCC = 4,
    ///  Rec. ITU-R BT.470-6 System B, G (historical)
    /// Rec. ITU-R BT.601-7 625
    /// Rec. ITU-R BT.1358-0 625 (historical)
    /// Rec. ITU-R BT.1700-0 625 PAL and 625 SECAM
    /// IEC 61966-2-1 sYCC
    /// IEC 61966-2-4 xvYCC601
    /// (functionally the same as the value 6)
    Bt470BG = 5,
    /// (functionally the same as the value 5)
    Smpte170m = 6,
    /// SMPTE ST 240
    Smpte240m = 7,
    /// YCgCo
    YCgCo = 8,
    /// Rec. ITU-R BT.2020-2 (non-constant luminance)
    /// Rec. ITU-R BT.2100-2 Y′CbCr
    Bt2020NonConstant = 9,
    /// Rec. ITU-R BT.2020-2 (constant luminance)
    Bt2020Constant = 10,
    /// SMPTE ST 2085
    Smpte2085 = 11,
    /// Chromaticity-derived non-constant luminance system
    ChromaticityDerivedNonConstant = 12,
    /// Chromaticity-derived constant luminance system
    ChromaticityDerivedConstant = 13,
    /// Rec. ITU-R BT.2100-2 ICTCp
    Bt2100 = 14,
    /// Colour representation developed in SMPTE as IPT-PQ-C2.
    IptPqC2 = 15,
    /// YCgCo with added bit-depth (2-bit).
    YCgCoRe = 16,
    /// YCgCo with added bit-depth (1-bit).
    YCgCoRo = 17,
}

impl CicpMatrixCoefficients {
    fn to_moxcms(self) -> Option<moxcms::MatrixCoefficients> {
        use moxcms::MatrixCoefficients as M;

        Some(match self {
            CicpMatrixCoefficients::Identity => M::Identity,
            CicpMatrixCoefficients::Bt709 => M::Bt709,
            CicpMatrixCoefficients::UsFCC => M::Fcc,
            CicpMatrixCoefficients::Bt470BG => M::Bt470Bg,
            CicpMatrixCoefficients::Smpte170m => M::Smpte170m,
            CicpMatrixCoefficients::Smpte240m => M::Smpte240m,
            CicpMatrixCoefficients::YCgCo => M::YCgCo,
            CicpMatrixCoefficients::Bt2020NonConstant => M::Bt2020Ncl,
            CicpMatrixCoefficients::Bt2020Constant => M::Bt2020Cl,
            CicpMatrixCoefficients::Smpte2085 => M::Smpte2085,
            CicpMatrixCoefficients::ChromaticityDerivedNonConstant => M::ChromaticityDerivedNCL,
            CicpMatrixCoefficients::ChromaticityDerivedConstant => M::ChromaticityDerivedCL,
            CicpMatrixCoefficients::Bt2100 => M::ICtCp,
            CicpMatrixCoefficients::IptPqC2
            | CicpMatrixCoefficients::YCgCoRe
            | CicpMatrixCoefficients::YCgCoRo => return None,
        })
    }
}

/// The used encoded value range.
#[repr(u8)]
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
#[non_exhaustive]
pub enum CicpVideoFullRangeFlag {
    /// The color components are encoded in a limited range, e.g., 16-235 for 8-bit. This was used
    /// for some in-band auxiliary data in the past (overswing of noisy filters to be clipped by
    /// the analog decoder), but is generally not used anymore.
    NarrowRange = 0,
    /// The color components are encoded in the full range, e.g., 0-255 for 8-bit.
    FullRange = 1,
}

/// Apply to colors of the input color space to get output color values.
///
/// We do not support all possible Cicp color spaces, but when we support one then all builtin
/// `Pixel` types can be converted with their respective components. This value is used to signify
/// that some particular combination is supported.
#[derive(Clone)]
pub struct CicpTransform {
    from: Cicp,
    into: Cicp,
    u8: RgbTransforms<u8>,
    u16: RgbTransforms<u16>,
    f32: RgbTransforms<f32>,
}

pub(crate) type CicpApplicable<'lt, C> = dyn Fn(&[C], &mut [C]) + Send + Sync + 'lt;

#[derive(Clone)]
struct RgbTransforms<C> {
    slices: [Arc<CicpApplicable<'static, C>>; 4],
}

impl CicpTransform {
    /// Construct a transform between two color spaces.
    ///
    /// Returns `Some` if the transform is guaranteed to be supported by `image`. Both color spaces
    /// are well understood and can be expected to be supported in future versions. However, we do
    /// not make guarantees about adjusting the rounding modes, accuracy, and exact numeric values
    /// used in the transform. Also, out-of-gamut colors may be handled differently per API.
    ///
    /// Returns `None` if the transformation is not (yet) supported.
    ///
    /// This is used with [`ConvertColorOptions`][`crate::ConvertColorOptions`],
    /// [`ImageBuffer::copy_from_color`][`crate::ImageBuffer::copy_from_color`].
    pub fn new(from: Cicp, into: Cicp) -> Option<Self> {
        if !from.qualify_stability() || !into.qualify_stability() {
            // To avoid regressions, we do not support all kinds of transforms from the start.
            // Instead, a selected list will be gradually enlarged as more in-depth tests are done
            // and the selected implementation library is checked for suitability in use.
            return None;
        }

        let mox_from = from.to_moxcms_profile()?;
        let mox_into = into.to_moxcms_profile()?;

        let opt = moxcms::TransformOptions::default();

        // TODO: really these should be lazy, eh?
        Some(CicpTransform {
            from,
            into,
            u8: Self::build_transforms(Self::LAYOUTS.map(|(from_layout, into_layout)| {
                let (from, from_layout) = mox_from.mox_profile(from_layout);
                let (into, into_layout) = mox_into.mox_profile(into_layout);

                from.create_transform_8bit(from_layout, into, into_layout, opt)
                    .map_err(|e| {
                        eprintln!("Error creating transform {from_layout:?}->{into_layout:?}: {e}")
                    })
                    .ok()
            }))?,
            u16: Self::build_transforms(Self::LAYOUTS.map(|(from_layout, into_layout)| {
                let (from, from_layout) = mox_from.mox_profile(from_layout);
                let (into, into_layout) = mox_into.mox_profile(into_layout);

                from.create_transform_16bit(from_layout, into, into_layout, opt)
                    .ok()
            }))?,
            f32: Self::build_transforms(Self::LAYOUTS.map(|(from_layout, into_layout)| {
                let (from, from_layout) = mox_from.mox_profile(from_layout);
                let (into, into_layout) = mox_into.mox_profile(into_layout);

                from.create_transform_f32(from_layout, into, into_layout, opt)
                    .ok()
            }))?,
        })
    }

    /// For a Pixel with known color layout (`ColorType`) get a transform that is accurate.
    ///
    /// This returns `None` if we do not support the transform. At writing that is true for
    /// instance for transforms involved 'Luma` pixels which are interpreted as the `Y` in a
    /// `YCbCr` color based off the actual whitepoint, with coefficients according to each
    /// primary's luminance. Only Rgb transforms are supported via `moxcms`.
    ///
    /// Maybe provide publicly?
    pub(crate) fn supported_transform_fn<From: PixelWithColorType, Into: PixelWithColorType>(
        &self,
    ) -> Result<&'_ CicpApplicable<'_, From::Subpixel>, UnsupportedError> {
        use crate::traits::private::{double_dispatch_transform_from_sealed, PrivateToken};

        if !matches!(
            From::layout(PrivateToken),
            LayoutWithColor::Rgb | LayoutWithColor::Rgba
        ) {
            return Err(UnsupportedError::from_format_and_kind(
                crate::error::ImageFormatHint::Unknown,
                UnsupportedErrorKind::ColorLayout(From::COLOR_TYPE),
            ));
        }

        if !matches!(
            Into::layout(PrivateToken),
            LayoutWithColor::Rgb | LayoutWithColor::Rgba
        ) {
            return Err(UnsupportedError::from_format_and_kind(
                crate::error::ImageFormatHint::Unknown,
                UnsupportedErrorKind::ColorLayout(Into::COLOR_TYPE),
            ));
        }

        Ok(double_dispatch_transform_from_sealed::<From, Into>(self))
    }

    /// Does this transform realize the conversion `from` to `into`.
    pub(crate) fn is_applicable(&self, from: Cicp, into: Cicp) -> bool {
        self.from == from && self.into == into
    }

    fn build_transforms<P: Copy + Default + Primitive + 'static>(
        trs: [Option<Box<dyn moxcms::TransformExecutor<P> + Send + Sync>>; 4],
    ) -> Option<RgbTransforms<P>> {
        // We would use `[array]::try_map` here, but it is not stable yet.
        if trs.iter().any(Option::is_none) {
            return None;
        }

        let trs = trs.map(Option::unwrap);

        Some(RgbTransforms {
            slices: trs.map(|tr| {
                Arc::new(move |input: &[P], output: &mut [P]| {
                    tr.transform(input, output).expect("transform failed")
                }) as Arc<dyn Fn(&[P], &mut [P]) + Send + Sync>
            }),
        })
    }

    // Note on this design: When we dispatch into this function, we have a `Self` type that is
    // qualified to have the appropriate bound here. However, for the target type of the transform
    // we have, e.g., `Rgba<Self::Subpixel>`. Now we know that these are also with color for the
    // most part but we can not convince the compiler (indeed, there is or was an asymmetry with
    // gray pixels where they do not have float equivalents). It is hence necessary to provide the
    // output layout as a runtime parameter, not a compile-time type.
    pub(crate) fn select_transform_u8<P: SealedPixelWithColorType<TransformableSubpixel = u8>>(
        &self,
        into: LayoutWithColor,
    ) -> &Arc<CicpApplicable<'static, u8>> {
        &self.u8.slices[Self::select_transform_index::<P>(into)]
    }

    pub(crate) fn select_transform_u16<O: SealedPixelWithColorType<TransformableSubpixel = u16>>(
        &self,
        into: LayoutWithColor,
    ) -> &Arc<CicpApplicable<'static, u16>> {
        &self.u16.slices[Self::select_transform_index::<O>(into)]
    }

    pub(crate) fn select_transform_f32<O: SealedPixelWithColorType<TransformableSubpixel = f32>>(
        &self,
        into: LayoutWithColor,
    ) -> &Arc<CicpApplicable<'static, f32>> {
        &self.f32.slices[Self::select_transform_index::<O>(into)]
    }

    const LAYOUTS: [(LayoutWithColor, LayoutWithColor); 4] = [
        (LayoutWithColor::Rgb, LayoutWithColor::Rgb),
        (LayoutWithColor::Rgb, LayoutWithColor::Rgba),
        (LayoutWithColor::Rgba, LayoutWithColor::Rgb),
        (LayoutWithColor::Rgba, LayoutWithColor::Rgba),
    ];

    fn select_transform_index<O: SealedPixelWithColorType>(into: LayoutWithColor) -> usize {
        use crate::traits::private::{LayoutWithColor as Layout, PrivateToken};
        let from = O::layout(PrivateToken);

        match (from, into) {
            (Layout::Rgb, Layout::Rgb) => 0,
            (Layout::Rgb, Layout::Rgba) => 1,
            (Layout::Rgba, Layout::Rgb) => 2,
            (Layout::Rgba, Layout::Rgba) => 3,
            _ => unreachable!("Unsupported CicpTransform layout {from:?} to {into:?}"),
        }
    }
}

impl Cicp {
    /// The sRGB color space, BT.709 transfer function and D65 whitepoint.
    pub const SRGB: Self = Cicp {
        primaries: CicpColorPrimaries::SRgb,
        transfer: CicpTransferFunction::SRgb,
        matrix: CicpMatrixCoefficients::Identity,
        full_range: CicpVideoFullRangeFlag::FullRange,
    };

    /// SRGB primaries and whitepoint with linear samples.
    pub const SRGB_LINEAR: Self = Cicp {
        primaries: CicpColorPrimaries::SRgb,
        transfer: CicpTransferFunction::Linear,
        matrix: CicpMatrixCoefficients::Identity,
        full_range: CicpVideoFullRangeFlag::FullRange,
    };

    /// The  Display-P3 color space, a wide-gamut choice with SMPTE RP 432-2 primaries.
    pub const DISPLAY_P3: Self = Cicp {
        primaries: CicpColorPrimaries::SmpteRp432,
        transfer: CicpTransferFunction::SRgb,
        matrix: CicpMatrixCoefficients::Identity,
        full_range: CicpVideoFullRangeFlag::FullRange,
    };

    fn to_moxcms_profile(self) -> Option<RgbGrayProfile> {
        let mut rgb = moxcms::ColorProfile::new_srgb();

        rgb.update_rgb_colorimetry_from_cicp(moxcms::CicpProfile {
            color_primaries: self.primaries.to_moxcms(),
            transfer_characteristics: self.transfer.to_moxcms(),
            matrix_coefficients: self.matrix.to_moxcms()?,
            full_range: match self.full_range {
                CicpVideoFullRangeFlag::NarrowRange => false,
                CicpVideoFullRangeFlag::FullRange => true,
            },
        });

        let gray = {
            // I'd use record update syntax but there's a private field.
            let mut p = rgb.clone();
            p.color_space = moxcms::DataColorSpace::YCbr;
            // Our luma models a Y component of a YCbCr color space. It turns out that ICC V4 does
            // not support pure Luma in any other whitepoint apart from D50 (the native profile
            // connection space). The use of a grayTRC does *not* take the chromatic adaptation
            // matrix into account. Of course we can encode the adaptation into the TRC as a
            // coefficient, the Y component of the product of the whitepoint adaptation matrix
            // inverse and the pcs's whitepoint XYZ, but that is only correct for gray -> gray
            // conversion (and that coefficient should generally be `1`).
            //
            // Hence we use a YCbCr. The data->pcs path could be modelled by ("M" curves, matrix,
            // "B" curves) where B curves are all the identity, which is a subset of the
            // capabilities that a lutAToBType allows. Unfortunately, this is not implemented in
            // moxcms yet and for efficiency we would like to have a masked `create_transform_*` in
            // which the CbCr channels are discarded / assumed 0 instead of them being in memory.
            p
        };

        Some(RgbGrayProfile { rgb, gray })
    }

    /// Whether we have invested enough testing to ensure that color values can be assumed to be
    /// stable and correspond to an intended effect, in particular if there even is a well-defined
    /// meaning to these color spaces.
    ///
    /// For instance, our current code for the 'luma' equivalent space assumes that the color space
    /// has a shared transfer function for all its color components. Also the judgment should not
    /// depend on whether we can represent the profile in `moxcms` but rather if we understand the
    /// profile well enough so that conversion implemented through another library can be derived.
    /// (Consider the case of a builtin transform-while-encoding that may be more performant for a
    /// format that does not support CICP or ICC profiles.)
    pub(crate) const fn qualify_stability(&self) -> bool {
        const _: () = {
            // Out public constants _should_ be stable.
            assert!(Cicp::SRGB.qualify_stability());
            assert!(Cicp::SRGB_LINEAR.qualify_stability());
            assert!(Cicp::DISPLAY_P3.qualify_stability());
        };

        matches!(self.full_range, CicpVideoFullRangeFlag::FullRange)
            && matches!(self.matrix, CicpMatrixCoefficients::Identity)
            && matches!(
                self.primaries,
                CicpColorPrimaries::SRgb
                    | CicpColorPrimaries::SmpteRp432
                    | CicpColorPrimaries::Bt601
                    | CicpColorPrimaries::Rgb240m
            )
            && matches!(
                self.transfer,
                CicpTransferFunction::SRgb
                    | CicpTransferFunction::Bt709
                    | CicpTransferFunction::Bt601
                    | CicpTransferFunction::Linear
            )
    }

    /// Discard matrix and range information.
    pub(crate) const fn into_rgb(self) -> CicpRgb {
        CicpRgb {
            primaries: self.primaries,
            transfer: self.transfer,
        }
    }
}

impl From<CicpRgb> for Cicp {
    fn from(cicp: CicpRgb) -> Self {
        Cicp {
            primaries: cicp.primaries,
            transfer: cicp.transfer,
            matrix: CicpMatrixCoefficients::Identity,
            full_range: CicpVideoFullRangeFlag::FullRange,
        }
    }
}

/// An RGB profile with its related (same tone-mapping) gray profile.
struct RgbGrayProfile {
    rgb: moxcms::ColorProfile,
    gray: moxcms::ColorProfile,
}

impl RgbGrayProfile {
    fn mox_profile(&self, layout: LayoutWithColor) -> (&moxcms::ColorProfile, moxcms::Layout) {
        match layout {
            LayoutWithColor::Rgb => (&self.rgb, moxcms::Layout::Rgb),
            LayoutWithColor::Rgba => (&self.rgb, moxcms::Layout::Rgba),
            LayoutWithColor::Luma => (&self.gray, moxcms::Layout::Gray),
            LayoutWithColor::LumaAlpha => (&self.gray, moxcms::Layout::GrayAlpha),
        }
    }
}

#[cfg(test)]
#[test]
fn moxcms() {
    let l = moxcms::TransferCharacteristics::Linear;
    assert_eq!(l.linearize(1.0), 1.0);
    assert_eq!(l.gamma(1.0), 1.0);

    assert_eq!(l.gamma(0.5), 0.5);
}

#[cfg(test)]
// Disabled tests for now, but keeping the reference values. Since pixel conversion is typed based
// we _should_ support all our primary `ColorType` options before offering this, currently Luma is
// difficult. Otherwise we could also refine `PixelWithColorType` but it is already sealed and
// internal and will, probably, quickly become highly confusing.
#[cfg(any())]
mod test_pixels {
    use super::{Cicp, CicpTransform};
    use crate::{Luma, Pixel, Rgba};

    #[test]
    fn can_create_transforms() {
        assert!(CicpTransform::new(Cicp::SRGB, Cicp::SRGB).is_some());
        assert!(CicpTransform::new(Cicp::SRGB, Cicp::DISPLAY_P3).is_some());
        assert!(CicpTransform::new(Cicp::DISPLAY_P3, Cicp::SRGB).is_some());
        assert!(CicpTransform::new(Cicp::DISPLAY_P3, Cicp::DISPLAY_P3).is_some());
    }

    #[test]
    fn transform_rgb() {
        let tr = CicpTransform::new(Cicp::SRGB, Cicp::DISPLAY_P3).unwrap();
        let p3 = Rgba::<u8>([255, 0, 0, 255]).to_rgba_with(&tr);
        // Validated by python colour-science:
        // colour.RGB_to_RGB(
        //   [1.0, 0.0, 0.0],
        //   colour.RGB_COLOURSPACES['sRGB'],
        //   colour.RGB_COLOURSPACES['Display P3'],
        //   apply_cctf_encoding=True,
        //   apply_cctf_decoding=True) * 255
        assert_eq!(p3.0, [234, 51, 35, 255]);
    }

    #[test]
    fn transform_luma() {
        let tr = CicpTransform::new(Cicp::SRGB, Cicp::SRGB).unwrap();
        // _, Y, _ = colour.RGB_to_XYZ(
        //   [1.0, 0.0, 0.0],
        //   colour.RGB_COLOURSPACES['sRGB'],
        //   apply_cctf_decoding=True)
        // colour.RGB_COLOURSPACES['sRGB']._cctf_encoding(Y)*255
        let luma = Rgba::<u8>([255, 0, 0, 255]).to_luma_with(&tr);
        assert_eq!(luma.0, [130u8]); // reference: 127.1021805160301

        let luma = Rgba::<u8>([0, 255, 0, 255]).to_luma_with(&tr);
        assert_eq!(luma.0, [220u8]); // reference: 219.93274897493274

        let luma = Rgba::<u8>([0, 0, 255, 255]).to_luma_with(&tr);
        assert_eq!(luma.0, [70u8]); // reference: 75.962697358369013
    }

    #[test]
    fn transform_luma_to_rgba() {
        let tr = CicpTransform::new(Cicp::DISPLAY_P3, Cicp::SRGB).unwrap();
        let srgb = Luma::<u8>([128]).to_rgb_with(&tr);
        // All of them using the same transfer function so this is unsurprising
        assert_eq!(srgb.0, [128, 128, 128]);
    }
}
