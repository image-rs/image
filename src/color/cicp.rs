use std::sync::Arc;

/// CICP (coding independent code points) defines the colorimetric interpretation of rgb-ish color
/// components.
use crate::{
    traits::private::{LayoutWithColor, SealedPixelWithColorType},
    Primitive,
};

/// Reference: <https://www.itu.int/rec/T-REC-H.273-202407-I/en> (V4)
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct Cicp {
    pub primaries: CicpColorPrimaries,
    pub transfer: CicpTransferFunction,
    pub matrix: CicpMatrixCoefficients,
    pub full_range: CicpVideoFullRangeFlag,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub(crate) struct CicpRgb {
    pub(crate) primaries: CicpColorPrimaries,
    pub(crate) transfer: CicpTransferFunction,
}

#[repr(u8)]
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
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
    fn to_moxcms(&self) -> moxcms::CicpColorPrimaries {
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

#[repr(u8)]
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub enum CicpTransferFunction {
    Bt709 = 1,
    Bt470M = 4,
    Bt470BG = 5,
    Bt601 = 6,
    Smpte240m = 7,
    Linear = 8,
    Log100 = 9,
    LogSqrt = 10,
    Iec61966_2_4 = 11,
    Bt1361 = 12,
    SRgb = 13,
    Bt2020_10bit = 14,
    Bt2020_12bit = 15,
    Smpte2084 = 16,
    Smpte428 = 17,
    Bt2100Hlg = 18,
}

impl CicpTransferFunction {
    fn to_moxcms(&self) -> moxcms::TransferCharacteristics {
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

#[repr(u8)]
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub enum CicpMatrixCoefficients {
    Identity = 0,
    Bt709 = 1,
    Unspecified = 2,
    UsFCC = 4,
    Bt470BG = 5,
    Smpte170m = 6,
    Smpte240m = 7,
    YCgCo = 8,
    Bt2020NonConstant = 9,
    Bt2020Constant = 10,
    Smpte2085 = 11,
    ChromaticityDerivedNonConstant = 12,
    ChromaticityDerivedConstant = 13,
    Bt2100 = 14,
    IptPqC2 = 15,
    /// YCgCo with added bit-depth (2-bit).
    YCgCoRe = 16,
    /// YCgCo with added bit-depth (1-bit).
    YCgCoRo = 17,
}

impl CicpMatrixCoefficients {
    fn to_moxcms(&self) -> Option<moxcms::MatrixCoefficients> {
        use moxcms::MatrixCoefficients as M;

        Some(match self {
            CicpMatrixCoefficients::Identity => M::Identity,
            CicpMatrixCoefficients::Bt709 => M::Bt709,
            CicpMatrixCoefficients::Unspecified => M::Unspecified,
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

#[repr(u8)]
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub enum CicpVideoFullRangeFlag {
    NarrowRange = 0,
    FullRange = 1,
}

/// Apply to colors of the input color space to get output color values.
///
/// We do not support all possible Cicp color spaces, but when we support one then all builtin
/// `Pixel` types can be converted with their respective components. This value is used to signify
/// that some particular combination is supported.
pub struct CicpTransform {
    from: Cicp,
    into: Cicp,
    u8: RgbTransforms<u8>,
    u16: RgbTransforms<u16>,
    f32: RgbTransforms<f32>,
}

struct RgbTransforms<C> {
    slices: [Arc<dyn Fn(&[C], &mut [C]) + Send + Sync>; 4 * 4],
}

impl CicpTransform {
    const LAYOUTS: [(LayoutWithColor, LayoutWithColor); 16] = [
        (LayoutWithColor::Rgb, LayoutWithColor::Rgb),
        (LayoutWithColor::Rgb, LayoutWithColor::Rgba),
        (LayoutWithColor::Rgb, LayoutWithColor::Gray),
        (LayoutWithColor::Rgb, LayoutWithColor::GrayAlpha),
        (LayoutWithColor::Rgba, LayoutWithColor::Rgb),
        (LayoutWithColor::Rgba, LayoutWithColor::Rgba),
        (LayoutWithColor::Rgba, LayoutWithColor::Gray),
        (LayoutWithColor::Rgba, LayoutWithColor::GrayAlpha),
        (LayoutWithColor::Gray, LayoutWithColor::Rgb),
        (LayoutWithColor::Gray, LayoutWithColor::Rgba),
        (LayoutWithColor::Gray, LayoutWithColor::Gray),
        (LayoutWithColor::Gray, LayoutWithColor::GrayAlpha),
        (LayoutWithColor::GrayAlpha, LayoutWithColor::Rgb),
        (LayoutWithColor::GrayAlpha, LayoutWithColor::Rgba),
        (LayoutWithColor::GrayAlpha, LayoutWithColor::Gray),
        (LayoutWithColor::GrayAlpha, LayoutWithColor::GrayAlpha),
    ];

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

                from.create_transform_8bit(from_layout, &into, into_layout, opt)
                    .map_err(|e| {
                        eprintln!("Error creating transform {from_layout:?}->{into_layout:?}: {e}")
                    })
                    .ok()
            }))?,
            u16: Self::build_transforms(Self::LAYOUTS.map(|(from_layout, into_layout)| {
                let (from, from_layout) = mox_from.mox_profile(from_layout);
                let (into, into_layout) = mox_into.mox_profile(into_layout);

                from.create_transform_16bit(from_layout, &into, into_layout, opt)
                    .ok()
            }))?,
            f32: Self::build_transforms(Self::LAYOUTS.map(|(from_layout, into_layout)| {
                let (from, from_layout) = mox_from.mox_profile(from_layout);
                let (into, into_layout) = mox_into.mox_profile(into_layout);

                from.create_transform_f32(from_layout, &into, into_layout, opt)
                    .ok()
            }))?,
        })
    }

    /// Does this transform realize the conversion `from` to `into`.
    pub(crate) fn is_applicable(&self, from: Cicp, into: Cicp) -> bool {
        self.from == from && self.into == into
    }

    fn build_transforms<P: Copy + Default + Primitive + 'static>(
        trs: [Option<Box<dyn moxcms::TransformExecutor<P> + Send + Sync>>; 16],
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
    ) -> &Arc<dyn Fn(&[u8], &mut [u8]) + Send + Sync> {
        &self.u8.slices[Self::select_transform_index::<P>(into)]
    }

    pub(crate) fn select_transform_u16<O: SealedPixelWithColorType<TransformableSubpixel = u16>>(
        &self,
        into: LayoutWithColor,
    ) -> &Arc<dyn Fn(&[u16], &mut [u16]) + Send + Sync> {
        &self.u16.slices[Self::select_transform_index::<O>(into)]
    }

    pub(crate) fn select_transform_f32<O: SealedPixelWithColorType<TransformableSubpixel = f32>>(
        &self,
        into: LayoutWithColor,
    ) -> &Arc<dyn Fn(&[f32], &mut [f32]) + Send + Sync> {
        &self.f32.slices[Self::select_transform_index::<O>(into)]
    }

    fn select_transform_index<O: SealedPixelWithColorType>(into: LayoutWithColor) -> usize {
        use crate::traits::private::{LayoutWithColor as Layout, PrivateToken};

        match (O::layout(PrivateToken), into) {
            (Layout::Rgb, Layout::Rgb) => 0,
            (Layout::Rgb, Layout::Rgba) => 1,
            (Layout::Rgb, Layout::Gray) => 2,
            (Layout::Rgb, Layout::GrayAlpha) => 3,
            (Layout::Rgba, Layout::Rgb) => 4,
            (Layout::Rgba, Layout::Rgba) => 5,
            (Layout::Rgba, Layout::Gray) => 6,
            (Layout::Rgba, Layout::GrayAlpha) => 7,
            (Layout::Gray, Layout::Rgb) => 8,
            (Layout::Gray, Layout::Rgba) => 9,
            (Layout::Gray, Layout::Gray) => 10,
            (Layout::Gray, Layout::GrayAlpha) => 11,
            (Layout::GrayAlpha, Layout::Rgb) => 12,
            (Layout::GrayAlpha, Layout::Rgba) => 13,
            (Layout::GrayAlpha, Layout::Gray) => 14,
            (Layout::GrayAlpha, Layout::GrayAlpha) => 15,
        }
    }
}

impl Cicp {
    pub const SRGB: Self = Cicp {
        primaries: CicpColorPrimaries::SRgb,
        transfer: CicpTransferFunction::SRgb,
        matrix: CicpMatrixCoefficients::Identity,
        full_range: CicpVideoFullRangeFlag::FullRange,
    };

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
            let mut p = moxcms::ColorProfile::new_gray_with_gamma(1.0);
            p.profile_class = moxcms::ProfileClass::DisplayDevice;
            p.rendering_intent = moxcms::RenderingIntent::Perceptual;
            p.pcs = moxcms::DataColorSpace::Xyz;
            // Using red; but assuming all channels have the same TRC.
            p.gray_trc = rgb.red_trc.clone();
            // I think this is a bug? The build_gamma_table uses (moxcms =0.7.1)
            //
            // let gray_gamma = dst_pr.build_gamma_table =:<T; 65536, GAMMA_CAP, BIT_DEPTH>(
            //     &dst_pr.red_trc;
            //     options.allow_use_cicp_transfer;
            // )?;
            //
            // This does not make much sense.
            p.red_trc = rgb.red_trc.clone();
            p.media_white_point = rgb.media_white_point;
            p.white_point = rgb.white_point;
            p.chromatic_adaptation = rgb.chromatic_adaptation;
            p
        };

        Some(RgbGrayProfile { rgb, gray })
    }

    pub(crate) const fn qualify_stability(&self) -> bool {
        const _: () = {
            // Out public constants _should_ be stable.
            assert!(Cicp::SRGB.qualify_stability());
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
            LayoutWithColor::Gray => (&self.gray, moxcms::Layout::Gray),
            LayoutWithColor::GrayAlpha => (&self.gray, moxcms::Layout::GrayAlpha),
        }
    }
}

#[cfg(test)]
mod tests {
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
        assert_eq!(p3.0, [234, 51, 35, 255]);
    }
}
