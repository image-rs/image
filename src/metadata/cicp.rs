use std::sync::Arc;

/// CICP (coding independent code points) defines the colorimetric interpretation of rgb-ish color
/// components.
use crate::{
    traits::{
        private::{LayoutWithColor, SealedPixelWithColorType},
        PixelWithColorType,
    },
    ColorType, DynamicImage,
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

/// An internal representation of what our `T: PixelWithColorType` can do, i.e. ImageBuffer.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub(crate) struct CicpRgb {
    pub(crate) primaries: CicpColorPrimaries,
    pub(crate) transfer: CicpTransferFunction,
    pub(crate) luminance: DerivedLuminance,
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
    /// SMPTE RP 431-2 (aka. DCI P3)
    SmpteRp431 = 11,
    /// SMPTE EG 432-1, DCI P3 variant with the D65 whitepoint (matching sRGB and BT.2020)
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

#[repr(u8)]
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub(crate) enum DerivedLuminance {
    /// Luminance is calculated in linear space:
    ///     Y' = dot(K_rgb, RGB)'
    #[allow(dead_code)] // We do not support this yet but should prepare call sites for the
    // eventuality.
    Constant,
    /// Luminance is calculated in the transferred space:
    ///     Y' = dot(K_rgb, RGB')
    NonConstant,
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
    input_coefs: [f32; 3],
    output_coefs: [f32; 3],
}

pub(crate) type CicpApplicable<'lt, C> = dyn Fn(&[C], &mut [C]) + Send + Sync + 'lt;

#[derive(Clone)]
struct RgbTransforms<C> {
    slices: [Arc<CicpApplicable<'static, C>>; 4],
    luma_rgb: [Arc<CicpApplicable<'static, C>>; 4],
    rgb_luma: [Arc<CicpApplicable<'static, C>>; 4],
    luma_luma: [Arc<CicpApplicable<'static, C>>; 4],
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

        let input_coefs = from.into_rgb().derived_luminance()?;
        let output_coefs = into.into_rgb().derived_luminance()?;

        let mox_from = from.to_moxcms_profile()?;
        let mox_into = into.to_moxcms_profile()?;

        let opt = moxcms::TransformOptions::default();

        let f32_fallback = {
            let try_f32 = Self::LAYOUTS.map(|(from_layout, into_layout)| {
                let (from, from_layout) = mox_from.mox_profile(from_layout);
                let (into, into_layout) = mox_into.mox_profile(into_layout);

                from.create_transform_f32(from_layout, into, into_layout, opt)
                    .ok()
            });

            if try_f32.iter().any(Option::is_none) {
                return None;
            }

            try_f32
                .map(Option::unwrap)
                .map(Arc::<dyn moxcms::TransformExecutor<f32> + Send + Sync>::from)
        };

        // TODO: really these should be lazy, eh?
        Some(CicpTransform {
            from,
            into,
            u8: Self::build_transforms(
                Self::LAYOUTS.map(|(from_layout, into_layout)| {
                    let (from, from_layout) = mox_from.mox_profile(from_layout);
                    let (into, into_layout) = mox_into.mox_profile(into_layout);

                    from.create_transform_8bit(from_layout, into, into_layout, opt)
                        .ok()
                }),
                f32_fallback.clone(),
                input_coefs,
                output_coefs,
            )?,
            u16: Self::build_transforms(
                Self::LAYOUTS.map(|(from_layout, into_layout)| {
                    let (from, from_layout) = mox_from.mox_profile(from_layout);
                    let (into, into_layout) = mox_into.mox_profile(into_layout);

                    from.create_transform_16bit(from_layout, into, into_layout, opt)
                        .ok()
                }),
                f32_fallback.clone(),
                input_coefs,
                output_coefs,
            )?,
            f32: Self::build_transforms(
                Self::LAYOUTS.map(|(from_layout, into_layout)| {
                    let (from, from_layout) = mox_from.mox_profile(from_layout);
                    let (into, into_layout) = mox_into.mox_profile(into_layout);

                    from.create_transform_f32(from_layout, into, into_layout, opt)
                        .ok()
                }),
                f32_fallback.clone(),
                input_coefs,
                output_coefs,
            )?,
            input_coefs,
            output_coefs,
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
    ) -> &'_ CicpApplicable<'_, From::Subpixel> {
        use crate::traits::private::double_dispatch_transform_from_sealed;
        double_dispatch_transform_from_sealed::<From, Into>(self)
    }

    /// Does this transform realize the conversion `from` to `into`.
    pub(crate) fn is_applicable(&self, from: Cicp, into: Cicp) -> bool {
        self.from == from && self.into == into
    }

    fn build_transforms<P: ColorComponentForCicp + Default + 'static>(
        trs: [Option<Box<dyn moxcms::TransformExecutor<P> + Send + Sync>>; 4],
        f32: [Arc<dyn moxcms::TransformExecutor<f32> + Send + Sync>; 4],
        input_coef: [f32; 3],
        output_coef: [f32; 3],
    ) -> Option<RgbTransforms<P>> {
        // We would use `[array]::try_map` here, but it is not stable yet.
        if trs.iter().any(Option::is_none) {
            return None;
        }

        let trs = trs
            .map(Option::unwrap)
            .map(Arc::<dyn moxcms::TransformExecutor<P> + Send + Sync>::from);

        // rgb-rgb transforms are done directly via moxcms.
        let slices = trs.clone().map(|tr| {
            Arc::new(move |input: &[P], output: &mut [P]| {
                tr.transform(input, output).expect("transform failed")
            }) as Arc<dyn Fn(&[P], &mut [P]) + Send + Sync>
        });

        const N: usize = 150;

        // luma-rgb transforms expand the Luma to Rgb (and LumaAlpha to Rgba)
        let luma_rgb = {
            let [tr33, tr34, tr43, tr44] = f32.clone();

            [
                Arc::new(move |input: &[P], output: &mut [P]| {
                    let mut ibuffer = [0.0f32; 3 * N];
                    let mut obuffer = [0.0f32; 3 * N];

                    for (luma, output) in input.chunks(N).zip(output.chunks_mut(3 * N)) {
                        let n = luma.len();
                        let ibuffer = &mut ibuffer[..3 * n];
                        let obuffer = &mut obuffer[..3 * n];
                        Self::expand_luma_rgb(luma, ibuffer, input_coef);
                        tr33.transform(ibuffer, obuffer).expect("transform failed");
                        Self::clamp_rgb(obuffer, output);
                    }
                }) as Arc<dyn Fn(&[P], &mut [P]) + Send + Sync>,
                Arc::new(move |input: &[P], output: &mut [P]| {
                    let mut ibuffer = [0.0f32; 3 * N];
                    let mut obuffer = [0.0f32; 4 * N];

                    for (luma, output) in input.chunks(N).zip(output.chunks_mut(4 * N)) {
                        let n = luma.len();
                        let ibuffer = &mut ibuffer[..3 * n];
                        let obuffer = &mut obuffer[..4 * n];
                        Self::expand_luma_rgb(luma, ibuffer, input_coef);
                        tr34.transform(ibuffer, obuffer).expect("transform failed");
                        Self::clamp_rgba(obuffer, output);
                    }
                }) as Arc<dyn Fn(&[P], &mut [P]) + Send + Sync>,
                Arc::new(move |input: &[P], output: &mut [P]| {
                    let mut ibuffer = [0.0f32; 4 * N];
                    let mut obuffer = [0.0f32; 3 * N];

                    for (luma, output) in input.chunks(2 * N).zip(output.chunks_mut(3 * N)) {
                        let n = luma.len() / 2;
                        let ibuffer = &mut ibuffer[..4 * n];
                        let obuffer = &mut obuffer[..3 * n];
                        Self::expand_luma_rgba(luma, ibuffer, input_coef);
                        tr43.transform(ibuffer, obuffer).expect("transform failed");
                        Self::clamp_rgb(obuffer, output);
                    }
                }) as Arc<dyn Fn(&[P], &mut [P]) + Send + Sync>,
                Arc::new(move |input: &[P], output: &mut [P]| {
                    let mut ibuffer = [0.0f32; 4 * N];
                    let mut obuffer = [0.0f32; 4 * N];

                    for (luma, output) in input.chunks(2 * N).zip(output.chunks_mut(4 * N)) {
                        let n = luma.len() / 2;
                        let ibuffer = &mut ibuffer[..4 * n];
                        let obuffer = &mut obuffer[..4 * n];
                        Self::expand_luma_rgba(luma, ibuffer, input_coef);
                        tr44.transform(ibuffer, obuffer).expect("transform failed");
                        Self::clamp_rgba(obuffer, output);
                    }
                }) as Arc<dyn Fn(&[P], &mut [P]) + Send + Sync>,
            ]
        };

        // rgb-luma transforms contract Rgb to Luma (and Rgba to LumaAlpha)
        let rgb_luma = {
            let [tr33, tr34, tr43, tr44] = f32.clone();

            [
                Arc::new(move |input: &[P], output: &mut [P]| {
                    debug_assert_eq!(input.len() / 3, output.len());

                    let mut ibuffer = [0.0f32; 3 * N];
                    let mut obuffer = [0.0f32; 3 * N];

                    for (rgb, output) in input.chunks(3 * N).zip(output.chunks_mut(N)) {
                        let n = output.len();
                        let ibuffer = &mut ibuffer[..3 * n];
                        let obuffer = &mut obuffer[..3 * n];
                        Self::expand_rgb(rgb, ibuffer);
                        tr33.transform(ibuffer, obuffer).expect("transform failed");
                        Self::clamp_rgb_luma(obuffer, output, output_coef);
                    }
                }) as Arc<dyn Fn(&[P], &mut [P]) + Send + Sync>,
                Arc::new(move |input: &[P], output: &mut [P]| {
                    debug_assert_eq!(input.len() / 3, output.len() / 2);

                    let mut ibuffer = [0.0f32; 3 * N];
                    let mut obuffer = [0.0f32; 4 * N];

                    for (rgb, output) in input.chunks(4 * N).zip(output.chunks_mut(2 * N)) {
                        let n = output.len() / 2;
                        let ibuffer = &mut ibuffer[..3 * n];
                        let obuffer = &mut obuffer[..4 * n];
                        Self::expand_rgb(rgb, ibuffer);
                        tr34.transform(ibuffer, obuffer).expect("transform failed");
                        Self::clamp_rgba_luma(obuffer, output, output_coef);
                    }
                }) as Arc<dyn Fn(&[P], &mut [P]) + Send + Sync>,
                Arc::new(move |input: &[P], output: &mut [P]| {
                    debug_assert_eq!(input.len() / 4, output.len());

                    let mut ibuffer = [0.0f32; 4 * N];
                    let mut obuffer = [0.0f32; 3 * N];

                    for (rgba, output) in input.chunks(4 * N).zip(output.chunks_mut(N)) {
                        let n = output.len();
                        let ibuffer = &mut ibuffer[..4 * n];
                        let obuffer = &mut obuffer[..3 * n];
                        Self::expand_rgba(rgba, ibuffer);
                        tr43.transform(ibuffer, obuffer).expect("transform failed");
                        Self::clamp_rgb_luma(obuffer, output, output_coef);
                    }
                }) as Arc<dyn Fn(&[P], &mut [P]) + Send + Sync>,
                Arc::new(move |input: &[P], output: &mut [P]| {
                    debug_assert_eq!(input.len() / 4, output.len() / 2);

                    let mut ibuffer = [0.0f32; 4 * N];
                    let mut obuffer = [0.0f32; 4 * N];

                    for (rgba, output) in input.chunks(4 * N).zip(output.chunks_mut(2 * N)) {
                        let n = output.len() / 2;
                        let ibuffer = &mut ibuffer[..4 * n];
                        let obuffer = &mut obuffer[..4 * n];
                        Self::expand_rgba(rgba, ibuffer);
                        tr44.transform(ibuffer, obuffer).expect("transform failed");
                        Self::clamp_rgba_luma(obuffer, output, output_coef);
                    }
                }) as Arc<dyn Fn(&[P], &mut [P]) + Send + Sync>,
            ]
        };

        // luma-luma both expand and contract
        let luma_luma = {
            let [tr33, tr34, tr43, tr44] = f32.clone();

            [
                Arc::new(move |input: &[P], output: &mut [P]| {
                    debug_assert_eq!(input.len(), output.len());
                    let mut ibuffer = [0.0f32; 3 * N];
                    let mut obuffer = [0.0f32; 3 * N];

                    for (luma, output) in input.chunks(N).zip(output.chunks_mut(N)) {
                        let n = luma.len();
                        let ibuffer = &mut ibuffer[..3 * n];
                        let obuffer = &mut obuffer[..3 * n];
                        Self::expand_luma_rgb(luma, ibuffer, input_coef);
                        tr33.transform(ibuffer, obuffer).expect("transform failed");
                        Self::clamp_rgb_luma(obuffer, output, output_coef);
                    }
                }) as Arc<dyn Fn(&[P], &mut [P]) + Send + Sync>,
                Arc::new(move |input: &[P], output: &mut [P]| {
                    debug_assert_eq!(input.len(), output.len() / 2);
                    let mut ibuffer = [0.0f32; 3 * N];
                    let mut obuffer = [0.0f32; 4 * N];

                    for (luma, output) in input.chunks(N).zip(output.chunks_mut(2 * N)) {
                        let n = luma.len();
                        let ibuffer = &mut ibuffer[..3 * n];
                        let obuffer = &mut obuffer[..4 * n];
                        Self::expand_luma_rgb(luma, ibuffer, input_coef);
                        tr34.transform(ibuffer, obuffer).expect("transform failed");
                        Self::clamp_rgba_luma(obuffer, output, output_coef);
                    }
                }) as Arc<dyn Fn(&[P], &mut [P]) + Send + Sync>,
                Arc::new(move |input: &[P], output: &mut [P]| {
                    debug_assert_eq!(input.len() / 2, output.len());
                    let mut ibuffer = [0.0f32; 4 * N];
                    let mut obuffer = [0.0f32; 3 * N];

                    for (luma, output) in input.chunks(2 * N).zip(output.chunks_mut(N)) {
                        let n = luma.len() / 2;
                        let ibuffer = &mut ibuffer[..4 * n];
                        let obuffer = &mut obuffer[..3 * n];
                        Self::expand_luma_rgba(luma, ibuffer, input_coef);
                        tr43.transform(ibuffer, obuffer).expect("transform failed");
                        Self::clamp_rgb_luma(obuffer, output, output_coef);
                    }
                }) as Arc<dyn Fn(&[P], &mut [P]) + Send + Sync>,
                Arc::new(move |input: &[P], output: &mut [P]| {
                    debug_assert_eq!(input.len() / 2, output.len() / 2);
                    let mut ibuffer = [0.0f32; 4 * N];
                    let mut obuffer = [0.0f32; 4 * N];

                    for (luma, output) in input.chunks(2 * N).zip(output.chunks_mut(2 * N)) {
                        let n = luma.len() / 2;
                        let ibuffer = &mut ibuffer[..4 * n];
                        let obuffer = &mut obuffer[..4 * n];
                        Self::expand_luma_rgba(luma, ibuffer, input_coef);
                        tr44.transform(ibuffer, obuffer).expect("transform failed");
                        Self::clamp_rgba_luma(obuffer, output, output_coef);
                    }
                }) as Arc<dyn Fn(&[P], &mut [P]) + Send + Sync>,
            ]
        };

        Some(RgbTransforms {
            slices,
            luma_rgb,
            rgb_luma,
            luma_luma,
        })
    }

    pub(crate) fn transform_dynamic(&self, lhs: &mut DynamicImage, rhs: &DynamicImage) {
        let mut ibuffer = [0.0f32; 1200];
        let mut obuffer = [0.0f32; 1200];

        let pixels = (u64::from(lhs.width()) * u64::from(lhs.height())) as usize;

        let input_samples;
        let output_samples;

        let inner_transform = match (lhs.color(), rhs.color()) {
            (
                ColorType::L8
                | ColorType::L16
                | ColorType::Rgb8
                | ColorType::Rgb16
                | ColorType::Rgb32F,
                ColorType::L8
                | ColorType::L16
                | ColorType::Rgb8
                | ColorType::Rgb16
                | ColorType::Rgb32F,
            ) => {
                output_samples = 3;
                input_samples = 3;
                &*self.f32.slices[0]
            }
            (
                ColorType::La8
                | ColorType::La16
                | ColorType::Rgba8
                | ColorType::Rgba16
                | ColorType::Rgba32F,
                ColorType::L8
                | ColorType::L16
                | ColorType::Rgb8
                | ColorType::Rgb16
                | ColorType::Rgb32F,
            ) => {
                output_samples = 4;
                input_samples = 3;
                &*self.f32.slices[1]
            }
            (
                ColorType::L8
                | ColorType::L16
                | ColorType::Rgb8
                | ColorType::Rgb16
                | ColorType::Rgb32F,
                ColorType::La8
                | ColorType::La16
                | ColorType::Rgba8
                | ColorType::Rgba16
                | ColorType::Rgba32F,
            ) => {
                output_samples = 3;
                input_samples = 4;
                &*self.f32.slices[2]
            }
            (
                ColorType::La8
                | ColorType::La16
                | ColorType::Rgba8
                | ColorType::Rgba16
                | ColorType::Rgba32F,
                ColorType::La8
                | ColorType::La16
                | ColorType::Rgba8
                | ColorType::Rgba16
                | ColorType::Rgba32F,
            ) => {
                output_samples = 4;
                input_samples = 4;
                &*self.f32.slices[3]
            }
        };

        const STEP: usize = 150;
        for start_idx in (0..pixels).step_by(STEP) {
            let end_idx = (start_idx + STEP).min(pixels);
            let count = end_idx - start_idx;

            // Expand pixels from `other` into `ibuffer`. All of these have different types, so
            // here's two large switch statements.
            match rhs {
                DynamicImage::ImageLuma8(buf) => {
                    CicpTransform::expand_luma_rgb(
                        &buf.inner_pixels()[start_idx..end_idx],
                        &mut ibuffer[..3 * count],
                        self.input_coefs,
                    );
                }
                DynamicImage::ImageLumaA8(buf) => {
                    CicpTransform::expand_luma_rgba(
                        &buf.inner_pixels()[2 * start_idx..2 * end_idx],
                        &mut ibuffer[..4 * count],
                        self.input_coefs,
                    );
                }
                DynamicImage::ImageRgb8(buf) => {
                    CicpTransform::expand_rgb(
                        &buf.inner_pixels()[3 * start_idx..3 * end_idx],
                        &mut ibuffer[..3 * count],
                    );
                }
                DynamicImage::ImageRgba8(buf) => {
                    CicpTransform::expand_rgba(
                        &buf.inner_pixels()[4 * start_idx..4 * end_idx],
                        &mut ibuffer[..4 * count],
                    );
                }
                DynamicImage::ImageLuma16(buf) => {
                    CicpTransform::expand_luma_rgb(
                        &buf.inner_pixels()[start_idx..end_idx],
                        &mut ibuffer[..3 * count],
                        self.input_coefs,
                    );
                }
                DynamicImage::ImageLumaA16(buf) => {
                    CicpTransform::expand_luma_rgba(
                        &buf.inner_pixels()[2 * start_idx..2 * end_idx],
                        &mut ibuffer[..4 * count],
                        self.input_coefs,
                    );
                }
                DynamicImage::ImageRgb16(buf) => {
                    CicpTransform::expand_rgb(
                        &buf.inner_pixels()[3 * start_idx..3 * end_idx],
                        &mut ibuffer[..3 * count],
                    );
                }

                DynamicImage::ImageRgba16(buf) => {
                    CicpTransform::expand_rgba(
                        &buf.inner_pixels()[4 * start_idx..4 * end_idx],
                        &mut ibuffer[..4 * count],
                    );
                }
                DynamicImage::ImageRgb32F(buf) => {
                    CicpTransform::expand_rgb(
                        &buf.inner_pixels()[3 * start_idx..3 * end_idx],
                        &mut ibuffer[..3 * count],
                    );
                }
                DynamicImage::ImageRgba32F(buf) => {
                    CicpTransform::expand_rgba(
                        &buf.inner_pixels()[4 * start_idx..4 * end_idx],
                        &mut ibuffer[..4 * count],
                    );
                }
            }

            let islice = &ibuffer[..input_samples * count];
            let oslice = &mut obuffer[..output_samples * count];

            inner_transform(islice, oslice);

            match lhs {
                DynamicImage::ImageLuma8(buf) => {
                    CicpTransform::clamp_rgb_luma(
                        &obuffer[..3 * count],
                        &mut buf.inner_pixels_mut()[start_idx..end_idx],
                        self.output_coefs,
                    );
                }
                DynamicImage::ImageLumaA8(buf) => {
                    CicpTransform::clamp_rgba_luma(
                        &obuffer[..4 * count],
                        &mut buf.inner_pixels_mut()[2 * start_idx..2 * end_idx],
                        self.output_coefs,
                    );
                }
                DynamicImage::ImageRgb8(buf) => {
                    CicpTransform::clamp_rgb(
                        &obuffer[..3 * count],
                        &mut buf.inner_pixels_mut()[3 * start_idx..3 * end_idx],
                    );
                }
                DynamicImage::ImageRgba8(buf) => {
                    CicpTransform::clamp_rgba(
                        &obuffer[..4 * count],
                        &mut buf.inner_pixels_mut()[4 * start_idx..4 * end_idx],
                    );
                }
                DynamicImage::ImageLuma16(buf) => {
                    CicpTransform::clamp_rgb_luma(
                        &obuffer[..3 * count],
                        &mut buf.inner_pixels_mut()[start_idx..end_idx],
                        self.output_coefs,
                    );
                }
                DynamicImage::ImageLumaA16(buf) => {
                    CicpTransform::clamp_rgba_luma(
                        &obuffer[..4 * count],
                        &mut buf.inner_pixels_mut()[2 * start_idx..2 * end_idx],
                        self.output_coefs,
                    );
                }
                DynamicImage::ImageRgb16(buf) => {
                    CicpTransform::clamp_rgba(
                        &obuffer[..3 * count],
                        &mut buf.inner_pixels_mut()[3 * start_idx..3 * end_idx],
                    );
                }

                DynamicImage::ImageRgba16(buf) => {
                    CicpTransform::clamp_rgba(
                        &obuffer[..4 * count],
                        &mut buf.inner_pixels_mut()[4 * start_idx..4 * end_idx],
                    );
                }
                DynamicImage::ImageRgb32F(buf) => {
                    CicpTransform::clamp_rgb(
                        &obuffer[..3 * count],
                        &mut buf.inner_pixels_mut()[3 * start_idx..3 * end_idx],
                    );
                }
                DynamicImage::ImageRgba32F(buf) => {
                    CicpTransform::clamp_rgba(
                        &obuffer[..4 * count],
                        &mut buf.inner_pixels_mut()[4 * start_idx..4 * end_idx],
                    );
                }
            }
        }
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
        self.u8.select_transform::<P>(into)
    }

    pub(crate) fn select_transform_u16<O: SealedPixelWithColorType<TransformableSubpixel = u16>>(
        &self,
        into: LayoutWithColor,
    ) -> &Arc<CicpApplicable<'static, u16>> {
        self.u16.select_transform::<O>(into)
    }

    pub(crate) fn select_transform_f32<O: SealedPixelWithColorType<TransformableSubpixel = f32>>(
        &self,
        into: LayoutWithColor,
    ) -> &Arc<CicpApplicable<'static, f32>> {
        self.f32.select_transform::<O>(into)
    }

    const LAYOUTS: [(LayoutWithColor, LayoutWithColor); 4] = [
        (LayoutWithColor::Rgb, LayoutWithColor::Rgb),
        (LayoutWithColor::Rgb, LayoutWithColor::Rgba),
        (LayoutWithColor::Rgba, LayoutWithColor::Rgb),
        (LayoutWithColor::Rgba, LayoutWithColor::Rgba),
    ];

    pub(crate) fn expand_luma_rgb<P: ColorComponentForCicp>(
        luma: &[P],
        rgb: &mut [f32],
        coef: [f32; 3],
    ) {
        for (&pix, rgb) in luma.iter().zip(rgb.chunks_exact_mut(3)) {
            let luma = pix.expand_to_f32();
            rgb[0] = luma * coef[0];
            rgb[1] = luma * coef[1];
            rgb[2] = luma * coef[2];
        }
    }

    pub(crate) fn expand_luma_rgba<P: ColorComponentForCicp>(
        luma: &[P],
        rgb: &mut [f32],
        coef: [f32; 3],
    ) {
        for (pix, rgb) in luma.chunks_exact(2).zip(rgb.chunks_exact_mut(4)) {
            let luma = pix[0].expand_to_f32();
            rgb[0] = luma * coef[0];
            rgb[1] = luma * coef[1];
            rgb[2] = luma * coef[2];
            rgb[3] = pix[1].expand_to_f32();
        }
    }

    pub(crate) fn expand_rgb<P: ColorComponentForCicp>(input: &[P], output: &mut [f32]) {
        for (&component, val) in input.iter().zip(output) {
            *val = component.expand_to_f32();
        }
    }

    pub(crate) fn expand_rgba<P: ColorComponentForCicp>(input: &[P], output: &mut [f32]) {
        for (&component, val) in input.iter().zip(output) {
            *val = component.expand_to_f32();
        }
    }

    pub(crate) fn clamp_rgb<P: ColorComponentForCicp>(input: &[f32], output: &mut [P]) {
        // Everything is mapped..
        for (&component, val) in input.iter().zip(output) {
            *val = P::clamp_from_f32(component);
        }
    }

    pub(crate) fn clamp_rgba<P: ColorComponentForCicp>(input: &[f32], output: &mut [P]) {
        for (&component, val) in input.iter().zip(output) {
            *val = P::clamp_from_f32(component);
        }
    }

    pub(crate) fn clamp_rgb_luma<P: ColorComponentForCicp>(
        input: &[f32],
        output: &mut [P],
        coef: [f32; 3],
    ) {
        for (rgb, pix) in input.chunks_exact(3).zip(output) {
            let luma = rgb[0] * coef[0] + rgb[1] * coef[1] + rgb[2] * coef[2];
            *pix = P::clamp_from_f32(luma);
        }
    }

    pub(crate) fn clamp_rgba_luma<P: ColorComponentForCicp>(
        input: &[f32],
        output: &mut [P],
        coef: [f32; 3],
    ) {
        for (rgba, pix) in input.chunks_exact(4).zip(output.chunks_exact_mut(2)) {
            let luma = rgba[0] * coef[0] + rgba[1] * coef[1] + rgba[2] * coef[2];
            pix[0] = P::clamp_from_f32(luma);
            pix[1] = P::clamp_from_f32(rgba[3]);
        }
    }
}

pub(crate) trait ColorComponentForCicp: Copy {
    fn expand_to_f32(self) -> f32;

    fn clamp_from_f32(val: f32) -> Self;
}

impl ColorComponentForCicp for u8 {
    fn expand_to_f32(self) -> f32 {
        self as f32 / Self::MAX as f32
    }

    #[inline]
    fn clamp_from_f32(val: f32) -> Self {
        (val * Self::MAX as f32 + 0.5).clamp(0., Self::MAX as f32) as u8
    }
}

impl ColorComponentForCicp for u16 {
    fn expand_to_f32(self) -> f32 {
        self as f32 / Self::MAX as f32
    }

    #[inline]
    fn clamp_from_f32(val: f32) -> Self {
        (val * Self::MAX as f32 + 0.5).clamp(0., Self::MAX as f32) as u16
    }
}

impl ColorComponentForCicp for f32 {
    fn expand_to_f32(self) -> f32 {
        self
    }

    fn clamp_from_f32(val: f32) -> Self {
        val
    }
}

impl<P> RgbTransforms<P> {
    fn select_transform<O: SealedPixelWithColorType>(
        &self,
        into: LayoutWithColor,
    ) -> &Arc<CicpApplicable<'static, P>> {
        use crate::traits::private::{LayoutWithColor as Layout, PrivateToken};
        let from = O::layout(PrivateToken);

        match (from, into) {
            (Layout::Rgb, Layout::Rgb) => &self.slices[0],
            (Layout::Rgb, Layout::Rgba) => &self.slices[1],
            (Layout::Rgba, Layout::Rgb) => &self.slices[2],
            (Layout::Rgba, Layout::Rgba) => &self.slices[3],
            (Layout::Rgb, Layout::Luma) => &self.rgb_luma[0],
            (Layout::Rgb, Layout::LumaAlpha) => &self.rgb_luma[1],
            (Layout::Rgba, Layout::Luma) => &self.rgb_luma[2],
            (Layout::Rgba, Layout::LumaAlpha) => &self.rgb_luma[3],
            (Layout::Luma, Layout::Rgb) => &self.luma_rgb[0],
            (Layout::Luma, Layout::Rgba) => &self.luma_rgb[1],
            (Layout::LumaAlpha, Layout::Rgb) => &self.luma_rgb[2],
            (Layout::LumaAlpha, Layout::Rgba) => &self.luma_rgb[3],
            (Layout::Luma, Layout::Luma) => &self.luma_luma[0],
            (Layout::Luma, Layout::LumaAlpha) => &self.luma_luma[1],
            (Layout::LumaAlpha, Layout::Luma) => &self.luma_luma[2],
            (Layout::LumaAlpha, Layout::LumaAlpha) => &self.luma_luma[3],
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
    ///
    /// Note that this modern Display P3 uses a D65 whitepoint. Use the primaries `SmpteRp431` for
    /// the previous standard. The advantage of the new standard is the color system shares its
    /// whitepoint with sRGB and BT.2020.
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
    ///
    /// A stable profile should also have `derived_luminance` implemented.
    pub(crate) const fn qualify_stability(&self) -> bool {
        const _: () = {
            // Out public constants _should_ be stable.
            assert!(Cicp::SRGB.qualify_stability());
            assert!(Cicp::SRGB_LINEAR.qualify_stability());
            assert!(Cicp::DISPLAY_P3.qualify_stability());
        };

        matches!(self.full_range, CicpVideoFullRangeFlag::FullRange)
            && matches!(
                self.matrix,
                // For pure RGB color
                CicpMatrixCoefficients::Identity
                    // The equivalent of our Luma color as a type..
                    | CicpMatrixCoefficients::ChromaticityDerivedNonConstant
            )
            && matches!(
                self.primaries,
                CicpColorPrimaries::SRgb
                    | CicpColorPrimaries::SmpteRp431
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
            luminance: DerivedLuminance::NonConstant,
        }
    }
}

impl CicpRgb {
    /// Calculate the luminance cofactors according to Rec H.273 (39) and (40).
    ///
    /// Returns cofactors for red, green, and blue in that order.
    pub(crate) fn derived_luminance(&self) -> Option<[f32; 3]> {
        fn det(yg: f32, zg: f32, yb: f32, zb: f32) -> f32 {
            yg * zb - zg * yb
        }

        let primaries = match self.primaries {
            CicpColorPrimaries::SRgb => moxcms::ColorPrimaries::BT_709,
            CicpColorPrimaries::SmpteRp431 => moxcms::ColorPrimaries::DISPLAY_P3,
            CicpColorPrimaries::SmpteRp432 => moxcms::ColorPrimaries::DISPLAY_P3,
            CicpColorPrimaries::Bt601 => moxcms::ColorPrimaries::BT_601,
            CicpColorPrimaries::Rgb240m => moxcms::ColorPrimaries::SMPTE_240,
            _ => return None,
        };

        let whitepoint = match self.primaries {
            CicpColorPrimaries::SRgb => moxcms::Chromaticity::D65,
            CicpColorPrimaries::SmpteRp431 => moxcms::Chromaticity::new(0.314, 0.351),
            CicpColorPrimaries::SmpteRp432 => moxcms::Chromaticity::D65,
            CicpColorPrimaries::Bt601 => moxcms::Chromaticity::D65,
            CicpColorPrimaries::Rgb240m => moxcms::Chromaticity::D65,
            _ => return None,
        };

        let r = primaries.red.to_xyz();
        let g = primaries.green.to_xyz();
        let b = primaries.blue.to_xyz();
        let w = whitepoint.to_xyz();

        let denominator = {
            let co_xr = det(g.y, g.z, b.y, b.z);
            let co_xg = det(b.y, b.z, r.y, r.z);
            let co_xb = det(r.y, r.z, g.y, g.z);

            w.y * (r.x * co_xr + g.x * co_xg + b.x * co_xb)
        };

        let nominator_r = {
            let co_xw = det(g.y, g.z, b.y, b.z);
            let co_yw = det(b.x, b.z, g.x, g.z);
            let co_zw = det(g.x, g.y, b.x, b.y);

            r.y * (w.x * co_xw + w.y * co_yw + w.z * co_zw)
        };

        let nominator_b = {
            let co_xw = det(r.y, r.z, g.y, g.z);
            let co_yw = det(g.x, g.z, r.x, r.z);
            let co_zw = det(r.x, r.y, g.x, g.y);

            b.y * (w.x * co_xw + w.y * co_yw + w.z * co_zw)
        };

        let kr = nominator_r / denominator;
        let kb = nominator_b / denominator;
        let kg = 1.0 - kr - kb;

        Some([kr, kg, kb])
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
#[test]
fn derived_luminance() {
    let luminance = Cicp::SRGB.into_rgb().derived_luminance();
    let [kr, kg, kb] = luminance.unwrap();
    assert!((kr - 0.2126).abs() < 1e-4);
    assert!((kg - 0.7152).abs() < 1e-4);
    assert!((kb - 0.0722).abs() < 1e-4);
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
