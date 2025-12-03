/// Contains bindings to `moxcms` as a [`super::CmsProvider`].
use std::sync::Arc;

use crate::{
    metadata::{
        cicp::ColorComponentForCicp, Cicp, CicpColorPrimaries, CicpMatrixCoefficients,
        CicpTransferCharacteristics, CicpTransform, CicpVideoFullRangeFlag,
    },
    traits::private::LayoutWithColor,
};

pub(crate) struct Moxcms;

impl super::CmsProvider for Moxcms {
    fn transform(&self, from: Cicp, into: Cicp) -> Option<CicpTransform> {
        if !from.qualify_stability() || !into.qualify_stability() {
            // To avoid regressions, we do not support all kinds of transforms from the start.
            // Instead, a selected list will be gradually enlarged as more in-depth tests are done
            // and the selected implementation library is checked for suitability in use.
            return None;
        }

        // Unused, but introduces symmetry to the supported color space transforms. That said we
        // calculate the derived luminance coefficients for all color that have a matching moxcms
        // profile so this really should not block anything.
        let _input_coefs = from.into_rgb().derived_luminance()?;
        let output_coefs = into.into_rgb().derived_luminance()?;

        let mox_from = from.to_moxcms_compute_profile()?;
        let mox_into = into.to_moxcms_compute_profile()?;

        let opt = moxcms::TransformOptions::default();

        let f32_fallback = {
            let try_f32 = CicpTransform::LAYOUTS.map(|(from_layout, into_layout)| {
                let (from, from_layout) = mox_from.map_layout(from_layout);
                let (into, into_layout) = mox_into.map_layout(into_layout);

                from.create_transform_f32(from_layout, into, into_layout, opt)
                    .map(Arc::<dyn moxcms::TransformExecutor<f32> + Send + Sync>::from)
                    .ok()
            });

            if try_f32.iter().any(Option::is_none) {
                return None;
            }

            try_f32.map(Option::unwrap)
        };

        Some(CicpTransform {
            from,
            into,
            u8: Self::build_transforms(
                CicpTransform::LAYOUTS.map(|(from_layout, into_layout)| {
                    let (from, from_layout) = mox_from.map_layout(from_layout);
                    let (into, into_layout) = mox_into.map_layout(into_layout);

                    from.create_transform_8bit(from_layout, into, into_layout, opt)
                        .map(Arc::<dyn moxcms::TransformExecutor<_> + Send + Sync>::from)
                        .ok()
                }),
                f32_fallback.clone(),
                output_coefs,
            )?,
            u16: Self::build_transforms(
                CicpTransform::LAYOUTS.map(|(from_layout, into_layout)| {
                    let (from, from_layout) = mox_from.map_layout(from_layout);
                    let (into, into_layout) = mox_into.map_layout(into_layout);

                    from.create_transform_16bit(from_layout, into, into_layout, opt)
                        .map(Arc::<dyn moxcms::TransformExecutor<_> + Send + Sync>::from)
                        .ok()
                }),
                f32_fallback.clone(),
                output_coefs,
            )?,
            f32: Self::build_transforms(
                f32_fallback.clone().map(Some),
                f32_fallback.clone(),
                output_coefs,
            )?,
            output_coefs,
        })
    }

    fn parse_icc(&self, icc: &[u8]) -> Option<Cicp> {
        let profile = moxcms::ColorProfile::new_from_slice(icc).ok()?;
        let cicp = profile.cicp?;

        use moxcms::CicpColorPrimaries as Mcp;
        use moxcms::MatrixCoefficients as Mmc;
        use moxcms::TransferCharacteristics as Mtc;

        Some(Cicp {
            primaries: match cicp.color_primaries {
                Mcp::Bt709 => CicpColorPrimaries::SRgb,
                Mcp::Unspecified | Mcp::Reserved => CicpColorPrimaries::Unspecified,
                Mcp::Bt470M => CicpColorPrimaries::RgbM,
                Mcp::Bt470Bg => CicpColorPrimaries::RgbB,
                Mcp::Bt601 => CicpColorPrimaries::Bt601,
                Mcp::Smpte240 => CicpColorPrimaries::Rgb240m,
                Mcp::GenericFilm => CicpColorPrimaries::GenericFilm,
                Mcp::Bt2020 => CicpColorPrimaries::Rgb2020,
                Mcp::Xyz => CicpColorPrimaries::Xyz,
                Mcp::Smpte431 => CicpColorPrimaries::SmpteRp431,
                Mcp::Smpte432 => CicpColorPrimaries::SmpteRp432,
                Mcp::Ebu3213 => CicpColorPrimaries::Industry22,
            },
            transfer: match cicp.transfer_characteristics {
                Mtc::Bt709 => CicpTransferCharacteristics::Bt709,
                Mtc::Unspecified | Mtc::Reserved => CicpTransferCharacteristics::Unspecified,
                Mtc::Bt470M => CicpTransferCharacteristics::Bt470M,
                Mtc::Bt470Bg => CicpTransferCharacteristics::Bt470BG,
                Mtc::Bt601 => CicpTransferCharacteristics::Bt601,
                Mtc::Smpte240 => CicpTransferCharacteristics::Smpte240m,
                Mtc::Linear => CicpTransferCharacteristics::Linear,
                Mtc::Log100 => CicpTransferCharacteristics::Log100,
                Mtc::Log100sqrt10 => CicpTransferCharacteristics::LogSqrt,
                Mtc::Iec61966 => CicpTransferCharacteristics::Iec61966_2_4,
                Mtc::Bt1361 => CicpTransferCharacteristics::Bt1361,
                Mtc::Srgb => CicpTransferCharacteristics::SRgb,
                Mtc::Bt202010bit => CicpTransferCharacteristics::Bt2020_10bit,
                Mtc::Bt202012bit => CicpTransferCharacteristics::Bt2020_12bit,
                Mtc::Smpte2084 => CicpTransferCharacteristics::Smpte2084,
                Mtc::Smpte428 => CicpTransferCharacteristics::Smpte428,
                Mtc::Hlg => CicpTransferCharacteristics::Bt2100Hlg,
            },
            matrix: match cicp.matrix_coefficients {
                Mmc::Identity => CicpMatrixCoefficients::Identity,
                Mmc::Unspecified | Mmc::Reserved => CicpMatrixCoefficients::Unspecified,
                Mmc::Bt709 => CicpMatrixCoefficients::Bt709,
                Mmc::Fcc => CicpMatrixCoefficients::UsFCC,
                Mmc::Bt470Bg => CicpMatrixCoefficients::Bt470BG,
                Mmc::Smpte170m => CicpMatrixCoefficients::Smpte170m,
                Mmc::Smpte240m => CicpMatrixCoefficients::Smpte240m,
                Mmc::YCgCo => CicpMatrixCoefficients::YCgCo,
                Mmc::Bt2020Ncl => CicpMatrixCoefficients::Bt2020NonConstant,
                Mmc::Bt2020Cl => CicpMatrixCoefficients::Bt2020Constant,
                Mmc::Smpte2085 => CicpMatrixCoefficients::Smpte2085,
                Mmc::ChromaticityDerivedNCL => {
                    CicpMatrixCoefficients::ChromaticityDerivedNonConstant
                }
                Mmc::ChromaticityDerivedCL => CicpMatrixCoefficients::ChromaticityDerivedConstant,
                Mmc::ICtCp => CicpMatrixCoefficients::Bt2100,
            },
            full_range: match cicp.full_range {
                false => CicpVideoFullRangeFlag::NarrowRange,
                true => CicpVideoFullRangeFlag::FullRange,
            },
        })
    }
}

impl Moxcms {
    fn build_transforms<P: ColorComponentForCicp + Default + 'static>(
        trs: [Option<Arc<dyn moxcms::TransformExecutor<P> + Send + Sync>>; 4],
        f32: [Arc<dyn moxcms::TransformExecutor<f32> + Send + Sync>; 4],
        output_coef: [f32; 3],
    ) -> Option<crate::metadata::cicp::RgbTransforms<P>> {
        // We would use `[array]::try_map` here, but it is not stable yet.
        if trs.iter().any(Option::is_none) {
            return None;
        }

        let trs = trs.map(Option::unwrap);

        // rgb-rgb transforms are done directly via moxcms.
        let slices = trs.clone().map(|tr| {
            Arc::new(move |input: &[P], output: &mut [P]| {
                tr.transform(input, output).expect("transform failed")
            }) as Arc<dyn Fn(&[P], &mut [P]) + Send + Sync>
        });

        const N: usize = 256;

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
                        CicpTransform::expand_luma_rgb(luma, ibuffer);
                        tr33.transform(ibuffer, obuffer).expect("transform failed");
                        CicpTransform::clamp_rgb(obuffer, output);
                    }
                }) as Arc<dyn Fn(&[P], &mut [P]) + Send + Sync>,
                Arc::new(move |input: &[P], output: &mut [P]| {
                    let mut ibuffer = [0.0f32; 3 * N];
                    let mut obuffer = [0.0f32; 4 * N];

                    for (luma, output) in input.chunks(N).zip(output.chunks_mut(4 * N)) {
                        let n = luma.len();
                        let ibuffer = &mut ibuffer[..3 * n];
                        let obuffer = &mut obuffer[..4 * n];
                        CicpTransform::expand_luma_rgb(luma, ibuffer);
                        tr34.transform(ibuffer, obuffer).expect("transform failed");
                        CicpTransform::clamp_rgba(obuffer, output);
                    }
                }) as Arc<dyn Fn(&[P], &mut [P]) + Send + Sync>,
                Arc::new(move |input: &[P], output: &mut [P]| {
                    let mut ibuffer = [0.0f32; 4 * N];
                    let mut obuffer = [0.0f32; 3 * N];

                    for (luma, output) in input.chunks(2 * N).zip(output.chunks_mut(3 * N)) {
                        let n = luma.len() / 2;
                        let ibuffer = &mut ibuffer[..4 * n];
                        let obuffer = &mut obuffer[..3 * n];
                        CicpTransform::expand_luma_rgba(luma, ibuffer);
                        tr43.transform(ibuffer, obuffer).expect("transform failed");
                        CicpTransform::clamp_rgb(obuffer, output);
                    }
                }) as Arc<dyn Fn(&[P], &mut [P]) + Send + Sync>,
                Arc::new(move |input: &[P], output: &mut [P]| {
                    let mut ibuffer = [0.0f32; 4 * N];
                    let mut obuffer = [0.0f32; 4 * N];

                    for (luma, output) in input.chunks(2 * N).zip(output.chunks_mut(4 * N)) {
                        let n = luma.len() / 2;
                        let ibuffer = &mut ibuffer[..4 * n];
                        let obuffer = &mut obuffer[..4 * n];
                        CicpTransform::expand_luma_rgba(luma, ibuffer);
                        tr44.transform(ibuffer, obuffer).expect("transform failed");
                        CicpTransform::clamp_rgba(obuffer, output);
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
                        CicpTransform::expand_rgb(rgb, ibuffer);
                        tr33.transform(ibuffer, obuffer).expect("transform failed");
                        CicpTransform::clamp_rgb_luma(obuffer, output, output_coef);
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
                        CicpTransform::expand_rgb(rgb, ibuffer);
                        tr34.transform(ibuffer, obuffer).expect("transform failed");
                        CicpTransform::clamp_rgba_luma(obuffer, output, output_coef);
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
                        CicpTransform::expand_rgba(rgba, ibuffer);
                        tr43.transform(ibuffer, obuffer).expect("transform failed");
                        CicpTransform::clamp_rgb_luma(obuffer, output, output_coef);
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
                        CicpTransform::expand_rgba(rgba, ibuffer);
                        tr44.transform(ibuffer, obuffer).expect("transform failed");
                        CicpTransform::clamp_rgba_luma(obuffer, output, output_coef);
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
                        CicpTransform::expand_luma_rgb(luma, ibuffer);
                        tr33.transform(ibuffer, obuffer).expect("transform failed");
                        CicpTransform::clamp_rgb_luma(obuffer, output, output_coef);
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
                        CicpTransform::expand_luma_rgb(luma, ibuffer);
                        tr34.transform(ibuffer, obuffer).expect("transform failed");
                        CicpTransform::clamp_rgba_luma(obuffer, output, output_coef);
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
                        CicpTransform::expand_luma_rgba(luma, ibuffer);
                        tr43.transform(ibuffer, obuffer).expect("transform failed");
                        CicpTransform::clamp_rgb_luma(obuffer, output, output_coef);
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
                        CicpTransform::expand_luma_rgba(luma, ibuffer);
                        tr44.transform(ibuffer, obuffer).expect("transform failed");
                        CicpTransform::clamp_rgba_luma(obuffer, output, output_coef);
                    }
                }) as Arc<dyn Fn(&[P], &mut [P]) + Send + Sync>,
            ]
        };

        Some(crate::metadata::cicp::RgbTransforms {
            slices,
            luma_rgb,
            rgb_luma,
            luma_luma,
        })
    }
}

/// An RGB profile with its related (same tone-mapping) gray profile.
///
/// This is the whole input information which we must be able to pass to the CMS in a support
/// transform, to handle all possible combinations of `ColorType` pixels that can be thrown at us.
/// For instance, in a previous iteration we had a separate gray profile here (but now handle that
/// internally by expansion to RGB through an YCbCr). Future iterations may add additional structs
/// to be computed for validating `CicpTransform::new`.
struct ColorProfile {
    rgb: moxcms::ColorProfile,
}

impl ColorProfile {
    fn map_layout(&self, layout: LayoutWithColor) -> (&moxcms::ColorProfile, moxcms::Layout) {
        match layout {
            LayoutWithColor::Rgb => (&self.rgb, moxcms::Layout::Rgb),
            LayoutWithColor::Rgba => (&self.rgb, moxcms::Layout::Rgba),
            // See comment in `to_moxcms_profile`.
            LayoutWithColor::Luma | LayoutWithColor::LumaAlpha => unreachable!(),
        }
    }
}

impl Cicp {
    /// Get an compute representation of an ICC profile for RGB.
    ///
    /// Note you should *not* be using this profile for export in a file, as discussed below.
    ///
    /// This is straightforward for Rgb and RgbA representations.
    ///
    /// Our luma models a Y component of a YCbCr color space. It turns out that ICC V4 does
    /// not support pure Luma in any other whitepoint apart from D50 (the native profile
    /// connection space). The use of a grayTRC does *not* take the chromatic adaptation
    /// matrix into account. Of course we can encode the adaptation into the TRC as a
    /// coefficient, the Y component of the product of the whitepoint adaptation matrix
    /// inverse and the pcs's whitepoint XYZ, but that is only correct for gray -> gray
    /// conversion (and that coefficient should generally be `1`).
    ///
    /// Hence we use a YCbCr. The data->pcs path could be modelled by ("M" curves, matrix, "B"
    /// curves) where B curves or M curves are all the identity, depending on whether constant or
    /// non-constant luma is in use. This is a subset of the capabilities that a lutAToBType
    /// allows. Unfortunately, this is not implemented in moxcms yet and for efficiency we would
    /// like to have a masked `create_transform_*` in which the CbCr channels are discarded /
    /// assumed 0 instead of them being in memory. Due to this special case and for supporting
    /// conversions between sample types, we implement said promotion as part of conversion to
    /// Rgba32F in this crate.
    ///
    /// For export to file, it would arguably correct to use a carefully crafted gray profile which
    /// we may implement in another function. That is, we could setup a tone reproduction curve
    /// which maps each sample value (which ICC regards as D50) into XYZ D50 in such a way that it
    /// _appears_ with the correct D50 luminance that we would get if we had used the conversion
    /// unders its true input whitepoint. The resulting color has a slightly wrong chroma as it is
    /// linearly dependent on D50 instead, but it's brightness would be correctly presented. At
    /// least for perceptual intent this might be alright.
    fn to_moxcms_compute_profile(self) -> Option<ColorProfile> {
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

        Some(ColorProfile { rgb })
    }
}

impl CicpColorPrimaries {
    fn to_moxcms(self) -> moxcms::CicpColorPrimaries {
        use moxcms::CicpColorPrimaries as M;

        match self {
            CicpColorPrimaries::SRgb => M::Bt709,
            CicpColorPrimaries::Unspecified => M::Unspecified,
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

impl CicpTransferCharacteristics {
    fn to_moxcms(self) -> moxcms::TransferCharacteristics {
        use moxcms::TransferCharacteristics as T;

        match self {
            CicpTransferCharacteristics::Bt709 => T::Bt709,
            CicpTransferCharacteristics::Unspecified => T::Unspecified,
            CicpTransferCharacteristics::Bt470M => T::Bt470M,
            CicpTransferCharacteristics::Bt470BG => T::Bt470Bg,
            CicpTransferCharacteristics::Bt601 => T::Bt601,
            CicpTransferCharacteristics::Smpte240m => T::Smpte240,
            CicpTransferCharacteristics::Linear => T::Linear,
            CicpTransferCharacteristics::Log100 => T::Log100,
            CicpTransferCharacteristics::LogSqrt => T::Log100sqrt10,
            CicpTransferCharacteristics::Iec61966_2_4 => T::Iec61966,
            CicpTransferCharacteristics::Bt1361 => T::Bt1361,
            CicpTransferCharacteristics::SRgb => T::Srgb,
            CicpTransferCharacteristics::Bt2020_10bit => T::Bt202010bit,
            CicpTransferCharacteristics::Bt2020_12bit => T::Bt202012bit,
            CicpTransferCharacteristics::Smpte2084 => T::Smpte2084,
            CicpTransferCharacteristics::Smpte428 => T::Smpte428,
            CicpTransferCharacteristics::Bt2100Hlg => T::Hlg,
        }
    }
}

impl CicpMatrixCoefficients {
    fn to_moxcms(self) -> Option<moxcms::MatrixCoefficients> {
        use moxcms::MatrixCoefficients as M;

        Some(match self {
            CicpMatrixCoefficients::Identity => M::Identity,
            CicpMatrixCoefficients::Unspecified => M::Unspecified,
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
