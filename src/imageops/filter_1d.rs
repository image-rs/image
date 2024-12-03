#![forbid(unsafe_code)]
use crate::error::{LimitError, LimitErrorKind};
use crate::ImageError;
use num_traits::{AsPrimitive, MulAdd};
use std::mem::size_of;
use std::ops::{Add, Mul};
use std::time::Instant;

#[cfg(any(
    all(
        any(target_arch = "x86", target_arch = "x86_64"),
        target_feature = "fma"
    ),
    all(target_arch = "aarch64", target_feature = "neon")
))]
#[inline(always)]
/// Uses fused multiply add when available
///
/// It is important not to call it if FMA flag is not turned on,
/// Rust compiles extremely slow case in this case (probably does software FMA?),
/// and we want to avoid this
fn mla<T: Copy + Mul<T, Output = T> + Add<T, Output = T> + MulAdd<T, Output = T>>(
    acc: T,
    a: T,
    b: T,
) -> T {
    MulAdd::mul_add(a, b, acc)
}

#[inline(always)]
#[cfg(not(any(
    all(
        any(target_arch = "x86", target_arch = "x86_64"),
        target_feature = "fma"
    ),
    all(target_arch = "aarch64", target_feature = "neon")
)))]
fn mla<T: Copy + Mul<T, Output = T> + Add<T, Output = T> + MulAdd<T, Output = T>>(
    acc: T,
    a: T,
    b: T,
) -> T {
    acc + a * b
}

struct RowArena<T> {
    arena: Vec<T>,
}

#[derive(Debug, Clone, Copy)]
struct KernelShape {
    width: usize,
    height: usize,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct FilterImageSize {
    pub(crate) width: usize,
    pub(crate) height: usize,
}

/// Pads an image row with *clamp* strategy
///
/// This method copies real content into center of new buffer
/// and filling leading and trailing physical padded parts with *clamp* strategy
fn make_arena_row<T, const COMPONENTS: usize>(
    image: &[T],
    source_y: usize,
    image_size: FilterImageSize,
    kernel_size: KernelShape,
) -> RowArena<T>
where
    T: Default + Copy + Send + Sync + 'static,
    f64: AsPrimitive<T>,
{
    assert_eq!(
        image.len(),
        COMPONENTS * image_size.width * image_size.height
    );

    let pad_w = kernel_size.width / 2;

    let arena_width = image_size.width * COMPONENTS + pad_w * 2 * COMPONENTS;
    let mut row = vec![T::default(); arena_width];

    let source_offset = source_y * image_size.width * COMPONENTS;

    let source_row = &image[source_offset..(source_offset + image_size.width * COMPONENTS)];

    let row_dst =
        &mut row[pad_w * COMPONENTS..(pad_w * COMPONENTS + image_size.width * COMPONENTS)];

    for (dst, src) in row_dst.iter_mut().zip(source_row.iter()) {
        *dst = *src;
    }

    for (x, dst) in (0..pad_w).zip(row.chunks_exact_mut(COMPONENTS)) {
        let old_x = x.saturating_sub(pad_w).min(image_size.width - 1);
        let old_px = old_x * COMPONENTS;
        let src_iter = &source_row[old_px..(old_px + COMPONENTS)];
        for (dst, src) in dst.iter_mut().zip(src_iter.iter()) {
            *dst = *src;
        }
    }

    for (x, dst) in
        (image_size.width..(image_size.width + pad_w)).zip(row.chunks_exact_mut(COMPONENTS).rev())
    {
        let old_x = x.max(0).min(image_size.width - 1);
        let old_px = old_x * COMPONENTS;
        let src_iter = &source_row[old_px..(old_px + COMPONENTS)];
        for (dst, src) in dst.iter_mut().zip(src_iter.iter()) {
            *dst = *src;
        }
    }

    RowArena { arena: row }
}

#[derive(Clone)]
struct ArenaColumns<T>
where
    T: Copy,
{
    pub top_pad: Vec<T>,
    pub bottom_pad: Vec<T>,
}

/// Pads a column image with *clamp* strategy
///
/// This method is used for *virtual* column padding.
/// It produces two arrays that represents virtual image top part and bottom
fn make_columns_arenas<T, const COMPONENTS: usize>(
    image: &[T],
    image_size: FilterImageSize,
    kernel_size: KernelShape,
) -> ArenaColumns<T>
where
    T: Default + Copy + Send + Sync + 'static,
    f64: AsPrimitive<T>,
{
    assert_eq!(
        image.len(),
        COMPONENTS * image_size.width * image_size.height
    );
    let pad_h = kernel_size.height / 2;

    let mut top_pad = vec![T::default(); pad_h * image_size.width * COMPONENTS];
    let mut bottom_pad = vec![T::default(); pad_h * image_size.width * COMPONENTS];

    let top_pad_stride = image_size.width * COMPONENTS;

    for (ky, dst) in (0..pad_h).zip(top_pad.chunks_exact_mut(top_pad_stride)) {
        for (kx, dst) in (0..image_size.width).zip(dst.chunks_exact_mut(COMPONENTS)) {
            let y = ky.saturating_sub(pad_h).min(image_size.height - 1);
            let v_src = y * top_pad_stride + kx * COMPONENTS;

            let src_iter = &image[v_src..(v_src + COMPONENTS)];
            for (dst, src) in dst.iter_mut().zip(src_iter.iter()) {
                *dst = *src;
            }
        }
    }

    let bottom_iter_dst = bottom_pad.chunks_exact_mut(top_pad_stride);

    for (ky, dst) in (0..pad_h).zip(bottom_iter_dst) {
        for (kx, dst) in (0..image_size.width).zip(dst.chunks_exact_mut(COMPONENTS)) {
            let y = (ky + image_size.height).min(image_size.height - 1);
            let v_src = y * top_pad_stride + kx * COMPONENTS;
            let src_iter = &image[v_src..(v_src + COMPONENTS)];
            for (dst, src) in dst.iter_mut().zip(src_iter.iter()) {
                *dst = *src;
            }
        }
    }

    ArenaColumns {
        top_pad,
        bottom_pad,
    }
}

trait ToStorage<S> {
    fn to_(self) -> S;
}

const PRECISION: i32 = 15;

impl ToStorage<u8> for i32 {
    #[inline(always)]
    #[allow(clippy::manual_clamp)]
    fn to_(self) -> u8 {
        ((self + (1 << (PRECISION - 1))) >> PRECISION)
            .min(255)
            .max(0) as u8
    }
}

impl ToStorage<u16> for f32 {
    #[inline(always)]
    fn to_(self) -> u16 {
        self.round().min(u16::MAX as f32).max(0f32) as u16
    }
}

impl ToStorage<f32> for f32 {
    #[inline(always)]
    fn to_(self) -> f32 {
        self
    }
}

#[derive(Copy, Clone)]
struct Arena<'a, T> {
    src: &'a [T],
    components: usize,
}

#[repr(C)]
#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq, Copy)]
struct ColorGroup<const COMPS: usize, J: Copy> {
    r: J,
    g: J,
    b: J,
    a: J,
}

macro_rules! ld_group {
    ($store: expr, $channels: expr, $offset: expr) => {{
        if $channels == 1 {
            ColorGroup {
                r: $store[$offset].as_(),
                g: 0.as_(),
                b: 0.as_(),
                a: 0.as_(),
            }
        } else if $channels == 2 {
            ColorGroup {
                r: $store[$offset].as_(),
                g: $store[$offset + 1].as_(),
                b: 0.as_(),
                a: 0.as_(),
            }
        } else if $channels == 3 {
            ColorGroup {
                r: $store[$offset].as_(),
                g: $store[$offset + 1].as_(),
                b: $store[$offset + 2].as_(),
                a: 0.as_(),
            }
        } else if $channels == 4 {
            ColorGroup {
                r: $store[$offset].as_(),
                g: $store[$offset + 1].as_(),
                b: $store[$offset + 2].as_(),
                a: $store[$offset + 3].as_(),
            }
        } else {
            unimplemented!()
        }
    }};
}

impl<const COMPS: usize, J> ColorGroup<COMPS, J>
where
    J: Copy + Default,
{
    #[inline]
    fn from_components(r: J, g: J, b: J, a: J) -> ColorGroup<COMPS, J> {
        ColorGroup { r, g, b, a }
    }
}

impl<const COMPS: usize, J> Mul<J> for ColorGroup<COMPS, J>
where
    J: Copy + Mul<Output = J> + Default + 'static,
{
    type Output = Self;

    #[inline]
    fn mul(self, rhs: J) -> Self::Output {
        if COMPS == 1 {
            ColorGroup::from_components(self.r * rhs, self.g, self.b, self.a)
        } else if COMPS == 2 {
            ColorGroup::from_components(self.r * rhs, self.g * rhs, self.b, self.a)
        } else if COMPS == 3 {
            ColorGroup::from_components(self.r * rhs, self.g * rhs, self.b * rhs, self.a)
        } else if COMPS == 4 {
            ColorGroup::from_components(self.r * rhs, self.g * rhs, self.b * rhs, self.a * rhs)
        } else {
            unimplemented!()
        }
    }
}

impl<const COMPS: usize, J> MulAdd<ColorGroup<COMPS, J>, J> for ColorGroup<COMPS, J>
where
    J: Copy + MulAdd<J, Output = J> + Default + 'static + Mul<J, Output = J> + Add<J, Output = J>,
{
    type Output = Self;

    #[inline]
    fn mul_add(self, a: ColorGroup<COMPS, J>, b: J) -> Self::Output {
        if COMPS == 1 {
            ColorGroup::from_components(mla(a.r, self.r, b), self.g, self.b, self.a)
        } else if COMPS == 2 {
            ColorGroup::from_components(mla(a.r, self.r, b), mla(a.g, self.g, b), self.b, self.a)
        } else if COMPS == 3 {
            ColorGroup::from_components(
                mla(a.r, self.r, b),
                mla(a.g, self.g, b),
                mla(a.b, self.b, b),
                self.a,
            )
        } else if COMPS == 4 {
            ColorGroup::from_components(
                mla(a.r, self.r, b),
                mla(a.g, self.g, b),
                mla(a.b, self.b, b),
                mla(a.a, self.a, b),
            )
        } else {
            unimplemented!();
        }
    }
}

impl<const COMPS: usize, J> Add<ColorGroup<COMPS, J>> for ColorGroup<COMPS, J>
where
    J: Copy + Add<Output = J> + Default + 'static,
{
    type Output = Self;

    #[inline]
    fn add(self, rhs: ColorGroup<COMPS, J>) -> Self::Output {
        if COMPS == 1 {
            ColorGroup::from_components(self.r + rhs.r, self.g, self.b, self.a)
        } else if COMPS == 2 {
            ColorGroup::from_components(self.r + rhs.r, self.g + rhs.g, self.b, self.a)
        } else if COMPS == 3 {
            ColorGroup::from_components(self.r + rhs.r, self.g + rhs.g, self.b + rhs.b, self.a)
        } else if COMPS == 4 {
            ColorGroup::from_components(
                self.r + rhs.r,
                self.g + rhs.g,
                self.b + rhs.b,
                self.a + rhs.a,
            )
        } else {
            unimplemented!();
        }
    }
}

macro_rules! st_group {
    ($store: expr, $channels: expr, $offset: expr, $vl: expr) => {{
        if $channels == 1 {
            $store[$offset] = $vl.r.to_();
        } else if $channels == 2 {
            $store[$offset] = $vl.r.to_();
            $store[$offset + 1] = $vl.g.to_();
        } else if $channels == 3 {
            $store[$offset] = $vl.r.to_();
            $store[$offset + 1] = $vl.g.to_();
            $store[$offset + 2] = $vl.b.to_();
        } else if $channels == 4 {
            $store[$offset] = $vl.r.to_();
            $store[$offset + 1] = $vl.g.to_();
            $store[$offset + 2] = $vl.b.to_();
            $store[$offset + 3] = $vl.a.to_();
        } else {
            unimplemented!()
        }
    }};
}

/// Performs column convolution pass for symmetrical filter
///
/// Common convolution formula O(x,y)=∑K(k)⋅I(x,y+k); where sums goes from 0...R
/// when filter is symmetric that we can half kernel reads by using formula
/// O(x,y)=(∑K(k)⋅(I(x,y+k) + I(x,y+(R-k)))) + K(R/2)⋅I(x,y+R/2); where sums goes from 0...R/2
fn filter_symmetric_column<T, F>(
    arena: Arena<T>,
    arena_src: &[&[T]],
    dst_row: &mut [T],
    image_size: FilterImageSize,
    kernel: &[F],
) where
    T: Copy + AsPrimitive<F>,
    F: ToStorage<T>
        + Mul<F, Output = F>
        + MulAdd<F, Output = F>
        + Add<F, Output = F>
        + Default
        + Copy
        + 'static,
{
    let dst_stride = image_size.width * arena.components;

    let length = kernel.len();
    let half_len = length / 2;

    let mut cx = 0usize;

    if size_of::<T>() == 1 {
        while cx + 32 < dst_stride {
            let coeff = kernel[half_len];

            let mut store0: [F; 16] = [F::default(); 16];
            let mut store1: [F; 16] = [F::default(); 16];

            let v_src0 = &arena_src[half_len][cx..(cx + 16)];
            let v_src1 = &arena_src[half_len][(cx + 16)..(cx + 32)];

            for (dst, src) in store0.iter_mut().zip(v_src0) {
                *dst = src.as_().mul(coeff);
            }
            for (dst, src) in store1.iter_mut().zip(v_src1) {
                *dst = src.as_().mul(coeff);
            }

            for (i, &coeff) in kernel.iter().take(half_len).enumerate() {
                let rollback = length - i - 1;
                let fw_src = arena_src[i];
                let rw_src = arena_src[rollback];
                let fw0 = &fw_src[cx..(cx + 16)];
                let bw0 = &rw_src[cx..(cx + 16)];
                let fw1 = &fw_src[(cx + 16)..(cx + 32)];
                let bw1 = &rw_src[(cx + 16)..(cx + 32)];

                for ((dst, fw), bw) in store0.iter_mut().zip(fw0).zip(bw0) {
                    *dst = mla(*dst, fw.as_().add(bw.as_()), coeff);
                }

                for ((dst, fw), bw) in store1.iter_mut().zip(fw1).zip(bw1) {
                    *dst = mla(*dst, fw.as_().add(bw.as_()), coeff);
                }
            }

            let shaped_dst0 = &mut dst_row[cx..(cx + 16)];

            for (src, dst) in store0.iter().zip(shaped_dst0.iter_mut()) {
                *dst = src.to_();
            }

            let shaped_dst1 = &mut dst_row[(cx + 16)..(cx + 32)];

            for (src, dst) in store1.iter().zip(shaped_dst1.iter_mut()) {
                *dst = src.to_();
            }

            cx += 32;
        }
    }

    while cx + 16 < dst_stride {
        let coeff = kernel[half_len];

        let mut store: [F; 16] = [F::default(); 16];

        let v_src = &arena_src[half_len][cx..(cx + 16)];

        for (dst, src) in store.iter_mut().zip(v_src) {
            *dst = src.as_().mul(coeff);
        }

        for (i, &coeff) in kernel.iter().take(half_len).enumerate() {
            let rollback = length - i - 1;
            let fw = &arena_src[i][cx..(cx + 16)];
            let bw = &arena_src[rollback][cx..(cx + 16)];

            for ((dst, fw), bw) in store.iter_mut().zip(fw).zip(bw) {
                *dst = mla(*dst, fw.as_().add(bw.as_()), coeff);
            }
        }

        let shaped_dst = &mut dst_row[cx..(cx + 16)];
        for (src, dst) in store.iter().zip(shaped_dst.iter_mut()) {
            *dst = src.to_();
        }

        cx += 16;
    }

    while cx + 4 < dst_stride {
        let coeff = kernel[half_len];

        let v_src = &arena_src[half_len][cx..(cx + 4)];

        let mut k0 = v_src[0].as_().mul(coeff);
        let mut k1 = v_src[1].as_().mul(coeff);
        let mut k2 = v_src[2].as_().mul(coeff);
        let mut k3 = v_src[3].as_().mul(coeff);

        for (i, &coeff) in kernel.iter().take(half_len).enumerate() {
            let rollback = length - i - 1;
            let fw = &arena_src[i][cx..(cx + 4)];
            let bw = &arena_src[rollback][cx..(cx + 4)];
            k0 = mla(k0, fw[0].as_().add(bw[0].as_()), coeff);
            k1 = mla(k1, fw[1].as_().add(bw[1].as_()), coeff);
            k2 = mla(k2, fw[2].as_().add(bw[2].as_()), coeff);
            k3 = mla(k3, fw[3].as_().add(bw[3].as_()), coeff);
        }

        let shaped_dst = &mut dst_row[cx..(cx + 4)];
        shaped_dst[0] = k0.to_();
        shaped_dst[1] = k1.to_();
        shaped_dst[2] = k2.to_();
        shaped_dst[3] = k3.to_();
        cx += 4;
    }

    #[allow(clippy::needless_range_loop)]
    for x in cx..dst_stride {
        let coeff = kernel[half_len];

        let v_src = &arena_src[half_len][x..(x + 1)];

        let mut k0 = v_src[0].as_().mul(coeff);

        for (i, &coeff) in kernel.iter().take(half_len).enumerate() {
            let rollback = length - i - 1;
            let fw = &arena_src[i][x..(x + 1)];
            let bw = &arena_src[rollback][x..(x + 1)];
            k0 = mla(k0, fw[0].as_().add(bw[0].as_()), coeff);
        }

        dst_row[x] = k0.to_();
    }
}

/// Performs horizontal convolution for row
///
/// Common convolution formula O(x,y)=∑K(k)⋅I(x+k,y); where sums goes from 0...R
/// when filter is symmetric that we can half kernel reads by using formula
/// O(x,y)=(∑K(k)⋅(I(x+k,y) + I(x+(R-k),y))) + K(R/2)⋅I(x+R/2,y); where sums goes from 0...R/2
///
/// It is important to use color groups because for whole group one weight must be applied
fn filter_symmetric_row<T, F, const N: usize>(
    arena: Arena<T>,
    dst_row: &mut [T],
    image_size: FilterImageSize,
    scanned_kernel: &[F],
) where
    T: Copy + AsPrimitive<F> + Default,
    F: ToStorage<T>
        + Mul<Output = F>
        + MulAdd<F, Output = F>
        + Default
        + Add<F, Output = F>
        + Copy
        + 'static,
    i32: AsPrimitive<F>,
{
    let width = image_size.width;

    let src = arena.src;

    let length = scanned_kernel.len();
    let half_len = length / 2;

    let mut cx = 0usize;

    while cx + 8 < width {
        let v_cx = cx * N;
        let src = &src[cx * N..(v_cx + length * N + N * 8)];
        let coeff = scanned_kernel[half_len];

        let mut k0: ColorGroup<N, F> = ld_group!(src, N, half_len * N).mul(coeff);
        let mut k1: ColorGroup<N, F> = ld_group!(src, N, half_len * N + N).mul(coeff);
        let mut k2: ColorGroup<N, F> = ld_group!(src, N, half_len * N + N * 2).mul(coeff);
        let mut k3: ColorGroup<N, F> = ld_group!(src, N, half_len * N + N * 3).mul(coeff);
        let mut k4: ColorGroup<N, F> = ld_group!(src, N, half_len * N + N * 4).mul(coeff);
        let mut k5: ColorGroup<N, F> = ld_group!(src, N, half_len * N + N * 5).mul(coeff);
        let mut k6: ColorGroup<N, F> = ld_group!(src, N, half_len * N + N * 6).mul(coeff);
        let mut k7: ColorGroup<N, F> = ld_group!(src, N, half_len * N + N * 7).mul(coeff);

        for (i, &coeff) in scanned_kernel.iter().take(half_len).enumerate() {
            let rollback = length - i - 1;
            let fw = &src[(i * N)..((i + 8) * N)];
            let bw = &src[(rollback * N)..((rollback + 8) * N)];
            k0 = ld_group!(fw, N, 0)
                .add(ld_group!(bw, N, 0))
                .mul_add(k0, coeff);
            k1 = ld_group!(fw, N, N)
                .add(ld_group!(bw, N, N))
                .mul_add(k1, coeff);
            k2 = ld_group!(fw, N, 2 * N)
                .add(ld_group!(bw, N, 2 * N))
                .mul_add(k2, coeff);
            k3 = ld_group!(fw, N, 3 * N)
                .add(ld_group!(bw, N, 3 * N))
                .mul_add(k3, coeff);
            k4 = ld_group!(fw, N, 4 * N)
                .add(ld_group!(bw, N, 4 * N))
                .mul_add(k4, coeff);
            k5 = ld_group!(fw, N, 5 * N)
                .add(ld_group!(bw, N, 5 * N))
                .mul_add(k5, coeff);
            k6 = ld_group!(fw, N, 6 * N)
                .add(ld_group!(bw, N, 6 * N))
                .mul_add(k6, coeff);
            k7 = ld_group!(fw, N, 7 * N)
                .add(ld_group!(bw, N, 7 * N))
                .mul_add(k7, coeff);
        }

        let dst_offset = cx * N;

        let shaped_dst = &mut dst_row[dst_offset..(dst_offset + N * 8)];
        st_group!(shaped_dst, N, 0, k0);
        st_group!(shaped_dst, N, N, k1);
        st_group!(shaped_dst, N, N * 2, k2);
        st_group!(shaped_dst, N, N * 3, k3);
        st_group!(shaped_dst, N, N * 4, k4);
        st_group!(shaped_dst, N, N * 5, k5);
        st_group!(shaped_dst, N, N * 6, k6);
        st_group!(shaped_dst, N, N * 7, k7);
        cx += 8;
    }

    while cx + 4 < width {
        let v_cx = cx * N;
        let src = &src[cx * N..(v_cx + length * N + N * 4)];
        let coeff = scanned_kernel[half_len];

        let mut k0: ColorGroup<N, F> = ld_group!(src, N, half_len * N).mul(coeff);
        let mut k1: ColorGroup<N, F> = ld_group!(src, N, half_len * N + N).mul(coeff);
        let mut k2: ColorGroup<N, F> = ld_group!(src, N, half_len * N + N * 2).mul(coeff);
        let mut k3: ColorGroup<N, F> = ld_group!(src, N, half_len * N + N * 3).mul(coeff);

        for (i, &coeff) in scanned_kernel.iter().take(half_len).enumerate() {
            let rollback = length - i - 1;
            let fw = &src[(i * N)..((i + 4) * N)];
            let bw = &src[(rollback * N)..((rollback + 4) * N)];
            k0 = ld_group!(fw, N, 0)
                .add(ld_group!(bw, N, 0))
                .mul_add(k0, coeff);
            k1 = ld_group!(fw, N, N)
                .add(ld_group!(bw, N, N))
                .mul_add(k1, coeff);
            k2 = ld_group!(fw, N, 2 * N)
                .add(ld_group!(bw, N, 2 * N))
                .mul_add(k2, coeff);
            k3 = ld_group!(fw, N, 3 * N)
                .add(ld_group!(bw, N, 3 * N))
                .mul_add(k3, coeff);
        }

        let dst_offset = cx * N;
        let shaped_dst = &mut dst_row[dst_offset..(dst_offset + N * 4)];
        st_group!(shaped_dst, N, 0, k0);
        st_group!(shaped_dst, N, N, k1);
        st_group!(shaped_dst, N, N * 2, k2);
        st_group!(shaped_dst, N, N * 3, k3);
        cx += 4;
    }

    #[allow(clippy::needless_range_loop)]
    for x in cx..width {
        let v_cx = x * N;
        let src = &src[v_cx..(v_cx + length * N)];
        let coeff = scanned_kernel[half_len];

        let mut k0: ColorGroup<N, F> = ld_group!(src, N, half_len * N).mul(coeff);

        for (i, &coeff) in scanned_kernel.iter().take(half_len).enumerate() {
            let rollback = length - i - 1;
            let fw = &src[(i * N)..((i + 1) * N)];
            let bw = &src[(rollback * N)..((rollback + 1) * N)];
            k0 = ld_group!(fw, N, 0)
                .add(ld_group!(bw, N, 0))
                .mul_add(k0, coeff);
        }

        let dst_offset = x * N;
        let shaped_dst = &mut dst_row[dst_offset..(dst_offset + N)];
        st_group!(shaped_dst, N, 0, k0);
    }
}

trait KernelTransformer<F, I> {
    fn transform(input: F) -> I;
}

impl KernelTransformer<f32, i32> for u8 {
    fn transform(input: f32) -> i32 {
        const SCALE: f32 = (1 << PRECISION) as f32;
        (input * SCALE).round() as i32
    }
}

impl KernelTransformer<f32, f32> for f32 {
    fn transform(input: f32) -> f32 {
        input
    }
}

impl KernelTransformer<f32, f32> for u16 {
    fn transform(input: f32) -> f32 {
        input
    }
}

/// Removes meaningless values from kernel preserving symmetry
fn prepare_symmetric_kernel<I: Copy + PartialEq + 'static>(kernel: &[I]) -> Vec<I>
where
    i32: AsPrimitive<I>,
{
    let zeros: I = 0i32.as_();
    let mut new_kernel = kernel.to_vec();
    while new_kernel.len() > 2
        && (new_kernel.last().unwrap().eq(&zeros) && new_kernel.first().unwrap().eq(&zeros))
    {
        new_kernel.remove(0);
        new_kernel.remove(new_kernel.len() - 1);
    }

    new_kernel
}

/// Performs 2D separable convolution on the image
///
/// Currently implemented only for symmetrical filters
///
/// # Arguments
///
/// * `image`: Single plane image
/// * `destination`: Destination image
/// * `image_size`: Image size see [FilterImageSize]
/// * `row_kernel`: Row kernel, *size must be odd*!
/// * `column_kernel`: Column kernel, *size must be odd*!
fn filter_1d<T, F, I, const N: usize>(
    image: &[T],
    destination: &mut [T],
    image_size: FilterImageSize,
    row_kernel: &[F],
    column_kernel: &[F],
) -> Result<(), ImageError>
where
    T: Copy + AsPrimitive<F> + Default + Send + Sync + KernelTransformer<F, I> + AsPrimitive<I>,
    F: Default + 'static + Copy,
    I: ToStorage<T>
        + Mul<I, Output = I>
        + Add<I, Output = I>
        + MulAdd<I, Output = I>
        + Send
        + Sync
        + PartialEq
        + Default
        + 'static
        + Copy,
    i32: AsPrimitive<F> + AsPrimitive<I>,
    f64: AsPrimitive<T>,
{
    if image.len() != image_size.width * image_size.height * N {
        return Err(ImageError::Limits(LimitError::from_kind(
            LimitErrorKind::DimensionError,
        )));
    }
    if destination.len() != image_size.width * image_size.height * N {
        return Err(ImageError::Limits(LimitError::from_kind(
            LimitErrorKind::DimensionError,
        )));
    }

    assert_ne!(row_kernel.len() & 1, 0, "Row kernel length must be odd");
    assert_ne!(
        column_kernel.len() & 1,
        0,
        "Column kernel length must be odd"
    );

    let mut scanned_row_kernel = row_kernel
        .iter()
        .map(|&x| T::transform(x))
        .collect::<Vec<I>>();
    let mut scanned_column_kernel = column_kernel
        .iter()
        .map(|&x| T::transform(x))
        .collect::<Vec<I>>();

    scanned_row_kernel = prepare_symmetric_kernel(&scanned_row_kernel);
    scanned_column_kernel = prepare_symmetric_kernel(&scanned_column_kernel);

    let zeros: I = 0i32.as_();

    if scanned_row_kernel.is_empty()
        || scanned_column_kernel.is_empty()
        || (scanned_row_kernel.len() == 1 && scanned_row_kernel.first().unwrap().eq(&zeros))
        || (scanned_column_kernel.len() == 1 && scanned_column_kernel.first().unwrap().eq(&zeros))
    {
        destination.fill(T::default());
        return Ok(());
    }

    let mut transient_image = vec![T::default(); image_size.width * image_size.height * N];

    let start_time = Instant::now();

    for (y, dst) in transient_image
        .chunks_exact_mut(image_size.width * N)
        .enumerate()
    {
        // This is important to perform padding before convolution.
        // That approach allows to iterate without unnecessary branching
        // and highly effective from low sized kernels to reasonable huge kernels.
        // It is especially more effective for low sized kernels than copying
        // specifically for big sized images.
        // If image row is `asdfgh` then this method with clamp will produce row
        // `aaa asdfgh hhh` padded exactly on half kernel size
        let arena = make_arena_row::<T, N>(
            image,
            y,
            image_size,
            KernelShape {
                width: row_kernel.len(),
                height: 0,
            },
        );

        filter_symmetric_row::<T, I, N>(
            Arena {
                src: &arena.arena,
                components: N,
            },
            dst,
            image_size,
            &scanned_row_kernel,
        );
    }

    println!("Horizontal time {:?}", start_time.elapsed());

    let column_kernel_shape = KernelShape {
        width: 0,
        height: scanned_column_kernel.len(),
    };

    // This is important to perform padding before convolution.
    // That approach allows to iterate without unnecessary branching
    // and highly effective from low sized kernels to reasonable huge kernels.
    // It is especially more effective for low sized kernels than copying
    // specifically for big sized images.
    // `This is virtual padding!` that means it produces two non-contiguous arrays.
    // They will virtually replace image row, when convolution kernel goes out of bounds on Y coordinate.
    let column_arena_k =
        make_columns_arenas::<T, N>(transient_image.as_slice(), image_size, column_kernel_shape);

    let top_pad = column_arena_k.top_pad.as_slice();
    let bottom_pad = column_arena_k.bottom_pad.as_slice();

    let pad_h = column_kernel_shape.height / 2;

    let transient_image_slice = transient_image.as_slice();

    let src_stride = image_size.width * N;

    let start_time = Instant::now();

    for (y, dst) in destination
        .chunks_exact_mut(image_size.width * N)
        .enumerate()
    {
        let mut brows: Vec<&[T]> = vec![&transient_image_slice[0..]; column_kernel_shape.height];

        for (k, row) in (0..column_kernel_shape.height).zip(brows.iter_mut()) {
            if (y as i64 - pad_h as i64 + k as i64) < 0 {
                *row = &top_pad[(pad_h - k - 1) * src_stride..];
            } else if (y as i64 - pad_h as i64 + k as i64) as usize >= image_size.height {
                *row = &bottom_pad[(k - pad_h - 1) * src_stride..];
            } else {
                let fy = (y as i64 + k as i64 - pad_h as i64) as usize;
                let start_offset = src_stride * fy;
                *row = &transient_image_slice[start_offset..(start_offset + src_stride)];
            }
        }

        let empty_vec = vec![];

        let brows_slice = brows.as_slice();

        filter_symmetric_column(
            Arena {
                src: &empty_vec,
                components: N,
            },
            brows_slice,
            dst,
            image_size,
            &scanned_column_kernel,
        );
    }

    println!("Vertical time {:?}", start_time.elapsed());

    Ok(())
}

pub(crate) fn filter_1d_plane(
    image: &[u8],
    destination: &mut [u8],
    image_size: FilterImageSize,
    row_kernel: &[f32],
    column_kernel: &[f32],
) -> Result<(), ImageError> {
    filter_1d::<u8, f32, i32, 1>(image, destination, image_size, row_kernel, column_kernel)
}

pub(crate) fn filter_1d_la(
    image: &[u8],
    destination: &mut [u8],
    image_size: FilterImageSize,
    row_kernel: &[f32],
    column_kernel: &[f32],
) -> Result<(), ImageError> {
    filter_1d::<u8, f32, i32, 2>(image, destination, image_size, row_kernel, column_kernel)
}

pub(crate) fn filter_1d_rgb(
    image: &[u8],
    destination: &mut [u8],
    image_size: FilterImageSize,
    row_kernel: &[f32],
    column_kernel: &[f32],
) -> Result<(), ImageError> {
    filter_1d::<u8, f32, i32, 3>(image, destination, image_size, row_kernel, column_kernel)
}

pub(crate) fn filter_1d_rgba(
    image: &[u8],
    destination: &mut [u8],
    image_size: FilterImageSize,
    row_kernel: &[f32],
    column_kernel: &[f32],
) -> Result<(), ImageError> {
    filter_1d::<u8, f32, i32, 4>(image, destination, image_size, row_kernel, column_kernel)
}

pub(crate) fn filter_1d_rgb_f32(
    image: &[f32],
    destination: &mut [f32],
    image_size: FilterImageSize,
    row_kernel: &[f32],
    column_kernel: &[f32],
) -> Result<(), ImageError> {
    filter_1d::<f32, f32, f32, 3>(image, destination, image_size, row_kernel, column_kernel)
}

pub(crate) fn filter_1d_rgba_f32(
    image: &[f32],
    destination: &mut [f32],
    image_size: FilterImageSize,
    row_kernel: &[f32],
    column_kernel: &[f32],
) -> Result<(), ImageError> {
    filter_1d::<f32, f32, f32, 4>(image, destination, image_size, row_kernel, column_kernel)
}

pub(crate) fn filter_1d_rgb_u16(
    image: &[u16],
    destination: &mut [u16],
    image_size: FilterImageSize,
    row_kernel: &[f32],
    column_kernel: &[f32],
) -> Result<(), ImageError> {
    filter_1d::<u16, f32, f32, 3>(image, destination, image_size, row_kernel, column_kernel)
}

pub(crate) fn filter_1d_rgba_u16(
    image: &[u16],
    destination: &mut [u16],
    image_size: FilterImageSize,
    row_kernel: &[f32],
    column_kernel: &[f32],
) -> Result<(), ImageError> {
    filter_1d::<u16, f32, f32, 4>(image, destination, image_size, row_kernel, column_kernel)
}

pub(crate) fn filter_1d_la_u16(
    image: &[u16],
    destination: &mut [u16],
    image_size: FilterImageSize,
    row_kernel: &[f32],
    column_kernel: &[f32],
) -> Result<(), ImageError> {
    filter_1d::<u16, f32, f32, 2>(image, destination, image_size, row_kernel, column_kernel)
}

pub(crate) fn filter_1d_plane_u16(
    image: &[u16],
    destination: &mut [u16],
    image_size: FilterImageSize,
    row_kernel: &[f32],
    column_kernel: &[f32],
) -> Result<(), ImageError> {
    filter_1d::<u16, f32, f32, 1>(image, destination, image_size, row_kernel, column_kernel)
}
