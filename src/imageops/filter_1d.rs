#![forbid(unsafe_code)]
use crate::error::{LimitError, LimitErrorKind};
use crate::ImageError;
use num_traits::{AsPrimitive, MulAdd};
use std::mem::size_of;
use std::ops::{Add, Mul};

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
/// Rust inserts libc `fmaf` based implementation here if FMA is clearly not available at compile time.
/// This needs for speed only, one rounding error don't do anything useful here, thus it's blocked when
/// we can't detect FMA availability at compile time.
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
fn make_arena_row<T, const N: usize>(
    image: &[T],
    row_buffer: &mut [T],
    source_y: usize,
    image_size: FilterImageSize,
    kernel_size: KernelShape,
) where
    T: Default + Copy + Send + Sync + 'static,
    f64: AsPrimitive<T>,
{
    assert_eq!(image.len(), N * image_size.width * image_size.height);

    let pad_w = (kernel_size.width / 2).max(1);

    let arena_width = image_size.width * N + pad_w * 2 * N;

    let source_offset = source_y * image_size.width * N;
    assert_eq!(row_buffer.len(), arena_width);

    let row_dst = &mut row_buffer[pad_w * N..(pad_w * N + image_size.width * N)];

    let source_row = &image[source_offset..(source_offset + image_size.width * N)];

    for (dst, src) in row_dst.iter_mut().zip(source_row.iter()) {
        *dst = *src;
    }

    for (x, dst) in (0..pad_w).zip(row_buffer.chunks_exact_mut(N)) {
        let old_x = x.saturating_sub(pad_w).min(image_size.width - 1);
        let old_px = old_x * N;
        let src_iter = &source_row[old_px..(old_px + N)];
        for (dst, src) in dst.iter_mut().zip(src_iter.iter()) {
            *dst = *src;
        }
    }

    for (x, dst) in
        (image_size.width..(image_size.width + pad_w)).zip(row_buffer.chunks_exact_mut(N).rev())
    {
        let old_x = x.max(0).min(image_size.width - 1);
        let old_px = old_x * N;
        let src_iter = &source_row[old_px..(old_px + N)];
        for (dst, src) in dst.iter_mut().zip(src_iter.iter()) {
            *dst = *src;
        }
    }
}

#[derive(Clone)]
struct ArenaColumns<T>
where
    T: Copy,
{
    top_pad: Vec<T>,
    bottom_pad: Vec<T>,
}

/// Pads a column image with *clamp* strategy
///
/// This method is used for *virtual* column padding.
/// It produces two arrays that represents virtual image top part and bottom
fn make_columns_arenas<T, const N: usize>(
    image: &[T],
    image_size: FilterImageSize,
    kernel_size: KernelShape,
) -> ArenaColumns<T>
where
    T: Default + Copy + Send + Sync + 'static,
    f64: AsPrimitive<T>,
{
    assert_eq!(image.len(), N * image_size.width * image_size.height);
    let pad_h = kernel_size.height / 2;

    let mut top_pad = vec![T::default(); pad_h * image_size.width * N];
    let mut bottom_pad = vec![T::default(); pad_h * image_size.width * N];

    let top_pad_stride = image_size.width * N;

    for (ky, dst) in (0..pad_h).zip(top_pad.chunks_exact_mut(top_pad_stride)) {
        for (kx, dst) in (0..image_size.width).zip(dst.chunks_exact_mut(N)) {
            let y = ky.saturating_sub(pad_h).min(image_size.height - 1);
            let v_src = y * top_pad_stride + kx * N;

            let src_iter = &image[v_src..(v_src + N)];
            for (dst, src) in dst.iter_mut().zip(src_iter.iter()) {
                *dst = *src;
            }
        }
    }

    let bottom_iter_dst = bottom_pad.chunks_exact_mut(top_pad_stride);

    for (ky, dst) in (0..pad_h).zip(bottom_iter_dst) {
        for (kx, dst) in (0..image_size.width).zip(dst.chunks_exact_mut(N)) {
            let y = (ky + image_size.height).min(image_size.height - 1);
            let v_src = y * top_pad_stride + kx * N;
            let src_iter = &image[v_src..(v_src + N)];
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

const Q0_15: i32 = 15;

impl ToStorage<u8> for u32 {
    #[inline(always)]
    #[allow(clippy::manual_clamp)]
    fn to_(self) -> u8 {
        ((self + (1 << (Q0_15 - 1))) >> Q0_15).min(255).max(0) as u8
    }
}

impl ToStorage<u16> for f32 {
    #[inline(always)]
    fn to_(self) -> u16 {
        self.round().min(u16::MAX as f32).max(0.) as u16
    }
}

impl ToStorage<f32> for f32 {
    #[inline(always)]
    fn to_(self) -> f32 {
        self
    }
}

/// Performs column convolution pass for symmetrical filter
///
/// Common convolution formula O(x,y)=∑K(k)⋅I(x,y+k); where sums goes from 0...R
/// when filter is symmetric that we can half kernel reads by using formula
/// O(x,y)=(∑K(k)⋅(I(x,y+k) + I(x,y+(R-k)))) + K(R/2)⋅I(x,y+R/2); where sums goes from 0...R/2
fn filter_symmetric_column<T, F, const N: usize>(
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
    let dst_stride = image_size.width * N;

    let length = kernel.len();
    let half_len = length / 2;

    let mut cx = 0usize;

    let coeff = kernel[half_len];

    if size_of::<T>() == 1 {
        while cx + 32 < dst_stride {
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
                let other_size = length - i - 1;
                let fw_src = arena_src[i];
                let rw_src = arena_src[other_size];
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
        let mut store: [F; 16] = [F::default(); 16];

        let v_src = &arena_src[half_len][cx..(cx + 16)];

        for (dst, src) in store.iter_mut().zip(v_src) {
            *dst = src.as_().mul(coeff);
        }

        for (i, &coeff) in kernel.iter().take(half_len).enumerate() {
            let other_size = length - i - 1;
            let fw = &arena_src[i][cx..(cx + 16)];
            let bw = &arena_src[other_size][cx..(cx + 16)];

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
        let v_src = &arena_src[half_len][cx..(cx + 4)];

        let mut k0 = v_src[0].as_().mul(coeff);
        let mut k1 = v_src[1].as_().mul(coeff);
        let mut k2 = v_src[2].as_().mul(coeff);
        let mut k3 = v_src[3].as_().mul(coeff);

        for (i, &coeff) in kernel.iter().take(half_len).enumerate() {
            let other_size = length - i - 1;
            let fw = &arena_src[i][cx..(cx + 4)];
            let bw = &arena_src[other_size][cx..(cx + 4)];
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
        let v_src = &arena_src[half_len][x..(x + 1)];

        let mut k0 = v_src[0].as_().mul(coeff);

        for (i, &coeff) in kernel.iter().take(half_len).enumerate() {
            let other_size = length - i - 1;
            let fw = &arena_src[i][x..(x + 1)];
            let bw = &arena_src[other_size][x..(x + 1)];
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
fn filter_symmetric_row<T, F, const N: usize>(arena: &[T], dst_row: &mut [T], scanned_kernel: &[F])
where
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
    let src = arena;

    let length = scanned_kernel.len();
    let half_len = length / 2;

    let hc = scanned_kernel[half_len];

    for (x, dst) in dst_row.chunks_exact_mut(4).enumerate() {
        let v_cx = x * 4;
        let src = &src[v_cx..];

        let mut k0 = src[half_len * N].as_() * hc;
        let mut k1 = src[half_len * N + 1].as_() * hc;
        let mut k2 = src[half_len * N + 2].as_() * hc;
        let mut k3 = src[half_len * N + 3].as_() * hc;

        for (i, &coeff) in scanned_kernel.iter().take(half_len).enumerate() {
            let other_size = length - i - 1;
            let fw = &src[(i * N)..(i * N) + 4];
            let bw = &src[(other_size * N)..(other_size * N) + 4];
            k0 = mla(k0, fw[0].as_() + bw[0].as_(), coeff);
            k1 = mla(k1, fw[1].as_() + bw[1].as_(), coeff);
            k2 = mla(k2, fw[2].as_() + bw[2].as_(), coeff);
            k3 = mla(k3, fw[3].as_() + bw[3].as_(), coeff);
        }

        dst[0] = k0.to_();
        dst[1] = k1.to_();
        dst[2] = k2.to_();
        dst[3] = k3.to_();
    }

    let dzx = dst_row.chunks_exact_mut(4).len() * 4;
    let remainder = dst_row.chunks_exact_mut(4).into_remainder();

    for (x, dst) in remainder.iter_mut().enumerate() {
        let v_cx = x + dzx;
        let src = &src[v_cx..];

        let mut k0 = src[half_len * N].as_() * hc;

        for (i, &coeff) in scanned_kernel.iter().take(half_len).enumerate() {
            let other_size = length - i - 1;
            let fw = &src[(i * N)..(i * N) + 1];
            let bw = &src[(other_size * N)..(other_size * N) + 1];
            k0 = mla(k0, fw[0].as_() + bw[0].as_(), coeff);
        }

        *dst = k0.to_();
    }
}

trait KernelTransformer<F, I> {
    fn transform(input: F) -> I;
}

impl KernelTransformer<f32, u32> for u8 {
    fn transform(input: f32) -> u32 {
        const SCALE: f32 = (1 << Q0_15) as f32;
        (input * SCALE).min(((1u32 << Q0_15) - 1) as f32).max(0.) as u32
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

const RING_QUEUE_CIRCULAR_CUTOFF: usize = 55;

fn filter_1d_ring_queue<T, F, I, const N: usize>(
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

    if scanned_row_kernel.is_empty() || scanned_column_kernel.is_empty() {
        for (dst, src) in destination.iter_mut().zip(image.iter()) {
            *dst = *src;
        }
        return Ok(());
    }

    let pad_w = (scanned_row_kernel.len() / 2).max(1);

    let arena_width = image_size.width * N + pad_w * 2 * N;
    let mut row_buffer = vec![T::default(); arena_width];

    // If column kernel is small better to use ring queue circular convolution

    let full_width = image_size.width * N;

    // Flat ring queue
    let mut buffer = vec![T::default(); image_size.width * N * scanned_column_kernel.len()];

    let column_kernel_len = scanned_column_kernel.len();

    let half_kernel = column_kernel_len / 2;

    make_arena_row::<T, N>(
        image,
        &mut row_buffer,
        0,
        image_size,
        KernelShape {
            width: scanned_row_kernel.len(),
            height: 0,
        },
    );
    filter_symmetric_row::<T, I, N>(&row_buffer, &mut buffer[..full_width], &scanned_row_kernel);

    let (src_row, rest) = buffer.split_at_mut(full_width);
    for dst in rest.chunks_exact_mut(full_width).take(half_kernel) {
        for (dst, src) in dst.iter_mut().zip(src_row.iter()) {
            *dst = *src;
        }
    }

    let mut start_ky = column_kernel_len / 2 + 1;

    start_ky %= column_kernel_len;

    for y in 1..image_size.height + half_kernel {
        let new_y = if y < image_size.height {
            y
        } else {
            y.min(image_size.height - 1)
        };

        make_arena_row::<T, N>(
            image,
            &mut row_buffer,
            new_y,
            image_size,
            KernelShape {
                width: scanned_row_kernel.len(),
                height: 0,
            },
        );

        filter_symmetric_row::<T, I, N>(
            &row_buffer,
            &mut buffer[start_ky * full_width..(start_ky + 1) * full_width],
            &scanned_row_kernel,
        );

        if y >= half_kernel {
            let mut brows = vec![image.as_ref(); column_kernel_len];

            for (i, brow) in brows.iter_mut().enumerate() {
                let ky = (i + start_ky + 1) % column_kernel_len;
                *brow = &buffer[ky * full_width..(ky + 1) * full_width];
            }

            let dy = y - half_kernel;

            let dst = &mut destination[dy * full_width..(dy + 1) * full_width];

            filter_symmetric_column::<T, I, N>(&brows, dst, image_size, &scanned_column_kernel);
        }

        start_ky += 1;
        start_ky %= column_kernel_len;
    }

    Ok(())
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

    if scanned_row_kernel.is_empty() || scanned_column_kernel.is_empty() {
        for (dst, src) in destination.iter_mut().zip(image.iter()) {
            *dst = *src;
        }
        return Ok(());
    }

    if column_kernel.len() < RING_QUEUE_CIRCULAR_CUTOFF {
        return filter_1d_ring_queue::<T, F, I, N>(
            image,
            destination,
            image_size,
            row_kernel,
            column_kernel,
        );
    }

    let pad_w = (scanned_row_kernel.len() / 2).max(1);

    let arena_width = image_size.width * N + pad_w * 2 * N;
    let mut row_buffer = vec![T::default(); arena_width];

    let mut transient_image = vec![T::default(); image_size.width * image_size.height * N];

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
        make_arena_row::<T, N>(
            image,
            &mut row_buffer,
            y,
            image_size,
            KernelShape {
                width: scanned_row_kernel.len(),
                height: 0,
            },
        );

        filter_symmetric_row::<T, I, N>(&row_buffer, dst, &scanned_row_kernel);
    }

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

        let brows_slice = brows.as_slice();

        filter_symmetric_column::<T, I, N>(brows_slice, dst, image_size, &scanned_column_kernel);
    }

    Ok(())
}

pub(crate) fn filter_1d_plane(
    image: &[u8],
    destination: &mut [u8],
    image_size: FilterImageSize,
    row_kernel: &[f32],
    column_kernel: &[f32],
) -> Result<(), ImageError> {
    filter_1d::<u8, f32, u32, 1>(image, destination, image_size, row_kernel, column_kernel)
}

pub(crate) fn filter_1d_la(
    image: &[u8],
    destination: &mut [u8],
    image_size: FilterImageSize,
    row_kernel: &[f32],
    column_kernel: &[f32],
) -> Result<(), ImageError> {
    filter_1d::<u8, f32, u32, 2>(image, destination, image_size, row_kernel, column_kernel)
}

pub(crate) fn filter_1d_rgb(
    image: &[u8],
    destination: &mut [u8],
    image_size: FilterImageSize,
    row_kernel: &[f32],
    column_kernel: &[f32],
) -> Result<(), ImageError> {
    filter_1d::<u8, f32, u32, 3>(image, destination, image_size, row_kernel, column_kernel)
}

pub(crate) fn filter_1d_rgba(
    image: &[u8],
    destination: &mut [u8],
    image_size: FilterImageSize,
    row_kernel: &[f32],
    column_kernel: &[f32],
) -> Result<(), ImageError> {
    filter_1d::<u8, f32, u32, 4>(image, destination, image_size, row_kernel, column_kernel)
}

pub(crate) fn filter_1d_la_f32(
    image: &[f32],
    destination: &mut [f32],
    image_size: FilterImageSize,
    row_kernel: &[f32],
    column_kernel: &[f32],
) -> Result<(), ImageError> {
    filter_1d::<f32, f32, f32, 2>(image, destination, image_size, row_kernel, column_kernel)
}

pub(crate) fn filter_1d_plane_f32(
    image: &[f32],
    destination: &mut [f32],
    image_size: FilterImageSize,
    row_kernel: &[f32],
    column_kernel: &[f32],
) -> Result<(), ImageError> {
    filter_1d::<f32, f32, f32, 1>(image, destination, image_size, row_kernel, column_kernel)
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
