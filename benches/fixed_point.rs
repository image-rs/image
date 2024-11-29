use criterion::{criterion_group, criterion_main, Criterion};
use image::{ImageBuffer, Rgb};

pub fn bench_jpeg_fixed_point(c: &mut Criterion) {
    let width = 1920;
    let height = 1920;
    let src = ImageBuffer::from_pixel(width, height, Rgb([213u8, 156, 64]));

    c.bench_function("Test JPEG floating point", |b| {
        let mut y_plane = vec![0u8; width as usize * height as usize];
        let mut u_plane = vec![0u8; width as usize * height as usize];
        let mut v_plane = vec![0u8; width as usize * height as usize];
        b.iter(|| {
            for (((rgb, y_dst), u_dst), v_dst) in src
                .chunks_exact(3)
                .zip(y_plane.iter_mut())
                .zip(u_plane.iter_mut())
                .zip(v_plane.iter_mut())
            {
                let r: f32 = rgb[0] as f32;
                let g: f32 = rgb[1] as f32;
                let b: f32 = rgb[2] as f32;

                // Coefficients from JPEG File Interchange Format (Version 1.02), multiplied for 255 maximum.
                let y = 0.299 * r + 0.587 * g + 0.114 * b;
                let cb = -0.1687 * r - 0.3313 * g + 0.5 * b + 128.;
                let cr = 0.5 * r - 0.4187 * g - 0.0813 * b + 128.;
                *y_dst = y as u8;
                *u_dst = cb as u8;
                *v_dst = cr as u8;
            }
        });
    });

    c.bench_function("Test JPEG fixed point", |b| {
        let mut y_plane = vec![0u8; width as usize * height as usize];
        let mut u_plane = vec![0u8; width as usize * height as usize];
        let mut v_plane = vec![0u8; width as usize * height as usize];
        b.iter(|| {
            for (((rgb, y_dst), u_dst), v_dst) in src
                .chunks_exact(3)
                .zip(y_plane.iter_mut())
                .zip(u_plane.iter_mut())
                .zip(v_plane.iter_mut())
            {
                let r = rgb[0] as i32;
                let g = rgb[1] as i32;
                let b = rgb[2] as i32;
                const C_YR: i32 = 19595; // 0.29900 = 19595 * 2^-16
                const C_YG: i32 = 38469; // 0.58700 = 38469 * 2^-16
                const C_YB: i32 = 7471; // 0.11400 = 7471 * 2^-16
                const Y_ROUNDING: i32 = (1 << 15) - 1; // + 0.5 to perform rounding shift right in-place
                const C_UR: i32 = 11059; // 0.16874 = 11059 * 2^-16
                const C_UG: i32 = 21709; // 0.33126 = 21709 * 2^-16
                const C_UB: i32 = 32768; // 0.5 = 32768 * 2^-16
                const UV_BIAS_ROUNDING: i32 = (128 * (1 << 16)) + ((1 << 15) - 1); // 128 + 0.5 = ((128 * (1 << 16)) + ((1 << 15) - 1)) * 2^-16 ; + 0.5 to perform rounding shift right in-place
                const C_VR: i32 = C_UB; // 0.5 = 32768 * 2^-16
                const C_VG: i32 = 27439; // 0.41869 = 27439 * 2^-16
                const C_VB: i32 = 5329; // 0.08131409 = 5329 * 2^-16

                let y = (C_YR * r + C_YG * g + C_YB * b + Y_ROUNDING) >> 16;
                let cb = (-C_UR * r - C_UG * g + C_UB * b + UV_BIAS_ROUNDING) >> 16;
                let cr = (C_VR * r - C_VG * g - C_VB * b + UV_BIAS_ROUNDING) >> 16;

                *y_dst = y as u8;
                *u_dst = cb as u8;
                *v_dst = cr as u8;
            }
        });
    });
}

criterion_group!(benches, bench_jpeg_fixed_point);
criterion_main!(benches);