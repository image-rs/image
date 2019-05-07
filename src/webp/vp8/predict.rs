use super::{IntraMode, clamp};

fn avg3(left: u8, this: u8, right: u8) -> u8 {
    let avg = (u16::from(left) + 2 * u16::from(this) + u16::from(right) + 2) >> 2;
    avg as u8
}

fn avg2(this: u8, right: u8) -> u8 {
    let avg = (u16::from(this) + u16::from(right) + 1) >> 1;
    avg as u8
}

pub fn add_residue(pblock: &mut [u8], rblock: &[i32], y0: usize, x0: usize, stride: usize) {
    for y in 0usize..4 {
        for x in 0usize..4 {
            let a = rblock[x + y * 4];
            let b = pblock[(y0 + y) * stride + x0 + x];
            let c = clamp(a + i32::from(b), 0, 255) as u8;
            pblock[(y0 + y) * stride + x0 + x] = c;
        }
    }
}

pub(super) fn pred4x4(ws: &mut [u8], stride: usize, modes: &[IntraMode], resdata: &[i32]) {
    for sby in 0usize..4 {
        for sbx in 0usize..4 {
            let i = sbx + sby * 4;
            let y0 = sby * 4 + 1;
            let x0 = sbx * 4 + 1;
            let rb = &resdata[i * 16..i * 16 + 16];

            match modes[i] {
                IntraMode::TM => tmpred(ws, 4, x0, y0, stride),
                IntraMode::VE => bvepred(ws, x0, y0, stride),
                IntraMode::HE => bhepred(ws, x0, y0, stride),
                IntraMode::DC => bdcpred(ws, x0, y0, stride),
                IntraMode::LD => bldpred(ws, x0, y0, stride),
                IntraMode::RD => brdpred(ws, x0, y0, stride),
                IntraMode::VR => bvrpred(ws, x0, y0, stride),
                IntraMode::VL => bvlpred(ws, x0, y0, stride),
                IntraMode::HD => bhdpred(ws, x0, y0, stride),
                IntraMode::HU => bhupred(ws, x0, y0, stride),
            }

            add_residue(ws, rb, y0, x0, stride);
        }
    }
}

pub fn vpred(a: &mut [u8], size: usize, x0: usize, y0: usize, stride: usize) {
    for y in 0usize..size {
        for x in 0usize..size {
            a[(x + x0) + stride * (y + y0)] = a[(x + x0) + stride * (y0 + y - 1)];
        }
    }
}

pub fn hpred(a: &mut [u8], size: usize, x0: usize, y0: usize, stride: usize) {
    for y in 0usize..size {
        for x in 0usize..size {
            a[(x + x0) + stride * (y + y0)] = a[(x + x0 - 1) + stride * (y0 + y)];
        }
    }
}

pub fn dcpred(a: &mut [u8], size: usize, stride: usize, above: bool, left: bool) {
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

pub fn tmpred(a: &mut [u8], size: usize, x0: usize, y0: usize, stride: usize) {
    for y in 0usize..size {
        for x in 0usize..size {
            let pred = i32::from(a[(y0 + y) * stride + x0 - 1])
                + i32::from(a[(y0 - 1) * stride + x0 + x])
                - i32::from(a[(y0 - 1) * stride + x0 - 1]);

            a[(x + x0) + stride * (y + y0)] = clamp(pred, 0, 255) as u8;
        }
    }
}

pub fn bdcpred(a: &mut [u8], x0: usize, y0: usize, stride: usize) {
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

pub fn topleft_pixel(a: &[u8], x0: usize, y0: usize, stride: usize) -> u8 {
    a[(y0 - 1) * stride + x0 - 1]
}

pub fn top_pixels(a: &[u8], x0: usize, y0: usize, stride: usize) -> (u8, u8, u8, u8, u8, u8, u8, u8) {
    let a0 = a[(y0 - 1) * stride + x0];
    let a1 = a[(y0 - 1) * stride + x0 + 1];
    let a2 = a[(y0 - 1) * stride + x0 + 2];
    let a3 = a[(y0 - 1) * stride + x0 + 3];
    let a4 = a[(y0 - 1) * stride + x0 + 4];
    let a5 = a[(y0 - 1) * stride + x0 + 5];
    let a6 = a[(y0 - 1) * stride + x0 + 6];
    let a7 = a[(y0 - 1) * stride + x0 + 7];

    (a0, a1, a2, a3, a4, a5, a6, a7)
}

pub fn left_pixels(a: &[u8], x0: usize, y0: usize, stride: usize) -> (u8, u8, u8, u8) {
    let l0 = a[y0 * stride + x0 - 1];
    let l1 = a[(y0 + 1) * stride + x0 - 1];
    let l2 = a[(y0 + 2) * stride + x0 - 1];
    let l3 = a[(y0 + 3) * stride + x0 - 1];

    (l0, l1, l2, l3)
}

pub fn edge_pixels(
    a: &[u8],
    x0: usize,
    y0: usize,
    stride: usize,
) -> (u8, u8, u8, u8, u8, u8, u8, u8, u8) {
    let e8 = a[(y0 - 1) * stride + x0 + 3];
    let e7 = a[(y0 - 1) * stride + x0 + 2];
    let e6 = a[(y0 - 1) * stride + x0 + 1];
    let e5 = a[(y0 - 1) * stride + x0];
    let e4 = a[(y0 - 1) * stride + x0 - 1];
    let e3 = a[y0 * stride + x0 - 1];
    let e2 = a[(y0 + 1) * stride + x0 - 1];
    let e1 = a[(y0 + 2) * stride + x0 - 1];
    let e0 = a[(y0 + 3) * stride + x0 - 1];

    (e0, e1, e2, e3, e4, e5, e6, e7, e8)
}

pub fn bvepred(a: &mut [u8], x0: usize, y0: usize, stride: usize) {
    let p = topleft_pixel(a, x0, y0, stride);
    let (a0, a1, a2, a3, a4, _, _, _) = top_pixels(a, x0, y0, stride);

    a[y0 * stride + x0] = avg3(p, a0, a1);
    a[(y0 + 1) * stride + x0] = avg3(p, a0, a1);
    a[(y0 + 2) * stride + x0] = avg3(p, a0, a1);
    a[(y0 + 3) * stride + x0] = avg3(p, a0, a1);

    a[y0 * stride + x0 + 1] = avg3(a0, a1, a2);
    a[(y0 + 1) * stride + x0 + 1] = avg3(a0, a1, a2);
    a[(y0 + 2) * stride + x0 + 1] = avg3(a0, a1, a2);
    a[(y0 + 3) * stride + x0 + 1] = avg3(a0, a1, a2);

    a[y0 * stride + x0 + 2] = avg3(a1, a2, a3);
    a[(y0 + 1) * stride + x0 + 2] = avg3(a1, a2, a3);
    a[(y0 + 2) * stride + x0 + 2] = avg3(a1, a2, a3);
    a[(y0 + 3) * stride + x0 + 2] = avg3(a1, a2, a3);

    a[y0 * stride + x0 + 3] = avg3(a2, a3, a4);
    a[(y0 + 1) * stride + x0 + 3] = avg3(a2, a3, a4);
    a[(y0 + 2) * stride + x0 + 3] = avg3(a2, a3, a4);
    a[(y0 + 3) * stride + x0 + 3] = avg3(a2, a3, a4);
}

pub fn bhepred(a: &mut [u8], x0: usize, y0: usize, stride: usize) {
    let p = topleft_pixel(a, x0, y0, stride);
    let (l0, l1, l2, l3) = left_pixels(a, x0, y0, stride);

    a[y0 * stride + x0] = avg3(p, l0, l1);
    a[y0 * stride + x0 + 1] = avg3(p, l0, l1);
    a[y0 * stride + x0 + 2] = avg3(p, l0, l1);
    a[y0 * stride + x0 + 3] = avg3(p, l0, l1);

    a[(y0 + 1) * stride + x0] = avg3(l0, l1, l2);
    a[(y0 + 1) * stride + x0 + 1] = avg3(l0, l1, l2);
    a[(y0 + 1) * stride + x0 + 2] = avg3(l0, l1, l2);
    a[(y0 + 1) * stride + x0 + 3] = avg3(l0, l1, l2);

    a[(y0 + 2) * stride + x0] = avg3(l1, l2, l3);
    a[(y0 + 2) * stride + x0 + 1] = avg3(l1, l2, l3);
    a[(y0 + 2) * stride + x0 + 2] = avg3(l1, l2, l3);
    a[(y0 + 2) * stride + x0 + 3] = avg3(l1, l2, l3);

    a[(y0 + 3) * stride + x0] = avg3(l2, l3, l3);
    a[(y0 + 3) * stride + x0 + 1] = avg3(l2, l3, l3);
    a[(y0 + 3) * stride + x0 + 2] = avg3(l2, l3, l3);
    a[(y0 + 3) * stride + x0 + 3] = avg3(l2, l3, l3);
}

pub fn bldpred(a: &mut [u8], x0: usize, y0: usize, stride: usize) {
    let (a0, a1, a2, a3, a4, a5, a6, a7) = top_pixels(a, x0, y0, stride);

    a[y0 * stride + x0] = avg3(a0, a1, a2);
    a[y0 * stride + x0 + 1] = avg3(a1, a2, a3);
    a[(y0 + 1) * stride + x0] = avg3(a1, a2, a3);
    a[y0 * stride + x0 + 2] = avg3(a2, a3, a4);
    a[(y0 + 1) * stride + x0 + 1] = avg3(a2, a3, a4);
    a[(y0 + 2) * stride + x0] = avg3(a2, a3, a4);
    a[y0 * stride + x0 + 3] = avg3(a3, a4, a5);
    a[(y0 + 1) * stride + x0 + 2] = avg3(a3, a4, a5);
    a[(y0 + 2) * stride + x0 + 1] = avg3(a3, a4, a5);
    a[(y0 + 3) * stride + x0] = avg3(a3, a4, a5);
    a[(y0 + 1) * stride + x0 + 3] = avg3(a4, a5, a6);
    a[(y0 + 2) * stride + x0 + 2] = avg3(a4, a5, a6);
    a[(y0 + 3) * stride + x0 + 1] = avg3(a4, a5, a6);
    a[(y0 + 2) * stride + x0 + 3] = avg3(a5, a6, a7);
    a[(y0 + 3) * stride + x0 + 2] = avg3(a5, a6, a7);
    a[(y0 + 3) * stride + x0 + 3] = avg3(a6, a7, a7);
}

pub fn brdpred(a: &mut [u8], x0: usize, y0: usize, stride: usize) {
    let (e0, e1, e2, e3, e4, e5, e6, e7, e8) = edge_pixels(a, x0, y0, stride);

    a[(y0 + 3) * stride + x0] = avg3(e0, e1, e2);
    a[(y0 + 3) * stride + x0 + 1] = avg3(e1, e2, e3);
    a[(y0 + 2) * stride + x0] = avg3(e1, e2, e3);
    a[(y0 + 3) * stride + x0 + 2] = avg3(e2, e3, e4);
    a[(y0 + 2) * stride + x0 + 1] = avg3(e2, e3, e4);
    a[(y0 + 1) * stride + x0] = avg3(e2, e3, e4);
    a[(y0 + 3) * stride + x0 + 3] = avg3(e3, e4, e5);
    a[(y0 + 2) * stride + x0 + 2] = avg3(e3, e4, e5);
    a[(y0 + 1) * stride + x0 + 1] = avg3(e3, e4, e5);
    a[y0 * stride + x0] = avg3(e3, e4, e5);
    a[(y0 + 2) * stride + x0 + 3] = avg3(e4, e5, e6);
    a[(y0 + 1) * stride + x0 + 2] = avg3(e4, e5, e6);
    a[y0 * stride + x0 + 1] = avg3(e4, e5, e6);
    a[(y0 + 1) * stride + x0 + 3] = avg3(e5, e6, e7);
    a[y0 * stride + x0 + 2] = avg3(e5, e6, e7);
    a[y0 * stride + x0 + 3] = avg3(e6, e7, e8);
}

pub fn bvrpred(a: &mut [u8], x0: usize, y0: usize, stride: usize) {
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

pub fn bvlpred(a: &mut [u8], x0: usize, y0: usize, stride: usize) {
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

pub fn bhdpred(a: &mut [u8], x0: usize, y0: usize, stride: usize) {
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

pub fn bhupred(a: &mut [u8], x0: usize, y0: usize, stride: usize) {
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
