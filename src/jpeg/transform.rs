//The forward dct's output coefficients are scaled by 8
//The inverse dct's output samples are clamped to the range [0, 255]

fn level_shift_up(a: i32) -> u8 {
    if a < -128 {0u8}
    else if a > 127 {255u8}
    else {a as u8 + 128u8}
}

/*
idct and fdct are Rust translations of jfdctint.c and jidctint.c from the
Independent JPEG Group's libjpeg version 9a
obtained from http://www.ijg.org/files/jpegsr9a.zip
They come with the following conditions of ditstribution and use:

	In plain English:

	1. We don't promise that this software works.  (But if you find any bugs,
		please let us know!)
	2. You can use this software for whatever you want.  You don't have to pay us.
	3. You may not pretend that you wrote this software.  If you use it in a
	   program, you must acknowledge somewhere in your documentation that
	   you've used the IJG code.

	In legalese:

	The authors make NO WARRANTY or representation, either express or implied,
	with respect to this software, its quality, accuracy, merchantability, or
	fitness for a particular purpose.  This software is provided "AS IS", and you,
	its user, assume the entire risk as to its quality and accuracy.

	This software is copyright (C) 1991-2014, Thomas G. Lane, Guido Vollbeding.
	All Rights Reserved except as specified below.

	Permission is hereby granted to use, copy, modify, and distribute this
	software (or portions thereof) for any purpose, without fee, subject to these
	conditions:
	(1) If any part of the source code for this software is distributed, then this
	README file must be included, with this copyright and no-warranty notice
	unaltered; and any additions, deletions, or changes to the original files
	must be clearly indicated in accompanying documentation.
	(2) If only executable code is distributed, then the accompanying
	documentation must state that "this software is based in part on the work of
	the Independent JPEG Group".
	(3) Permission for use of this software is granted only if the user accepts
	full responsibility for any undesirable consequences; the authors accept
	NO LIABILITY for damages of any kind.

	These conditions apply to any software derived from or based on the IJG code,
	not just to the unmodified library.  If you use our work, you ought to
	acknowledge us.

	Permission is NOT granted for the use of any IJG author's name or company name
	in advertising or publicity relating to this software or products derived from
	it.  This software may be referred to only as "the Independent JPEG Group's
	software".

	We specifically permit and encourage the use of this software as the basis of
	commercial products, provided that all warranty or liability claims are
	assumed by the product vendor.
*/

static CONST_BITS: i32 = 13;
static PASS1_BITS: i32 = 2;

static FIX_0_298631336: i32 = 2446;
static FIX_0_390180644: i32 = 3196;
static FIX_0_541196100: i32 = 4433;
static FIX_0_765366865: i32 = 6270;
static FIX_0_899976223: i32 = 7373;
static FIX_1_175875602: i32 = 9633;
static FIX_1_501321110: i32 = 12299;
static FIX_1_847759065: i32 = 15137;
static FIX_1_961570560: i32 = 16069;
static FIX_2_053119869: i32 = 16819;
static FIX_2_562915447: i32 = 20995;
static FIX_3_072711026: i32 = 25172;

pub fn fdct(samples: &[u8], coeffs: &mut [i32]) {
    //Pass 1: process rows.
    //Results are scaled by sqrt(8) compared to a true DCT
    //furthermore we scale the results by 2**PASS1_BITS
    for y in range(0u, 8) {
        let y0 = y * 8;

        //Even part
        let t0 = samples[y0 + 0] as i32 + samples[y0 + 7] as i32;
        let t1 = samples[y0 + 1] as i32 + samples[y0 + 6] as i32;
        let t2 = samples[y0 + 2] as i32 + samples[y0 + 5] as i32;
        let t3 = samples[y0 + 3] as i32 + samples[y0 + 4] as i32;

        let t10 = t0 + t3;
        let t12 = t0 - t3;
        let t11 = t1 + t2;
        let t13 = t1 - t2;

        let t0 = samples[y0 + 0] as i32 - samples[y0 + 7] as i32;
        let t1 = samples[y0 + 1] as i32 - samples[y0 + 6] as i32;
        let t2 = samples[y0 + 2] as i32 - samples[y0 + 5] as i32;
        let t3 = samples[y0 + 3] as i32 - samples[y0 + 4] as i32;

        //Apply unsigned -> signed conversion
        coeffs[y0 + 0] = (t10 + t11 - 8 * 128) << PASS1_BITS as uint;
        coeffs[y0 + 4] = (t10 - t11) << PASS1_BITS as uint;

        let mut z1 = (t12 + t13) * FIX_0_541196100;
        //Add fudge factor here for final descale
        z1 += 1 << (CONST_BITS - PASS1_BITS - 1) as uint;

        coeffs[y0 + 2] = (z1 + t12 * FIX_0_765366865) >> (CONST_BITS - PASS1_BITS) as uint;
        coeffs[y0 + 6] = (z1 - t13 * FIX_1_847759065) >> (CONST_BITS - PASS1_BITS) as uint;

        //Odd part
        let t12 = t0 + t2;
        let t13 = t1 + t3;

        let mut z1 = (t12 + t13) * FIX_1_175875602;
        //Add fudge factor here for final descale
        z1 += 1 << (CONST_BITS - PASS1_BITS - 1) as uint;

        let mut t12 = t12 * (-FIX_0_390180644);
        let mut t13 = t13 * (-FIX_1_961570560);
        t12 += z1;
        t13 += z1;

        let z1 = (t0 + t3) * (-FIX_0_899976223);
        let mut t0 = t0 * FIX_1_501321110;
        let mut t3 = t3 * FIX_0_298631336;
        t0 += z1 + t12;
        t3 += z1 + t13;

        let z1 = (t1 + t2) * (-FIX_2_562915447);
        let mut t1 = t1 * FIX_3_072711026;
        let mut t2 = t2 * FIX_2_053119869;
        t1 += z1 + t13;
        t2 += z1 + t12;

        coeffs[y0 + 1] = t0 >> (CONST_BITS - PASS1_BITS) as uint;
        coeffs[y0 + 3] = t1 >> (CONST_BITS - PASS1_BITS) as uint;
        coeffs[y0 + 5] = t2 >> (CONST_BITS - PASS1_BITS) as uint;
        coeffs[y0 + 7] = t3 >> (CONST_BITS - PASS1_BITS) as uint;
    }

    //Pass 2: process columns
    //We remove the PASS1_BITS scaling but leave the results scaled up an
    //overall factor of 8
    for x in range(0u, 8).rev() {
        //Even part
        let t0 = coeffs[x + 8 * 0] + coeffs[x + 8 * 7];
        let t1 = coeffs[x + 8 * 1] + coeffs[x + 8 * 6];
        let t2 = coeffs[x + 8 * 2] + coeffs[x + 8 * 5];
        let t3 = coeffs[x + 8 * 3] + coeffs[x + 8 * 4];

        //Add fudge factor here for final descale
        let t10 = t0 + t3 + (1 << (PASS1_BITS - 1) as uint);
        let t12 = t0 - t3;
        let t11 = t1 + t2;
        let t13 = t1 - t2;

        let t0 = coeffs[x + 8 * 0] - coeffs[x + 8 * 7];
        let t1 = coeffs[x + 8 * 1] - coeffs[x + 8 * 6];
        let t2 = coeffs[x + 8 * 2] - coeffs[x + 8 * 5];
        let t3 = coeffs[x + 8 * 3] - coeffs[x + 8 * 4];

        coeffs[x + 8 * 0] = (t10 + t11) >> PASS1_BITS as uint;
        coeffs[x + 8 * 4] = (t10 - t11) >> PASS1_BITS as uint;

        let mut z1 = (t12 + t13) * FIX_0_541196100;
        //Add fudge factor here for final descale
        z1 += 1 << (CONST_BITS + PASS1_BITS - 1) as uint;

        coeffs[x + 8 * 2] = (z1 + t12 * FIX_0_765366865) >> (CONST_BITS + PASS1_BITS) as uint;
        coeffs[x + 8 * 6] = (z1 - t13 * FIX_1_847759065) >> (CONST_BITS + PASS1_BITS) as uint;

        //Odd part
        let t12 = t0 + t2;
        let t13 = t1 + t3;

        let mut z1 = (t12 + t13) * FIX_1_175875602;
        //Add fudge factor here for final descale
        z1 += 1 << (CONST_BITS - PASS1_BITS - 1) as uint;

        let mut t12 = t12 * (-FIX_0_390180644);
        let mut t13 = t13 * (-FIX_1_961570560);
        t12 += z1;
        t13 += z1;

        let z1 = (t0 + t3) * (-FIX_0_899976223);
        let mut t0 = t0 * FIX_1_501321110;
        let mut t3 = t3 * FIX_0_298631336;
        t0 += z1 + t12;
        t3 += z1 + t13;

        let z1 = (t1 + t2) * (-FIX_2_562915447);
        let mut t1 = t1 * FIX_3_072711026;
        let mut t2 = t2 * FIX_2_053119869;
        t1 += z1 + t13;
        t2 += z1 + t12;

        coeffs[x + 8 * 1] = t0 >> (CONST_BITS + PASS1_BITS) as uint;
        coeffs[x + 8 * 3] = t1 >> (CONST_BITS + PASS1_BITS) as uint;
        coeffs[x + 8 * 5] = t2 >> (CONST_BITS + PASS1_BITS) as uint;
        coeffs[x + 8 * 7] = t3 >> (CONST_BITS + PASS1_BITS) as uint;
    }
}

pub fn idct(coeffs: &[i32], samples: &mut [u8]) {
    let mut tmp = [0i32, ..64];

    for x in range(0u, 8).rev() {
        if coeffs[x + 8 * 1] == 0 && coeffs[x + 8 * 2] == 0 && coeffs[x + 8 * 3] == 0 &&
            coeffs[x + 8 * 4] == 0 && coeffs[x + 8 * 5] == 0 && coeffs[x + 8 * 6] == 0 &&
            coeffs[x + 8 * 7] == 0 {
            let dcval = coeffs[x + 8 * 0] << PASS1_BITS as uint;

            tmp[x + 8 * 0] = dcval;
            tmp[x + 8 * 1] = dcval;
            tmp[x + 8 * 2] = dcval;
            tmp[x + 8 * 3] = dcval;
            tmp[x + 8 * 4] = dcval;
            tmp[x + 8 * 5] = dcval;
            tmp[x + 8 * 6] = dcval;
            tmp[x + 8 * 7] = dcval;

            continue
        }

        //Even part: reverse the even part of the forward DCT
        let z2 = coeffs[x + 8 * 2];
        let z3 = coeffs[x + 8 * 6];

        let z1 = (z2 + z3) * FIX_0_541196100;
        let t2 = z1 + z2 * FIX_0_765366865;
        let t3 = z1 - z3 * FIX_1_847759065;

        let mut z2 = coeffs[x + 8 * 0];
        let mut z3 = coeffs[x + 8 * 4];
        z2 <<= CONST_BITS as uint;
        z3 <<= CONST_BITS as uint;

        z2 += 1 << (CONST_BITS - PASS1_BITS - 1) as uint;

        let t0 = z2 + z3;
        let t1 = z2 - z3;

        let t10 = t0 + t2;
        let t13 = t0 - t2;
        let t11 = t1 + t3;
        let t12 = t1 - t3;

        let t0 = coeffs[x + 8 * 7];
        let t1 = coeffs[x + 8 * 5];
        let t2 = coeffs[x + 8 * 3];
        let t3 = coeffs[x + 8 * 1];

        let z2 = t0 + t2;
        let z3 = t1 + t3;

        let z1 = (z2 + z3) * FIX_1_175875602;
        let mut z2 = z2 * (-FIX_1_961570560);
        let mut z3 = z3 * (-FIX_0_390180644);
        z2 += z1;
        z3 += z1;

        let z1 = (t0 + t3) * (-FIX_0_899976223);
        let mut t0 = t0 * FIX_0_298631336;
        let mut t3 = t3 * FIX_1_501321110;
        t0 += z1 + z2;
        t3 += z1 + z3;

        let z1 = (t1 + t2) * (-FIX_2_562915447);
        let mut t1 = t1 * FIX_2_053119869;
        let mut t2 = t2 * FIX_3_072711026;
        t1 += z1 + z3;
        t2 += z1 + z2;

        tmp[x + 8 * 0] = (t10 + t3) >> (CONST_BITS - PASS1_BITS) as uint;
        tmp[x + 8 * 7] = (t10 - t3) >> (CONST_BITS - PASS1_BITS) as uint;
        tmp[x + 8 * 1] = (t11 + t2) >> (CONST_BITS - PASS1_BITS) as uint;
        tmp[x + 8 * 6] = (t11 - t2) >> (CONST_BITS - PASS1_BITS) as uint;
        tmp[x + 8 * 2] = (t12 + t1) >> (CONST_BITS - PASS1_BITS) as uint;
        tmp[x + 8 * 5] = (t12 - t1) >> (CONST_BITS - PASS1_BITS) as uint;
        tmp[x + 8 * 3] = (t13 + t0) >> (CONST_BITS - PASS1_BITS) as uint;
        tmp[x + 8 * 4] = (t13 - t0) >> (CONST_BITS - PASS1_BITS) as uint;
    }

    for y in range(0u, 8) {
        let y0 = y * 8;

        let z2 = tmp[y0 + 2];
        let z3 = tmp[y0 + 6];

        let z1 = (z2 + z3) * FIX_0_541196100;
        let t2 = z1 + z2 * FIX_0_765366865;
        let t3 = z1 - z3 * FIX_1_847759065;

        let z2 = tmp[y0 + 0] + (1 << (PASS1_BITS + 2) as uint);
        let z3 = tmp[y0 + 4];

        let t0 = (z2 + z3) << CONST_BITS as uint;
        let t1 = (z2 - z3) << CONST_BITS as uint;

        let t10 = t0 + t2;
        let t13 = t0 - t2;
        let t11 = t1 + t3;
        let t12 = t1 - t3;

        let t0 = tmp[y0 + 7];
        let t1 = tmp[y0 + 5];
        let t2 = tmp[y0 + 3];
        let t3 = tmp[y0 + 1];

        let z2 = t0 + t2;
        let z3 = t1 + t3;

        let z1 = (z2 + z3) * FIX_1_175875602;
        let mut z2 = z2 * (-FIX_1_961570560);
        let mut z3 = z3 * (-FIX_0_390180644);
        z2 += z1;
        z3 += z1;

        let z1 = (t0 + t3) * (-FIX_0_899976223);
        let mut t0 = t0 * FIX_0_298631336;
        let mut t3 = t3 * FIX_1_501321110;
        t0 += z1 + z2;
        t3 += z1 + z3;

        let z1 = (t1 + t2) * (-FIX_2_562915447);
        let mut t1 = t1 * FIX_2_053119869;
        let mut t2 = t2 * FIX_3_072711026;
        t1 += z1 + z3;
        t2 += z1 + z2;

        let a = (t10 + t3) >> (CONST_BITS + PASS1_BITS + 3) as uint;
        samples[y0 + 0] = level_shift_up(a);

        let a = (t10 - t3) >> (CONST_BITS + PASS1_BITS + 3) as uint;
        samples[y0 + 7] = level_shift_up(a);

        let a = (t11 + t2) >> (CONST_BITS + PASS1_BITS + 3) as uint;
        samples[y0 + 1] = level_shift_up(a);

        let a = (t11 - t2) >> (CONST_BITS + PASS1_BITS + 3) as uint;
        samples[y0 + 6] = level_shift_up(a);

        let a = (t12 + t1) >> (CONST_BITS + PASS1_BITS + 3) as uint;
        samples[y0 + 2] = level_shift_up(a);

        let a = (t12 - t1) >> (CONST_BITS + PASS1_BITS + 3) as uint;
        samples[y0 + 5] = level_shift_up(a);

        let a = (t13 + t0) >> (CONST_BITS + PASS1_BITS + 3) as uint;
        samples[y0 + 3] = level_shift_up(a);

        let a = (t13 - t0) >> (CONST_BITS + PASS1_BITS + 3) as uint;
        samples[y0 + 4] = level_shift_up(a);
    }
}