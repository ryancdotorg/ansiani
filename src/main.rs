mod term;
use term::{TermRgb, xterm256_nearest};

include!(concat!(env!("OUT_DIR"),"/lut_partial.rs"));

use std::collections::HashSet;
use rayon::prelude::*;

use palette::{
    Srgb,
    lab::Lab,
    convert::FromColor,
    color_difference::Ciede2000,
};

fn srgb_to_lab(srgb: Srgb<u8>) -> Lab {
    Lab::from_color(srgb.into_linear::<f32>())
}

fn main() {
    /*
    let foo = TermRgb { r: 98, g: 98, b: 98 };
    println!("{:?}", foo);
    println!("{}", foo.to_string());
    let bar = TermRgb { r: 0, g: 95, b: 55 };
    println!("{:?}", bar);
    println!("{}", bar.to_string());
    let qux = TermRgb { r: 42, g: 123, b: 234 };
    println!("{:?}", qux);
    println!("{}", qux.to_string());
    */

    let mut xt = Vec::<(u8, Lab)>::new();

    for idx in 16u8..=255u8 {
        xt.push((idx, TermRgb::from_index(idx).to_lab()));
    }

    for (idx, lab) in xt.iter() {
        let l = lab.l;
        let a = lab.a;
        let b = lab.b;
        println!("LAB {} {:e} {:e} {:e}", idx, l, a, b);
    }

    let _best_idx_lut = |srgb: Srgb<u8>| {
        let (r, g, b) = (srgb.red, srgb.green, srgb.blue);
        let lab = srgb_to_lab(srgb);

        let mut best_diff = 999999.0;
        let mut best_idx = 0;

        let entry = (((r as usize) >> (8 - LUT_R_BITS)) << (LUT_R_SHIFT))
                  | (((g as usize) >> (8 - LUT_G_BITS)) << (LUT_G_SHIFT))
                  | (((b as usize) >> (8 - LUT_B_BITS)) << (LUT_B_SHIFT)) ;

        let w_pos = LUT_OFFSETS[entry] as usize;
        let (w, pos) = (w_pos >> 13, w_pos & 0x1fff);
        let values = if w > 0 {
            &LUT_PARTIAL[pos..(pos+w)]
        } else {
            let w = LUT_PARTIAL[pos] as usize;
            &LUT_PARTIAL[(pos+1)..(pos+w+1)]
        };

        for idx in values.iter() {
            let (_, other) = xt[(idx-16) as usize];
            let diff = lab.difference(other);
            if diff < best_diff {
                best_diff = diff;
                best_idx = *idx;
            }
        }

        best_idx
    };

    let (r_bits, g_bits, b_bits) = (4, 4, 4);
    //let (r_shl, g_shl, b_shl) = (0, r_bits, r_bits + g_bits);
    let (r_shl, g_shl, b_shl) = (g_bits + b_bits, b_bits, 0);
    let (r_shr, g_shr, b_shr) = (8 - r_bits, 8 - g_bits, 8 - b_bits);
    let (r_mask, g_mask, b_mask) = ((1 << r_bits) - 1, (1 << g_bits) - 1, (1 << b_bits) - 1);
    let (r_count, g_count, b_count) = ((1 << r_shr) - 1, (1 << g_shr) - 1, (1 << b_shr) - 1);

    let mut entries = Vec::<((u8, u8, u8), HashSet<u8>)>::new();
    for q in 0..(1 << (r_bits + g_bits + b_bits)) {
        let r_base = (((q >> r_shl) as u8) & r_mask) << r_shr;
        let g_base = (((q >> g_shl) as u8) & g_mask) << g_shr;
        let b_base = (((q >> b_shl) as u8) & b_mask) << b_shr;
        entries.push(((r_base, g_base, b_base), HashSet::<u8>::new()));
    }

    entries.par_iter_mut().for_each(|(key, ref mut set)| {
        let (r_base, g_base, b_base) = *key;

        for r in r_base..=(r_base+r_count) {
            for g in g_base..=(g_base+g_count) {
                for b in b_base..=(b_base+b_count) {
                    let best_1 = xterm256_nearest(r, g, b);
                    set.insert(best_1);
                }
            }
        }
    });

    let mut lut_idx = Vec::<u16>::new();
    let mut lut_dat = Vec::<u8>::new();

    for (_key, set) in entries {
        let mut values = set.into_iter().collect::<Vec<_>>();
        values.sort();
        //println!("//\t{:?}\t{:?}\t{}", key, values, values.len());

        let n: u8 = values.len().try_into().unwrap();

        let n_prefix = if n < 8 {
            // for small palette entry lists, encode length in top three bits
            (n as u16) << 13
        } else {
            // otherwise, add length prefix to palette entry list
            values.insert(0, n);
            0u16
        };

        'store: {
            // is this set of values the same as the last one?
            if lut_dat.len() >= (n as usize) {
                let tail = &lut_dat[(lut_dat.len()-(n as usize))..];
                let mut v = tail.to_vec();
                v.sort();
                if v == values {
                    let pos: u16 = (lut_dat.len()-(n as usize)).try_into().unwrap();
                    lut_idx.push(n_prefix | pos);
                    break 'store;
                }
            }

            // sliding window search for a match
            for (i, w) in lut_dat.windows(n as usize).enumerate() {
                let mut v = Vec::from(w);
                // if length is to be encoded in offset, order doesn't matter
                if n < 8 { v.sort(); }
                if v == values {
                    let pos: u16 = i.try_into().unwrap();
                    lut_idx.push(n_prefix | pos);
                    break 'store;
                }
            }

            // can we add just one index maybe?
            let back: usize = (n - 1).into();
            if n > 1 && lut_dat.len() >= back && n < 8 {
                let pos: u16 = (lut_dat.len()-back).try_into().unwrap();
                let mut tail = Vec::from(&lut_dat[(pos as usize)..]);
                tail.sort();

                for p in 0..(n as usize) {
                    let next = values[p];
                    let mut subset = Vec::from(&values[0..p]);
                    let mut end = Vec::from(&values[(p+1)..]);
                    subset.append(&mut end);

                    if tail == subset {
                        //println!("SUB {:?} {:?} {:?}", values, &lut_dat[(pos as usize)..], next);
                        lut_idx.push(n_prefix | pos);
                        lut_dat.push(next);
                        break 'store;
                    }
                }
            }

            let pos: u16 = lut_dat.len().try_into().unwrap();
            lut_idx.push(n_prefix | pos);

            for value in values.iter() {
                lut_dat.push(*value);
            }
        }

        //println!("{:?}\t{:?}\t{}", key, values, values.len());
    }
    println!("const LUT{}{}{}_IDX: &[u16] = &{:?};", r_bits, g_bits, b_bits, lut_idx);
    println!("const LUT{}{}{}_DAT: &[u8] = &{:?};", r_bits, g_bits, b_bits, lut_dat);
    println!("// {} + {} = {}", lut_idx.len() * 2, lut_dat.len(), lut_idx.len() * 2 + lut_dat.len());

    /*
    let a = Srgb::new(42u8, 123, 234);
    let b = Srgb::new(44u8, 124, 233);
    let aa = srgb_to_lab(a);
    println!("{:?}", aa);
    let bb = Lab::from_color(b.into_linear::<f32>());
    println!("{:?}", bb);

    println!("{}", aa.improved_difference(bb));
    */

}

