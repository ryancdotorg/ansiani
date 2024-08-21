use std::{
    env,
    cmp::{min, max},
    collections::HashSet,
    fs::File,
    io::{Read, BufWriter, Write},
    path::Path,
    sync::Mutex,
};

use atomic_counter::{AtomicCounter, RelaxedCounter};

use blake3;

use counter::Counter;

use itertools::iproduct;

use palette::{
    Srgb,
    lab::Lab,
    convert::FromColor,
    color_difference::Ciede2000,
};

use rayon::prelude::*;

use zstd::stream::{
    read::Decoder as ZstdDecoder,
    write::Encoder as ZstdEncoder,
};

/// a <= v <= b
fn clamp<T>(a: T, v: T, b: T) -> T where T: Ord {
    max(a, min(v, b))
}

fn sorted<T>(slice: &[T]) -> Vec<T> where T: Clone + Ord {
    let mut v = slice.to_vec();
    v.sort(); v
}

fn b3(data: &[u8]) -> String {
    blake3::hash(&data).to_hex().to_string()
}

fn rgb_to_lab(rgb: (u8, u8, u8)) -> Lab {
    let srgb = Srgb::from_components(rgb);
    Lab::from_color(srgb.into_linear::<f32>())
}

/// fast naive grey match
fn xterm256_grey_approx(level: u8) -> i8 {
    // levels are (8, 18, ..., 238)
    match level {
        0..=13 => 0, 234..=255 => 23,
        _ => ((level - 4) / 10) as i8,
    }
}

/// fast naive r/g/b match
fn xterm256_rgb_approx(level: u8) -> u8 {
    // levels are (0, 95, 135, 175, 215, 255)
    match level {
        0..=47 => 0, 48..=114 => 1,
        _ => ((level - 35) / 40) as u8,
    }
}

/// Generate list of candidate palette entries that might be the nearest color.
/// This allows us to skip about 84% of color difference calculations.
fn xterm256_index_approx(r: u8, g: u8, b: u8) -> Vec<u8> {
    let mut ret = Vec::<u8>::new();

    // ranges determined experimentally
    let r_range = match xterm256_rgb_approx(r) {
        0 => 0..=2, 1 => 0..=3, 2 => 0..=4, 3 => 1..=4, 4 => 3..=5, 5 => 4..=5,
        _ => panic!("bad r"),
    };

    let g_range = match xterm256_rgb_approx(g) {
        0 => 0..=1, 1 => 0..=2, 2 => 1..=3, 3 => 2..=4, 4 => 3..=5, 5 => 4..=5,
        _ => panic!("bad g"),
    };

    let b_range = match xterm256_rgb_approx(b) {
        0 => 0..=1, 1 => 0..=3, 2 => 1..=4, 3 => 1..=5, 4 => 3..=5, 5 => 4..=5,
        _ => panic!("bad b"),
    };

    for (r_idx, g_idx, b_idx) in iproduct!(r_range, g_range, b_range) {
        // push computed palette index
        ret.push(16 + r_idx * 36 + g_idx * 6 + b_idx);
    }

    // average component value as grey (w) level
    let w = ((r as u16 + g as u16 + b as u16 + 1) / 3) as u8;
    let w_part = xterm256_grey_approx(w);

    for w_idx in max(0, w_part - 1)..=clamp(2, w_part + 2, 23) {
        // push computed palette index
        ret.push(232 + w_idx as u8);
    }

    ret
}

/// Generate rgb value for given xterm256 palette index.
fn xterm256_index_to_rgb(index: u8) -> (u8, u8, u8) {
    match index {
        // xterm 256 color cube
        16..=231 => {
            let level = |n| if n == 0 { 0 } else { 55 + n * 40 };
            (
                level(((index - 16) / 36) % 6),
                level(((index - 16) /  6) % 6),
                level(((index - 16) /  1) % 6),
            )
        },
        // xterm 256 grey scale
        232..=255 => {
            let level = 10 * (index - 231u8) - 2;
            (level, level, level)
        },
        _ => panic!("bad index: {}", index)
    }
}

/// Generate the Xterm256 palette.
fn gen_xterm256_lut_rgb<F>(
    write: &mut F,
    xterm256_rgb: &Vec<(u8, (u8, u8, u8))>
)
where
    F: FnMut(&str) -> (),
{
    write("const LUT_PALETTE: [(u8, u8, u8); 240] = [\n");
    for &(index, (r, g, b)) in xterm256_rgb.iter() {
        write(&format!("    ({:3}, {:3}, {:3}),", r, g, b));
        if index == 255 || index % 4 == 3 { write("\n"); }
    }
    write("];\n");
}

/// Pre-compute nearest Xterm256 palette entry for all 24-bit RGB colors.
fn gen_xterm256_lut_nearest<F>(
    status: &mut F,
    xterm256_rgb: &Vec<(u8, (u8, u8, u8))>
) -> Vec<u8>
where
    F: FnMut(&str) -> () + Sync + Send,
{
    // try to use a cached version
    let manifest_dir = env::var_os("CARGO_MANIFEST_DIR").unwrap();
    let nearest_bin_path = Path::new(&manifest_dir).join(".nearest.zst");
    if let Ok(file) = File::open(&nearest_bin_path) {
        let mut reader = ZstdDecoder::new(file).unwrap();
        let mut lut_nearest = Vec::<u8>::new();
        if 16777216 == reader.read_to_end(&mut lut_nearest).unwrap() {
            return lut_nearest;
        }
    }

    // pre-generate lab colors
    let xterm256_lab: Vec::<(u8, Lab)> = xterm256_rgb.iter().map(
        |&(index, rgb)| {
            let lab = rgb_to_lab(rgb);
            (index, lab)
        }
    ).collect();

    // find the best matching palette index
    let best_index = |r: u8, g: u8, b: u8| {
        let lab = rgb_to_lab((r, g, b));
        let mut best_diff = 1e9;
        let mut best_index = 0;
        let approx = xterm256_index_approx(r, g, b);

        for (index, other) in approx.iter().map(|i| &xterm256_lab[*i as usize - 16]) {
            let diff = lab.difference(*other);
            if diff < best_diff {
                best_diff = diff;
                best_index = *index;
            }
        }

        best_index
    };

    let m_status = Mutex::new(status);
    m_status.lock().unwrap()(
        "\x1b[G\x1b[K \x1b[1;35mCalculating\x1b[0m CIEDE2000 Differences: \x1b[s  0.0%"
    );

    // find best matches in parallel
    let lut_nearest: Vec<u8> = {
        let n = 16777216;
        let done = RelaxedCounter::new(0);
        (0..n).into_par_iter().map(|color| {
            let r = ((color >> 16) & 255) as u8;
            let g = ((color >>  8) & 255) as u8;
            let b = ((color >>  0) & 255) as u8;

            let index = best_index(r, g, b);

            done.inc();
            let curr_done = done.get();
            let last_done = curr_done - 1;
            let curr_progress = (curr_done * 1000 + 5) / n;
            let last_progress = (last_done * 1000 + 5) / n;
            // only update progres if number would change
            if curr_progress > last_progress {
                m_status.lock().unwrap()(&format!(
                    "\x1b[u\x1b[K{:5.1}%",
                    curr_progress as f64 / 10.0,
                ));
            }

            index
        }).collect()
    };

    // save the data
    if let Ok(file) = File::create(&nearest_bin_path) {
        let mut writer = ZstdEncoder::new(file, 11).unwrap();
        writer.write_all(&lut_nearest).unwrap();
        writer.finish().unwrap();
    }

    lut_nearest
}

/// Generate partial lookup tables for nearest Xterm256 palette match.
fn gen_xterm256_lut_partial<F, G>(
    write: &mut F,
    status: &mut G,
    lut_nearest: &Vec<u8>,
    r_bits: u8, g_bits: u8, b_bits: u8
)
where
    F: FnMut(&str) -> (),
    G: FnMut(&str) -> () + Sync + Send,
{
    // Chunk iterators
    let r_mins = (0u8..=255).step_by(1 << (8 - r_bits));
    let g_mins = (0u8..=255).step_by(1 << (8 - g_bits));
    let b_mins = (0u8..=255).step_by(1 << (8 - b_bits));

    // Divide up the sRGB color space into "chunks".
    let mut chunks = Vec::<(usize, (u8, u8, u8), HashSet<u8>)>::new();
    for (i, (r_min, g_min, b_min)) in iproduct!(r_mins, g_mins, b_mins).enumerate() {
        let mut set = HashSet::<u8>::new();
        let r_range = r_min..=(r_min + ((1 << (8 - r_bits)) - 1));
        let g_range = g_min..=(g_min + ((1 << (8 - g_bits)) - 1));
        let b_range = b_min..=(b_min + ((1 << (8 - b_bits)) - 1));

        // Generate the set of possible nearest palette matches for each chunk.
        for (r, g, b) in iproduct!(r_range, g_range, b_range) {
            let color = ((r as usize) << 16) + ((g as usize) << 8) + b as usize;
            set.insert(lut_nearest[color]);
        }

        chunks.push((i, (r_min, g_min, b_min), set));
    }

    let mut lut_offsets = Vec::<u16>::new();
    let mut lut_partial = Vec::<u8>::new();

    let m_status = Mutex::new(status);
    m_status.lock().unwrap()(&format!(
        "\x1b[G\x1b[K  \x1b[1;35mGenerating\x1b[0m RGB{}{}{} Lookup Tables:  \x1b[s  0.0%",
        r_bits, g_bits, b_bits,
    ));

    let n_chunks = chunks.len() as f64;
    let freq = chunks.into_iter().map(|(i, _mins, set)| {
        // turn the set of values into a sorted list
        let mut values = set.into_iter().collect::<Vec<_>>();
        values.sort();

        let n: u8 = values.len().try_into().unwrap();
        let n_sz = n as usize;

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
            if lut_partial.len() >= n_sz {
                let v = sorted(&lut_partial[(lut_partial.len()-(n_sz))..]);
                if v == values {
                    let pos: u16 = (lut_partial.len()-(n_sz)).try_into().unwrap();
                    lut_offsets.push(n_prefix | pos);
                    break 'store;
                }
            }

            // sliding window search for a match
            for (i, w) in lut_partial.windows(n_sz).enumerate() {
                let mut v = Vec::from(w);
                // if length is to be encoded in offset, order doesn't matter
                if n_prefix != 0 { v.sort(); }
                if v == values {
                    let pos: u16 = i.try_into().unwrap();
                    lut_offsets.push(n_prefix | pos);
                    break 'store;
                }
            }

            // can we add just one index maybe?
            let back = n_sz - 1;
            if n > 1 && lut_partial.len() >= back && n_prefix != 0 {
                let pos: u16 = (lut_partial.len()-back).try_into().unwrap();
                let tail = sorted(&lut_partial[(pos as usize)..]);
                for p in 0..(n_sz) {
                    let next = values[p];
                    let mut subset = Vec::from(&values[0..p]);
                    let mut end = Vec::from(&values[(p+1)..]);
                    subset.append(&mut end);

                    if tail == subset {
                        lut_offsets.push(n_prefix | pos);
                        lut_partial.push(next);
                        break 'store;
                    }
                }
            }

            let pos: u16 = lut_partial.len().try_into().unwrap();
            lut_offsets.push(n_prefix | pos);

            for value in values.iter() {
                lut_partial.push(*value);
            }
        }

        if i > 0 {
            let curr_progress = (i * 1000 + 5) as f64 / n_chunks;
            let last_progress = ((i - 1) * 1000 + 5) as f64 / n_chunks;
            if curr_progress > last_progress {
                m_status.lock().unwrap()(&format!(
                    "\x1b[u\x1b[K{:5.1}%",
                    curr_progress as f64 / 10.0,
                ));
            }
        }

        return n;
    }).collect::<Counter<_>>();

    let total_entries = freq.iter().fold(0, |t, (&k, v)| t + (k as usize) * v);
    let bytes_t1 = n_chunks as usize * 2;
    let bytes_t2 = lut_partial.len();
    let bytes_t = bytes_t1 + bytes_t2;

    write(&format!(
        "\n// RGB{}{}{} {} + {} = {} bytes, {:.2} bytes/chunk, {:.2} entries/chunk\n",
        r_bits, g_bits, b_bits,
        bytes_t1, bytes_t2, bytes_t,
        bytes_t as f64 / n_chunks,
        total_entries as f64 / n_chunks,
    ));

    write(&format!(
        "const LUT_R_BITS: u8 = {}; const LUT_R_SHIFT: u8 = {};\n",
        r_bits, (g_bits + b_bits),
    ));

    write(&format!(
        "const LUT_G_BITS: u8 = {}; const LUT_G_SHIFT: u8 = {};\n",
        g_bits, b_bits,
    ));

    write(&format!(
        "const LUT_B_BITS: u8 = {}; const LUT_B_SHIFT: u8 = {};\n",
        b_bits, 0,
    ));

    write(&format!("const LUT_OFFSETS: [u16; {}] = [\n", lut_offsets.len()));

    for (i, v) in lut_offsets.iter().enumerate() {
        if i % 11 == 0 { write("   "); }
        write(&format!(" {:5},", v));
        if i == lut_offsets.len() - 1 || i % 11 == 10 {
            write("\n");
        }
    }

    write("];\n");

    write(&format!("\nconst LUT_PARTIAL: [u8; {}] = [\n", lut_partial.len()));

    for (i, v) in lut_partial.iter().enumerate() {
        if i % 15 ==  0 { write("   "); }
        if i % 15 ==  5 { write(" "); }
        if i % 15 == 10 { write(" "); }
        write(&format!(" {:3},", v));
        if i == lut_partial.len() - 1 || i % 15 == 14 {
            write("\n");
        }
    }

    write("];\n");
}

fn mk_write(name: &str) -> Result<impl FnMut(&str) -> (), std::io::Error> {
    let out_dir = env::var_os("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join(name);
    let file = File::create(dest_path)?;
    let mut writer = BufWriter::new(file);
    Ok(move |s: &str| writer.write_all(s.as_bytes()).unwrap())
}

fn mk_status() -> Box<dyn FnMut(&str) -> () + Sync + Send> {
    match File::options().write(true).open("/dev/tty") {
        Ok(mut f) => Box::new(move |s: &str| f.write_all(s.as_bytes()).unwrap()),
        Err(_) => Box::new(|_s: &str| ()),
    }
}

fn main() {
    let xterm256_rgb: Vec::<(u8, (u8, u8, u8))> = (16u8..=255).map(|index| {
        (index, xterm256_index_to_rgb(index))
    }).collect();

    let mut status = mk_status();
    status("\n");

    let mut write = mk_write("lut_palette.rs").unwrap();
    gen_xterm256_lut_rgb(&mut write, &xterm256_rgb);

    let lut_nearest = gen_xterm256_lut_nearest(&mut status, &xterm256_rgb);

    let mut write = mk_write("lut_partial.rs").unwrap();
    write(&format!("#[allow(dead_code)]\n"));
    write(&format!("const NEAREST_BLAKE3: &str = \"{}\";\n", b3(&lut_nearest)));

    gen_xterm256_lut_partial(&mut write, &mut status, &lut_nearest, 5, 5, 4);
    status("\x1b[2K\x1b[F");

    println!("cargo::rerun-if-changed=build.rs");
}
