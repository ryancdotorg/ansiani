// our own imports
mod term;
use term::{
    TermCell,
    TermColor,
    change_attr_string,
    parse_cells,
    rgb_difference,
    xterm256_index_to_rgb,
    xterm256_nearest,
};

mod input;
use input::Input;

// standard library
use std::{
    collections::HashMap,
    env,
    fs::{self, File},
    io::{self, BufRead, Write, BufWriter},
    path::Path,
};

// crates
use btree_range_map::RangeMap;
use byteorder::{BE, WriteBytesExt};
use clap::{Args, Parser, Subcommand};
use format_bytes::write_bytes;
use itertools::{iproduct, Itertools};
use zstd::stream::write::Encoder as ZstdEncoder;

#[derive(Debug, Clone, Parser)]
#[command(version, name = env!("CARGO_PKG_NAME"), long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Debug, Clone, Subcommand)]
enum Commands {
    Decode {
        #[command(flatten)]
        source: Source,
        #[arg(short = 'n', long, value_name = "NS")]
        delay: Option<u64>,
    },
    Encode {
        #[arg(short, long, value_name = "FILE")]
        input: String,
        #[arg(short, long, value_name = "FILE")]
        subtitles: Option<String>,
    },
    Map {
        #[arg(short, long, value_name = "FILE")]
        input: String,
    },
    Nearest {
    },
}

#[derive(Debug, Clone, Args)]
#[group(multiple = false)]
struct Source {
    #[arg(short, long, group = "source", value_name = "DIR")]
    directory: Option<String>,

    #[arg(short, long, group = "source", value_name = "FILE")]
    input: Option<String>,
}

#[derive(Clone, Debug, PartialEq)]
struct SubtitleEntry {
    show: u32, hide: u32,
    row: usize, col: usize,
    text: String,
}


fn main() {
    let cli = Cli::parse();

    if let Some(cmd) = &cli.command {
        match cmd {
            Commands::Decode { source, delay, .. } => {
                let _ = decode(source.clone(), *delay, None);
            },
            Commands::Encode {
                input,
                subtitles,
                ..
            } => {
                let input = Input::from_path(input).expect("open failed: {:?}");
                let subtitles = subtitles.as_ref().map(|s| {
                    Input::from_path(s).expect("open failed: {:?}")
                });
                let _ = encode(input, subtitles);
            },
            Commands::Map { input, .. } => {
                let _ = map(Input::from_path(input).expect("open failed: {:?}")  );
            },
            Commands::Nearest { .. } => {
                nearest();
            },
            #[allow(unreachable_patterns)]
            _ => todo!(),
        }
    }
}

fn parse_subtitles(subtitles: Input) -> Vec<SubtitleEntry> {
    subtitles
    .lines()
    .filter_map(|s| {
        match s {
            Ok(s) => {
                let s = s.trim_end().to_string();
                if s.is_empty() { return None; }
                if s.starts_with("#") { return None; }
                Some(s)
            }
            Err(_) => None,
        }
    })
    .map(|line| {
        let parts = line.split('\t').collect_tuple().unwrap();
        let (_n, show, hide, row, col, text) = parts;
        // convert
        let show = parse_fsec_to_ms(show);
        let hide = parse_fsec_to_ms(hide);
        let row = row.parse::<usize>().unwrap();
        let col = col.parse::<usize>().unwrap();
        let text = text.to_string();
        SubtitleEntry { show, hide, row, col, text }
    })
    .collect()
}

fn parse_fsec_to_ms(s: &str) -> u32 {
        let tv: Vec<_> = s.split('.').collect();
        tv[0].parse::<u32>().unwrap() * 1000
        + tv[1].parse::<u32>().unwrap()
}

fn should_update(a: &TermColor, b: &TermColor, threshold: f32) -> bool {
    a != b && {
        let a_rgb = a.to_rgb();
        let b_rgb = b.to_rgb();
        if let Some(a_rgb) = a_rgb {
            if let Some(b_rgb) = b_rgb {
                return rgb_difference(a_rgb, b_rgb) > threshold;
            }
        }

        true
    }
}

fn put_string(rows: &mut [Vec<TermCell>], r: usize, c: usize, s: &str) {
    let mut attr = 0u32;
    let mut state = 0;
    let mut c = c;
    // A crappy HTML parser...
    for codepoint in s.chars() {
        let cc = u32::from(codepoint);
        match (state << 24) | cc {
            // tag start
            0x000003c => /* < */ { state = 1; }
            // tag end (open)
            0x200003e => /* > */ { state = 0; },
            // tag end (close)
            0x300003e => /* > */ { state = 0; },
            // tag close
            0x100002f => /* / */ { state = 3; }
            // begin attribute
            0x1000062 => /* b */ { state = 2; attr |= 0x02; },
            0x1000069 => /* i */ { state = 2; attr |= 0x08; },
            0x1000075 => /* u */ { state = 2; attr |= 0x10; },
            // end attribute
            0x3000062 => /* b */ { state = 2; attr &= 0xfd; },
            0x3000069 => /* i */ { state = 2; attr &= 0xf7; },
            0x3000075 => /* u */ { state = 2; attr &= 0xef; },
            _ => {
                let v = (attr << 24) | cc;
                rows[r][c] = TermCell::from_packed((0x010000ba, 0x01000010, v)).unwrap();
                state = 0;
                c += 1;
            },
        }
    }
}

#[inline]
fn write_codepoint_ansi(w: &mut dyn Write, codepoint: char) -> io::Result<()> {
    let mut b = [255; 4];
    w.write_all(codepoint.encode_utf8(&mut b).as_bytes())
}

#[inline]
fn encode_frame(
    w: &mut dyn Write,
    mut rows: Vec<Vec<TermCell>>,
    subs: &Option<&Vec<SubtitleEntry>>,
) -> io::Result<()> {
    let mut last = TermCell::from_packed((9, 9, 0)).unwrap();

    if let Some(subs) = subs {
        for ent in *subs {
            put_string(&mut rows, ent.row, ent.col, &ent.text);
        }
    }

    for row in rows.into_iter() {
        for cell in row.into_iter() {
            /*
            if let Some((r, g, b)) = cell.fg.to_rgb() {
                if let Some(index) = xterm256_threshold(r, g, b, 0.7) {
                    cell.set_fg_index(index);
                }
            }
            if let Some((r, g, b)) = cell.bg.to_rgb() {
                if let Some(index) = xterm256_threshold(r, g, b, 0.7) {
                    cell.set_bg_index(index);
                }
            }
            */

            // TODO break this out into a function...
            let update_fg = should_update(&last.fg, &cell.fg, 0.7);
            let update_bg = should_update(&last.bg, &cell.bg, 0.7);
            let update_attr = cell.attr != last.attr;

            let mut e = Vec::<String>::new();
            if update_attr { e.push(change_attr_string(&last.attr, &cell.attr)); }
            if update_fg { e.push(cell.fg_string()); }
            if update_bg { e.push(cell.bg_string()); }
            if !e.is_empty() {
                write_bytes!(w, b"\x1b[{}m", e.join(";").into_bytes())?;
            }

            write_codepoint_ansi(w, cell.codepoint)?;

            last = cell;
        }
    }

    // end of transmission block (frame)
    write_bytes!(w, b"\x17")?;

    Ok(())
}

fn encode(mut reader: Input, subtitles: Option<Input>) -> io::Result<()> {
    let mut w = BufWriter::new(std::io::stdout());

    let mut first_frame = true;

    let mut rows = Vec::<Vec::<TermCell>>::new();
    let mut row = Vec::<TermCell>::new();

    let sub_ents = match subtitles {
        Some(subtitles) => parse_subtitles(subtitles),
        None => Vec::<_>::new(),
    };

    // NB: assumes no overlaps
    let mut sub_map = HashMap::<(u32, u32), Vec<SubtitleEntry>>::new();
    for ent in sub_ents {
        let key = (ent.show, ent.hide);
        match sub_map.get_mut(&key) {
            Some(v) => {
                v.push(ent);
            },
            None => {
                sub_map.insert(key, vec![ent]);
            },
        };
    }

    let mut subs = RangeMap::<u32, Vec<SubtitleEntry>>::new();
    for (key, val) in sub_map.drain() {
        subs.insert(key.0..=key.1, val);
    }

    let mut timecode = 0;

    // process cells
    loop {
        let cell = TermCell::read_packed(&mut reader);
        match cell {
            Ok(cell) => match cell.codepoint {
                '\x16' => {
                    // timestamp
                    timecode = cell.as_timecode().unwrap();
                    write_bytes!(&mut w, b"\x1b[H{}\x1b[1H", cell.fg_bytes())?;
                    if first_frame {
                        write_bytes!(&mut w, b"\x1b[J")?;
                        first_frame = false;
                    }
                },
                '\x17' => {
                    // end of frame
                    let v = subs.get(timecode);
                    encode_frame(&mut w, rows, &v)?;
                    rows = Vec::<Vec::<TermCell>>::new();
                },
                '\n' => {
                    // end of row
                    row.push(cell);
                    rows.push(row);
                    row = Vec::<TermCell>::new();
                },
                '\x0c' => {/* ignored */},
                _ => row.push(cell),
            },
            Err(_) => { break Ok(()); },
        }
    }
}

/// frame syncronize with timecode
#[inline]
fn write_timecode_cell(w: &mut dyn Write, timecode: u32) -> io::Result<()> {
    if timecode >= (1<<24) {
        return Err(io::Error::other(format!("bad timecode: {}", timecode)));
    }
    w.write_u8(0x0f)?; w.write_u24::<BE>(timecode)?;
    w.write_u8(0x0f)?; w.write_u24::<BE>(timecode)?;
    w.write_u32::<BE>(0x16)
}

/// write end of frame cell
#[inline]
fn write_end_frame_cell(w: &mut dyn Write) -> io::Result<()> {
    w.write_all(b"\0\0\0\x09\0\0\0\x09\0\0\0\x17")
}

fn decode(
    source: Source,
    delay: Option<u64>,
    timecode: Option<u32>
) -> io::Result<()> {
    let mut w = BufWriter::new(std::io::stdout());

    let mut line = String::new();

    let inputs = if let Some(directory) = source.directory {
        let paths = fs::read_dir(directory).unwrap();
        let mut paths: Vec<_> = paths
            .into_iter()
            .map(|item| item.unwrap().path())
            .collect();
        paths.sort();
        paths
            .into_iter()
            .map(Input::from_path)
            .collect::<io::Result<Vec<_>>>()?
    } else if let Some(input) = source.input {
        vec![input.try_into()?]
    } else {
        vec![Input::from_stdin()?]
    };

    let mut timecode = timecode.unwrap_or(0);
    let mut timecode_ns = timecode as u64 * 1000000;
    for mut r in inputs.into_iter() {
        // TODO: move to function
        let mut frame_row = 0;
        let mut frame_count = 0;
        // each line of file
        loop {
            line.clear();
            if r.read_line(&mut line)? == 0 {
                // end of transmission block (end of frame)
                write_end_frame_cell(&mut w)?;
                break;
            }

            let cells: Vec<_> = parse_cells(&mut line.chars())
                .map_err(io::Error::other)?
                .into_iter()
                .filter(|item| item.codepoint != '\x17')
                .collect();

            // a line with no cells is probably eof
            if cells.is_empty() {
                eprintln!("no cells frame {} row {}", frame_count, frame_row);
                break;
            }

            match cells[0].codepoint {
                '\x0c' | '\x16' => {
                    if cells.len() == 1 {
                        // probably end of file
                        break;
                    }
                    //eprintln!("\x1b[95m[+] NEW FRAME: {}\x1b[m", frame_count);
                    frame_row = 0;
                },
                _ => {},
            }

            if frame_row == 0 {
                if frame_count > 0 {
                    write_end_frame_cell(&mut w)?;
                    timecode_ns += delay.expect("need a frame delay");
                    timecode = (timecode_ns / 1000000) as u32;
                }
                if cells[0].as_timecode().is_none() {
                    write_timecode_cell(&mut w, timecode)?;
                }
                frame_count += 1;
            }

            /*
            eprintln!(
                "\x1b[92mGot {} cells ({} chars) for frame {} row {}\x1b[m",
                cells.len(), line.len(),
                frame_count, frame_row
            );
            */

            for bytes in cells.into_iter().filter_map(|cell| {
                // skip start of frame cells without a timecode
                match cell.codepoint {
                    '\x0c' => None,
                    _ => Some(cell.to_packed_bytes()),
                }
            }) {
                w.write_all(&bytes)?;
            }

            frame_row += 1;
        }
    }

    Ok(())
}

fn map(mut reader: Input) -> io::Result<()> {
    let mut line = String::new();
    let mut row = String::new();
    let binding = reader.as_label();
    let binding: &Path = binding.as_ref();
    let mut base = binding.to_path_buf();
    let _ = base.pop();
    let mut count = 1;

    let mut last_frame_stem: String = "".to_string();

    loop {
        line.clear();
        if reader.read_line(&mut line).unwrap() == 0 {
            break Ok(());
        }
        let line = line.trim_end();
        let parts: Vec<_> = line.split_whitespace().collect();
        let frame_stem = parts[0];
        let time_ms = parse_fsec_to_ms(parts[1]);

        if last_frame_stem != frame_stem {
            let mut ansi = base.clone();
            ansi.push("ansi");
            ansi.push(format!("{}.ansi.zst", frame_stem));

            let mut cell = base.clone();
            cell.push("cell");
            cell.push(format!("{:06}.cell.zst", count));

            last_frame_stem = frame_stem.to_string();

            let mut input = Input::from_path(ansi.clone()).unwrap();
            let ofile = File::create(cell).unwrap();
            //let mut writer = BufWriter::new(ofile);
            let mut writer = ZstdEncoder::new(ofile, 11).unwrap();
            let time = vec![
                // emit syncronize
                0x0f,
                (time_ms >> 16) as u8,
                (time_ms >>  8) as u8,
                (time_ms      ) as u8,

                0x0f,
                (time_ms >> 16) as u8,
                (time_ms >>  8) as u8,
                (time_ms      ) as u8,

                0, 0, 0, 0x16,
            ];

            //time.append(&mut b"\0\0\0\x16".to_vec());

            let _ = writer.write_all(&time);

            // each line of file
            loop {
                row.clear();
                if input.read_line(&mut row).unwrap() == 0 {
                    break;
                }

                let mut chars = row.chars();
                let cells = parse_cells(&mut chars);
                let mut bytes = Vec::<u8>::new();
                for mut packed in cells.unwrap().into_iter().map(|c| c.to_packed_bytes()) {
                    bytes.append(&mut packed);
                }

                let _ = writer.write_all(&bytes);
            }

            // end of transmission block (end of frame)
            writer.write_all(b"\0\0\0\x09\0\0\0\x09\0\0\0\x17")?;

            writer.finish()?;

            println!("{:06} {:8} {} {:?}", count, time_ms, frame_stem, ansi);
            count += 1;
        }

    }
}

#[allow(dead_code)]
fn read_timecode(reader: &mut Input, buf: &mut Vec<u8>) -> io::Result<u64> {
    buf.truncate(0);
    match reader.read_until(0x16, buf) {
        Ok(size) => {
            if size == 0 { return Ok(0); }
            buf.truncate(0);
            reader.read_until(0x74/*t*/, buf).unwrap();
            Ok(
                String::from_utf8_lossy(buf)
                .trim_end_matches('t')
                .to_string()
                .parse::<u64>()
                .unwrap()
            )
        },
        Err(e) => Err(e),
    }
}

fn nearest() {
    for (r, g, b) in iproduct!(0u8..=255, 0u8..=255, 0u8..=255) {
        let rgb = (r, g, b);
        let index = xterm256_nearest(r, g, b);
        let compare = xterm256_index_to_rgb(index);
        let diff = rgb_difference(rgb, compare);
        if diff < 20.0 {
            println!("{:3}\t#{:02x}{:02x}{:02x}\t{:.6}", index, r, g, b, diff);
        }
    }
}

