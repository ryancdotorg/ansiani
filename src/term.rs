//use lru::LruCache;
//use std::num::NonZeroUsize;

use enumset::{enum_set, EnumSet, EnumSetType};
//use format_bytes::{DisplayBytes, format_bytes, write_bytes};
use format_bytes::write_bytes;
use lazy_static::lazy_static;
use std::io::{self, Write};
use std::sync::atomic::{AtomicU8, Ordering};
use std::str::Chars;

include!(concat!(env!("OUT_DIR"),"/lut_palette.rs"));
include!(concat!(env!("OUT_DIR"),"/lut_partial.rs"));

#[allow(dead_code, unused_imports)]
use palette::{
    Srgb,
    lab::Lab,
    convert::FromColor,
    convert::IntoColor,
    color_difference::Ciede2000,
    color_difference::ImprovedCiede2000,
};

use thiserror::Error as ThisError;
#[derive(ThisError, Debug)]
pub enum Error {
    #[error("invalid packed color: {0:08x}")]
    BadPacked(u32),
    #[error("invalid color index, must be [16..=255], not {0}")]
    BadIndex256(u8),
    #[error("invalid color index, must be [0..=7 | 9 | 60..=67], not {0}")]
    BadIndex16(u8),
    #[error("parse failed, integer overflow")]
    ParseIntOverflow,
    #[error("parse failed, unexpected codepoint {codepoint} in state {state}")]
    ParseBadChar { state: u32, codepoint: u32 },
    #[error("parse failed, unexpected code")]
    ParseBadCode,
    #[error("parse failed, truncated")]
    ParseTruncated,
}

lazy_static! {
    static ref lut_xterm256_nearest: Vec<AtomicU8> = {
        let mut v = Vec::<AtomicU8>::new();
        v.resize_with(16777216, || AtomicU8::new(0));
        v
    };

    static ref lut_xterm256_lab: Vec<Lab> = {
        (0u8..=255).into_iter().map(|idx| {
            rgb_to_lab(xterm256_index_to_rgb(idx))
        }).collect::<Vec<_>>()
    };
}

fn rgb_to_lab(rgb: (u8, u8, u8)) -> Lab {
    let srgb = Srgb::from_components(rgb);
    Lab::from_color(srgb.into_linear::<f32>())
}

fn pack_rgb(r: u8, g: u8, b: u8) -> usize {
    ((r as usize) << 16) | ((g as usize) << 8) | (b as usize)
}

fn xterm256_rgb_part(v: u8) -> Option<u8> {
    if v == 0 {
        Some(0)
    } else if v >= 55 && (v - 55) % 40 == 0 {
        Some(1 + (v - 55) / 40)
    } else {
        None
    }
}

fn xterm256_grey_part(v: u8) -> Option<u8> {
    if v >= 8 && (v - 8) % 10 == 0 {
        Some(1 + (v - 8) / 10)
    } else {
        None
    }
}

pub fn xterm256_index_to_rgb(index: u8) -> (u8, u8, u8) {
    match index {
        // XXX depends on terminal settings
        0 =>  (  0,   0,   0),
        1 =>  (205,   0,   0),
        2 =>  (  0, 205,   0),
        3 =>  (205, 205,   0),
        4 =>  (  0,   0, 238),
        5 =>  (205,   0, 205),
        6 =>  (  0, 205, 205),
        7 =>  (229, 229, 229),
        8 =>  (127, 127, 127),
        9 =>  (255,   0,   0),
        10 => (  0, 252,   0),
        11 => (255, 255,   0),
        12 => (  0,   0, 252),
        13 => (255,   0, 255),
        14 => (  0, 255, 255),
        15 => (255, 255, 255),
        16..=255 => LUT_PALETTE[(index as usize) - 16],
    }
}

pub fn xterm256_exact(r: u8, g: u8, b: u8) -> Option<u8> {
    // xterm256 color cube
    if let Some(r_part) = xterm256_rgb_part(r) {
        if let Some(g_part) = xterm256_rgb_part(g) {
            if let Some(b_part) = xterm256_rgb_part(b) {
                return Some(16 + 36 * r_part + 6 * g_part + b_part);
            }
        }
    }

    // xterm256 grey scale
    if r == g && r == b {
        if let Some(grey_part) = xterm256_grey_part(r) {
            return Some(231 + grey_part);
        }
    }

    None
}

pub fn xterm256_nearest(r: u8, g: u8, b: u8) -> u8 {
    let packed_rgb = pack_rgb(r, g, b);
    let index = lut_xterm256_nearest[packed_rgb].load(Ordering::Relaxed);
    if index != 0 {
        index
    } else {
        let lab = rgb_to_lab((r, g, b));

        let mut best_diff = 999999.0;
        let mut index = 0;

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

        for &candidate in values.iter() {
            let other = lut_xterm256_lab[candidate as usize];
            let diff = lab.difference(other);
            if diff < best_diff {
                best_diff = diff;
                index = candidate;
            }
        }

        lut_xterm256_nearest[packed_rgb].store(index, Ordering::Relaxed);

        index
    }
}


#[allow(dead_code)]
#[non_exhaustive]
#[derive(Clone, Copy, Debug, Hash, PartialEq)]
pub struct Xterm256(u8);

impl TryFrom<u8> for Xterm256 {
    type Error = Error;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            16..=255 => Ok(Self(value)),
            _ => Err(Error::BadIndex256(value)),
        }
    }
}

#[repr(u8)]
#[allow(dead_code)]
#[derive(Clone, Copy, Debug, Hash, PartialEq)]
pub enum Basic {
    Black = 0,
    Red,
    Green,
    Yellow,
    Blue,
    Magenta,
    Cyan,
    White,
    Default = 9,
    BrightBlack = 60,
    BrightRed,
    BrightGreen,
    BrightYellow,
    BrightBlue,
    BrightMagenta,
    BrightCyan,
    BrightWhite,
}

impl Basic {
    fn to_index(self) -> Option<u8> {
        let value = self as u8;
        match value {
            0..=7 => Some(value),
            60..=67 => Some(value - 52),
            9 => None,
            _ => panic!(),
        }
    }
}


impl TryFrom<u8> for Basic {
    type Error = Error;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0..=7 | 9 | 60..=67 => Ok(unsafe { std::mem::transmute::<_, Self>(value) }),
            _ => Err(Error::BadIndex16(value)),
        }
    }
}

#[repr(u8)]
#[derive(Clone, Copy, Debug, Hash, PartialEq)]
enum FgBg {
    Foreground = 30,
    Background = 40,
}

const FOREGROUND: FgBg = FgBg::Foreground;
const BACKGROUND: FgBg = FgBg::Background;

#[allow(dead_code)]
#[derive(Clone, Copy, Debug, Hash, PartialEq)]
pub enum TermColor {
    Rgb { r: u8, g: u8, b: u8, },
    Xterm256(Xterm256),
    Basic(Basic),
}

#[allow(dead_code)]
impl TermColor {
    #[inline(always)]
    fn fgbg_write(&self, base: FgBg, w: &mut dyn Write) -> io::Result<()> {
        let base = base as u8;
        match *self {
            Self::Rgb { r, g, b } => write_bytes!(w, b"{};2;{};{};{}", &(base + 8), r, g, b),
            Self::Xterm256 { 0: inner } => write_bytes!(w, b"{};5;{}", &(base + 8), inner.0),
            Self::Basic { 0: inner } => write_bytes!(w, b"{}", &(base + inner as u8)),
        }
    }

    #[inline(always)]
    fn fgbg_bytes(&self, base: FgBg) -> Vec<u8> {
        let mut vec = Vec::<u8>::new();
        // Never panics since `impl std::fmt::Write for Vec<u8>` never errors
        self.fgbg_write(base, &mut vec).unwrap();
        vec
    }

    #[inline]
    pub fn fg_write(&self, w: &mut dyn Write) -> io::Result<()> {
        self.fgbg_write(FOREGROUND, w)
    }

    #[inline]
    pub fn bg_write(&self, w: &mut dyn Write) -> io::Result<()> {
        self.fgbg_write(BACKGROUND, w)
    }

    #[inline]
    pub fn fg_bytes(&self) -> Vec<u8> {
        self.fgbg_bytes(FOREGROUND)
    }

    #[inline]
    pub fn bg_bytes(&self) -> Vec<u8> {
        self.fgbg_bytes(BACKGROUND)
    }

    pub fn from_packed(packed: u32) -> Result<Self, Error> {
        match packed {
            0..=7 | 9 | 60..=67 => {
                Ok(Self::Basic((packed as u8).try_into().unwrap()))
            },
            0x01000010..=0x010000ff => {
                Ok(Self::Xterm256((packed as u8).try_into().unwrap()))
            },
            0x02000000..=0x02ffffff => {
                let r = (packed >> 16) as u8;
                let g = (packed >> 16) as u8;
                let b = (packed >> 16) as u8;
                Ok(Self::Rgb { r, g, b })
            },
            _ => Err(Error::BadPacked(packed)),
        }
    }

    pub fn to_packed(&self) -> u32 {
        match *self {
            Self::Rgb { r, g, b } => (2u32 << 24) | (pack_rgb(r, g, b) as u32),
            Self::Xterm256 { 0: inner } => (1u32 << 24) | (inner.0 as u32),
            Self::Basic { 0: inner } => inner as u32,
        }
    }

    pub fn from_rgb(rgb: (u8, u8, u8)) -> Self {
        let (r, g, b) = rgb;
        match xterm256_exact(r, g, b) {
            None => Self::Rgb { r, g, b },
            Some(index) => Self::Xterm256(Xterm256(index)),
        }
    }

    pub fn to_rgb(&self) -> Option<(u8, u8, u8)> {
        match *self {
            Self::Rgb { r, g, b } => Some((r, g, b)),
            Self::Xterm256 { 0: inner } => Some(xterm256_index_to_rgb(inner.0)),
            Self::Basic { 0: _ } => None,
        }
    }

    pub fn from_index(index: Option<u8>) -> Self {
        match index {
            None => Self::Basic(Basic::Default),
            Some(index) => match index {
                0..=7 => Self::Basic(index.try_into().unwrap()),
                8..=15 => Self::Basic((index + 60).try_into().unwrap()),
                16..=255 => Self::Xterm256(Xterm256(index)),
            },
        }
    }

    pub fn to_index(&self) -> Option<u8> {
        match *self {
            Self::Rgb { r, g, b } => Some(xterm256_nearest(r, g, b)),
            Self::Xterm256 { 0: inner } => Some(inner.0),
            Self::Basic { 0: inner } => inner.to_index(),
        }
    }

    /*
    pub fn to_components(&self) -> (u8, u8, u8) {
        (self.r, self.g, self.b)
    }

    pub fn to_srgb(&self) -> Srgb<u8> {
        Srgb::from_components(self.to_components())
    }

    pub fn to_lab(&self) -> Lab {
        Lab::from_color(self.to_srgb().into_linear::<f32>())
    }
    */
}

/*
impl std::hash::Hash for TermRgb {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let packed_rgb = pack_rgb(self.r, self.g, self.b);
        packed_rgb.hash(state);
    }
}

impl ToString for TermRgb {
    fn to_string(&self) -> String {
        match xterm256_exact(self.r, self.g, self.b) {
            Some(index) => format!("5;{}", index),
            None => format!("2;{};{};{}", self.r, self.g, self.b)
        }
    }
}
*/

// 22: not bold, 23: not italic, 24: not underline
#[derive(EnumSetType, Debug, Hash)]
pub enum TermAttr {
    Bold = 1,
    Italic = 3,
    Underline = 4,
}

pub fn parse_ansi(c: &mut Chars) -> Result<Option<(Vec<u8>, char)>, Error> {
    let mut accum = 0u32;
    let mut state = 0u32;
    let mut numbers = Vec::<u8>::new();
    loop {
        if let Some(cc) = c.next() {
            let v: u32 = state | u32::from(cc);
            //println!("GOT {:08x}", v);
            match v {
                0..=26 | 28..=0x10ffff => {
                    return Ok(Some((numbers, v.try_into().unwrap())));
                },
                0x1b => /* escape */ { state = 1 << 24; },
                0x100005b => /* [ */ { state = 2 << 24; },
                0x2000030..=0x2000039 => /* 0-9 */ {
                    accum = accum * 10 + (v & 0xf);
                    if accum > 255 {
                        return Err(Error::ParseIntOverflow);
                    }
                },
                0x200003b => /* ; */ {
                    numbers.push(accum as u8);
                    accum = 0;
                },
                0x200006d => /* m */ {
                    numbers.push(accum as u8);
                    accum = 0;
                    state = 0;
                },
                0x2000048 => /* H */ {
                    numbers.truncate(0);
                    state = 0;
                },
                _ => {
                    return Err(Error::ParseBadChar{
                        state: v >> 24,
                        codepoint: v & 0xffffff
                    });
                },
            }
        } else {
            return Ok(None);
        }
    }
}


#[derive(Clone, Copy, Debug, Hash, PartialEq)]
pub struct TermCell {
    pub fg: TermColor,
    pub bg: TermColor,
    pub attr: EnumSet<TermAttr>,
    pub codepoint: char,
}

// pub fn parse_ansi(c: &mut Chars) -> Result<Option<(Vec<u8>, char)>, Error> {

fn mk_cell(
    last_fg: &TermColor,
    last_bg: &TermColor,
    last_attr: &EnumSet<TermAttr>,
    numbers: Vec<u8>,
    codepoint: char
) -> Result<TermCell, Error> {
    let mut iter = numbers.into_iter();

    let mut fg: Option<TermColor> = None;
    let mut bg: Option<TermColor> = None;
    let mut attr = last_attr.clone();

    loop {
        match iter.next() {
            Some(number) => match number {
                0 => {
                    fg = Some(TermColor::from_index(None));
                    bg = Some(TermColor::from_index(None));
                    attr = enum_set!() as EnumSet<TermAttr>;
                },
                1 => { attr.insert(TermAttr::Bold); },
                22 => { attr.remove(TermAttr::Bold); },
                3 => { attr.insert(TermAttr::Italic); },
                23 => { attr.remove(TermAttr::Italic); },
                4 => { attr.insert(TermAttr::Underline); },
                24 => { attr.remove(TermAttr::Underline); },
                30..=37 | 39 | 90..=97 => {
                    fg = Some(TermColor::Basic((number - 30).try_into().unwrap()));
                },
                40..=47 | 49 | 100..=107 => {
                    bg = Some(TermColor::Basic((number - 40).try_into().unwrap()));
                },
                38 | 48 => {
                    let fgbg = match number {
                        38 => FOREGROUND,
                        48 => BACKGROUND,
                        _ => unreachable!(),
                    };

                    match iter.next() {
                        Some(number) => {
                            let color = match number {
                                5 => {
                                    let r = iter.next();
                                    let g = iter.next();
                                    let b = iter.next();
                                    if r.is_some() && g.is_some() && b.is_some() {
                                        let rgb = (r.unwrap(), g.unwrap(), b.unwrap());
                                        TermColor::from_rgb(rgb)
                                    } else {
                                        return Err(Error::ParseTruncated);
                                    }
                                },
                                2 => {
                                    match iter.next() {
                                        Some(index) => match index {
                                            16..=255 => TermColor::from_index(Some(index)),
                                            _ => { return Err(Error::ParseBadCode); },
                                        },
                                        None => { return Err(Error::ParseTruncated); },
                                    }
                                },
                                _ => { return Err(Error::ParseBadCode); },
                            };

                            match fgbg {
                                FOREGROUND => { fg = Some(color); },
                                BACKGROUND => { bg = Some(color); },
                            }
                        },
                        None => { return Err(Error::ParseTruncated); },
                    }
                },
                _ => { return Err(Error::ParseBadCode); },
            },
            None => { break; },
        }
    }

    Ok(TermCell {
        fg: fg.unwrap_or_else(|| last_fg.clone()),
        bg: bg.unwrap_or_else(|| last_bg.clone()),
        attr,
        codepoint,
    })
}

pub fn parse_cells(c: &mut Chars) -> Result<Vec<TermCell>, Error> {
    let mut last_fg = TermColor::from_index(None);
    let mut last_bg = TermColor::from_index(None);
    let mut last_attr: EnumSet<TermAttr> = enum_set!();
    let mut cells = Vec::<TermCell>::new();

    loop {
        match parse_ansi(c) {
            Ok(result) => match result {
                Some((numbers, chr)) => {
                    if chr == '\x0c' {
                        println!("START OF FRAME");
                    } else if chr == '\n' {
                        println!("END OF LINE");
                    } else {
                        /* construct cell */
                        todo!();
                    }
                },
                None => { break; },
            },
            Err(error) => { return Err(error); },
        }
    }

    Ok(cells)
}
