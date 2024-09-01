use std::{
    io::{self, Read, Write},
    str::Chars,
    sync::atomic::{AtomicU8, Ordering},
};

use byteorder::{BE, ReadBytesExt};
use enumset::{enum_set, EnumSet, EnumSetType};
use format_bytes::write_bytes;
use lazy_static::lazy_static;

include!(concat!(env!("OUT_DIR"),"/lut_palette.rs"));
include!(concat!(env!("OUT_DIR"),"/lut_partial.rs"));

use palette::{
    Srgb,
    lab::Lab,
    convert::FromColor,
    color_difference::Ciede2000,
    color_difference::ImprovedCiede2000,
};

use thiserror::Error as ThisError;
#[derive(ThisError, Debug)]
pub enum Error {
    #[error("invalid packed color: {0:08x}")]
    BadPacked(u32),
    #[error("invalid codepoint: {0}")]
    BadCodepoint(u32),
    #[error("invalid color index, must be [16..=255], not {0}")]
    BadIndex256(u8),
    #[error("invalid color index, must be [0..=7 | 9 | 60..=67], not {0}")]
    BadIndex16(u8),
    #[error("invalid time code, must be [0..=16777215], not {0}")]
    BadTimeCode(u32),
    #[error("parse failed, integer overflow")]
    ParseIntOverflow,
    #[error("parse failed, unexpected codepoint {codepoint} in state {state}")]
    ParseBadChar { state: u32, codepoint: u32 },
    #[error("parse failed, unexpected code {code}")]
    ParseBadCode { code: u8 },
    #[error("parse failed, truncated")]
    ParseTruncated,
//    #[error("io error {0:?}")]
//    IoError(io::Error),
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

pub fn rgb_difference(rgb1: (u8, u8, u8), rgb2: (u8, u8, u8)) -> f32 {
    let lab1 = rgb_to_lab(rgb1);
    let lab2 = rgb_to_lab(rgb2);

    lab1.improved_difference(lab2)
}

fn pack_rgb(r: u8, g: u8, b: u8) -> usize {
    ((r as usize) << 16) | ((g as usize) << 8) | (b as usize)
}

fn xterm256_rgb_part(v: u8) -> Option<u8> {
    match v {
          0 => Some(0),
         95 => Some(1),
        135 => Some(2),
        175 => Some(3),
        215 => Some(4),
        255 => Some(5),
        _ => None,
    }
}

fn xterm256_grey_part(v: u8) -> Option<u8> {
    if v >= 8 && (v - 8) % 10 == 0 && v <= 238 {
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

pub fn xterm256_threshold(r: u8, g: u8, b: u8, threshold: f32) -> Option<u8> {
    // NaN comparisons always fail, so we don't want to use < 0.0
    if !(threshold >= 0.0) {
        None
    } else if let Some(index) = xterm256_exact(r, g, b) {
        let packed_rgb = pack_rgb(r, g, b);
        lut_xterm256_nearest[packed_rgb].store(index, Ordering::Relaxed);
        Some(index)
    } else {
        let index = xterm256_nearest(r, g, b);
        // biggest difference from nearest is for #002502
        if threshold > 11.51406 { return index; }
        let other = lut_xterm256_lab[index as usize];
        let lab = rgb_to_lab((r, g, b));
        if lab.difference(other) <= threshold {
            Some(index)
        } else {
            None
        }
    }
}

pub fn xterm256_nearest(r: u8, g: u8, b: u8) -> u8 {
    let packed_rgb = pack_rgb(r, g, b);
    let index = lut_xterm256_nearest[packed_rgb].load(Ordering::Relaxed);
    if index != 0 {
        index
    } else if let Some(index) = xterm256_exact(r, g, b) {
        lut_xterm256_nearest[packed_rgb].store(index, Ordering::Relaxed);
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


#[non_exhaustive]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
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

#[non_exhaustive]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct TimeCode(u32);

impl TryFrom<u32> for TimeCode {
    type Error = Error;

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        match value {
            0u32..=16777215 => Ok(Self(value)),
            _ => Err(Error::BadTimeCode(value)),
        }
    }

}

impl TimeCode {
    fn as_u32(self) -> u32 {
        self.0 as u32
    }
}

#[repr(u8)]
#[allow(dead_code)]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum Basic {
    Default = 9,
    Black = 0,
    Red,
    Green,
    Yellow,
    Blue,
    Magenta,
    Cyan,
    White,
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

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum TermColor {
    Rgb { r: u8, g: u8, b: u8 },
    Xterm256(Xterm256),
    Basic(Basic),
    TimeCode(TimeCode),
}

impl TermColor {
    #[inline(always)]
    fn fgbg_write(&self, base: FgBg, w: &mut dyn Write) -> io::Result<()> {
        let base = base as u8;
        match *self {
            Self::Rgb { r, g, b } => write_bytes!(w, b"{};2;{};{};{}", &(base + 8), r, g, b),
            Self::Xterm256 { 0: inner } => write_bytes!(w, b"{};5;{}", &(base + 8), inner.0),
            Self::Basic { 0: inner } => write_bytes!(w, b"{}", &(base + inner as u8)),
            Self::TimeCode { 0: inner } => {
                match base {
                    30 => write_bytes!(w, b"\x16{}t", inner.as_u32()),
                    _ => write_bytes!(w, b""),
                }
            }
        }
    }

    #[inline(always)]
    fn fgbg_string(&self, base: FgBg) -> String {
        let base = base as u8;
        match *self {
            Self::Rgb { r, g, b } => format!("{};2;{};{};{}", &(base + 8), r, g, b),
            Self::Xterm256 { 0: inner } => format!("{};5;{}", &(base + 8), inner.0),
            Self::Basic { 0: inner } => format!("{}", &(base + inner as u8)),
            Self::TimeCode { 0: inner } => {
                match base {
                    30 => format!("\x16{}t", inner.as_u32()),
                    _ => format!(""),
                }
            }
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
    #[allow(dead_code)]
    pub fn fg_write(&self, w: &mut dyn Write) -> io::Result<()> {
        self.fgbg_write(FOREGROUND, w)
    }

    #[inline]
    #[allow(dead_code)]
    pub fn bg_write(&self, w: &mut dyn Write) -> io::Result<()> {
        self.fgbg_write(BACKGROUND, w)
    }

    #[inline]
    #[allow(dead_code)]
    pub fn fg_bytes(&self) -> Vec<u8> {
        self.fgbg_bytes(FOREGROUND)
    }

    #[inline]
    #[allow(dead_code)]
    pub fn bg_bytes(&self) -> Vec<u8> {
        self.fgbg_bytes(BACKGROUND)
    }

    #[inline]
    #[allow(dead_code)]
    pub fn fg_string(&self) -> String {
        self.fgbg_string(FOREGROUND)
    }

    #[inline]
    #[allow(dead_code)]
    pub fn bg_string(&self) -> String {
        self.fgbg_string(BACKGROUND)
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
                let g = (packed >>  8) as u8;
                let b = (packed >>  0) as u8;
                Ok(Self::Rgb { r, g, b })
            },
            0x0f000000..=0x0fffffff => {
                let t = packed & 0xffffff;
                Ok(Self::TimeCode(t.try_into().unwrap()))
            },
            _ => Err(Error::BadPacked(packed)),
        }
    }

    pub fn to_packed(&self) -> u32 {
        match *self {
            Self::Rgb { r, g, b } => (2u32 << 24) | (pack_rgb(r, g, b) as u32),
            Self::Xterm256 { 0: inner } => (1u32 << 24) | (inner.0 as u32),
            Self::Basic { 0: inner } => inner as u32,
            Self::TimeCode { 0: inner } => 0x0f000000 + inner.as_u32(),
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
            Self::TimeCode { 0: _ } => None,
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

    #[allow(dead_code)]
    pub fn to_index(&self) -> Option<u8> {
        match *self {
            Self::Rgb { r, g, b } => Some(xterm256_nearest(r, g, b)),
            Self::Xterm256 { 0: inner } => Some(inner.0),
            Self::Basic { 0: inner } => inner.to_index(),
            Self::TimeCode { 0: _ } => None,
        }
    }

    #[inline]
    pub fn as_timecode(&self) -> Option<u32> {
        match *self {
            Self::TimeCode { 0: inner } => Some(inner.as_u32()),
            _ => None,
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

// 1: bold
// 2: dim
// 3: italic
// 4: underline
// 5: blink
// 7: reverse
// 8: conceal
// 9: strike
// 21: not bold (non-standard/bug)
// 22: not bold/dim
// 23: not italic
// 24: not underline
// 25: not blink
// 27: not reverse
// 28: not conceal
// 29: not strike
#[derive(EnumSetType, Debug, Hash)]
pub enum TermAttr {
    Bold = 1,
    Italic = 3,
    Underline = 4,
}

impl ToString for TermAttr {
    fn to_string(&self) -> String {
        match *self {
            TermAttr::Bold => "1".to_string(),
            TermAttr::Italic => "3".to_string(),
            TermAttr::Underline => "4".to_string(),
        }
    }
}

#[inline]
fn push_attr(
    attrs: &mut Vec<String>,
    a: &EnumSet<TermAttr>,
    b: &EnumSet<TermAttr>,
    check: TermAttr,
    code: &str,
) {
    if a.contains(check) && !b.contains(check) {
        attrs.push(code.to_string());
    }
}

pub fn change_attr_string(old: &EnumSet<TermAttr>, new: &EnumSet<TermAttr>) -> String {
    let mut attrs = Vec::<String>::new();

    // clear
    push_attr(&mut attrs, &old, &new, TermAttr::Bold, "22");
    push_attr(&mut attrs, &old, &new, TermAttr::Italic, "23");
    push_attr(&mut attrs, &old, &new, TermAttr::Underline, "24");

    // set
    push_attr(&mut attrs, &new, &old, TermAttr::Bold, "1");
    push_attr(&mut attrs, &new, &old, TermAttr::Italic, "3");
    push_attr(&mut attrs, &new, &old, TermAttr::Underline, "4");

    attrs.join(";")
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
                0..=0x15 | 0x17..=0x1a | 0x1c..=0x10ffff => {
                    return Ok(Some((numbers, v.try_into().unwrap())));
                },
                0x0000016 => /* frame syncronize */ {
                    state = 0xf << 24;
                },
                0x000001b => /* escape */ { state = 1 << 24; },
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
                0x2000047 => /* G */ {
                    numbers.truncate(0);
                    state = 0;
                },
                0x2000048 => /* H */ {
                    return Ok(Some((numbers, '\x0c')));
                },
                0x200004a => /* J */ {
                    numbers.truncate(0);
                    state = 0;
                },
                0xf000030..=0xf000039 => /* 0-9 */ {
                    accum = accum * 10 + (v & 0xf);
                    if accum > 16777215 {
                        return Err(Error::ParseIntOverflow);
                    }
                },
                0xf000074 => /* t */ {
                    numbers.push(255u8);
                    numbers.push((accum >> 16) as u8);
                    numbers.push((accum >>  8) as u8);
                    numbers.push((accum >>  0) as u8);
                    accum = 0;
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

impl TermCell {
    pub fn from_packed(tup: (u32, u32, u32)) -> Result<Self, Error> {
        let fg = TermColor::from_packed(tup.0)?;
        let bg = TermColor::from_packed(tup.1)?;
        let attr = EnumSet::<TermAttr>::from_u32(tup.2 >> 24);
        let c = tup.2 & 0xffffff;
        let codepoint = char::from_u32(c).ok_or_else(|| Error::BadCodepoint(c))?;

        Ok(Self { fg, bg, attr, codepoint })
    }

    #[inline]
    pub fn to_packed(&self) -> (u32, u32, u32) {
        let fg = self.fg.to_packed();
        let bg = self.bg.to_packed();
        let attr = self.attr.as_u32() << 24;
        let codepoint: u32 = attr | u32::from(self.codepoint);

        (fg, bg, codepoint)
    }

    pub fn to_packed_bytes(&self) -> Vec<u8> {
        let mut bytes = Vec::<u8>::new();
        let packed = self.to_packed();
        bytes.extend_from_slice(&mut packed.0.to_be_bytes());
        bytes.extend_from_slice(&mut packed.1.to_be_bytes());
        bytes.extend_from_slice(&mut packed.2.to_be_bytes());

        bytes
    }

    pub fn read_packed(r: &mut dyn Read) -> io::Result<Self> {
        let p0 = r.read_u32::<BE>()?;
        let p1 = r.read_u32::<BE>()?;
        let p2 = r.read_u32::<BE>()?;
        //println!("PACKED {:08x} {:08x} {:08x}", p0, p1, p2);
        Self::from_packed((p0, p1, p2)).map_err(|e| io::Error::other(e))
    }

    #[inline]
    #[allow(dead_code)]
    pub fn fg_write(&self, w: &mut dyn Write) -> io::Result<()> {
        self.fg.fgbg_write(FOREGROUND, w)
    }

    #[inline]
    #[allow(dead_code)]
    pub fn bg_write(&self, w: &mut dyn Write) -> io::Result<()> {
        self.bg.fgbg_write(BACKGROUND, w)
    }

    #[inline]
    #[allow(dead_code)]
    pub fn fg_bytes(&self) -> Vec<u8> {
        self.fg.fgbg_bytes(FOREGROUND)
    }

    #[inline]
    #[allow(dead_code)]
    pub fn bg_bytes(&self) -> Vec<u8> {
        self.bg.fgbg_bytes(BACKGROUND)
    }

    #[inline]
    pub fn fg_string(&self) -> String {
        self.fg.fgbg_string(FOREGROUND)
    }

    #[inline]
    pub fn bg_string(&self) -> String {
        self.bg.fgbg_string(BACKGROUND)
    }

    #[inline]
    pub fn as_timecode(&self) -> Option<u32> {
        self.fg.as_timecode()
    }

    #[allow(dead_code)]
    pub fn set_fg_index(&mut self, index: u8) {
        self.fg = TermColor::from_index(Some(index));
    }

    #[allow(dead_code)]
    pub fn set_bg_index(&mut self, index: u8) {
        self.bg = TermColor::from_index(Some(index));
    }
}

fn mk_cell(
    last_fg: &TermColor,
    last_bg: &TermColor,
    last_attr: &EnumSet<TermAttr>,
    numbers: Vec<u8>,
    codepoint: char,
) -> Result<TermCell, Error> {
    //eprintln!("\nparsed {:?} U+{:X}\n", numbers, codepoint as u32);
    //eprintln!("last fg({:?}) bg({:?})", last_fg, last_bg);
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
                255 => {
                    let x = iter.next();
                    let y = iter.next();
                    let z = iter.next();
                    if x.is_some() && y.is_some() && z.is_some() {
                        let t = (x.unwrap() as u32) << 16
                              | (x.unwrap() as u32) <<  8
                              | (x.unwrap() as u32)     ;

                        fg = Some(TermColor::TimeCode(t.try_into().unwrap()));
                        bg = Some(TermColor::TimeCode(t.try_into().unwrap()));
                    } else {
                        return Err(Error::ParseTruncated);
                    }
                }
                38 | 48 => {
                    let fgbg = match number {
                        38 => FOREGROUND,
                        48 => BACKGROUND,
                        _ => unreachable!(),
                    };

                    match iter.next() {
                        Some(number) => {
                            let color = match number {
                                2 => {
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
                                5 => {
                                    match iter.next() {
                                        Some(index) => match index {
                                            16..=255 => TermColor::from_index(Some(index)),
                                            _ => { return Err(Error::ParseBadCode { code: index }); },
                                        },
                                        None => { return Err(Error::ParseTruncated); },
                                    }
                                },
                                _ => { return Err(Error::ParseBadCode { code: number }); },
                            };

                            match fgbg {
                                FOREGROUND => { fg = Some(color); },
                                BACKGROUND => { bg = Some(color); },
                            }
                        },
                        None => { return Err(Error::ParseTruncated); },
                    }
                },
                _ => { return Err(Error::ParseBadCode { code: number }); },
            },
            None => { break; },
        }
    }

    let cell = TermCell {
        fg: fg.unwrap_or_else(|| last_fg.clone()),
        bg: bg.unwrap_or_else(|| last_bg.clone()),
        attr,
        codepoint,
    };
    //eprintln!("{:?}", cell);
    Ok(cell)
}

pub fn parse_cells(c: &mut Chars) -> Result<Vec<TermCell>, Error> {
    let mut last_fg = TermColor::from_index(None);
    let mut last_bg = TermColor::from_index(None);
    let mut last_attr: EnumSet<TermAttr> = enum_set!();
    let mut cells = Vec::<TermCell>::new();

    loop {
        match parse_ansi(c) {
            Ok(result) => match result {
                Some((v, chr)) => {
                    let numbers: Vec<u8> = match chr {
                        '\n' | '\x0c' | '\x16' | '\x17' => vec![0],
                        _ => v,
                    };

                    let cell = mk_cell(
                        &last_fg, &last_bg, &last_attr,
                        numbers, chr
                    )?;

                    last_fg = cell.fg.clone();
                    last_bg = cell.bg.clone();
                    last_attr = cell.attr.clone();

                    cells.push(cell);
                },
                None => { break; },
            },
            Err(error) => { return Err(error); },
        }
    }

    Ok(cells)
}
