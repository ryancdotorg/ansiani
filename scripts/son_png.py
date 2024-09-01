#!/usr/bin/env python3

from sys import argv, exit, stdin, stdout, stderr, version_info
from functools import partial
eprint = partial(print, file=stderr)

# Python standard library imports
import re
from pathlib import Path
from dataclasses import dataclass
from collections.abc import Iterable
# Third party library imports
import numpy
from PIL import Image, ImageDraw
# End imports

filename_son = Path(argv[1])
directory = filename_son.parent
filename_srt = Path(directory, filename_son.stem + '.srt')

RGB_RED = (255, 0, 0)
RGBA_BACKGROUND = (31, 31, 31, 255)
RGBA_CELL_BORDER = (0, 127, 255, 255)
RGBA_CELL_FILL = (0, 63, 127, 255)
RGBA_BLOCK = (127, 0, 63, 255)

ANSI_BOLD_ON =  '\033[1m'
ANSI_BOLD_OFF = '\033[22m'
ANSI_ITALIC_ON =  '\033[3m'
ANSI_ITALIC_OFF = '\033[23m'

RE_AREA = r'[(](\d+)\s+(\d+)\s+(\d+)\s+(\d+)[)]'

MAP_ROW = {
    4: 4,
    6: 5,
    7: 6,
    9: 7,

    15: 16,
    16: 17,
    18: 18,
    19: 19, 20: 19,
}

@dataclass
class Size:
    w: int
    h: int

    @property
    def wh(self): return (self.w, self.h)

    def __iter__(self):
        return iter((self.w, self.h))

    def __str__(self):
        return f'{self.w}x{self.h}'

@dataclass
class Position:
    x: int
    y: int

    def __iter__(self):
        return iter((self.x, self.y))

    def __str__(self):
        return f'{self.x:+}{self.y:+}'

@dataclass
class Area:
    x0: int
    y0: int
    x1: int
    y1: int

    @property
    def x(self): return self.x0

    @property
    def y(self): return self.y0

    @property
    def w(self): return abs(self.x1 - self.x0) + 1

    @property
    def h(self): return abs(self.y1 - self.y0) + 1

    @property
    def size(self): return Size(self.w, self.h)

    @property
    def pos(self): return Position(self.x0, self.y0)

    def __iter__(self):
        return iter(((self.x0, self.y0), (self.x1, self.y1)))

    def __str__(self):
        return f'{self.w}x{self.h}{self.x:+}{self.y:+}'

def fix_time(t):
    return round((t-104.651)*(7181.88/7181.908)+104.683, 3)

def parse_tuple(s):
    if m := re.fullmatch(r'[^(]*[(]([0-9, ]*)[)]', s):
        return tuple(map(int, re.split(r'[ ,]+', m.group(1).strip())))

def to_transparent(i, r, g=None, b=None):
    if g is None:
        r, g, b = r
    # https://stackoverflow.com/a/3753428
    rgba = numpy.array(i)
    red = (rgba.T[0] == r) & (rgba.T[1] == g) & (rgba.T[2] == b)
    rgba[..., :][red.T] = (0, 0, 0, 0)
    return Image.fromarray(rgba)

def find_blocks(i, area, num=None):
    # find rows of text
    rgba = numpy.array(i)
    pixels_rc = (rgba.T[3] == 255).transpose()
    rows = []
    row_start, row_end = None, None
    last_c1 = None

    for x in range(area.h):
        used = pixels_rc[x].any()
        c1 = numpy.argmax(pixels_rc[x] == True) if used else -1
        if row_start is None:
            if used:
                row_start, row_end = x, x
                last_c1 = c1
        elif used:
            row_end = x
        # allow gaps between lines in the same block
        #elif x - row_end >= 16:
        else:
            rows.append((row_start, row_end))
            row_start, row_end = None, None

        last_c1 = c1

    if row_start is not None:
        rows.append((row_start, row_end))

    #print(rows)

    blocks = []
    for row in rows:
        col_start, col_end = None, None
        for r in range(*row):
            for c in range(area.w):
                if pixels_rc[r][c]:
                    if col_start is None:
                        col_start, col_end = c, c
                    else:
                        col_start = min(col_start, c)
                        col_end = max(col_end, c)

        blocks.append(Area(
            area.x + col_start,
            area.y + row[0],
            area.x + col_end,
            area.y + row[1],
        ))

    return blocks

def italic_count(s):
    i_open =  len(list(re.finditer(r'<i>', block, re.MULTILINE)))
    i_close = len(list(re.finditer(r'</i>', block, re.MULTILINE)))
    return (i_open, i_close)

def bold_count(s):
    b_open =  len(list(re.finditer(r'<b>', block, re.MULTILINE)))
    b_close = len(list(re.finditer(r'</b>', block, re.MULTILINE)))
    return (b_open, b_close)

captions = {}
times = [None]

with open(filename_srt, 'r') as f:
    subid = None
    block = ''
    blocks = []
    t_start, t_end = None, None
    for line in map(str.strip, f):
        if re.fullmatch(r'[1-9][0-9]{0,4}', line):
            if len(blocks): captions[subid] = blocks

            block = ''
            blocks = []
            subid = int(line)
        elif m := re.fullmatch(r'([0-9:,]+) --> ([0-9:,]+)', line):
            g_start = m.group(1).replace(',', '.').split(':')
            g_end = m.group(2).replace(',', '.').split(':')
            t_start = fix_time(int(g_start[0]) * 3600 + int(g_start[1]) * 60 + float(g_start[2]))
            t_end = fix_time(int(g_end[0]) * 3600 + int(g_end[1]) * 60 + float(g_end[2]))
            times.append((t_start, t_end))
        elif not block and line and subid in (171, 220):
            block = line + '\n'
        elif line:
            block += line
            i_open =  len(list(re.finditer(r'<i>', block, re.MULTILINE)))
            i_close = len(list(re.finditer(r'</i>', block, re.MULTILINE)))
            if   i_open > i_close: block = block + '</i>'
            elif i_open < i_close: block = '<i>' + block
            blocks.append(block)
            block = ''

    if len(blocks): captions[subid] = blocks

with open(filename_son, 'r') as f:
    # x0, y0, x1, y1
    pix_area = Area(0, 0, 719, 479)
    dsp_area = Area(0, 0, 0, 0)
    size = Size(720, 480)

    for line in map(str.strip, f):
        cmd, *parts = re.split(r'\t+', line)

        if cmd == 'Pixel_Area':
            y0, y1 = parse_tuple(parts[0])
            pix_area = Area(0, y0, 719, y1)
            size.h = y1 + 1
        elif cmd == 'Display_Area':
            dsp_area = Area(*parse_tuple(parts[0]))
        elif re.fullmatch(r'\d+', cmd):
            num = int(cmd)
            show = parts[0]
            hide = parts[1]
            bmp = Path(directory, parts[2])
            png = Path(directory, f'vobsub_{num:05}.png')

            # new image
            i = Image.new('RGBA', tuple(size), RGBA_BACKGROUND)

            # load subtitle image
            subtitle = Image.open(bmp).convert('RGBA')
            # make subtitle background transparent
            subtitle = to_transparent(subtitle, RGB_RED)
            # find text regions
            blocks = find_blocks(subtitle, dsp_area, num)
            #if len(blocks) > 1:
            #    print(num, ' '.join(map(str, blocks)))

            # draw cells
            # XXX assumes 16:9 playback aspect ratio and 720x480 resolution
            d = ImageDraw.Draw(i)
            for x in range(4, 80, 5):
                x0 = int(x * (720/80))
                x1 = int((x + 1) * (720/80))
                d.rectangle(((x0, 0), (x1, 479)), RGBA_CELL_FILL)

            for y in range(0, 23):
                if y in MAP_ROW.values():
                    y0 = int(y * (480/22.5))
                    y1 = int((y + 1) * (480/22.5))
                    d.rectangle(((0, y0), (719, y1)), RGBA_CELL_FILL)

            # draw cell borders
            for x in range(1, 80):
                x0 = int(x * (720/80))
                d.line((x0 - 1, 0, x0 - 1, 479), RGBA_CELL_BORDER)
                d.line((x0, 0, x0, 479), RGBA_CELL_BORDER)

            for y in range(1, 23):
                y0 = int(y * (480/22.5))
                d.line((0, y0 - 1, 719, y0 - 1), RGBA_CELL_BORDER)
                d.line((0, y0, 719, y0), RGBA_CELL_BORDER)

            # draw block backgrounds
            for block in blocks:
                d.rectangle(tuple(block), RGBA_BLOCK)

            if len(blocks) != len(captions[num]):
                print('MISMATCH', num, len(blocks), len(captions[num]))

            for n, block in enumerate(blocks):
                x, y = tuple(block.pos)
                c = int(x * 80.0 / 720 + 0.5)
                r = MAP_ROW[int(y * 22.5 / 480 + 0.5)]
                start, end = times[num]
                caption = '<br>'.join(captions[num][n].split('\n'))
                #print(f'num={num}', (x, y), f'(row={r}, col={c})')
                #print(caption)
                print(f'{num}\t{start:.3f}\t{end:.3f}\t{r}\t{c}\t{caption}')
            print('')

            # composite subtitle image with background
            i.alpha_composite(subtitle, tuple(dsp_area.pos))

            # write png
            #print(png)
            i.save(png)

            # convert in.bmp -transparent red out.png
            # timecode
            args = [
                'convert',
                '-size', size,
                'xc:transparent',
                bmp,
                '-geometry', dsp_area,
                '-transparent', 'red',
                '-composite',
                png,
            ]
            #print(' '.join(map(str, args)))
