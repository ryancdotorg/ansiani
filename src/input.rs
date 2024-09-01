use std::fs::File;
use std::io::{self, BufRead, BufReader, Read};
use std::path::{Path, PathBuf};

use zstd::stream::read::Decoder as ZstdDecoder;
use flate2::bufread::MultiGzDecoder;

pub struct Input<'a> {
    pub label: String,
    inner: Box<dyn BufRead + 'a>,
}


//const XZ_MAGIC:    [u8; 6] = *b"\xfd7zXZ\0";
//const BZIP2_MAGIC: [u8; 3] = *b"BZh";
//const LZ4_MAGIC:   [u8; 4] = [0x04, 0x22, 0x4d, 0x18];
//const LZO_MAGIC:   [u8; 9] = *b"\x89LZO\0\r\n\x1a\n";
const ZSTD_MAGIC:  [u8; 4] = [0x28, 0xb5, 0x2f, 0xfd];
const GZIP_MAGIC:  [u8; 3] = [0x1f, 0x8b, 0x08];

impl<'a> Input<'a> {
    fn new(label: impl ToString, reader: impl BufRead + 'a) -> Self {
        let label = label.to_string();
        let inner = Box::new(reader);
        Input { label, inner, }
    }

    // stdin -> reader
    #[allow(dead_code)]
    pub fn from_stdin() -> io::Result<Input<'a>> {
        Input::from_reader(io::stdin().lock(), "STDIN")
    }

    // path -> file -> reader
    pub fn from_path(path: impl AsRef<Path>) -> io::Result<Input<'a>> {
        let path: &Path = path.as_ref();
        let label = path.as_os_str().to_string_lossy();
        File::open(path).map(|file| Input::from_file(file, &label).unwrap())
    }

    // file -> reader
    pub fn from_file(file: File, label: &str) -> io::Result<Input<'a>> {
        Input::from_reader(BufReader::new(file), label)
    }

    // reader (always gets called)
    pub fn from_reader(mut reader: impl BufRead + 'a, label: &str) -> io::Result<Input<'a>> {
        let buf = reader.fill_buf()?;

        if buf.len() >= 4 && &buf[0..=3] == ZSTD_MAGIC {
            let reader = ZstdDecoder::with_buffer(reader)?;
            let reader = BufReader::new(reader);
            return Ok(Input::new(label, reader));
        }

        if buf.len() >= 3 && &buf[0..=2] == GZIP_MAGIC {
            let reader = MultiGzDecoder::new(reader);
            let reader = BufReader::new(reader);
            return Ok(Input::new(label, reader));
        }

        Ok(Input::new(label, reader))
    }

    #[allow(dead_code)]
    pub fn as_label(&self) -> String {
        self.label.clone()
    }
}

impl<'a> Read for Input<'a> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.inner.read(buf)
    }
}

impl<'a> BufRead for Input<'a> {
    fn fill_buf(&mut self) -> io::Result<&[u8]> {
        self.inner.fill_buf()
    }

    fn consume(&mut self, amt: usize) {
        self.inner.consume(amt);
    }
}

macro_rules! input_try_from_path {
    ($t:ty) => {
        impl<'a> TryFrom<$t> for Input<'a> {
            type Error = io::Error;

            fn try_from(path: $t) -> Result<Self, Self::Error> {
                Input::from_path(path)
            }
        }
    };
}

input_try_from_path!(&Path);
input_try_from_path!(&PathBuf);
input_try_from_path!(String);
