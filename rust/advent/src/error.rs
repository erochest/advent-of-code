use std::convert::From;
use std::error;
use std::fmt;
use std::io;
use std::num::ParseIntError;
use std::result;

pub type Result<R> = result::Result<R, Error>;

#[derive(Debug)]
pub enum Error {
    IoError(io::Error),
    ParseError(ParseIntError),
    RangeParseError(String),
}

use Error::*;

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IoError(ref err) => err.fmt(f),
            ParseError(ref err) => err.fmt(f),
            RangeParseError(ref value) => write!(f, "Error parsing: {:?}", value),
        }
    }
}

impl error::Error for Error {}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Error {
        IoError(err)
    }
}

impl From<ParseIntError> for Error {
    fn from(value: ParseIntError) -> Self {
        ParseError(value)
    }
}
