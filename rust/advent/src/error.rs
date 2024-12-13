use std::convert::From;
use std::error;
use std::fmt;
use std::io;
use std::num::ParseIntError;
use std::result;

pub type Result<R> = result::Result<R, Error>;

#[derive(Debug)]
pub enum Error {
    Io(io::Error),
    Parse(ParseIntError),
    RangeParse(String),
    MapNetworkParse(String),
    InvalidInputSource(String),
}

use Error::*;

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Io(ref err) => err.fmt(f),
            Parse(ref err) => err.fmt(f),
            RangeParse(ref value) => write!(f, "Error parsing: {:?}", value),
            MapNetworkParse(ref msg) => write!(f, "Error parsing map network: {}", msg),
            InvalidInputSource(ref msg) => write!(f, "Invalid input source: {}", msg),
        }
    }
}

impl error::Error for Error {}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Error {
        Io(err)
    }
}

impl From<ParseIntError> for Error {
    fn from(value: ParseIntError) -> Self {
        Parse(value)
    }
}
