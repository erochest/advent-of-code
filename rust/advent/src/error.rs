use std::io;
use std::num::ParseIntError;
use std::result;

use thiserror::Error;

pub type Result<R> = result::Result<R, Error>;

#[derive(Error, Debug)]
pub enum Error {
    #[error("io error: {0}")]
    Io(#[from] io::Error),
    #[error("parsing error: {0}")]
    Parse(#[from] ParseIntError),
    #[error("range parsing error: {0}")]
    RangeParse(String),
    #[error("map network parse error: {0}")]
    MapNetworkParse(String),
    #[error("invalid input source: {0}")]
    InvalidInputSource(String),
}
