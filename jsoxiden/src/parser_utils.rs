use std::{error::Error, fmt};

/// Error types that can occur when parsing a JSON string.
#[derive(Debug)]
pub enum ParseErrorType {
    UnexpectedCharacter,
    UnexpectedEOF,
    InvalidHex,
    InvalidUnicode,
    InvalidInteger,
    InvalidFraction,
    InvalidExponent,
    NumberOutOfBounds,
    InvalidValue,
    ExpectedEOF,
    RecursionDepthReached,
    IOError,
    InvalidSurrogatePair
}

impl fmt::Display for ParseErrorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseErrorType::UnexpectedCharacter => write!(f, "Unexpected character encountered"),
            ParseErrorType::UnexpectedEOF => write!(f, "Unexpected EOF encountered"),
            ParseErrorType::InvalidHex => write!(f, "Invalid hex number encountered"),
            ParseErrorType::InvalidUnicode => write!(f, "Invalid unicode literal encountered"),
            ParseErrorType::InvalidInteger => write!(f, "Could not parse integer"),
            ParseErrorType::InvalidFraction => write!(f, "Could not parse fractional part"),
            ParseErrorType::InvalidExponent => write!(f, "Could not parse exponent"),
            ParseErrorType::InvalidValue => write!(f, "Invalid bollean/null literal"),
            ParseErrorType::InvalidSurrogatePair => write!(f, "The surrogate pair is invalid"),
            ParseErrorType::NumberOutOfBounds => write!(f, "Number cannot be stored in an double"),
            ParseErrorType::ExpectedEOF => write!(f, "Expected EOF but character encountered"),
            ParseErrorType::RecursionDepthReached => write!(f, "The maximum recursion depth was reached"),
            ParseErrorType::IOError => write!(f, "Couldn't read file"),
        }
    }
}

/// An error that occurs when parsing a JSON string.
#[derive(Debug)]
pub struct ParseError {
    error_type: ParseErrorType,
    col: usize,
    line: usize,
    value: Option<String>,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.value {
            Some(string) => write!(f, "Error: {}, Column: {}, On line: {}, around: {}", self.error_type, self.col, self.line, string),
            None => write!(f, "Error: {}, Column: {} On line: {}", self.error_type, self.col, self.line),
        }
    }
}

impl Error for ParseError {}

impl From<(ParseErrorType, usize, usize, char)> for ParseError {
    fn from((error_type, col, line, character): (ParseErrorType, usize, usize, char)) -> Self {
        ParseError { error_type, col, line, value: Some(String::from(character)) }
    }
}

impl From<(ParseErrorType, usize, usize)> for ParseError {
    fn from((error_type, col, line): (ParseErrorType, usize, usize)) -> Self {
        ParseError { error_type, col, line, value: None }
    }
}

impl From<(ParseErrorType, usize, usize, String)> for ParseError {
    fn from((error_type, col, line, string): (ParseErrorType, usize, usize, String)) -> Self {
        ParseError { error_type, col, line, value: Some(string) }
    }
}

macro_rules! unexpected_boilerplate {
    ($v:ident, $pat:pat, $e:expr, $c:expr, $l:expr) => {
        match $v {
            Some(ch) if matches!(ch, $pat) => $e,
            Some(ch) => return Err((ParseErrorType::UnexpectedCharacter, $c, $l, ch).into()),
            None => return Err((ParseErrorType::UnexpectedEOF, $c, $l).into()),
        }
    };
}
pub(super) use unexpected_boilerplate;