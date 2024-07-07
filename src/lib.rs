use core::fmt;
use std::{collections::BTreeMap, error::Error};

/// Number type for floats and integers.
#[derive(Debug)]
pub enum Number {
    Int(i64),
    Float(f64),
}

impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Number::Int(i) => write!(f, "{}", i),
            Number::Float(fl) => write!(f, "{}", fl),
        }
    }
}

/// All possible JSON value types.
#[derive(Debug)]
pub enum Value {
    Null,
    Bool(bool),
    Number(Number),
    String(String),
    Array(Vec<Box<Value>>),
    Object(BTreeMap<String, Box<Value>>)
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Null => write!(f, "null"),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Number(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::Array(a) => {
                let elements = a.iter().map(|v| format!("{}", v)).collect::<Vec<_>>().join(", ");
                write!(f, "[{}]", elements)
            },
            Value::Object(o) => {
                let entries = o.iter().map(|(k, v)| format!("\"{}\": {}", k, v)).collect::<Vec<_>>().join(", ");
                write!(f, "{{{}}}", entries)
            },
        }
    }
}

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
            ParseErrorType::UnexpectedCharacter => write!(f, "Unexpected character encountered."),
            ParseErrorType::UnexpectedEOF => write!(f, "Unexpected EOF encountered."),
            ParseErrorType::InvalidHex => write!(f, "Invalid hex number encountered."),
            ParseErrorType::InvalidUnicode => write!(f, "Invalid unicode literal encountered."),
            ParseErrorType::InvalidInteger => write!(f, "Could not parse integer."),
            ParseErrorType::InvalidFraction => write!(f, "Could not parse fractional part."),
            ParseErrorType::InvalidExponent => write!(f, "Could not parse exponent."),
            ParseErrorType::NumberOutOfBounds => write!(f, "Number cannot be stored in an double."),
            ParseErrorType::InvalidValue => write!(f, "Invalid bollean/null literal."),
            ParseErrorType::ExpectedEOF => write!(f, "Expected EOF but character encountered."),
            ParseErrorType::RecursionDepthReached => write!(f, "The maximum recursion depth was reached."),
            ParseErrorType::IOError => write!(f, "Couldn't read file"),
            ParseErrorType::InvalidSurrogatePair => write!(f, "The surrogate pair is invalid."),
        }
    }
}

/// TODO: Add position in line
#[derive(Debug)]
pub struct ParseError {
    error_type: ParseErrorType,
    line: usize,
    string: Option<String>,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.string {
            Some(c) => write!(f, "Error: {} On line: {}, around: {}", self.error_type, self.line, c),
            None => write!(f, "Error: {} On line: {}, character EOF", self.error_type, self.line),
        }
    }
}

impl Error for ParseError {}

impl From<(ParseErrorType, usize, Option<char>)> for ParseError {
    fn from((error_type, line, character): (ParseErrorType, usize, Option<char>)) -> Self {
        let string;
        match character {
            Some(c) => string = Some(String::from(c)),
            None => string = None,
        }
        ParseError {
            error_type,
            line,
            string
        }
    }
}

impl From<(ParseErrorType, usize, String)> for ParseError {
    fn from((error_type, line, string): (ParseErrorType, usize, String)) -> Self {
        ParseError {
            error_type,
            line,
            string: Some(string)
        }
    }
}

macro_rules! unexpected_boilerplate {
    ($v:ident, $pat:pat, $e:expr, $l:expr) => {
        match $v {
            Some(c) if matches!(c, $pat) => $e,
            Some(k) => return Err((ParseErrorType::UnexpectedCharacter, $l, Some(k)).into()),
            None => return Err((ParseErrorType::UnexpectedEOF, $l, None).into()),
        }
    };
}

/// Parse a JSON string and return a Value.
pub fn from_str(input: &str) -> Result<Value, ParseError> {
    let mut parser = Parser::new(input);
    return parser.parse()
}

/// Parse JSON from a file and return a Value.
pub fn from_file(filepath: &str) -> Result<Value, Box<dyn Error>> {
    let input = std::fs::read_to_string(filepath)?;
    let mut parser = Parser::new(&input);
    return Ok(parser.parse()?)
}

/// Parser struct.
struct Parser<'a> {
    input: &'a str,
    pos: usize,
    line: usize,
    recurse_depth: i16,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Parser {
        Parser {
            input,
            pos: 0,
            line: 0,
            recurse_depth: 0,
        }
    }

    /// Peek the next character without consuming.
    fn peek_char(&self) -> Option<char> {
        let mut chars = self.input[self.pos..].chars();
        chars.next()
    }

    /// Get the next character in the input string.
    fn consume_char(&mut self) -> Option<char> {
        let mut chars = self.input[self.pos..].chars();
        let next_char = chars.next()?;
        let next_pos = next_char.len_utf8();
        self.pos += next_pos;
        if next_char == '\n' { self.line += 1 }
        Some(next_char)
    }

    /// Eat whitespace characters.
    fn eat_whitespace(&mut self) {
        while let Some(c) = self.peek_char() {
            match c {
                '\u{0020}' | '\u{000A}' | '\u{000D}' | '\u{0009}' => _ = self.consume_char(),
                _ => break,
            }
        }
    }

    /// Parse the input string.
    fn parse(&mut self) -> Result<Value, ParseError> {
        let value = self.element()?;
        match self.peek_char() {
            None => Ok(value),
            c => Err((ParseErrorType::ExpectedEOF, self.line, c).into())
        }
    }

    /// Value rule.
    fn value(&mut self) -> Result<Value, ParseError> {
        let char = self.peek_char();
        match char {
            Some('{') => self.object(),
            Some('[') => self.array(),
            Some('"') => {
                let value = self.string()?;
                Ok(Value::String(value))
            },
            Some('t' | 'f') => {
                if let Some('t') = char {
                    let mut value = String::from("");
                    for _ in 0..4 {
                        match self.consume_char() {
                            Some(c) => value.push(c),
                            None => return Err((ParseErrorType::UnexpectedEOF, self.line, None).into()),
                        }
                    }

                    if value == "true" {
                        Ok(Value::Bool(true))
                    } else {
                        Err((ParseErrorType::InvalidValue, self.line, value).into())
                    }
                } else {
                    let mut value = String::from("");
                    for _ in 0..5 {
                        match self.consume_char() {
                            Some(c) => value.push(c),
                            None => return Err((ParseErrorType::UnexpectedEOF, self.line, None).into()),
                        }
                    }

                    if value == "false" {
                        Ok(Value::Bool(false))
                    } else {
                        Err((ParseErrorType::InvalidValue, self.line, value).into())
                    }
                }
            },
            Some('n') => {
                let mut value = String::from("");
                    for _ in 0..4 {
                        match self.consume_char() {
                            Some(c) => value.push(c),
                            None => return Err((ParseErrorType::UnexpectedEOF, self.line, None).into()),
                        }
                    }

                    if value == "null" {
                        Ok(Value::Null)
                    } else {
                        Err((ParseErrorType::InvalidValue, self.line, value).into())
                    }
            },
            Some(_) => self.number(),
            None => Err((ParseErrorType::UnexpectedEOF, self.line, None).into()),
        }
    }

    /// Object rule.
    fn object(&mut self) -> Result<Value, ParseError> {
        self.recurse_depth += 1;
        if self.recurse_depth > 100 {
            return Err((ParseErrorType::RecursionDepthReached, self.line, None).into());
        }

        let mut char = self.consume_char();
        unexpected_boilerplate!(char, '{', {}, self.line);

        self.eat_whitespace();
        char = self.peek_char();
        let value;
        match char {
            Some('}') => {
                _ = self.consume_char();
                return Ok(Value::Object(BTreeMap::new()))
            },
            Some(_) => value = self.members()?,
            None => return Err((ParseErrorType::UnexpectedEOF, self.line, None).into()),
        }

        self.recurse_depth -= 1;

        char = self.consume_char();
        unexpected_boilerplate!(char, '}', Ok(value), self.line)
    }

    /// Members rule.
    fn members(&mut self) -> Result<Value, ParseError> {
        let mut char;
        let mut map =  BTreeMap::new();
        loop {
            let (key, value) = self.member()?;

            map.insert(key, Box::new(value));

            char = self.peek_char();
            if char != Some(',') {
                break;
            }
            _ = self.consume_char();
        }

        Ok(Value::Object(map))
    }

    /// Member rule.
    fn member(&mut self) -> Result<(String, Value), ParseError> {
        self.eat_whitespace();
        let key = self.string()?;
        self.eat_whitespace();

        let char = self.consume_char();
        unexpected_boilerplate!(char, ':', {}, self.line);

        let value = self.element()?;
        Ok((key, value))
    }

    /// Array rule.
    fn array(&mut self) -> Result<Value, ParseError> {
        self.recurse_depth += 1;
        if self.recurse_depth > 100 {
            return Err((ParseErrorType::RecursionDepthReached, self.line, None).into());
        }

        let mut char = self.consume_char();
        unexpected_boilerplate!(char, '[', {}, self.line);

        self.eat_whitespace();
        char = self.peek_char();
        let value;
        match char {
            Some(']') => {
                _ = self.consume_char();
                return Ok(Value::Array(Vec::new()))
            },
            Some(_) => value = self.elements()?,
            None => return Err((ParseErrorType::UnexpectedEOF, self.line, None).into()),
        }

        self.recurse_depth -= 1;

        char = self.consume_char();
        unexpected_boilerplate!(char, ']', Ok(value), self.line)
    }

    /// Elements rule.
    fn elements(&mut self) -> Result<Value, ParseError> {
        let mut char;
        let mut vec = Vec::new();
        loop {
            let value = self.element()?;

            vec.push(Box::new(value));

            char = self.peek_char();
            if char != Some(',') {
                break;
            }
            _ = self.consume_char();
        }

        Ok(Value::Array(vec))
    }

    /// Element rule.
    fn element(&mut self) -> Result<Value, ParseError> {
        self.eat_whitespace();
        let value = self.value()?;
        self.eat_whitespace();
        return Ok(value);
    }

    /// String rule.
    fn string(&mut self) -> Result<String, ParseError> {
        let mut char = self.consume_char();
        unexpected_boilerplate!(char, '"', {}, self.line);

        let value = self.characters()?;

        char = self.consume_char();
        unexpected_boilerplate!(char, '"', Ok(value), self.line)
    }

    /// Characters rule.
    fn characters(&mut self) -> Result<String, ParseError> {
        let mut string = String::from("");
        loop {
            let char = self.character()?;
            match char {
                Some(char) => string.push(char),
                None => break,
            }
        }

        Ok(string)
    }

    /// Character rule.
    fn character(&mut self) -> Result<Option<char>, ParseError> {
        let char = self.peek_char();
        match char {
            Some('"') => Ok(None),
            Some('\\') => {
                _ = self.consume_char();
                let char = self.escape()?;
                Ok(Some(char))
            },
            Some('\u{0020}'..='\u{10FFFF}') => {
                _ = self.consume_char();
                Ok(char)
            },
            Some(c) => Err((ParseErrorType::UnexpectedCharacter, self.line, Some(c)).into()),
            None => Err((ParseErrorType::UnexpectedEOF, self.line, None).into()),
        }
    }

    /// Escape rule.
    fn escape(&mut self) -> Result<char, ParseError> {
        let char = self.consume_char();
        match char {
            Some('"') => Ok('"'),
            Some('\\') => Ok('\\'),
            Some('/') => Ok('/'),
            Some('b') => Ok('\u{0008}'),
            Some('f') => Ok('\u{000C}'),
            Some('n') => Ok('\n'),
            Some('r') => Ok('\r'),
            Some('t') => Ok('\t'),
            Some('u') => {
                let mut hex_string = String::new();
                for _ in 0..4 {
                    hex_string.push(self.hex()?);
                }

                let representation;
                if let Ok(i) = u32::from_str_radix(&hex_string, 16) {
                    representation = i;
                } else {
                    return Err((ParseErrorType::InvalidHex, self.line, hex_string).into())
                }

                if let Some(c) = char::from_u32(representation) { return Ok(c) }

                if !(representation >= 0xD800 && representation <= 0xDBFF) { return Err((ParseErrorType::InvalidUnicode, self.line, hex_string).into()) }

                let mut char = self.consume_char();
                unexpected_boilerplate!(char, '\\', {}, self.line);
                
                char = self.consume_char();
                unexpected_boilerplate!(char, 'u', {}, self.line);


                let mut hex_string2 = String::new();
                for _ in 0..4 {
                    hex_string2.push(self.hex()?);
                }

                match u32::from_str_radix(&hex_string2, 16) {
                    Ok(representation2) => match char::from_u32(representation2) {
                        Some(c) => Err((ParseErrorType::InvalidSurrogatePair, self.line, Some(c)).into()),
                        None => {
                            if !(representation2 >= 0xDC00 && representation2 <= 0xDFFF) { return Err((ParseErrorType::InvalidUnicode, self.line, hex_string).into()) }

                            let upper = (representation - 0xD800) << 6;
                            let lower = representation2 - 0xDC00;

                            let char = upper + lower + 0x10000;
                            match char::from_u32(char) {
                                Some(c) => Ok(c),
                                None => Err((ParseErrorType::InvalidSurrogatePair, self.line, None).into())
                            }
                        }
                    },
                    Err(_) => Err((ParseErrorType::InvalidSurrogatePair, self.line, None).into())
                }
            },
            Some(c) => Err((ParseErrorType::UnexpectedCharacter, self.line, Some(c)).into()),
            None => Err((ParseErrorType::UnexpectedEOF, self.line, None).into()),
        }
    }

    /// Hex rule.
    fn hex(&mut self) -> Result<char, ParseError> {
        let char = self.peek_char();
        match char {
            Some('A'..='F' | 'a'..='f') => {
                _ = self.consume_char();
                Ok(char.unwrap())
            },
            Some(_) => self.digit(),
            None => Err((ParseErrorType::UnexpectedEOF, self.line, None).into()),
        }
    }

    /// Number rule.
    fn number(&mut self) -> Result<Value, ParseError> {
        let int = self.integer()?;
        let frac = self.fraction()?;
        let exp = self.exponent()?;

        match frac {
            Some(i) => {
                match exp {
                    Some(j) => {
                        Ok(Value::Number(Number::Float((int as f64 + i) * j)))
                    },
                    None => Ok(Value::Number(Number::Float(int as f64 + i))),
                }
            },
            None => {
                match exp {
                    Some(i) => {
                        if i > 0f64 {
                            match int.checked_mul(i as i64) {
                                Some(j) => Ok(Value::Number(Number::Int(j))),
                                None => Ok(Value::Number(Number::Float(int as f64 * i))),
                            }
                        } else {
                            Ok(Value::Number(Number::Float(int as f64 * i)))
                        }
                    },
                    None => Ok(Value::Number(Number::Int(int))),
                }
            },
        }
    }

    /// Integer rule.
    fn integer(&mut self) -> Result<i64, ParseError> {
        let mut neg = false;
        match self.peek_char() {
            Some('-') => {
                neg = true;
                _ = self.consume_char();
            },
            _ => {} 
        }

        match self.peek_char() {
            Some('0'..='9') => {
                let digits = self.digits()?;

                if digits.chars().nth(0) == Some('0') && digits.len() > 1 {
                    return Err((ParseErrorType::InvalidInteger, self.line, digits).into())
                }

                match digits.parse::<i64>() {
                    Ok(mut i) => {
                        if neg { i = -i; }
                        Ok(i)
                    },
                    Err(_) => Err((ParseErrorType::InvalidInteger, self.line, digits).into())
                }
            }
            Some(c) => Err((ParseErrorType::UnexpectedCharacter, self.line, Some(c)).into()),
            None => Err((ParseErrorType::UnexpectedEOF, self.line, None).into()),
        }
    }

    /// Digits rule.
    fn digits(&mut self) -> Result<String, ParseError> {
        let mut output = String::new();
        loop {
            if !matches!(self.peek_char(), Some('0'..='9')) {
                break;
            }

            output.push(self.digit()?);
        }

        Ok(output)
    }

    /// Digit rule.
    fn digit(&mut self) -> Result<char, ParseError> {
        match self.peek_char() {
            Some('0') => {
                _ = self.consume_char();
                Ok('0')
            },
            Some(_) => self.onenine(),
            None => Err((ParseErrorType::UnexpectedEOF, self.line, None).into()),
        }
    }

    /// Onenine rule.
    fn onenine(&mut self) -> Result<char, ParseError> {
        let char = self.consume_char();
        unexpected_boilerplate!(char, '1'..='9', Ok(char.unwrap()), self.line)
    }

    /// Fraction rule.
    fn fraction(&mut self) ->  Result<Option<f64>, ParseError>{
        if self.peek_char() == Some('.') {
            _ = self.consume_char();
            let fraction = self.digits()?;
            match fraction.parse::<f64>() {
                Ok(i) => {
                    let divisor = 10f64.powi(fraction.len() as i32);
                    return Ok(Some(i / divisor));
                },
                Err(_) => return Err((ParseErrorType::InvalidFraction, self.line, fraction).into()),
            }
        }

        Ok(None)
    }

    /// Exponent rule.
    fn exponent(&mut self) -> Result<Option<f64>, ParseError> {
        match self.peek_char() {
            Some('E' | 'e') => {
                _ = self.consume_char();

                let mut pos = true;
                match self.peek_char() {
                    Some('+') => _ = self.consume_char(),
                    Some('-') => {
                        _ = self.consume_char();
                        pos = false
                    },
                    _ => {}
                }

                let digits = self.digits()?;
                match digits.parse::<i32>() {
                    Ok(i) => {
                        if !pos {
                            Ok(Some(-10f64.powi(i)))
                        } else {
                            Ok(Some(10f64.powi(i)))
                        }
                    },
                    Err(_) => Err((ParseErrorType::InvalidExponent, self.line, digits).into()),
                }
            },
            _ => Ok(None),
        }
    }
}