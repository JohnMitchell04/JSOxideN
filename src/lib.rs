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

// TODO: Maybe extend this to a struct with the enum but also info about the error.
// i.e. line number and character that the error was encountered on.
// Maybe also make them more specific, i.e. missing a bracket etc.
#[derive(Debug)]
pub enum ParseError {
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
    RecursionDepthReached
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::UnexpectedCharacter => write!(f, "Unexpected character encountered"),
            ParseError::UnexpectedEOF => write!(f, "Unexpected EOF encountered"),
            ParseError::InvalidHex => write!(f, "Invalid hex number encountered"),
            ParseError::InvalidUnicode => write!(f, "Invalid unicode literal encountered"),
            ParseError::InvalidInteger => write!(f, "Could not parse integer"),
            ParseError::InvalidFraction => write!(f, "Could not parse fractional part"),
            ParseError::InvalidExponent => write!(f, "Could not parse exponent"),
            ParseError::NumberOutOfBounds => write!(f, "Number cannot be stored in an double"),
            ParseError::InvalidValue => write!(f, "Invalid bollean/null literal"),
            ParseError::ExpectedEOF => write!(f, "Expected EOF but character encountered"),
            ParseError::RecursionDepthReached => write!(f, "The maximum recursion depth was reached"),
        }
    }
}

impl Error for ParseError {}

macro_rules! unexpected_boilerplate {
    ($v:ident, $pat:pat, $e:expr) => {
        match $v {
            Some(c) if matches!(c, $pat) => $e,
            Some(_) => return Err(ParseError::UnexpectedCharacter),
            None => return Err(ParseError::UnexpectedEOF),
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
    return parser.parse().map_err(|e| Box::new(e) as Box<dyn Error>)
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
            _ => Err(ParseError::ExpectedEOF)
        }
    }

    /// Value rule.
    fn value(&mut self) -> Result<Value, ParseError> {
        self.recurse_depth += 1;
        if self.recurse_depth > 1000 {
            return Err(ParseError::RecursionDepthReached);
        }

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
                            None => return Err(ParseError::UnexpectedEOF),
                        }
                    }

                    if value == "true" {
                        Ok(Value::Bool(true))
                    } else {
                        Err(ParseError::InvalidValue)
                    }
                } else {
                    let mut value = String::from("");
                    for _ in 0..5 {
                        match self.consume_char() {
                            Some(c) => value.push(c),
                            None => return Err(ParseError::UnexpectedEOF),
                        }
                    }

                    if value == "false" {
                        Ok(Value::Bool(false))
                    } else {
                        Err(ParseError::InvalidValue)
                    }
                }
            },
            Some('n') => {
                let mut value = String::from("");
                    for _ in 0..4 {
                        match self.consume_char() {
                            Some(c) => value.push(c),
                            None => return Err(ParseError::UnexpectedEOF),
                        }
                    }

                    if value == "null" {
                        Ok(Value::Null)
                    } else {
                        Err(ParseError::InvalidValue)
                    }
            },
            Some(_) => self.number(),
            None => Err(ParseError::UnexpectedEOF),
        }
    }

    /// Object rule.
    fn object(&mut self) -> Result<Value, ParseError> {
        let mut char = self.consume_char();
        unexpected_boilerplate!(char, '{', {});

        self.eat_whitespace();
        char = self.peek_char();
        let value;
        match char {
            Some('}') => return Ok(Value::Object(BTreeMap::new())),
            Some(_) => value = self.members()?,
            None => return Err(ParseError::UnexpectedEOF),
        }

        char = self.consume_char();
        unexpected_boilerplate!(char, '}', Ok(value))
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
        unexpected_boilerplate!(char, ':', {});

        let value = self.element()?;
        Ok((key, value))
    }

    /// Array rule.
    fn array(&mut self) -> Result<Value, ParseError> {
        let mut char = self.consume_char();
        unexpected_boilerplate!(char, '[', {});

        self.eat_whitespace();
        char = self.peek_char();
        let value;
        match char {
            Some(']') => return Ok(Value::Array(Vec::new())),
            Some(_) => value = self.elements()?,
            None => return Err(ParseError::UnexpectedEOF),
        }

        char = self.consume_char();
        unexpected_boilerplate!(char, ']', Ok(value))
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
        unexpected_boilerplate!(char, '"', {});

        let value = self.characters()?;

        char = self.consume_char();
        unexpected_boilerplate!(char, '"', Ok(value))
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
                let char = self.escape()?;
                Ok(Some(char))
            },
            Some('\u{0020}'..='\u{10FFFF}') => {
                _ = self.consume_char();
                Ok(char)
            },
            Some(_) => Err(ParseError::UnexpectedCharacter),
            None => Err(ParseError::UnexpectedEOF),
        }
    }

    /// Escape rule.
    fn escape(&mut self) -> Result<char, ParseError> {
        let char = self.consume_char();
        match char {
            Some('"') => Ok('"'),
            Some('\\') => Ok('\\'),
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

                match u32::from_str_radix(&hex_string, 16) {
                    Ok(representation) => match char::from_u32(representation) {
                        Some(c) => Ok(c),
                        None => Err(ParseError::InvalidUnicode)
                    }, 
                    Err(_) => Err(ParseError::InvalidHex),
                }
            },
            Some(_) => Err(ParseError::UnexpectedCharacter),
            None => Err(ParseError::UnexpectedEOF),
        }
    }

    /// Hex rule.
    fn hex(&mut self) -> Result<char, ParseError> {
        let char = self.consume_char();
        match char {
            Some('A'..='F' | 'a'..='f') => Ok(char.unwrap()),
            Some(_) => self.digit(),
            None => Err(ParseError::UnexpectedEOF),
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
        match self.peek_char() {
            Some('-') => {
                _ = self.consume_char();
                let char = self.peek_char();
                match char {
                    Some('0'..='9') => {
                        match self.digits()?.parse::<i64>() {
                            Ok(i) => Ok(-i),
                            Err(_) => Err(ParseError::InvalidInteger)
                        }
        
                    },
                    Some(_) => Err(ParseError::UnexpectedCharacter),
                    None => Err(ParseError::UnexpectedEOF)
                }
            },
            Some('0'..='9') => {
                match self.digits()?.parse::<i64>() {
                    Ok(i) => Ok(i),
                    Err(_) => Err(ParseError::InvalidInteger)
                }

            },
            Some(_) => Err(ParseError::UnexpectedCharacter),
            None => Err(ParseError::UnexpectedEOF),
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

        match output.chars().nth(0) {
            Some('0') if output.len() > 1 => return Err(ParseError::InvalidInteger),
            _ => {}
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
            None => Err(ParseError::UnexpectedEOF),
        }
    }

    /// Onenine rule.
    fn onenine(&mut self) -> Result<char, ParseError> {
        let char = self.consume_char();
        unexpected_boilerplate!(char, '1'..='9', Ok(char.unwrap()))
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
                Err(_) => return Err(ParseError::InvalidFraction),
            }
        }

        Ok(None)
    }

    /// Exponent rule.
    fn exponent(&mut self) -> Result<Option<f64>, ParseError> {
        match self.peek_char() {
            Some('E' | 'e') => {
                match self.digits()?.parse::<i32>() {
                    Ok(i) => {
                        Ok(Some(10f64.powi(i)))
                    },
                    Err(_) => Err(ParseError::InvalidExponent),
                }
            },
            _ => Ok(None),
        }
    }
}