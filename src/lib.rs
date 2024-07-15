use core::fmt;
use std::{collections::BTreeMap, error::Error, iter::Peekable, ops::Index, str::Chars};

/// Number type for floats and integers.
#[derive(Debug, Clone, Copy)]
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

#[derive(Debug)]
pub enum ValueError {
    IncorrectType,
    InvalidKey
}

/// All possible JSON value types.
#[derive(Debug, Clone)]
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

impl Index<&str> for Value {
    type Output = Value;

    fn index(&self, key: &str) -> &Self::Output {
        self.get(key).expect("Key is not present or this is not an object")
    }
}

impl Value {
    pub fn get(&self, key: &str) -> Result<&Value, ValueError> {
        match self {
            Value::Object(map) => {
                match map.get(key) {
                    Some(value) => Ok(&**value),
                    None => Err(ValueError::InvalidKey)
                }
            },
            _ => Err(ValueError::IncorrectType)
        }
    }

    pub fn as_bool(&self) -> Result<&bool, ValueError> {
        match self {
            Value::Bool(ref b) => Ok(b),
            _ => Err(ValueError::IncorrectType),
        }
    }

    pub fn as_number(&self) -> Result<&Number, ValueError> {
        match self {
            Value::Number(ref n) => Ok(n),
            _ => Err(ValueError::IncorrectType),
        }
    }

    pub fn as_string(&self) -> Result<&String, ValueError> {
        match self {
            Value::String(ref s) => Ok(s),
            _ => Err(ValueError::IncorrectType),
        }
    }

    pub fn as_array(&self) -> Result<&Vec<Box<Value>>, ValueError> {
        match self {
            Value::Array(ref a) => Ok(a),
            _ => Err(ValueError::IncorrectType),
        }
    }

    pub fn as_map(&self) -> Result<&BTreeMap<String, Box<Value>>, ValueError> {
        match self {
            Value::Object(ref o) => Ok(o),
            _ => Err(ValueError::IncorrectType),
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
            None => write!(f, "Error: {}, Column: {} On line: {}", self.col, self.error_type, self.line),
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
    input: Peekable<Chars<'a>>,
    col: usize,
    line: usize,
    recurse_depth: i16,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Parser {
        let iter = input.chars().peekable();

        Parser {
            input: iter,
            col: 1,
            line: 1,
            recurse_depth: 0,
        }
    }

    /// Get the next character in the input string.
    fn consume(&mut self) -> Option<char> {
        let char = self.input.next();
        self.col += 1;
        if let Some('\n') = char {
            self.line += 1;
            self.col = 1;
        };

        char
    }

    /// Eat whitespace characters.
    fn eat_whitespace(&mut self) {
        while let Some(c) = self.input.peek() {
            match c {
                '\u{0020}' | '\u{000A}' | '\u{000D}' | '\u{0009}' => _ = self.consume(),
                _ => break,
            }
        }
    }

    /// Parse the input string.
    fn parse(&mut self) -> Result<Value, ParseError> {
        let value = self.element()?;
        match self.input.peek() {
            None => Ok(value),
            Some(char) => Err((ParseErrorType::ExpectedEOF, self.col, self.line, *char).into())
        }
    }

    /// Value rule.
    fn value(&mut self) -> Result<Value, ParseError> {
        let char = self.input.peek();
        match char {
            Some('{') => self.object(),
            Some('[') => self.array(),
            Some('"') => Ok(Value::String(self.string()?)),
            Some('t' | 'f') => {
                if let Some('t') = char {
                    let value = self.input.by_ref().take(4).collect::<String>();

                    if value.len() < 4 { return Err((ParseErrorType::UnexpectedEOF, self.col, self.line).into()) }
                    if value != "true" { return Err((ParseErrorType::InvalidValue, self.col, self.line, value).into()) }

                    Ok(Value::Bool(true))
                } else {
                    let value = self.input.by_ref().take(5).collect::<String>();

                    if value.len() < 5 { return Err((ParseErrorType::UnexpectedEOF, self.col, self.line).into()) }
                    if value != "false" { return Err((ParseErrorType::InvalidValue, self.col, self.line, value).into()) }

                    Ok(Value::Bool(false))
                }
            },
            Some('n') => {
                let value = self.input.by_ref().take(4).collect::<String>();
                
                if value.len() < 4 { return Err((ParseErrorType::UnexpectedEOF, self.col, self.line).into()) }
                if value != "null" { return Err((ParseErrorType::InvalidValue, self.col, self.line, value).into()) }

                Ok(Value::Null)
            },
            Some(_) => self.number(),
            None => Err((ParseErrorType::UnexpectedEOF, self.col, self.line).into()),
        }
    }

    /// Object rule.
    fn object(&mut self) -> Result<Value, ParseError> {
        self.recurse_depth += 1;
        if self.recurse_depth > 100 { return Err((ParseErrorType::RecursionDepthReached, self.col, self.line).into()) }

        let char = self.consume();
        unexpected_boilerplate!(char, '{', {}, self.col, self.line);

        self.eat_whitespace();
        let char = self.input.peek();
        let value;
        match char {
            Some('}') => {
                _ = self.consume();
                return Ok(Value::Object(BTreeMap::new()))
            },
            Some(_) => value = self.members()?,
            None => return Err((ParseErrorType::UnexpectedEOF, self.col, self.line).into()),
        }

        self.recurse_depth -= 1;

        let char = self.consume();
        unexpected_boilerplate!(char, '}', Ok(value), self.col, self.line)
    }

    /// Members rule.
    fn members(&mut self) -> Result<Value, ParseError> {
        let mut map =  BTreeMap::new();
        loop {
            let (key, value) = self.member()?;

            map.insert(key, Box::new(value));

            let char = self.input.peek();
            if char != Some(&',') {
                break;
            }
            _ = self.consume();
        }

        Ok(Value::Object(map))
    }

    /// Member rule.
    fn member(&mut self) -> Result<(String, Value), ParseError> {
        self.eat_whitespace();
        let key = self.string()?;
        self.eat_whitespace();

        let char = self.consume();
        unexpected_boilerplate!(char, ':', {}, self.col, self.line);

        let value = self.element()?;
        Ok((key, value))
    }

    /// Array rule.
    fn array(&mut self) -> Result<Value, ParseError> {
        self.recurse_depth += 1;
        if self.recurse_depth > 100 { return Err((ParseErrorType::RecursionDepthReached, self.col, self.line).into()) }

        let char = self.consume();
        unexpected_boilerplate!(char, '[', {}, self.col, self.line);

        self.eat_whitespace();
        let char = self.input.peek();
        let value;
        match char {
            Some(']') => {
                _ = self.consume();
                return Ok(Value::Array(Vec::new()))
            },
            Some(_) => value = self.elements()?,
            None => return Err((ParseErrorType::UnexpectedEOF, self.col, self.line).into()),
        }

        self.recurse_depth -= 1;

        let char = self.consume();
        unexpected_boilerplate!(char, ']', Ok(value), self.col, self.line)
    }

    /// Elements rule.
    fn elements(&mut self) -> Result<Value, ParseError> {
        let mut vec = Vec::new();
        loop {
            let value = self.element()?;

            vec.push(Box::new(value));

            let char = self.input.peek();
            if char != Some(&',') {
                break;
            }
            _ = self.consume();
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
        let mut char = self.consume();
        unexpected_boilerplate!(char, '"', {}, self.col, self.line);

        let value = self.characters()?;

        char = self.consume();
        unexpected_boilerplate!(char, '"', Ok(value), self.col, self.line)
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
        let char = self.input.peek();
        match char {
            Some('"') => Ok(None),
            Some('\\') => {
                _ = self.consume();
                let char = self.escape()?;
                Ok(Some(char))
            },
            Some('\u{0020}'..='\u{10FFFF}') => Ok(self.consume()),
            Some(char) => Err((ParseErrorType::UnexpectedCharacter, self.col, self.line, *char).into()),
            None => Err((ParseErrorType::UnexpectedEOF, self.col, self.line).into()),
        }
    }

    /// Escape rule.
    fn escape(&mut self) -> Result<char, ParseError> {
        let char = self.consume();
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
                let hex_string = self.input.by_ref().take(4).collect::<String>();

                let representation = u32::from_str_radix(&hex_string, 16)
                    .map_err(|_| -> ParseError { (ParseErrorType::InvalidHex, self.col, self.line, hex_string.clone()).into() })?;

                if let Some(c) = char::from_u32(representation) { return Ok(c) }
                if !(representation >= 0xD800 && representation <= 0xDBFF) { return Err((ParseErrorType::InvalidUnicode, self.col, self.line, hex_string).into()) }

                let mut char = self.consume();
                unexpected_boilerplate!(char, '\\', {}, self.col, self.line);
                
                char = self.consume();
                unexpected_boilerplate!(char, 'u', {}, self.col, self.line);


                let hex_string2 = self.input.by_ref().take(4).collect::<String>();

                let representation2 = u32::from_str_radix(&hex_string2, 16)
                    .map_err(|_| -> ParseError { (ParseErrorType::InvalidHex, self.col, self.line, hex_string2).into() })?;

                match char::from_u32(representation2) {
                    Some(char) => Err((ParseErrorType::InvalidSurrogatePair, self.col, self.line, char).into()),
                    None => {
                        if !(representation2 >= 0xDC00 && representation2 <= 0xDFFF) { return Err((ParseErrorType::InvalidUnicode, self.col, self.line, hex_string).into()) }

                        let upper = (representation - 0xD800) << 10;
                        let lower = representation2 - 0xDC00;

                        let char = upper + lower + 0x10000;
                        match char::from_u32(char) {
                            Some(c) => Ok(c),
                            None => Err((ParseErrorType::InvalidSurrogatePair, self.col, self.line).into())
                        }
                    }
                }
            },
            Some(char) => Err((ParseErrorType::UnexpectedCharacter, self.col, self.line, char).into()),
            None => Err((ParseErrorType::UnexpectedEOF, self.col, self.line).into()),
        }
    }

    /// Hex rule.
    fn hex(&mut self) -> Result<char, ParseError> {
        let char = self.input.peek();
        match char {
            Some('A'..='F' | 'a'..='f') => Ok(self.consume().unwrap()),
            Some(_) => self.digit(),
            None => Err((ParseErrorType::UnexpectedEOF, self.col, self.line).into()),
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
                    Some(j) => Ok(Value::Number(Number::Float((int as f64 + i) * j))),
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
        match self.input.peek() {
            Some('-') => {
                neg = true;
                _ = self.consume();
            },
            _ => {} 
        }

        match self.input.peek() {
            Some('0'..='9') => {
                let digits = self.digits()?;

                if digits.chars().nth(0) == Some('0') && digits.len() > 1 { return Err((ParseErrorType::InvalidInteger, self.col ,self.line, digits).into()) }

                match digits.parse::<i64>() {
                    Ok(mut i) => {
                        if neg { i = -i; }
                        Ok(i)
                    },
                    Err(_) => Err((ParseErrorType::InvalidInteger, self.col, self.line, digits).into())
                }
            }
            Some(char) => Err((ParseErrorType::UnexpectedCharacter, self.col, self.line, *char).into()),
            None => Err((ParseErrorType::UnexpectedEOF, self.col, self.line).into()),
        }
    }

    /// Digits rule.
    fn digits(&mut self) -> Result<String, ParseError> {
        let mut output = String::new();
        loop {
            if !matches!(self.input.peek(), Some('0'..='9')) { break }

            output.push(self.digit()?);
        }

        Ok(output)
    }

    /// Digit rule.
    fn digit(&mut self) -> Result<char, ParseError> {
        match self.input.peek() {
            Some('0') => {
                _ = self.consume();
                Ok('0')
            },
            Some(_) => self.onenine(),
            None => Err((ParseErrorType::UnexpectedEOF, self.col, self.line).into()),
        }
    }

    /// Onenine rule.
    fn onenine(&mut self) -> Result<char, ParseError> {
        let char = self.consume();
        unexpected_boilerplate!(char, '1'..='9', Ok(char.unwrap()), self.col, self.line)
    }

    /// Fraction rule.
    fn fraction(&mut self) ->  Result<Option<f64>, ParseError>{
        if self.input.peek() == Some(&'.') {
            _ = self.consume();
            let fraction = self.digits()?;
            match fraction.parse::<f64>() {
                Ok(i) => {
                    let divisor = 10f64.powi(fraction.len() as i32);
                    return Ok(Some(i / divisor));
                },
                Err(_) => return Err((ParseErrorType::InvalidFraction, self.col, self.line, fraction).into()),
            }
        }

        Ok(None)
    }

    /// Exponent rule.
    fn exponent(&mut self) -> Result<Option<f64>, ParseError> {
        match self.input.peek() {
            Some('E' | 'e') => {
                _ = self.consume();

                let mut pos = true;
                match self.input.peek() {
                    Some('+') => _ = self.consume(),
                    Some('-') => {
                        _ = self.consume();
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
                    Err(_) => Err((ParseErrorType::InvalidExponent, self.col, self.line, digits).into()),
                }
            },
            _ => Ok(None),
        }
    }
}