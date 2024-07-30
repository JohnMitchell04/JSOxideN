//! JSON deserialiser for Rust. 
//! 
//! Parses JSON strings and files and provides a clean [`Value`] type to access the data.
//! Users may also derive the [`Deserialise`] trait for their structs to provide easier strongly typed access.
//! 
//! # Examples
//! 
//! Parsing and accessing via [`Value`]:
//! ```rust
//! use jsoxiden;
//!
//! let json = r#"
//!    {
//!         "name": "John Doe",
//!         "age": 43,
//!         "is_student": false,
//!         "address": {
//!             "street": "123 Fake St",
//!             "city": "Springfield",
//!             "postcode": "12345"
//!         }
//!     }
//! "#;
//! let value = jsoxiden::from_str(json).unwrap();
//! let name = value["name"].as_str().unwrap();
//! println!("{}", name);
//! ```
//! 
//! Parsing and accessing via [`Deserialise`]:
//! ```rust
//! use jsoxiden::Deserialise;
//! 
//! #[derive(Deserialise)]
//! struct Address {
//!     street: String,
//!     city: String,
//!     postcode: String,
//! }
//! 
//! #[derive(Deserialise)]
//! struct Person {
//!     name: String,
//!     age: i32,
//!     is_student: bool,
//!     address: Address,
//! }
//! 
//!
//! let json = r#"
//!    {
//!         "name": "John Doe",
//!         "age": 43,
//!         "is_student": false,
//!         "address": {
//!             "street": "123 Fake St",
//!             "city": "Springfield",
//!             "postcode": "12345"
//!         }
//!     }
//! "#;
//!
//! let person = Person::from_str(json).unwrap();
//! println!("{}", person.name);
//! ```
//! 
//! Using [`Serialise`] to print out a struct in JSON format:
//! ```rust
//! use jsoxiden::{Deserialise, Serialise};
//! 
//! #[derive(Deserialise, Serialise)]
//! struct Address {
//!     street: String,
//!     city: String,
//!     postcode: String,
//! }
//! 
//! #[derive(Deserialise, Serialise)]
//! struct Person {
//!     name: String,
//!     age: i32,
//!     is_student: bool,
//!     address: Address,
//! }
//! 
//!
//! let json = r#"
//!    {
//!         "name": "John Doe",
//!         "age": 43,
//!         "is_student": false,
//!         "address": {
//!             "street": "123 Fake St",
//!             "city": "Springfield",
//!             "postcode": "12345"
//!         }
//!     }
//! "#;
//!
//! let person = Person::from_str(json).unwrap();
//! println!("{}", person.to_json_string());
//! ```

// TODO: Allow the user to decide whether they want to use IndexMap or a normal BtreeMap

use std::{error::Error, iter::Peekable, str::Chars};

pub mod parser_utils;
pub mod value;
pub use macros::*;
pub use parser_utils::*;
pub use value::*;

use indexmap::IndexMap;

/// Parse a JSON string and return a [`Value`].
/// 
/// # Arguments:
/// * `input` - The JSON string.
/// 
/// # Returns:
/// * `Result<Value, ParseError>` - The parsed JSON value or a [`ParseError`] if the JSON is invalid.
/// 
/// **Example:**
/// 
/// ```rust
/// use jsoxiden;
///
/// let json = r#"
///    {
///         "name": "John Doe",
///         "age": 43,
///         "is_student": false,
///         "address": {
///             "street": "123 Fake St",
///             "city": "Springfield",
///             "postcode": "12345"
///         }
///     }
/// "#;
/// let value = jsoxiden::from_str(json).unwrap();
/// let name = value["name"].as_str().unwrap();
/// assert_eq!(name, "John Doe");
///
/// ```
pub fn from_str(input: &str) -> Result<Value, ParseError> {
    let mut parser = Parser::new(input);
    parser.parse()
}

/// Parse JSON from a file and return a [`Value`].
/// 
/// # Arguments:
/// * `filepath` - The path to the file.
/// 
/// # Returns:
/// * `Result<Value, Box<dyn Error>>` - The parsed JSON value or an [`std::io::Error`] if the file could not be read or a [`ParseError`] if the JSON is invalid.
pub fn from_file(filepath: &str) -> Result<Value, Box<dyn Error>> {
    let input = std::fs::read_to_string(filepath)?;
    let mut parser = Parser::new(&input);
    Ok(parser.parse()?)
}

struct Parser<'a> {
    input: Peekable<Chars<'a>>,
    col: usize,
    line: usize,
    recurse_depth: i16,
}

impl<'a> Parser<'a> {
    /// Create new parser from the provided string.
    fn new(input: &'a str) -> Parser {
        let iter = input.chars().peekable();

        Parser { input: iter, col: 1, line: 1, recurse_depth: 0 }
    }

    /// Get the next character in the input string and update column and line counters.
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
                return Ok(Value::Object(IndexMap::new()))
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
        let mut map =  IndexMap::new();
        loop {
            let (key, value) = self.member()?;

            map.insert(key, value);

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

            vec.push(value);

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
        Ok(value)
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
                if !(0xD800..=0xDBFF).contains(&representation) { return Err((ParseErrorType::InvalidUnicode, self.col, self.line, hex_string).into()) }

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
                        if !(0xDC00..=0xDFFF).contains(&representation2) { return Err((ParseErrorType::InvalidUnicode, self.col, self.line, hex_string).into()) }

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
                            match int.checked_mul(i as i128) {
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
    fn integer(&mut self) -> Result<i128, ParseError> {
        let mut neg = false;
        if let Some('-') = self.input.peek() {
            neg = true;
            _ = self.consume();
        }

        match self.input.peek() {
            Some('0'..='9') => {
                let digits = self.digits()?;

                if digits.chars().nth(0) == Some('0') && digits.len() > 1 { return Err((ParseErrorType::InvalidInteger, self.col ,self.line, digits).into()) }

                match digits.parse::<i128>() {
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
                            Ok(Some(-(10f64.powi(i))))
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