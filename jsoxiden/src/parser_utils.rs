use std::{collections::BTreeMap, error::Error, fmt, ops::Index};

// If macro_metavar_expr_concat gets stabilised, this macro could be used to generate the boilerplate for the value methods
// macro_rules! value_boilerplate {
//     ( $( $variant:ident, $type:ty ),* ) => {
//         pub fn macro_metavar_expr_concat(as_, $variant)(&self) -> Result<&type, ValueError> {
//             match self {
//                 Value::$variant(ref b) => Ok(b),
//                 _ => Err((ValueErrorType::IncorrectType, self.value_type().to_string()).into()),
//             }
//         }
//     };
// }

macro_rules! value_integer_boilerplate {
    ( $( $type:ty ),* ) => {
        $(
            impl TryFromValue for $type {
                fn try_from_value(value: Value) -> Result<Self, ValueError> {
                    match value {
                        Value::Number(Number::Int(n)) => {
                            if n >= <$type>::MIN as i128 && n <= <$type>::MAX as i128 {
                                Ok(n as $type)
                            } else if n >= <$type>::MAX as i128 {
                                Err((ValueErrorType::NumericOverflow, n.to_string()).into())
                            } else {
                                Err((ValueErrorType::NegativeOverflow, n.to_string()).into())
                            }
                        },
                        _ => Err((ValueErrorType::IncorrectType, value.value_type().to_string()).into()),
                    }
                }
            }
        )*
    };
}

/// An error that can occur when deserialising a JSON string. Could be a [`ParseError`] i.e. the JSON data is incorrectly formatted,
/// or a [`ValueError`] i.e. the JSON data is correctly formatted but the value cannot be converted to the desired type.
#[derive(Debug)]
pub enum DeserialiseError {
    ParseError(ParseError),
    ValueError(ValueError),
}

impl From<ParseError> for DeserialiseError {
    fn from(error: ParseError) -> Self {
        DeserialiseError::ParseError(error)
    }
}

impl From<ValueError> for DeserialiseError {
    fn from(error: ValueError) -> Self {
        DeserialiseError::ValueError(error)
    }
}

impl fmt::Display for DeserialiseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

/// A trait that indicates the type can be deserialised from a JSON string.
pub trait Deserialise {

    /// Deserialise a JSON string into the desired type.
    /// 
    /// # Arguments:
    /// * `input` - The JSON string to deserialise.
    /// 
    /// # Returns:
    /// The deserialised value or a [`DeserialiseError`].
    fn from_str(input: &str) -> Result<Self, DeserialiseError> where Self: Sized;
}

/// Number type for floats and integers.
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum Number {
    Int(i128),
    Float(f64),
}

impl Number {
    pub fn is_int(&self) -> bool {
        match self {
            Number::Int(_) => true,
            Number::Float(_) => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            Number::Int(_) => false,
            Number::Float(_) => true,
        }
    }

    fn value_type(&self) -> &'static str {
        match self {
            Number::Int(_) => "Integer",
            Number::Float(_) => "Float",
        }
    }
}

impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Number::Int(i) => write!(f, "{}", i),
            Number::Float(fl) => write!(f, "{}", fl),
        }
    }
}

/// Possible error types that can occur when trying to convert a JSON value to a Rust type.
#[derive(Debug)]
pub enum ValueErrorType {
    IncorrectType,
    InvalidKey,
    NumericOverflow,
    NegativeOverflow,
}

impl fmt::Display for ValueErrorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ValueErrorType::IncorrectType => write!(f, "Incorrect type"),
            ValueErrorType::InvalidKey => write!(f, "Invalid key"),
            ValueErrorType::NumericOverflow => write!(f, "Number is too large for destination type"),
            ValueErrorType::NegativeOverflow => write!(f, "Attempted to convert a negative number to an unsigned type"),
        }
    }
}

/// An error that occurs when trying to convert a JSON value to a Rust type. 
#[derive(Debug)]
pub struct ValueError {
    error_type: ValueErrorType,
    info: String,
}

impl fmt::Display for ValueError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.error_type {
            ValueErrorType::IncorrectType => write!(f, "This value is a: {}, not an object", self.info),
            ValueErrorType::InvalidKey => write!(f, "Key: {}, is not present", self.info),
            ValueErrorType::NumericOverflow => write!(f, "Number: {}, is too large for destination type", self.info),
            ValueErrorType::NegativeOverflow => write!(f, "Number: {}, is negative and cannot be converted to an unsigned type", self.info),
        }
    }
}

impl Error for ValueError {}

impl From<(ValueErrorType, String)> for ValueError {
    fn from((error_type, info): (ValueErrorType, String)) -> Self {
        ValueError { error_type, info }
    }
}

/// All possible JSON value types.
#[derive(Debug, Clone)]
pub enum Value {
    Null,
    Bool(bool),
    Number(Number),
    String(String),
    Array(Vec<Value>),
    Object(BTreeMap<String, Value>)
}

impl Value {
    pub fn as_bool(&self) -> Result<&bool, ValueError> {
        match self {
            Value::Bool(ref b) => Ok(b),
            _ => Err((ValueErrorType::IncorrectType, self.value_type().to_string()).into()),
        }
    }

    pub fn as_number(&self) -> Result<&Number, ValueError> {
        match self {
            Value::Number(ref n) => Ok(n),
            _ => Err((ValueErrorType::IncorrectType, self.value_type().to_string()).into()),
        }
    }

    pub fn as_str(&self) -> Result<&str, ValueError> {
        match self {
            Value::String(ref s) => Ok(s),
            _ => Err((ValueErrorType::IncorrectType, self.value_type().to_string()).into()),
        }
    }

    pub fn as_array(&self) -> Result<&Vec<Value>, ValueError> {
        match self {
            Value::Array(ref a) => Ok(a),
            _ => Err((ValueErrorType::IncorrectType, self.value_type().to_string()).into()),
        }
    }

    pub fn as_map(&self) -> Result<&BTreeMap<String, Value>, ValueError> {
        match self {
            Value::Object(ref o) => Ok(o),
            _ => Err((ValueErrorType::IncorrectType, self.value_type().to_string()).into()),
        }
    }

    /// Retrieves the value associated with the key as a reference.
    /// 
    /// # Arguments:
    /// * `key` - The key to retrieve.
    /// 
    /// # Returns:
    /// The value associated with the key, a [`ValueErrorType::IncorrectType`] error if the value is not an object
    /// or a [`ValueErrorType::InvalidKey`] error if the key is not present.
    pub fn get(&self, key: &str) -> Result<&Value, ValueError> {
        match self {
            Value::Object(map) => {
                match map.get(key) {
                    Some(value) => Ok(value),
                    None => Err((ValueErrorType::InvalidKey, key.to_string()).into()),
                }
            },
            _ => Err((ValueErrorType::IncorrectType, self.value_type().to_string()).into()),
        }
    }

    /// Retieves the value associated with the key and removes it from the object.
    /// 
    /// # Arguments:
    /// * `key` - The key to remove.
    /// 
    /// # Returns:
    /// The value associated with the key, a [`ValueErrorType::IncorrectType`] error if the value is not an object
    /// or a [`ValueErrorType::InvalidKey`] error if the key is not present.
    pub fn remove(&mut self, key: &str) -> Result<Value, ValueError> {
        match self {
            Value::Object(ref mut map) => {
                match map.remove(key) {
                    Some(value) => Ok(value),
                    None => Err((ValueErrorType::InvalidKey, key.to_string()).into()),
                }
            }
            _ => Err((ValueErrorType::IncorrectType, self.value_type().to_string()).into()),
        }
    }

    fn value_type(&self) -> &'static str {
        match self {
            Value::Null => "Null",
            Value::Bool(_) => "Boolean",
            Value::Number(num) => num.value_type(),
            Value::String(_) => "String",
            Value::Array(_) => "Array",
            Value::Object(_) => "Object",
        }
    }
}

/// A trait that indicates the type can be converted from a [`Value`] type.
pub trait TryFromValue: Sized {
    fn try_from_value(value: Value) -> Result<Self, ValueError>;
}

impl Value {
    pub fn try_from_value<T: TryFromValue>(self) -> Result<T, ValueError> {
        T::try_from_value(self)
    }
}

impl TryFromValue for Value {
    fn try_from_value(value: Value) -> Result<Self, ValueError> {
        Ok(value)
    }
}

impl TryFromValue for bool {
    fn try_from_value(value: Value) -> Result<Self, ValueError> {
        match value {
            Value::Bool(b) => Ok(b),
            _ => Err((ValueErrorType::IncorrectType, value.value_type().to_string()).into()),
        }
    }
}

value_integer_boilerplate!(i8, u8, i16, u16, i32, u32, i64, u64, i128, u128);

impl TryFromValue for f32 {
    fn try_from_value(value: Value) -> Result<Self, ValueError> {
        match value {
            Value::Number(Number::Float(f)) => Ok(f as f32),
            _ => Err((ValueErrorType::IncorrectType, value.value_type().to_string()).into()),
        }
    }

}

impl TryFromValue for f64 {
    fn try_from_value(value: Value) -> Result<Self, ValueError> {
        match value {
            Value::Number(Number::Float(f)) => Ok(f as f64),
            _ => Err((ValueErrorType::IncorrectType, value.value_type().to_string()).into()),
        }
    }

}

impl TryFromValue for String {
    fn try_from_value(value: Value) -> Result<Self, ValueError> {
        match value {
            Value::String(s) => Ok(s),
            _ => Err((ValueErrorType::IncorrectType, value.value_type().to_string()).into()),
        }
    }
}

impl<T> TryFromValue for Vec<T>
    where T: TryFromValue
{
    fn try_from_value(value: Value) -> Result<Self, ValueError> {
        match value {
            Value::Array(v) => Ok(v.into_iter().map(|e| T::try_from_value(e).unwrap()).collect()),
            _ => Err((ValueErrorType::IncorrectType, value.value_type().to_string()).into()),
        }
    }
}

impl<T> TryFromValue for Option<T>
where
    T: TryFromValue,
{
    fn try_from_value(value: Value) -> Result<Self, ValueError> {
        match value {
            Value::Null => Ok(None),
            _ => T::try_from_value(value).map(Some),
        }
    }
}

// TODO: Re-write to be prettier
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
                let entries = o.iter().map(|(k, v)| format!("\"{}\": {}", k, v)).collect::<Vec<_>>().join(",\n");
                write!(f, "{{{}}}", entries)
            },
        }
    }
}

impl Index<&str> for Value {
    type Output = Value;

    fn index(&self, key: &str) -> &Self::Output {
        match self.get(key) {
            Ok(v) => v,
            Err(err) => panic!("{}", err),
        }
    }
}

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