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

use std::{error::Error, fmt, ops::Index};
use indexmap::IndexMap;
use super::ParseError;

macro_rules! value_to_integer_boilerplate {
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

macro_rules! value_from_integer_boilerplate {
    ( $( $type:ty ),* ) => {
        $(
            impl From<$type> for Value {
                fn from(i: $type) -> Self {
                    Value::Number(Number::Int(i as i128))
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
        match self {
            DeserialiseError::ParseError(e) => write!(f, "{}", e),
            DeserialiseError::ValueError(e) => write!(f, "{}", e),
        }
    }
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

/// Possible error types that can occur when trying to convert a [`Value`] to a Rust type.
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
    Object(IndexMap<String, Value>)
}

impl Value {
    /// Returns a reference to the underlying boolean.
    /// 
    /// # Returns:
    /// A reference to the underlying boolean or a [`ValueErrorType::IncorrectType`] error if the value does not represent a boolean.
    pub fn as_bool(&self) -> Result<&bool, ValueError> {
        match self {
            Value::Bool(ref b) => Ok(b),
            _ => Err((ValueErrorType::IncorrectType, self.value_type().to_string()).into()),
        }
    }

    /// Returns a mutable reference to the underlying boolean.
    /// 
    /// # Returns:
    /// A mutable reference to the underlying boolean or a [`ValueErrorType::IncorrectType`] error if the value does not represent a boolean.
    pub fn as_mut_bool(&mut self) -> Result<&mut bool, ValueError> {
        match self {
            Value::Bool(ref mut b) => Ok(b),
            _ => Err((ValueErrorType::IncorrectType, self.value_type().to_string()).into()),
        }
    }

    /// Returns a reference to the underlying number.
    /// 
    /// # Returns:
    /// A reference to the underlying number or a [`ValueErrorType::IncorrectType`] error if the value does not represent a number.
    pub fn as_number(&self) -> Result<&Number, ValueError> {
        match self {
            Value::Number(ref n) => Ok(n),
            _ => Err((ValueErrorType::IncorrectType, self.value_type().to_string()).into()),
        }
    }

    /// Returns a mutable reference to the underlying number.
    /// 
    /// # Returns:
    /// A mutable reference to the underlying number or a [`ValueErrorType::IncorrectType`] error if the value does not represent a number.
    pub fn as_mut_number(&mut self) -> Result<&mut Number, ValueError> {
        match self {
            Value::Number(ref mut n) => Ok(n),
            _ => Err((ValueErrorType::IncorrectType, self.value_type().to_string()).into()),
        }
    }

    /// Returns a reference to the underlying string.
    /// 
    /// # Returns:
    /// A reference to the underlying string or a [`ValueErrorType::IncorrectType`] error if the value does not represent a string.
    pub fn as_str(&self) -> Result<&str, ValueError> {
        match self {
            Value::String(ref s) => Ok(s),
            _ => Err((ValueErrorType::IncorrectType, self.value_type().to_string()).into()),
        }
    }

    /// Returns a mutable reference to the underlying string.
    /// 
    /// # Returns:
    /// A mutable reference to the underlying string or a [`ValueErrorType::IncorrectType`] error if the value does not represent a string.
    pub fn as_mut_str(&mut self) -> Result<&mut str, ValueError> {
        match self {
            Value::String(ref mut s) => Ok(s),
            _ => Err((ValueErrorType::IncorrectType, self.value_type().to_string()).into()),
        }
    }

    /// Returns a reference to the underlying vector.
    /// 
    /// # Returns:
    /// A reference to the underlying vector or a [`ValueErrorType::IncorrectType`] error if the value does not represent an array.
    pub fn as_array(&self) -> Result<&Vec<Value>, ValueError> {
        match self {
            Value::Array(ref a) => Ok(a),
            _ => Err((ValueErrorType::IncorrectType, self.value_type().to_string()).into()),
        }
    }

    /// Returns a mutable reference to the underlying vector.
    /// 
    /// # Returns:
    /// A mutable reference to the underlying vector or a [`ValueErrorType::IncorrectType`] error if the value does not represent a vector.
    pub fn as_mut_array(&mut self) -> Result<&mut Vec<Value>, ValueError> {
        match self {
            Value::Array(ref mut v) => Ok(v),
            _ => Err((ValueErrorType::IncorrectType, self.value_type().to_string()).into()),
        }
    }

    /// Returns a reference to the underlying map representing a JSON object.
    /// 
    /// # Returns:
    /// A reference to the underlying map or a [`ValueErrorType::IncorrectType`] error if the value does not represent an object.
    pub fn as_map(&self) -> Result<&IndexMap<String, Value>, ValueError> {
        match self {
            Value::Object(ref o) => Ok(o),
            _ => Err((ValueErrorType::IncorrectType, self.value_type().to_string()).into()),
        }
    }

    /// Returns a mutable reference to the underlying map representing a JSON object.
    /// 
    /// # Returns:
    /// A mutable reference to the underlying map or a [`ValueErrorType::IncorrectType`] error if the value does not represent an object.
    pub fn as_mut_map(&mut self) -> Result<&mut IndexMap<String, Value>, ValueError> {
        match self {
            Value::Object(ref mut o) => Ok(o),
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

    fn serialise_internal(&self, indent: usize) -> String {
        let current_indent = "\t".repeat(indent);
        let next_indent = "\t".repeat(indent + 1);

        match self {
            Value::Null => "null".to_string(),
            Value::Bool(b) => b.to_string(),
            Value::Number(n) => n.to_string(),
            Value::String(s) => format!("\"{}\"", s),
            Value::Array(a) => {
                let elements = a.iter()
                    .map(|v| v.serialise_internal(indent + 1))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("[{}]", elements)
            },
            Value::Object(o) => {
                let entries = o.iter()
                    .map(|(k, v)| format!("{}\"{}\": {}", next_indent, k, v.serialise_internal(indent + 1)))
                    .collect::<Vec<_>>()
                    .join(",\n");
                format!("{{\n{}\n{}}}", entries, current_indent)
            },
        }
    }

    /// Serialise the contained JSON data into a string.
    pub fn to_json_string(&self) -> String {
        self.serialise_internal(0)
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

value_to_integer_boilerplate!(i8, u8, i16, u16, i32, u32, i64, u64, i128, u128);

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
            Value::Number(Number::Float(f)) => Ok(f),
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
    where T: TryFromValue,
{
    fn try_from_value(value: Value) -> Result<Self, ValueError> {
        match value {
            Value::Null => Ok(None),
            _ => T::try_from_value(value).map(Some),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_json_string())
    }
}

impl Index<&str> for Value {
    type Output = Value;

    fn index(&self, key: &str) -> &Self::Output {
        match self.as_map().unwrap().get(key) {
            Some(v) => v,
            _ => &Value::Null,
        }
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Self {
        Value::Bool(b)
    }
}

value_from_integer_boilerplate!(i8, u8, i16, u16, i32, u32, i64, u64, i128, u128);

impl From<f32> for Value {
    fn from(f: f32) -> Self {
        Value::Number(Number::Float(f as f64))
    }
}

impl From<f64> for Value {
    fn from(f: f64) -> Self {
        Value::Number(Number::Float(f))
    }
}

impl From<String> for Value {
    fn from(s: String) -> Self {
        Value::String(s)
    }
}

impl<T> From<Vec<T>> for Value
    where T: Into<Value>
{
    fn from(v: Vec<T>) -> Self {
        Value::Array(v.into_iter().map(|value| value.into()).collect())
    }
}

impl<T> From<Option<T>> for Value
    where T: Into<Value>
{
    fn from(o: Option<T>) -> Self {
        match o {
            Some(val) => val.into(),
            None => Value::Null,
        }   
    }
}