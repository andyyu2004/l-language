pub mod l_types;
pub mod type_checker;

pub use l_types::LType;
use std::fmt::{Display, Formatter, Error};
use crate::lexing::{Token};
use crate::errors::LError;
use crate::types::LTypeError::{TypeError, TypeMismatch, NonFunction};

#[derive(Debug, Eq, PartialEq)]
pub enum LTypeError {
    TypeMismatch(LType, LType, Token),
    TypeError(LType, LType, Token),
    NonFunction(LType, Token)
}

impl Display for LTypeError {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            TypeMismatch(l, r, token) => write!(f, "{}", LError::from_token(format!("Couldn't match types {} and {}", l, r), token)),
            TypeError(l, r, token) => write!(f, "{}", LError::from_token(format!("Expected {}, got {}", l, r), token)),
            NonFunction(t, token) => write!(f, "{}", LError::from_token(format!("Expected function type, got {}", t), token)),
        }
    }
}