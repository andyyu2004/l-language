pub mod l_types;
pub mod type_checker;
mod operator_type_map;

pub use l_types::LType;
use std::fmt::{Display, Formatter, Error};
use crate::lexing::{Token};
use crate::errors::LError;
use crate::types::LTypeError::{TypeError, TypeMismatch, NonFunction, InvalidDeclaration, RequireTypeAnnotation, NonExistentType};

#[derive(Debug, Eq, PartialEq)]
pub enum LTypeError {
    TypeMismatch(LType, LType, Token),
    TypeError(LType, LType, Token),
    NonFunction(LType, Token),
    NonExistentType(Token),
    RequireTypeAnnotation(Token),
    InvalidDeclaration, // When function does not exist, due to definition having failed, don't report error as it is fallthrough
}

impl Display for LTypeError {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            TypeMismatch(l, r, token) => write!(f, "{}", LError::from_token(format!("Couldn't match types {} and {}", l, r), token)),
            TypeError(l, r, token) => write!(f, "{}", LError::from_token(format!("Expected type {}, got {}", l, r), token)),
            NonFunction(t, token) => write!(f, "{}", LError::from_token(format!("Expected function type, got {}", t), token)),
            RequireTypeAnnotation(token) => write!(f, "{}", LError::from_token("Unintialised var declaration requires type signature".to_string(), token)),
            NonExistentType(token) => write!(f, "{}", LError::from_token(format!("Cannot find type {}", token.lexeme), token)),
            InvalidDeclaration => write!(f, "Invalid decl") // Ok(()) // Cascaded failure
        }
    }
}