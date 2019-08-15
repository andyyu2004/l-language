pub mod l_types;
pub mod type_checker;
mod operator_type_map;

pub use l_types::LType;
use std::fmt::{Display, Formatter, Error};
use crate::lexing::{Token};
use crate::errors::LError;
use crate::types::LTypeError::*;
use crate::interpreting::LPattern;
use crate::types::l_types::TypeName;

#[derive(Debug, PartialEq)]
pub enum LTypeError {
    TypeMismatch(LType, LType, Token, String),
    TypeError(LType, LType, Token, String),
    NonFunction(LType, Token, String),
    NonExistentType(TypeName),
    NonExistentDataConstructor(Token),
    NonExistentField(Token, LType),
    NotGettable(Token, LType),
    RequireTypeAnnotation(Token, String),
    BadPattern(LPattern, LType, Token, String),
    UndeclaredTypeParameter(Token, String),
    BadKind(usize, usize, Token, String),
    InvalidDeclaration, // When function does not exist, due to definition having failed, don't report error as it is fallthrough
}

impl Display for LTypeError {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            TypeMismatch(l, r, token, note) => write!(f, "{}", LError::from_token(format!("Couldn't match types {} and {}. {}", l, r, note), token)),
            TypeError(l, r, token, note) =>
                write!(f, "{}", LError::from_token(format!("Expected type {}, got {}. {}", l, r, note), token)),
            NonFunction(t, token, note) => write!(f, "{}", LError::from_token(format!("Expected function type, got {}. {}", t, note), token)),
            RequireTypeAnnotation(token, note) => write!(f, "{}", LError::from_token(format!("Require type annotation. {}", note), token)),
            NonExistentType(typename) => write!(f, "{}", LError::from_token(format!("Cannot find type {}", typename), &typename.name)),
            NonExistentField(token, ltype) => write!(f, "{}", LError::from_token(format!("Field {} does not exist on type {}", token.lexeme, ltype), token)),
            InvalidDeclaration => write!(f, ""),
//                write!(f, "Invalid decl"), // Ok(()) // Caused by cascaded failure, otherwise static analysis would have caught it
            NotGettable(token, ltype) => write!(f, "{}", LError::from_token(format!("{} is not gettable", ltype), token)),
            NonExistentDataConstructor(token) => write!(f, "{}", LError::from_token(format!("Cannot find data constructor {}", token.lexeme), token)),
            BadPattern(pattern, ltype, token, note) =>
                write!(f, "{}", LError::from_token(format!("Cannot match pattern {} against type {}. {}", pattern, ltype, note), token)),
            UndeclaredTypeParameter(token, note) =>
                write!(f, "{}", LError::from_token(format!("Use of undeclared type parameter {}. {}", token, note), token)),
            BadKind(expected, got, adt, note) =>
                write!(f, "{}", LError::from_token(format!("{} has kind {}, but got kind {}. {}", adt, format_kind(*expected), format_kind(*got), note), adt))
        }
    }
}

// Where kind 0 is *, kind 1 is * -> * etc. The number represents the number of type parameters

fn format_kind(n: usize) -> String {
    let mut acc = String::new();
    for i in 0..=n {
        if i == n {
            acc.push_str("*")
        } else {
            acc.push_str("* -> ")
        }
    }
    acc
}