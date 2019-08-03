use crate::errors::LError;
use crate::interpreting::LObject;
use std::fmt::{Display, Formatter, Error};

// Implement return using result instead of catching panic
#[derive(Debug, Clone)]
pub enum InterpreterError {
    Error(LError),
    Return(LObject)
}

impl From<LError> for InterpreterError {
    fn from(error: LError) -> Self {
        InterpreterError::Error(error)
    }
}

impl Display for InterpreterError {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            InterpreterError::Error(err) => write!(f, "{}", err),
            InterpreterError::Return(obj) => writeln!(f, "throw return: {}", obj)
        }
    }
}