use LObject::{LNumber, LString, LBool, LFunction, LUnit};
use std::fmt::{Display, Formatter, Error};
use crate::interpreting::Function;

#[derive(Debug, Clone)]
pub enum LObject {
    LString(String),
    LNumber(f64),
    LBool(bool),
    LFunction(Function),
    LUnit
}

// Should never panic if type system works
impl LObject {
    pub fn string(&self) -> &String {
        if let LString(s) = self { s }
        else { panic!("Expected LObject to be a string") }
    }

    pub fn number(&self) -> f64 {
        if let LNumber(s) = self { *s }
        else { panic!("Expected LObject to be a number") }
    }

    pub fn boolean(&self) -> bool {
        if let LBool(b) = self { *b }
        else { panic!("Expected LObject to be a boolean") }
    }
}

impl Display for LObject {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            LString(s) => write!(f, "{}", s),
            LNumber(n) => write!(f, "{}", n),
            LBool(b) => write!(f, "{}", b),
            LUnit => write!(f, "()"),
            x => write!(f, "{:?}", x)
        }
    }
}
