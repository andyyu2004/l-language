use LObject::{LNumber, LString, LBool, LUnit};
use std::fmt::{Display, Formatter, Error};
use crate::interpreting::Function;
use crate::interpreting::l_object::LObject::{LFunction, LTuple};
use crate::parsing::expr::format_tuple;

#[derive(Debug, Clone)]
pub enum LObject {
    LString(String),
    LNumber(f64),
    LBool(bool),
    LTuple(Vec<LObject>),
    LFunction(Function),
    LUnit
}

// Should never panic if type system works
impl LObject {
//    pub fn string(&self) -> &String {
//        if let LString(s) = self { s }
//        else { panic!("Expected LObject to be a string") }
//    }

    pub fn number(&self) -> f64 {
        if let LNumber(s) = self { *s }
        else { panic!("Expected LObject to be a number") }
    }
//
//    pub fn boolean(&self) -> bool {
//        if let LBool(b) = self { *b }
//        else { panic!("Expected LObject to be a boolean") }
//    }
}

impl Display for LObject {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            LString(s) => write!(f, "{}", s),
            LNumber(n) => write!(f, "{}", n),
            LBool(b) => write!(f, "{}", b),
            LUnit => write!(f, "()"),
            LFunction(function) => write!(f, "{}", function),
            LTuple(xs) => write!(f, "({})", format_tuple(xs)),
            x => write!(f, "{:?}", x)
        }
    }
}
