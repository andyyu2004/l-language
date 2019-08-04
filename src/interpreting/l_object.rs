use LObject::{LNumber, LString, LBool, LUnit};
use std::fmt::{Display, Formatter, Error};
use crate::interpreting::Function;
use crate::interpreting::l_object::LObject::{LFunction, LTuple, LRecord};
use crate::parsing::expr::format_tuple;
use crate::types::l_types::Pair;

#[derive(Debug, Clone, PartialEq)]
pub enum LObject {
    LString(String),
    LNumber(f64),
    LBool(bool),
    LTuple(Vec<LObject>),
    LFunction(Function),
    LRecord(Vec<Pair<LObject>>),
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

    pub fn boolean(&self) -> bool {
        if let LBool(b) = self { *b }
        else { panic!("Expected LObject to be a boolean") }
    }

    pub fn function(&mut self) -> &mut Function {
        if let LFunction(f) = self { f }
        else { panic!("Expected LObject to be a number") }
    }

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
            LRecord(xs) => write!(f, "{{{}}}", format_tuple(xs)),
            x => write!(f, "{:?}", x)
        }
    }
}
