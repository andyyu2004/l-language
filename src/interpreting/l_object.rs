use LObject::{LNumber, LString, LBool, LUnit};
use std::fmt::{Display, Formatter, Error};
use crate::interpreting::Function;
use crate::interpreting::l_object::LObject::{LFunction, LTuple, LRecord};
use crate::parsing::expr::{format_tuple, format_record};
use std::collections::HashMap;
use std::cell::RefCell;
use std::rc::Rc;
use itertools::Itertools;

#[derive(Debug, Clone, PartialEq)]
pub enum LObject {
    LString(String),
    LNumber(f64),
    LBool(bool),
    LTuple(Vec<Rc<RefCell<LObject>>>),
    LFunction(Function),
    LRecord(HashMap<String, Rc<RefCell<LObject>>>),
    LUnit
}

// Should never panic if type system works
impl LObject {
//    pub fn string(&self) -> &String {
//        if let LString(s) = self { s }
//        else { panic!("Expected LObject to be a string") }
//    }

    pub fn number_mut(&mut self) -> &mut f64 {
        if let LNumber(ref mut s) = self { s }
        else { panic!("Expected LObject to be a number") }
    }

    pub fn number(&self) -> f64 {
        if let LNumber(s) = self { *s }
        else { panic!("Expected LObject to be a number") }
    }

    pub fn boolean(&self) -> bool {
        if let LBool(b) = self { *b }
        else { panic!("Expected LObject to be a number") }
    }

    pub fn boolean_mut(&mut self) -> &mut bool {
        if let LBool(ref mut b) = self { b }
        else { panic!("Expected LObject to be a boolean") }
    }

    pub fn function_mut(&mut self) -> &mut Function {
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
            LTuple(xs) => write!(f, "({})", format_tuple(&xs.iter().map(|x| x.borrow().clone()).collect_vec())),
            LRecord(xs) => {
                let mut ys = HashMap::new();
                for (k, v) in xs {
                    ys.insert(k.clone(), v.borrow().clone());
                }
                write!(f, "{{{}}}", format_record(&ys))
            },
            x => write!(f, "{:?}", x)
        }
    }
}
