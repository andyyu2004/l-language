use LObject::{LNumber, LString, LBool, LUnit};
use std::fmt::{Display, Formatter, Error};
use crate::parsing::expr::{format_tuple, format_record};
use std::collections::{HashMap, VecDeque};
use std::cell::RefCell;
use std::rc::Rc;
use itertools::{Itertools, join};
use crate::interpreting::objects::l_object::LObject::{LRecord, LVariant, LTuple, LFunction, LStruct, LList};
use crate::interpreting::objects::{Function, Variant, Struct, Tuple};
use crate::interpreting::pattern_matching::Matchable;
use crate::interpreting::{LPattern, Interpreter};
use crate::interpreting::LPattern::*;
use std::ops::Deref;

#[derive(Debug, Clone, PartialEq)]
pub enum LObject {
    LString(String),
    LNumber(f64),
    LBool(bool),
    LTuple(Tuple),
    LRecord(HashMap<String, Rc<RefCell<LObject>>>),
    LFunction(Function),
    LStruct(Struct),
    LVariant(Variant),
    LList(VecDeque<Rc<RefCell<LObject>>>),
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
        else { panic!("Expected LObject to be a boolean") }
    }

    pub fn boolean_mut(&mut self) -> &mut bool {
        if let LBool(ref mut b) = self { b }
        else { panic!("Expected LObject to be a boolean") }
    }

    pub fn function_mut(&mut self) -> &mut Function {
        if let LFunction(f) = self { f }
        else { panic!("Expected LObject to be a function") }
    }

    pub fn tuple(&self) -> &Tuple {
        if let LTuple(tuple) = self { tuple }
        else { panic!("Expected LObject to be a tuple") }
    }

    pub fn tuple_mut(&mut self) -> &mut Tuple {
        if let LTuple(tuple) = self { tuple }
        else { panic!("Expected LObject to be a tuple") }
    }

    pub fn variant(&self) -> &Variant {
        if let LVariant(variant) = self { variant }
        else { panic!("Expected LObject to be a variant") }
    }

    pub fn variant_mut(&mut self) -> &mut Variant {
        if let LVariant(variant) = self { variant }
        else { panic!("Expected LObject to be a variant") }
    }

}

impl Matchable<Self> for LObject {
    fn is_match(&self, pattern: &LPattern) -> bool {
        match pattern {
            PConstructor(l, r) => false,
            PIdentifier(x) => true,
            PWildcard => true,
            PTuple(_) => self.tuple().is_match(pattern),
            PRecord => false,
            PLiteral(x) => Interpreter::literal_to_l_object(x).borrow().deref() == self,
            PVariant(..) => self.variant().is_match(pattern),
        }
    }

    fn bindings(&mut self, pattern: &LPattern) -> Vec<(String, LObject)> {
        match pattern {
            PConstructor(_, _) => vec![],
            PIdentifier(x) => vec![(x.lexeme.clone(), self.clone())],
            PWildcard => vec![],
            PTuple(_) => self.tuple_mut().bindings(pattern),
            PRecord => vec![],
            PLiteral(_) => vec![],
            PVariant(..) => self.variant_mut().bindings(pattern)
        }
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
            LTuple(xs) => write!(f, "{}", xs),
            LList(xs) => write!(f, "[{}]", format_tuple(&xs.iter().map(|x| x.borrow().clone()).collect_vec())),
            LVariant(variant) => write!(f, "{}", variant),
            LRecord(xs) => {
                let mut ys = HashMap::new();
                for (k, v) in xs {
                    ys.insert(k.clone(), v.borrow().clone());
                }
                write!(f, "{{{}}}", format_record(&ys, ", "))
            },
            x => write!(f, "{:?}", x)
        }
    }
}
