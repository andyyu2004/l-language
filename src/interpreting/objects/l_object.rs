use LObject::{LNumber, LString, LBool, LUnit};
use std::fmt::{Display, Formatter, Error};
use crate::parsing::expr::{format_tuple, format_record};
use std::collections::{HashMap, VecDeque};
use std::cell::RefCell;
use std::rc::Rc;
use itertools::{Itertools};
use crate::interpreting::objects::l_object::LObject::*;
use crate::interpreting::objects::{Function, Variant, Struct, Tuple, Lambda, LInvocable};
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
    LLambda(Lambda),
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

    pub fn function(&self) -> &Function {
        if let LFunction(f) = self { f }
        else { panic!("Expected LObject to be a function") }
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
        else { panic!("Expected LObject to be a variant, found {}", self) }
    }

    pub fn variant_mut(&mut self) -> &mut Variant {
        if let LVariant(variant) = self { variant }
        else { panic!("Expected LObject to be a variant, found {}", self) }
    }

    pub fn lambda(&self) -> &Lambda {
        if let LLambda(lambda) = self { lambda }
        else { panic!("Expected LObject to be a lambda, found {}", self) }
    }

    pub fn invocable(&self) -> &dyn LInvocable {
        match self {
            LLambda(lambda) => lambda,
            LFunction(function) => function,
            _ => panic!("Expected LObject to be an invocable, found {}", self)
        }
    }

}

impl Matchable<Self> for LObject {
    fn is_match(&self, pattern: &LPattern) -> bool {
        match pattern {
            PConstructor(_, _) => false,
            PIdentifier(_) => true,
            PWildcard => true,
            PTuple(_) => self.tuple().is_match(pattern),
            PRecord => false,
            PLiteral(x) => Interpreter::literal_to_l_object(x).borrow().deref() == self,
            PVariant(..) => self.variant().is_match(pattern),
//            POr(l, r) => self.is_match(l) || self.is_match(r)
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
            PVariant(..) => self.variant_mut().bindings(pattern),
//            POr(l, r) => if self.is_match(l) { self.bindings(l) } else { self.bindings(r) }
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
            LLambda(lambda) => write!(f, "{}", lambda),
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
