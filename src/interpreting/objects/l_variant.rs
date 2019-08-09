use crate::parsing::Stmt;
use std::collections::HashMap;
use crate::interpreting::{Env, LPattern};
use std::cell::RefCell;
use std::rc::Rc;
use std::fmt::{Display, Formatter, Error};
use crate::interpreting::pattern_matching::Matchable;
use crate::interpreting::objects::LObject;
use crate::interpreting::LPattern::*;

#[derive(Debug, Clone, PartialEq)]
pub struct Variant {
    tag: String,
    arg: Rc<RefCell<LObject>>
}

impl Variant {
//    pub fn from_env(tag: String)
    pub fn new(tag: String, arg: Rc<RefCell<LObject>>) -> Variant {
        Variant {
            tag, arg
        }
    }
}

impl Matchable<LObject> for Variant {
    fn is_match(&self, pattern: &LPattern) -> bool {
        if let PVariant(x, p) = pattern {
            x.lexeme == self.tag
        } else {
            false
        }
    }

    fn bindings(&self, pattern: &LPattern) -> Vec<(String, LObject)> {
        match pattern {
            PVariant(_, p) => match **p {
                // Match containing object with pattern is only used to check whether the tag matches or not
                // The token in pvariant is only for
                Some(ref p) => self.arg.borrow().bindings(p),
                None => vec![],
            }
            _ => panic!()
        }
    }
}

impl Display for Variant {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match &*self.arg.borrow() {
            LObject::LUnit => write!(f, "{}", self.tag),
            x => write!(f, "{} {}", self.tag, x)
        }
    }
}

pub struct Data {
    variants: HashMap<String, Variant>
}