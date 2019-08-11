use crate::parsing::Stmt;
use std::collections::HashMap;
use crate::interpreting::{Env, LPattern};
use std::cell::RefCell;
use std::rc::Rc;
use std::fmt::{Display, Formatter, Error};
use crate::interpreting::pattern_matching::Matchable;
use crate::interpreting::objects::LObject;
use crate::interpreting::LPattern::*;
use itertools::{join, Itertools};
use crate::interpreting::objects::l_object::LObject::LUnit;

#[derive(Debug, Clone, PartialEq)]
pub struct Variant {
    tag: String,
    args: Vec<Rc<RefCell<LObject>>>,
    index: usize,
}

impl Variant {
//    pub fn from_env(tag: String)
    pub fn new(tag: String, args: Vec<Rc<RefCell<LObject>>>) -> Variant {
        Variant {
            tag, args, index: 0
        }
    }
}

impl Matchable<LObject> for Variant {
    fn is_match(&self, pattern: &LPattern) -> bool {
        if let PVariant(x, _) = pattern {
            x.lexeme == self.tag
        } else {
            false
        }
    }

    fn bindings(&mut self, pattern: &LPattern) -> Vec<(String, LObject)> {
        self.index = 0; // Reset index each match to allow for multiple matches on same object
        self.bindings1(pattern)
    }
}

impl Variant {
    fn bindings1(&mut self, pattern: &LPattern) -> Vec<(String, LObject)> {
        match pattern {
            PVariant(_, p) => {
                if let Some(p) = p {
                    self.bindings1(p)
                } else {
                    vec![]
                }
            },
            PConstructor(l, r) => {
                let xs = self.args[self.index].borrow_mut().bindings(l);
                self.index += 1;
                let rs = self.bindings1(r);
                xs.into_iter().chain(rs).collect_vec()
            },
//            PIdentifier(id) => vec![(id.lexeme.clone(), self.args[self.index].borrow().clone())],
//            PWildcard => vec![],
            p => self.args[self.index].borrow_mut().bindings(p),
        }
    }

}

impl Display for Variant {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match &self.args {
            args if args.len() == 1 && *args[0].borrow() == LUnit => write!(f, "{}", self.tag),
            args => write!(f, "({} {})", self.tag, join(args.iter().map(|x| x.borrow()), " "))
        }
    }
}

pub struct Data {
    variants: HashMap<String, Variant>
}