use crate::interpreting::objects::LObject;
use std::cell::RefCell;
use std::rc::Rc;
use crate::interpreting::pattern_matching::Matchable;
use crate::interpreting::LPattern::*;
use crate::interpreting::LPattern;
use std::fmt::{Display, Formatter, Error};
use crate::parsing::expr::format_tuple;
use itertools::Itertools;
use crate::interpreting::objects::l_object::LObject::LTuple;

#[derive(Debug, Clone, PartialEq)]
pub struct Tuple {
    elements: Vec<Rc<RefCell<LObject>>>
}

impl Tuple {
    pub fn new(elements: Vec<Rc<RefCell<LObject>>>) -> Tuple {
        Tuple { elements }
    }
}

impl Matchable<LObject> for Tuple {
    fn is_match(&self, pattern: &LPattern) -> bool {
        if let PTuple(ps) = pattern {
            // Type checker will ensure length is correct
            self.elements.iter()
                .zip(ps)
                .all(|(x, p)| x.borrow().is_match(p))
        } else { false }
    }

    fn bindings(&mut self, pattern: &LPattern) -> Vec<(String, LObject)> {
        if let PTuple(ps) = pattern {
            self.elements.iter()
                .zip(ps)
                .flat_map(|(x,p)| x.borrow_mut().bindings(p))
                .collect_vec()

        } else { unreachable!() }
    }
}

impl Display for Tuple {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "({})", format_tuple(&self.elements.iter().map(|x| x.borrow().clone()).collect_vec()))
    }
}
