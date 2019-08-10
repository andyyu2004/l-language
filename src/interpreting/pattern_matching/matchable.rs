use crate::interpreting::LPattern::*;
use crate::interpreting::LPattern;
use crate::interpreting::objects::LObject;
use std::cell::RefCell;
use std::rc::Rc;

pub trait Matchable<T> {

    fn is_match(&self, pattern: &LPattern) -> bool;

    // bool indicating match success or failure, and vector of pairs of name value bindings
    fn bindings(&mut self, pattern: &LPattern) -> Vec<(String, T)>; // {
//        match pattern {
//            PWildcard => (true, vec![]),
//            PIdentifier(id) => vec![(id.lexeme.clone(), self.clone())],
//            _ => (false, vec![]),
//        }
//    }

}