use crate::lexing::Token;
use std::fmt::{Display, Error, Formatter};
use crate::interpreting::pattern_matching::LPattern::*;
use crate::interpreting::pattern_matching::pattern::LPattern::PTuple;
use crate::parsing::expr::format_tuple;

#[derive(Debug, Clone, PartialEq)]
pub enum LPattern {
//    POr(Box<LPattern>, Box<LPattern>),
    PRecord,
    PConstructor(Box<LPattern>, Box<LPattern>), // Somewhat analogous to the type TArrow, for matching curried constructors
    PTuple(Vec<LPattern>),
    PLiteral(Token),
    PVariant(Token, Option<Box<LPattern>>),
    PIdentifier(Token),
    PWildcard
}

impl Display for LPattern {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            PConstructor(l, r) => write!(f, "({} -> {})", l, r),
            PVariant(token, p) => match p {
                Some(p) => write!(f, "PVariant {} {}", token.lexeme, p),
                None => write!(f, "PVariant {}", token.lexeme),
            }
//            PRecord => {}
            PTuple(xs) => write!(f, "({})", format_tuple(xs)),
            PIdentifier(token) => write!(f, "PId {}", token.lexeme),
            PLiteral(x) => write!(f, "PLit {}", x.lexeme),
            x => write!(f, "{:?}", x)
        }
    }
}

impl LPattern {

    pub fn constructor_arity(&self) -> i16 {
        match self {
            PConstructor(_, r) => 1 + r.constructor_arity(),
            _ => 1
        }
    }

    pub fn is_refutable(&self) -> bool {
        match self {
            PLiteral(_) => true,
            PWildcard | PIdentifier(_) => false,
            PRecord => true, // Not implemented properly yet, just say true
            PConstructor(_, _) => true,
            PTuple(xs) => xs.iter().any(|p| p.is_refutable()), // PTuple is refutable if any of its subpatterns are refutable
            PVariant(_, _) => true,
//            POr(l, r) => l.is_refutable() && r.is_refutable() // Refutable if all are refutable
        }
    }

}




