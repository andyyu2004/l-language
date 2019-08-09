use crate::lexing::Token;
use std::fmt::{Display, Error, Formatter};
use crate::interpreting::pattern_matching::LPattern::{PVariant, PIdentifier, PRecord};
use crate::interpreting::pattern_matching::pattern::LPattern::PTuple;
use crate::parsing::expr::format_tuple;

#[derive(Debug, Clone, PartialEq)]
pub enum LPattern {
    PRecord,
    PTuple(Vec<LPattern>),
    PLiteral(Token),
    PVariant(Token, Box<Option<LPattern>>),
    PIdentifier(Token),
    PWildcard
}

impl Display for LPattern {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            PVariant(token, p) => match **p {
                Some(ref p) => write!(f, "PVariant {} {}", token.lexeme, p),
                None => write!(f, "PVariant {}", token.lexeme),
            }
//            PRecord => {}
            PTuple(xs) => write!(f, "({})", format_tuple(xs)),
            PIdentifier(token) => write!(f, "PId {}", token.lexeme),
            x => write!(f, "{:?}", x)
        }
    }
}