use std::fmt::{Display, Formatter, Error};
use crate::parsing::expr::format_tuple;
use LType::{TBool,TArrow,TTuple,TUnit,TNum};

#[derive(Clone, Debug)]
pub enum LType {
    TBool,
    TNum,
    TArrow(Box<LType>, Box<LType>),
    TTuple(Vec<LType>),
    TUnit
}

impl Display for LType {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            LType::TBool => write!(f, "Bool"),
            LType::TNum => write!(f, "Number"),
            LType::TUnit => write!(f, "Unit"),
            LType::TTuple(xs) =>
                if xs.len() == 0 { write!(f, "{}", TUnit)}
                else if xs.len() == 1 { write!(f, "{}", xs[0]) }
                else { write!(f, "({})", format_tuple(xs)) },
            LType::TArrow(left, right) => match **left {
                LType::TArrow(_, _) => write!(f, "({}) -> {}", left, right),
                _                   => write!(f, "{} -> {}", left, right),
            }
        }
    }
}

// Has full equivalence
impl Eq for LType {}

impl PartialEq for LType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (TBool, TBool) => true,
            (TNum, TNum) => true,
            (TArrow(t, u), TArrow(x, y)) => t == x && u == y,
            (TTuple(xs), TTuple(ys)) => xs == ys,
            (TUnit, TUnit) => true,
            (TTuple(xs), TUnit) => xs.is_empty(), // TUnit is just convenient way to represent empty tuple
            (TUnit, TTuple(ys)) => ys.is_empty(),
            (TTuple(xs), t) => xs.len() == 1 && &xs[0] == t, // Singleton tuple is equivalent to the containing type
            (t, TTuple(ys)) => ys.len() == 1 && &ys[0] == t,
            _ => false
        }
    }
}

#[derive(Debug, Clone)]
pub struct NameTypePair {
    pub name: String,
    pub ltype: LType
}

impl NameTypePair {
    pub fn new(name: String, ltype: LType) -> NameTypePair {
        NameTypePair { name, ltype }
    }
}

impl Display for NameTypePair {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{}: {}", self.name, self.ltype)
    }
}