use std::fmt::{Display, Formatter, Error};
use crate::parsing::expr::{format_tuple, format_record};
use LType::{TBool,TArrow,TTuple,TUnit,TNum,TTop};
use crate::types::l_types::LType::*;
use crate::interpreting::Env;
use crate::types::LTypeError;
use crate::lexing::Token;
use crate::types::LTypeError::NonExistentType;
use std::collections::HashMap;
use itertools::Itertools;

#[derive(Clone, Debug)]
pub enum LType {
    TTop,
    TBool,
    TNum,
    TString,
    TArrow(Box<LType>, Box<LType>),
    TTuple(Vec<LType>),
    TRecord(HashMap<String, LType>),
    TList(Box<LType>),
    TUnit,
    TVariant(HashMap<String, LType>),
    TName(Token),
    TData(String),
    TNothing
}

impl LType {

    // If only one simple type, it remains
    pub fn rightmost_type(&self) -> &Self {
        match self {
            TArrow(_, r) => r.rightmost_type(),
            t => t,
        }
    }

    // Number of arrows
    // i.e. the number of parameters the function takes to become fully applied
    pub fn curried_arity(&self) -> i16 {
        match self {
            TArrow(l, r) => 1 + r.curried_arity(),
            t => 0
        }
    }

    // If only one type to start with, it remains
    pub fn remove_rightmost(self) -> Self {
        match self {
            TArrow(l, r) => match *r {
                TArrow(ref ll, ref rr) => TArrow(l, Box::from(r.remove_rightmost())),
                _ => *l
            }
            t => panic!("Attempting to remove rightmost type on a simple type {}", t)
        }
    }

    pub fn map_string_to_type(self, env: &Env<LType>) -> Result<LType, LTypeError> {
        match self {
            TList(t) => Ok(TList(Box::new(t.map_string_to_type(env)?))),
            TArrow(l, r) => Ok(TArrow(Box::new(l.map_string_to_type(env)?), Box::new(r.map_string_to_type(env)?))),
            TTuple(xs) => Ok(TTuple(xs.into_iter().map(|x| x.map_string_to_type(env)).collect::<Result<Vec<_>, _>>()?)),
            TRecord(xs) | TVariant(xs) => {
                let mut map = HashMap::new();
                for (k, v) in xs {
                    map.insert(k, v.map_string_to_type(env)?);
                }
                Ok(TRecord(map))
            },
            TUnit => Ok(TUnit),
            TName(name) => match env.resolve(&name) {
                Ok(t) => Ok(t),
                Err(_) => Err(NonExistentType(name))
            },
            _ => Ok(self)
        }
    }
}

impl Display for LType {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            TTop => write!(f, "TTop"),
            TBool => write!(f, "Bool"),
            TNum => write!(f, "Number"),
            TUnit => write!(f, "Unit"),
            TString => write!(f, "TString"),
            TList(x) => write!(f, "[{}]", x),
            TName(s) => write!(f, "'{}", s),
            TNothing => write!(f, "TNothing"),
            TRecord(xs) | TVariant(xs) => write!(f, "{{{}}}", format_record(xs, ", ")),
            TTuple(xs) =>
                if xs.len() == 0 { write!(f, "{}", TUnit)}
                else if xs.len() == 1 { write!(f, "{}", xs[0]) }
                else { write!(f, "({})", format_tuple(xs)) },
            TArrow(left, right) => match **left {
                TArrow(_, _) => write!(f, "({}) -> {}", left, right),
                _                   => write!(f, "{} -> {}", left, right),
            },
            TData(name) => write!(f, "{}", name)
        }
    }
}

// Has full equivalence
impl Eq for LType {}

impl PartialEq<Vec<LType>> for LType {
    fn eq(&self, other: &Vec<LType>) -> bool {
        match (self, other) {
            (TTuple(xs), ys) => xs == ys,
            _ => false
        }
    }
}

impl PartialEq for LType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (TTop, _) | (_, TTop) => true, // TTop can be used in place of any type
            (TBool, TBool) => true,
            (TString, TString) => true,
            (TNum, TNum) => true,
            (TArrow(t, u), TArrow(x, y)) => t == x && u == y,
            (TUnit, TUnit) => true,
            (TTuple(xs), TUnit) => xs.is_empty(), // TUnit is just convenient way to represent empty tuple
            (TUnit, TTuple(ys)) => ys.is_empty(),
            (TTuple(xs), TTuple(ys)) => if xs.len() == 1 { &xs[0] == ys } // Allow nested singleton tuple equivalence
                else if ys.len() == 1 { &ys[0] == xs } else { xs == ys },
            (TTuple(xs), t) => xs.len() == 1 && t == &xs[0], // Order matters for some reason for eq
            (t, TTuple(ys)) => ys.len() == 1 && t == &ys[0],
//            (TTuple(xs), t) => xs.len() == 1 && &xs[0] == t, // Singleton tuple is equivalent to the containing type
//            (t, TTuple(ys)) => ys.len() == 1 && &ys[0] == t,
            (TData(x), TData(y)) => x == y,
            // Order is not important, but naming is in records
            (TRecord(xs), TRecord(ys)) => xs == ys,
//                 xs.iter().map(|x| &x.value).collect::<Vec<&LType>>() == ys.iter().map(|y| &y.value).collect::<Vec<&LType>>(),
            (TList(x), TList(y)) => x == y,
            _ => false
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Pair<T> where T : Display {
    pub name: String,
    pub value: T
}

impl<T> Pair<T> where T : Display {
    pub fn new(name: String, value: T) -> Pair<T> {
        Pair { name, value }
    }
}

impl<T> Display for Pair<T> where T : Display {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{}: {}", self.name, self.value)
    }
}