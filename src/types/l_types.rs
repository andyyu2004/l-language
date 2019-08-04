use std::fmt::{Display, Formatter, Error};
use crate::parsing::expr::format_tuple;
use LType::{TBool,TArrow,TTuple,TUnit,TNum};
use crate::types::l_types::LType::{TRecord, TName, TNothing};
use crate::interpreting::Env;
use crate::types::LTypeError;
use crate::lexing::Token;
use crate::types::LTypeError::NonExistentType;

#[derive(Clone, Debug)]
pub enum LType {
    TBool,
    TNum,
    TArrow(Box<LType>, Box<LType>),
    TTuple(Vec<LType>),
    TRecord(Vec<Pair<LType>>),
    TUnit,
    TName(Token),
    TNothing
}

impl LType {
    pub fn map_string_to_type(self, env: &Env<LType>) -> Result<LType, LTypeError> {
        match self {
            TBool => Ok(TBool),
            TNum => Ok(TNum),
            TArrow(l, r) => Ok(TArrow(Box::new(l.map_string_to_type(env)?), Box::new(r.map_string_to_type(env)?))),
            TTuple(xs) => Ok(TTuple(xs.into_iter().map(|x| x.map_string_to_type(env)).collect::<Result<Vec<_>, _>>()?)),
            TRecord(xs) => {
                let mut names = xs.iter().map(|x| x.name.clone()).collect::<Vec<String>>().into_iter();
                let mut pairs = vec![];
                for (i, expr) in xs.into_iter().map(|x| x.value).enumerate() {
                    pairs.push(Pair::new(names.nth(i).unwrap(), expr.map_string_to_type(env)?));
                }
                Ok(TRecord(pairs))
            },
            TUnit => Ok(TUnit),
            TName(name) => match env.resolve(&name) {
                Ok(t) => Ok(t),
                Err(_) => Err(NonExistentType(name))
            },
            TNothing => Ok(TNothing)
        }
    }
}

impl Display for LType {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            LType::TBool => write!(f, "Bool"),
            LType::TNum => write!(f, "Number"),
            LType::TUnit => write!(f, "Unit"),
            LType::TName(s) => write!(f, "name-{}", s),
            LType::TNothing => write!(f, "TNothing"),
            LType::TRecord(xs) => write!(f, "{{{}}}", format_tuple(xs)),
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
            (TRecord(xs), TRecord(ys)) =>
                xs.iter().map(|x| &x.value).collect::<Vec<&LType>>() == ys.iter().map(|y| &y.value).collect::<Vec<&LType>>(),
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