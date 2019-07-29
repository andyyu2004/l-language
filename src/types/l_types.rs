use std::fmt::{Display, Formatter, Error};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum LType {
    TBool,
    TNum,
    TArrow(Box<LType>, Box<LType>),
    TUnit
}

impl Display for LType {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            LType::TBool => write!(f, "Bool"),
            LType::TNum => write!(f, "Number"),
            LType::TUnit => write!(f, "Unit"),
            LType::TArrow(left, right) => match **left {
                LType::TArrow(_, _) => write!(f, "({}) -> {}", left, right),
                _                   => write!(f, "{} -> {}", left, right),
            }
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