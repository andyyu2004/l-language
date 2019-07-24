use crate::TokenType;
use std::fmt::{Display, Error, Formatter};

#[derive(Debug)]
// Choose to store binary in prefix form
pub enum Expr {
    Binary(TokenType, Box<Expr>, Box<Expr>),
    Unary(TokenType, Box<Expr>),
    Grouping(Box<Expr>),
    Operand(f64),
    Variable(&'static str), // Name of variable
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Expr::Operand(x) => write!(f, "{}", x),
            Expr::Binary(op, l, r) => write!(f, "{}{{{}}}{{{}}}", op, l, r),
            Expr::Unary(op, expr) => write!(f, "{}{{{}}}", op, expr),
            Expr::Grouping(expr) => write!(f, "group {{{}}}", expr),
            Expr::Variable(var) => write!(f, "var {}", var),
//            _ => write!(f, "")
        }
    }
}