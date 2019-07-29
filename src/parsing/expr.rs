use std::fmt::{Display, Error, Formatter};

use crate::lexing::Token;
use itertools::join;

#[derive(Debug, Clone)]
pub enum Expr {
    Binary(Token, Box<Expr>, Box<Expr>),
    Unary(Token, Box<Expr>),
    Variable(Token),
    Literal(Token),
    Application(Token, Box<Expr>, Vec<Expr>)
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Expr::Binary(op, l, r) => write!(f, "{}{{{}}}{{{}}}", op.lexeme, l, r),
            Expr::Unary(op, expr) => write!(f, "{}{{{}}}", op.lexeme, expr),
            Expr::Variable(var) => write!(f, "var {}", var.lexeme),
            Expr::Literal(x) => write!(f, "{}", x.lexeme),
            Expr::Application(_, callee, args) => write!(f, "{}({})", callee, format_args(args)),
            x => write!(f, "{:?}", x)
        }
    }
}

pub fn format_args(args: &Vec<Expr>) -> String {
    join(args, ", ")
}