use std::fmt::{Display, Error, Formatter};

use crate::lexing::Token;
use itertools::join;
use crate::parsing::expr::Expr::{EBinary, EUnary, EVariable, ELiteral, ETuple, ECurryApplication};

#[derive(Debug, Clone)]
pub enum Expr {
    EBinary(Token, Box<Expr>, Box<Expr>),
    EUnary(Token, Box<Expr>),
    EVariable(Token),
    ELiteral(Token),
//    EApplication(Token, Box<Expr>, Vec<Expr>),
    ECurryApplication(Token, Box<Expr>, Box<Expr>),
    ETuple(Vec<Expr>),
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            EBinary(op, l, r) => write!(f, "{}{{{}}}{{{}}}", op.lexeme, l, r),
            EUnary(op, expr) => write!(f, "{}{{{}}}", op.lexeme, expr),
            EVariable(var) => write!(f, "var {}", var.lexeme),
            ELiteral(x) => write!(f, "{}", x.lexeme),
            ETuple(xs) => write!(f, "({})", format_tuple(xs)),
//            EApplication(_, callee, args) => write!(f, "{}({})", callee, format_tuple(args)),
            ECurryApplication(_, callee, arg) => write!(f, "{} {}", callee, arg),
//            x => write!(f, "{:?}", x)
        }
    }
}

pub fn format_tuple<T>(args: &Vec<T>) -> String where T : Display {
    join(args, ", ")
}

