use std::fmt::{Display, Error, Formatter};

use crate::lexing::Token;
use itertools::join;
use crate::parsing::expr::Expr::{EBinary, EUnary, EVariable, ELiteral, ETuple, EApplication, EAssignment};

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    EBinary { operator: Token, left: Box<Expr>, right: Box<Expr> },
    EUnary { operator: Token, operand: Box<Expr> },
    EVariable { name: Token },
    EAssignment { lvalue: Token, expr: Box<Expr> },
    ELiteral(Token),
    EApplication { token: Token, callee: Box<Expr>, arg: Box<Expr> }, // Curried
    ETuple(Vec<Expr>),
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            EBinary { operator, left, right } => write!(f, "{}{{{}}}{{{}}}", operator.lexeme, left, right),
            EUnary { operator, operand } => write!(f, "{}{{{}}}", operator.lexeme, operand),
            EVariable { name } => write!(f, "var {}", name.lexeme),
            ELiteral(x) => write!(f, "{}", x.lexeme),
            ETuple(xs) => write!(f, "({})", format_tuple(xs)),
            EAssignment { lvalue, expr} => write!(f, "{} = {}", lvalue, expr),
            EApplication { callee, arg, .. } => write!(f, "{} {}", callee, arg),
//            x => write!(f, "{:?}", x)
        }
    }
}

pub fn format_tuple<T>(args: &Vec<T>) -> String where T : Display {
    join(args, ", ")
}

