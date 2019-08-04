use std::fmt::{Display, Error, Formatter};

use crate::lexing::Token;
use itertools::join;
use crate::parsing::expr::Expr::{EBinary, EUnary, EVariable, ELiteral, ETuple, EApplication, EAssignment, EBlock, EIf, ERecord, ELogic};
use crate::parsing::Stmt;
use crate::parsing::stmt::format_block;
use crate::types::l_types::Pair;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    EBinary { operator: Token, left: Box<Expr>, right: Box<Expr> },
    EUnary { operator: Token, operand: Box<Expr> },
    ELogic { operator: Token, left: Box<Expr>, right: Box<Expr> },
    EVariable { name: Token },
    EBlock(Vec<Stmt>),
    EAssignment { lvalue: Token, expr: Box<Expr> },
    ELiteral(Token),
    EApplication { token: Token, callee: Box<Expr>, arg: Box<Expr> }, // Curried
    ETuple(Vec<Expr>),
    ERecord(Vec<Pair<Expr>>),
    EIf { token: Token, condition: Box<Expr>, left: Box<Expr>, right: Box<Expr> }
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            EBinary { operator, left, right } => write!(f, "{}{{{}}}{{{}}}", operator.lexeme, left, right),
            EUnary { operator, operand } => write!(f, "{}{{{}}}", operator.lexeme, operand),
            EVariable { name } => write!(f, "var {}", name.lexeme),
            ELiteral(x) => write!(f, "{}", x.lexeme),
            EBlock(xs) => write!(f, "{{{}}}", format_block(xs)),
            ETuple(xs) => write!(f, "({})", format_tuple(xs)),
            ERecord(xs) => write!(f, "record {{{}}}", format_tuple(xs)),
            EAssignment { lvalue, expr} => write!(f, "{} = {}", lvalue, expr),
            EApplication { callee, arg, .. } => write!(f, "{} {}", callee, arg),
            ELogic { operator, left, right } => write!(f, "{}{{{}}}{{{}}}", operator, left, right),
            EIf { token, condition, left, right } => match **right {
                EBlock(ref xs) if !xs.is_empty() => write!(f, "if {} then {} else {}", condition, left, right),
                _ => write!(f, "if {} then {}", condition, left),
            }

            x => write!(f, "{:?}", x)
        }
    }
}

pub fn format_tuple<T>(args: &Vec<T>) -> String where T : Display {
    join(args, ", ")
}

