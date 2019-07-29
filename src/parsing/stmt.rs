use std::fmt::{Display, Error, Formatter};

use crate::parsing::Expr;
use crate::types::LType;
use crate::lexing::Token;
use itertools::join;
use crate::types::l_types::NameTypePair;

#[derive(Debug, Clone)]
pub enum Stmt {
    ExprStmt(Expr),
    PrintStmt(Expr),
    Var(Token, Option<Expr>), // name: String -> init: Maybe Expr -> Token
    Let(Token, Expr),
    Fn(Token, Vec<NameTypePair>, LType, Vec<Stmt>)
}

impl Display for Stmt {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Stmt::ExprStmt(expr) => write!(f, "ExprStmt(\n\t{}\n)", expr),
            Stmt::PrintStmt(expr) => write!(f, "Print(\n\t{}\n)", expr),
            Stmt::Var(name, Some(expr)) => write!(f, "Var(\n\t{},\n\t{}\n)", name.lexeme, expr),
            Stmt::Var(name, None) => write!(f, "Var(\n\t{}\n)", name.lexeme),
            Stmt::Let(name, expr) => write!(f, "Let(\n\t{},\n\t{}\n)", name.lexeme, expr),
            Stmt::Fn(name, args, ret, body) =>
                write!(f, "fn {} ({}) -> {} {{\n{}}}", name.lexeme, format_args(args), ret, format_block(body)),
            x => write!(f, "{:?}", x)
        }
    }
}

fn format_args(args: &Vec<NameTypePair>) -> String {
    join(args, ", ")
}

fn format_block(statements: &Vec<Stmt>) -> String {
    join(statements, ";\n")
}