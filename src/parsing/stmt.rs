use crate::parsing::Expr;
use std::fmt::{Display, Formatter, Error};

#[derive(Debug)]
pub enum Stmt {
    ExprStmt(Expr),
    PrintStmt(Expr),
    Var(&'static str, Option<Expr>), // name: String -> init: Maybe Expr -> Token
    Let(&'static str, Expr),
}

impl Display for Stmt {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Stmt::ExprStmt(expr) => write!(f, "ExprStmt(\n\t{}\n)", expr),
            Stmt::PrintStmt(expr) => write!(f, "Print(\n\t{}\n)", expr),
            Stmt::Var(name, Some(expr)) => write!(f, "Var(\n\t{},\n\t{}\n)", name, expr),
            Stmt::Var(name, None) => write!(f, "Var(\n\t{}\n)", name),
            Stmt::Let(name, expr) => write!(f, "Let(\n\t{},\n\t{}\n)", name, expr),
//            _ => {}
        }
    }
}