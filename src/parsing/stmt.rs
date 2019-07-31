use std::fmt::{Display, Error, Formatter};

use crate::parsing::Expr;
use crate::types::LType;
use crate::lexing::Token;
use itertools::join;
use crate::types::l_types::NameTypePair;
use crate::parsing::stmt::Stmt::{ExprStmt, PrintStmt, VarStmt, FnStmt, LetStmt, Curried};

#[derive(Debug, Clone)]
pub enum Stmt {
    ExprStmt(Expr),
    PrintStmt(Expr),
    VarStmt(Token, LType, Option<Expr>),
    LetStmt(Token, LType, Expr),
    FnStmt(Option<String>, Token, Vec<NameTypePair>, LType, Vec<Stmt>), // Not all functions are named
//    CurriedFn(Token, Vec<NameTypePair>, LType, Vec<Stmt>),
    Curried(Option<String>, Token, NameTypePair, Box<Stmt>) // Name, Some relevant token, param:type pair, return
}

impl Display for Stmt {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            ExprStmt(expr) => write!(f, "ExprStmt(\n\t{}\n)", expr),
            PrintStmt(expr) => write!(f, "Print(\n\t{}\n)", expr),
            VarStmt(name, ltype, Some(expr)) => write!(f, "var {}: {} <- {}", name.lexeme, ltype, expr),
            VarStmt(name, ltype, None) => write!(f, "var {}: {}", name.lexeme, ltype),
            LetStmt(name, ltype, expr) => write!(f, "var {}: {} <- {})", name.lexeme, ltype, expr),
//            CurriedFn(, name, params, ret, body) => write!(f, "fn {} = {} : {} {{\n{}}}", name.lexeme, format_args(params, " => "), ret, format_block(body)),
            FnStmt(name, _, params, ret, body) => match name {
                Some(name) => write!(f, "fn {} ({}) -> {} {{\n{}}}", name, format_args(params, ", "), ret, format_block(body)),
                None => write!(f, "fn ({}) -> {} {{\n{}}}", format_args(params, ", "), ret, format_block(body)),
            }
            Curried(name, _, nt, ret) => match name {
                Some(name) => write!(f, "cfn {} {} => {}", name, nt, ret),
                None => write!(f, "cfn {} => {}", nt, ret)
            }
        }
    }
}

fn format_args(args: &Vec<NameTypePair>, sep: &str) -> String {
    join(args, sep)
}

fn format_block(statements: &Vec<Stmt>) -> String {
    join(statements, ";\n")
}