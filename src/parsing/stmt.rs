use std::fmt::{Display, Error, Formatter};

use crate::parsing::Expr;
use crate::types::LType;
use crate::lexing::Token;
use itertools::join;
use crate::types::l_types::NameTypePair;
use crate::parsing::stmt::Stmt::{ExprStmt, LStmt, VarStmt, FnStmt, LetStmt, FnCurried, ReturnStmt, PrintStmt};

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    ExprStmt(Expr),
    LStmt(Expr), // Expr result not coerced to LUnit like exprstmt
    PrintStmt(Expr),
    ReturnStmt { token: Token, value: Option<Expr> },
    VarStmt { name: Token, ltype: LType, init: Option<Expr> },
    LetStmt { name: Token, ltype: LType, init: Expr },
    FnStmt { name: Option<String>, token: Token, params: Vec<NameTypePair>, ret_type: LType, body: Vec<Stmt> },
    FnCurried { name: Option<String>, token: Token, param: NameTypePair, ret: Box<Stmt> } // Name, Some relevant token, param:type pair, return
}

impl Display for Stmt {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            ExprStmt(expr) => write!(f, "ExprStmt(\n\t{}\n)", expr),
            PrintStmt(expr) => write!(f, "Print(\n\t{}\n)", expr),
            LStmt(expr) => write!(f, "LStmt(\n\t{}\n)", expr),
            VarStmt { name, ltype, init } if init.is_some() => write!(f, "var {}: {} <- {}", name.lexeme, ltype, init.as_ref().unwrap()),
            VarStmt { name, ltype, .. } => write!(f, "var {}: {}", name.lexeme, ltype),
            LetStmt { name, ltype, init } => write!(f, "var {}: {} <- {})", name.lexeme, ltype, init),
//            CurriedFn(, name, params, ret, body) => write!(f, "fn {} = {} : {} {{\n{}}}", name.lexeme, format_args(params, " => "), ret, format_block(body)),
            FnStmt { name, params, ret_type, body, .. } => match name {
                Some(name) => write!(f, "fn {} ({}) -> {} {{\n{}}}", name, format_args(params, ", "), ret_type, format_block(body)),
                None => write!(f, "fn ({}) -> {} {{\n{}}}", format_args(params, ", "), ret_type, format_block(body)),
            }
            FnCurried { name, param, ret, .. } => match name {
                Some(name) => write!(f, "cfn {} {} => {}", name, param, ret),
                None => write!(f, "cfn {} => {}", param, ret)
            },
            ReturnStmt { value, .. } => match value {
                Some(value) => write!(f, "return {};", value),
                None => write!(f, "return;")
            }
        }
    }
}

pub fn format_args(args: &Vec<NameTypePair>, sep: &str) -> String {
    join(args, sep)
}

pub fn format_block(statements: &Vec<Stmt>) -> String {
    join(statements, ";\n")
}