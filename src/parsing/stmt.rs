use std::fmt::{Display, Error, Formatter};

use crate::parsing::Expr;
use crate::types::LType;
use crate::lexing::Token;
use itertools::join;
use crate::types::l_types::Pair;
use crate::parsing::stmt::Stmt::{ExprStmt, LStmt, VarStmt, FnStmt, LetStmt, FnCurried, ReturnStmt, PrintStmt, TypeAlias, WhileStmt, StructDecl, DataDecl};
use std::collections::HashMap;
use crate::parsing::expr::format_record;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    ExprStmt(Expr),
    LStmt(Expr), // Expr result not coerced to LUnit like exprstmt
    PrintStmt(Expr),
    WhileStmt { token: Token, condition: Expr, body: Vec<Stmt> },
    TypeAlias { name: Token, ltype: LType },
    ReturnStmt { token: Token, value: Option<Expr> },
    VarStmt { name: Token, ltype: Option<LType>, init: Option<Expr> },
    LetStmt { name: Token, ltype: Option<LType>, init: Expr },
    FnStmt { name: Option<String>, token: Token, param: Option<Pair<LType>>, ret_type: LType, body: Vec<Stmt> },
    StructDecl { name: Token, fields: HashMap<String, LType> },
    FnCurried { name: Option<String>, token: Token, param: Pair<LType>, ret: Box<Stmt> },
    DataDecl { name: Token, variants: HashMap<String, LType> },
}

impl Display for Stmt {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            ExprStmt(expr) => write!(f, "ExprStmt({})", expr),
            PrintStmt(expr) => write!(f, "Print({})", expr),
            LStmt(expr) => write!(f, "LStmt({})", expr),
            VarStmt { name, ltype, init } if init.is_some() => write!(f, "var {} <- {}", name.lexeme, init.as_ref().unwrap()),
            VarStmt { name, ltype, .. } => write!(f, "var {}", name.lexeme),
            LetStmt { name, ltype, init } => write!(f, "var {} <- {}", name.lexeme, init),
            WhileStmt { condition, body, .. } => write!(f, "while {} {}", condition, format_block(body)),
//            CurriedFn(, name, params, ret, body) => write!(f, "fn {} = {} : {} {{\n{}}}", name.lexeme, format_args(params, " => "), ret, format_block(body)),
            FnStmt { name, param, ret_type, body, .. } => match name {
                Some(name) => write!(f, "fn {} ({}) -> {} {{{}}}", name, format_option(param), ret_type, format_block(body)),
                None => write!(f, "fn ({}) -> {} {{{}}}",format_option(param), ret_type, format_block(body)),
            }
            FnCurried { name, param, ret, .. } => match name {
                Some(name) => write!(f, "cfn {} {} => {}", name, param, ret),
                None => write!(f, "cfn {} => {}", param, ret)
            },
            ReturnStmt { value, .. } => match value {
                Some(value) => write!(f, "return {}", value),
                None => write!(f, "return;")
            },
            TypeAlias { name, ltype } => write!(f, "type {} = {}", name, ltype),
            StructDecl { name, fields } => write!(f, "struct {} {{{}}}", name, format_record(fields, ", ")),
            DataDecl { name, variants } => write!(f, "data {} = {}", name, format_record(variants, " | ")),
            x => write!(f, "data {:?}", x)
        }
    }
}

pub fn format_option<T>(arg: &Option<T>) -> String where T : Display {
    match arg {
        Some(x) => format!("{}", x),
        None => "".to_string()
    }
}

pub fn format_args<T>(args: &Vec<Pair<T>>, sep: &str) -> String where T : Display {
    join(args, sep)
}

pub fn format_block(statements: &Vec<Stmt>) -> String {
    join(statements, "; \n")
}