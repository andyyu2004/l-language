use std::fmt::{Display, Error, Formatter};

use crate::lexing::Token;
use itertools::{join};
use crate::parsing::expr::Expr::{EBinary, EUnary, EVariable, ELiteral, ETuple, EApplication, EAssignment, EBlock, EIf, ERecord, ELogic, EGet, ESet, EDataConstructor, EVariant, EMatch, EIfLet};
use crate::parsing::Stmt;
use crate::parsing::stmt::format_block;
use std::collections::HashMap;
use crate::types::LType;
use crate::interpreting::pattern_matching::LPattern;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    EBinary { operator: Token, left: Box<Expr>, right: Box<Expr> },
    EUnary { operator: Token, operand: Box<Expr> },
    ELogic { operator: Token, left: Box<Expr>, right: Box<Expr> },
    EVariable { name: Token },
    EMatch { token: Token, expr: Box<Expr>, branches: Vec<(LPattern, Expr)>}, // Use vec as order is important
    EDataConstructor { name: Token },
    EBlock(Vec<Stmt>),
    EGet { name: Token, expr: Box<Expr> }, // .
//    EGetS { name: Token, expr: Box<Expr> }, ::
    ESet { name: Token, expr: Box<Expr>, value: Box<Expr> },
    EAssignment { lvalue: Token, expr: Box<Expr> },
    ELiteral(Token),
    EApplication { token: Token, callee: Box<Expr>, arg: Box<Expr> }, // Curried
    ETuple(Token, Vec<Expr>),
    ERecord(Token, HashMap<String, Expr>),
    EIf { token: Token, condition: Box<Expr>, left: Box<Expr>, right: Box<Expr> },
    // Using Vector for left as it may require variable insertion into env and block evaluation builds empty env automatically
    EIfLet { token: Token, pattern: LPattern, scrutinee: Box<Expr>, left: Vec<Stmt>, right: Box<Expr> },
    EVariant(String, Box<Expr>),
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            EBinary { operator, left, right } => write!(f, "{}{{{}}}{{{}}}", operator.lexeme, left, right),
            EUnary { operator, operand } => write!(f, "{}{{{}}}", operator.lexeme, operand),
            EVariable { name } => write!(f, "var {}", name.lexeme),
            EDataConstructor { name } => write!(f, "{}", name.lexeme),
            ELiteral(x) => write!(f, "{}", x.lexeme),
            EBlock(xs) => write!(f, "{{{}}}", format_block(xs)),
            ETuple(_, xs) => write!(f, "({})", format_tuple(xs)),
            EGet{ name, expr } => write!(f, "{}.{}", expr, name.lexeme),
            ESet{ name, expr, value } => write!(f, "{}.{} = {}", expr, name.lexeme, value),
            ERecord(_, xs) => write!(f, "record {{{}}}", format_record(xs, ", ")),
            EAssignment { lvalue, expr} => write!(f, "{} = {}", lvalue, expr),
            EApplication { callee, arg, .. } => write!(f, "{} {}", callee, arg),
            ELogic { operator, left, right } => write!(f, "{}{{{}}}{{{}}}", operator, left, right),
            EVariant(name, expr) => write!(f, "variant {}: {}", name, expr),
            EMatch { expr, branches,.. } => write!(f, "match {} {{{}}}", expr, format_paired_vec(branches, " | ")),
            EIf { token, condition, left, right } => match **right {
                EBlock(ref xs) if !xs.is_empty() => write!(f, "if {} then {} else {}", condition, left, right),
                _ => write!(f, "if {} then {}", condition, left),
            },
            EIfLet { token, pattern, scrutinee, left, right } => match **right {
                EBlock(ref xs) if !xs.is_empty() => write!(f, "if let {} = {} then {} else {}", pattern, scrutinee, format_block(left), right),
                _ => write!(f, "if let {} = {} then {}", pattern, scrutinee, format_block(left)),
            },
            x => write!(f, "{:?}", x)
        }
    }
}

pub fn format_tuple<T>(args: &Vec<T>) -> String where T : Display {
    join(args, ", ")
}

pub fn format_paired_vec<T, U>(xs: &Vec<(T, U)>, sep: &str) -> String where T : Display, U : Display {
    let mut string = String::new();
    for (i, (s, x)) in xs.iter().enumerate() {
        if i < xs.len() - 1 {
            string.push_str(&format!("{} -> {}{}", s, x, sep))
        } else {
            string.push_str(&format!("{} -> {}", s, x))
        }

    }
    string
}

// what trait defines iter to share this code?
pub fn format_record<T>(xs: &HashMap<String, T>, sep: &str) -> String where T : Display {
    let mut string = String::new();
    for (i, (s, x)) in xs.iter().enumerate() {
        if i < xs.len() - 1 {
            string.push_str(&format!("{}: {}{}", s, x, sep))
        } else {
            string.push_str(&format!("{}: {}", s, x))
        }

    }
    string
}


