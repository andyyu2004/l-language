use crate::parsing::{Expr, Stmt};
use crate::lexing::TokenType;
use crate::types::{LType, LTypeError};
use crate::types::l_types::LType::{TBool, TNum};

pub fn type_of(expr: &Expr) -> Result<LType, LTypeError> {
    match expr {
        Expr::Literal(_, _) => type_of_literal(expr),
        Expr::Binary(_, left, right) => {
            let ltype = type_of(left)?;
            if type_of(right)? == ltype {
                Ok(ltype)
            } else {
                Err(LTypeError::new("Invalid type in binary".to_string()))
            }
        }
        _ => unimplemented!("Unsupported")
    }
}

fn type_of_literal(expr: &Expr) -> Result<LType, LTypeError> {
    if let Expr::Literal(x, _) = expr {
        match x.ttype {
            TokenType::True | TokenType::False => Ok(TBool),
            TokenType::Number => Ok(TNum),
            _ => unreachable!()
        }
    } else {
        unreachable!()
    }
}

pub fn type_check(statements: Vec<Stmt>) -> Result<(), Vec<LTypeError>> {
    let mut errors = Vec::<LTypeError>::new();
    for ref statement in statements {
        if let Err(err) = type_check_statement(statement) {
            errors.push(err);
        }
    }
    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

fn type_check_statement(stmt: &Stmt) -> Result<(), LTypeError> {
    match stmt {
        Stmt::PrintStmt(expr) => type_check_expression(expr),
        _ => Ok(())
    }
}

fn type_check_expression(expr: &Expr) -> Result<(), LTypeError> {
    match expr {
        Expr::Binary(token, ref left, ref right) => if
            type_of(left)? == type_of(right)? {
            Ok(())
        } else {
            Err(LTypeError::new("Mismatched types in binary expression".to_string()))
        },
        Expr::Literal(_, _) => Ok(()),
        _ => unimplemented!()
    }
}





