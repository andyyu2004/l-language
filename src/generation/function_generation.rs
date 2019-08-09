use crate::interpreting::InterpreterError;
use crate::types::LType;
use crate::types::l_types::LType::TArrow;
use crate::parsing::{Stmt, Expr};
use crate::errors::LError;
use crate::interpreting::interpreter_error::InterpreterError::Error;
use crate::parsing::stmt::Stmt::{FnCurried, FnStmt, LStmt};
use crate::lexing::{Token, TokenType};
use crate::types::l_types::Pair;
use crate::parsing::expr::Expr::ELiteral;

pub fn generate_function_from_type(ltype: &LType, ret: Expr) -> Result<Stmt, InterpreterError> {
    if let TArrow(left, right) = ltype {
        match **right {
            TArrow(_, _) => Ok(FnCurried {
                    name: None,
                    token: Token::dummy(),
                    param: Pair::new("x".to_string(), *left.clone()),
                    ret: Box::new(generate_function_from_type(right, ret)?)
            }),
            ref x => Ok(FnStmt {
                name: None,
                token: Token::dummy(),
                params: vec![Pair::new("y".to_string(), *left.clone())],
                ret_type: x.clone(),
                body: vec![LStmt(ret)]
            })
        }
    } else { Ok(LStmt(ret)) }

}