use crate::interpreting::InterpreterError;
use crate::types::LType;
use crate::types::l_types::LType::TArrow;
use crate::parsing::{Stmt, Expr};
use crate::parsing::stmt::Stmt::*;
use crate::lexing::{Token};
use crate::types::l_types::Pair;
use crate::parsing::expr::Expr::{EVariant};
use itertools::Itertools;

//pub fn generate_function_from_type(ltype: &LType, ret: Expr) -> Result<Stmt, InterpreterError> {
//    if let TArrow(left, right) = ltype {
//        match **right {
//            TArrow(_, _) => Ok(FnCurried {
//                    name: None,
//                    token: Token::dummy(),
//                    param: Pair::new("x".to_string(), *left.clone()),
//                    ret: Box::new(generate_function_from_type(right, ret)?)
//            }),
//            ref x => Ok(FnStmt {
//                name: None,
//                token: Token::dummy(),
//                params: vec![Pair::new("y".to_string(), *left.clone())],
//                ret_type: x.clone(),
//                body: vec![LStmt(ret)]
//            })
//        }
//    } else { Ok(LStmt(ret)) }
//
//}

pub struct Generator {
    vars: Vec<String>,
    used: Vec<String>,
    fn_name: String,
    is_first_iter: bool
}

impl Generator {

    // Use once struct
    pub fn new(fn_name: String) -> Generator {
        Generator {
            fn_name,
            vars: "abcdefghijklmnopqrstuvwxyzABCDEFGIJKLMNOPQRSTUVWXYZ".chars().map(|x| x.to_string()).collect(),
            used: vec![],
            is_first_iter: true
        }
    }

    fn get_var(&mut self) -> String {
        let name = self.vars.pop().unwrap();  // Just panic for now, probably not going to use an adt with 52 fields anyway
        self.used.push(name.clone());
        name
    }

    pub fn generate_function_from_type(&mut self, ltype: &LType) -> Result<Stmt, InterpreterError> {
        if let TArrow(left, right) = ltype {
            match **right {
                TArrow(_, _) => Ok(FnCurried {
                    name: if self.is_first_iter { Some(self.fn_name.clone()) } else { None },
                    token: Token::dummy(),
                    param: Pair::new(self.get_var(), *left.clone()),
                    ret: Box::new(self.generate_function_from_type(right)?)
                }),
                ref x => Ok(FnStmt {
                    name: if self.is_first_iter { Some(self.fn_name.clone()) } else { None },
                    token: Token::dummy(),
                    param: Some(Pair::new(self.get_var(), *left.clone())),
                    ret_type: x.clone(),
                    body: vec![
                        LStmt(EVariant(
                            self.fn_name.clone(),
                            self.used.iter().map(|x| Expr::EVariable {
                                name: Token::name(x.clone())
                            }).collect_vec()
                        ))
                    ]
                })
            }
        } else { panic!("Function generator giving non function type") }

    }
}
