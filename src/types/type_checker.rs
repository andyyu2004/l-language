use crate::parsing::{Expr, Stmt};
use crate::lexing::{TokenType, Token};
use crate::types::{LType, LTypeError};
use crate::types::l_types::LType::{TBool, TNum, TArrow, TTuple};
use crate::types::l_types::NameTypePair;
use crate::parsing::expr::Expr::{EBinary, ELiteral, EVariable, ETuple, EApplication, EAssignment};
use crate::interpreting::Env;
use crate::types::LTypeError::{TypeError, NonFunction};
use crate::parsing::stmt::Stmt::{PrintStmt, FnStmt, VarStmt, LetStmt, FnCurried, ExprStmt};
use std::env::var;
use crate::errors::LError;
use crate::lexing::token::TokenType::{Greater, GreaterEqual, Caret, Slash, Plus, Star, LessEqual, Less, Minus};
use std::process::exit;

pub struct TypeChecker {
    env: Env<LType>
}

impl TypeChecker {

    pub fn new() -> TypeChecker {
        TypeChecker {
            env: Env::new(None)
        }
    }

}

impl TypeChecker {

    pub fn type_of_expr(&self, expr: &Expr) -> Result<LType, LTypeError> {
        match expr {
            ELiteral(token) => TypeChecker::type_of_literal(token),
            EBinary { operator, left, right } => self.type_of_binary(operator, left, right),
            EVariable { name,.. } => Ok(self.env.resolve(&name).unwrap()), // Static analysis should make unwrap ok
            EApplication { token, callee, arg} => self.type_of_curry_application(token, callee, arg),
            EAssignment { lvalue, expr } => self.type_of_assignment(lvalue, expr),
            ETuple(xs) => self.type_of_tuple(xs),
            x => unimplemented!("Unsupported: {}", x)
        }
    }

    fn type_of_curry_application(&self, token: &Token, callee: &Expr, arg: &Expr) -> Result<LType, LTypeError> {
        let tcallee = self.type_of_expr(callee)?;
        let targ = self.type_of_expr(arg)?;
        if let TArrow(tparam, tret) = tcallee {
            if targ != *tparam { Err(TypeError(*tparam, targ, token.clone())) }
            else { Ok(*tret) }
        } else {
            Err(NonFunction(tcallee, token.clone()))
        }
    }

    fn type_of_assignment(&self, lvalue: &Token, expr: &Expr) -> Result<LType, LTypeError> {
        let tlvalue = self.env.resolve(lvalue).unwrap();
        let texpr = self.type_of_expr(expr)?;
        if tlvalue == texpr { Ok(tlvalue) }
        else { Err(TypeError(tlvalue, texpr, lvalue.clone())) }
    }

    fn type_of_binary(&self, operator: &Token, left: &Expr, right: &Expr) -> Result<LType, LTypeError> {
        let num_ops = vec![Greater, GreaterEqual, Less, LessEqual, Star, Plus, Minus, Slash, Caret];
        let tleft = self.type_of_expr(left)?;
        let tright = self.type_of_expr(right)?;
        if num_ops.contains(&operator.ttype) {
            if tleft != TNum { return Err(TypeError(TNum, tleft, operator.clone())) }
            else if tright != TNum { return Err(TypeError(TNum, tright, operator.clone())) }
            else { return Ok(TNum) }
        }
        if tleft == tright { Ok(tleft) }
        else { Err(LTypeError::TypeMismatch(tleft, tright, operator.clone())) }
    }

//    fn type_of_application(&self, token: &Token, callee: &Expr, args: &Vec<Expr>) -> Result<LType, LTypeError> {
//        let tcallee = self.type_of_expr(callee)?;
//        let mut vargs = args.iter().map(|x| self.type_of_expr(x)).collect::<Result<Vec<LType>, LTypeError>>()?;
//        if let TArrow(tparams, tret) = tcallee.clone() {
//            if let TArrow(_, _) = *tret {
//                // Curried function application
//                self.type_of_curried_application(token, &tcallee, &mut vargs)
//            } else {
//                // Non-curried function application
//                let targs = TTuple(vargs);
//                if *tparams != targs { Err(TypeError(*tparams, targs, token.clone())) }
//                else { Ok(*tret) }
//            }
//        } else {
//            Err(NonFunction(tcallee, token.clone()))
//        }
//    }
//
//    fn type_of_curried_application(&self, token: &Token, ltype: &LType, args: &mut Vec<LType>) -> Result<LType, LTypeError> {
//        // Assume there are not too many arguments due to static checker
//        if let TArrow(t, ts) = ltype {
//            if args.len() == 0 { // Applied all the given arguments
//                Ok(ltype.clone())
//            } else if **t != args[0] {
//                Err(TypeError(*t.clone(), args[0].clone(), token.clone()))
//            } else {
//                args.remove(0);
//                self.type_of_curried_application(token, ts, args)
//            }
//        } else {
//            Ok(ltype.clone())
//        }
//    }

    fn type_of_tuple(&self, xs: &Vec<Expr>) -> Result<LType, LTypeError> {
        let v = xs.iter().map(|x| self.type_of_expr(x)).collect::<Result<Vec<LType>, LTypeError>>()?;
        Ok(TTuple(v))
    }

    fn type_of_literal(x: &Token) -> Result<LType, LTypeError> {
        match x.ttype {
            TokenType::True | TokenType::False => Ok(TBool),
            TokenType::Number => Ok(TNum),
            _ => unreachable!()
        }
    }

    pub fn type_check(&mut self, statements: &Vec<Stmt>) -> Result<(), Vec<LTypeError>> {
        let mut errors = Vec::<LTypeError>::new();
        for statement in statements {
//        if let Err(err) = type_check_statement(statement) {
//            errors.push(err);
//        }
            match self.type_of_statement(statement) {
                Err(err) => errors.push(err),
                Ok(t) => println!("Type: {}", t)
            }
        }
        if errors.is_empty() { Ok(()) } else { Err(errors) }
    }

    fn type_of_statement(&mut self, stmt: &Stmt) -> Result<LType, LTypeError> {
        match stmt {
            PrintStmt(expr) | ExprStmt(expr) => self.type_of_expr(expr),
            FnStmt { name, token, params, ret_type, .. } => Ok(self.type_of_fn(name, params, ret_type)),
            FnCurried { name, param, ret, .. } => self.type_of_curried_fn(name, param, ret),
            VarStmt { name, ltype, init }  => self.type_of_var(name, ltype, init.as_ref()),
            LetStmt { name, ltype, init } => self.type_of_var(name, ltype, Some(init)),
            _ => panic!("Unimplented in type_check_stmt")
        }
    }

    fn type_of_var(&mut self, token: &Token, ltype: &LType, init: Option<&Expr>) -> Result<LType, LTypeError> {
        if let Some(expr) = init {
            let t_init = self.type_of_expr(expr)?;
            if ltype != &t_init { return Err(TypeError(ltype.clone(), t_init, token.clone())) }
        }
        self.env.define(token.lexeme.clone(), ltype.clone());
        Ok(ltype.clone())

    }

    fn type_of_fn(&mut self, name: &Option<String>, ntpair: &Vec<NameTypePair>, ret: &LType) -> LType {
        let paramtype: Vec<LType> = ntpair.iter().map(|x| x.ltype.clone()).collect();
        let ftype = TArrow(Box::new(TTuple(paramtype)), Box::new(ret.clone()));
        if let Some(name) = name {
            self.env.define(name.clone(), ftype.clone());
        }
        ftype
    }

    fn type_of_curried_fn(&mut self, name: &Option<String>, ntpair: &NameTypePair, ret: &Stmt) -> Result<LType, LTypeError> {
        let ftype = TArrow(Box::new(ntpair.ltype.clone()), Box::new(self.type_of_statement(ret)?));
        if let Some(name) = name {
            self.env.define(name.clone(), ftype.clone());
        }
        Ok(ftype)
    }

//    fn type_of_curried_fn(&mut self, token: &Token, ntpair: &Vec<NameTypePair>, ret: &LType) -> LType {
//        let types = ntpair.iter().map(|x| x.ltype.clone()).collect::<Vec<LType>>();
//        let ftype = types.iter().rev().fold(ret.clone(), |acc,x| TArrow(Box::new(x.clone()), Box::new(acc)));
//        self.env.define(token.lexeme.clone(), ftype.clone());
//        ftype
//    }

    pub fn get_type(&self, name: &str) -> Option<&LType> {
        self.env.resolve_str(name)
    }

}
