use crate::parsing::{Stmt, Expr};
use crate::errors::LError;
use crate::parsing::stmt::Stmt::{LStmt, ExprStmt, VarStmt, LetStmt, FnStmt, FnCurried, ReturnStmt, PrintStmt, TypeAlias, WhileStmt, StructDecl, DataDecl};
use crate::interpreting::{Env, LPattern};
use crate::parsing::expr::Expr::{EVariable, EApplication, EAssignment, EBlock, EIf, ERecord, ELogic, EBinary, EGet, ETuple, ESet, EDataConstructor, EMatch, EIfLet, EList};
use crate::static_analysis::StaticInfo;
use crate::static_analysis::static_info::StaticInfo::{IVariable, ILetBinding, IFunction, IEmpty, IConstructor};
use crate::lexing::Token;
use crate::types::l_types::Pair;
use crate::types::LType;
use std::collections::HashMap;
use itertools::Itertools;
use std::rc::Rc;
use std::cell::RefCell;
use crate::interpreting::pattern_matching::LPattern::*;

enum FunctionEnv {
    None, Function
}

// Check validity of code
// Unintialized and undefined variables

pub struct StaticAnalyser {
    env: Rc<RefCell<Env<StaticInfo>>>,
    fstack: Vec<FunctionEnv>
}

impl StaticAnalyser {
    pub fn new() -> StaticAnalyser {
        StaticAnalyser { env: Rc::new(RefCell::new(Env::new(None))), fstack: Vec::new() }
    }
}

impl StaticAnalyser {

    pub fn analyse(&mut self, statements: &Vec<Stmt>) -> Result<(), Vec<LError> >{
        let mut errors = Vec::<LError>::new();
        for stmt in statements {
            if let Err(err) = self.analyse_stmt(stmt) {
                errors.push(err);
            }
        }
        if errors.is_empty() { Ok(()) }
        else { Err(errors) }
    }

    fn analyse_stmt(&mut self, stmt: &Stmt) -> Result<StaticInfo, LError> {
        match stmt {
            LStmt(expr) | ExprStmt(expr) | PrintStmt(expr) => self.analyse_expr(expr),
            VarStmt { name , init, .. } => self.analyse_var_decl(name, init),
            LetStmt{ name , init, ..} => self.analyse_let_binding(name, init),
            FnStmt { name, param, body, ..} => self.analyse_fn_decl(name, param, body),
            FnCurried{ name, param, ret, ..} => self.analyse_curried_fn_decl(name, param, ret),
            ReturnStmt { token, value } => self.analyse_return_stmt(token, value),
            WhileStmt { condition, body, ..} => {
                self.analyse_expr(condition)?;
                self.analyse_block_e(body)
            }
            TypeAlias {..} => Ok(IEmpty),
            DataDecl { name, variants } => self.analyse_data_decl(name, variants),
            StructDecl { name, fields} => self.analyse_struct(name, fields),
            x => unimplemented!("Unimplemented in analyse stmt {}", x)
        }
    }

    fn analyse_data_decl(&self, name: &Token, variants: &HashMap<String, LType>) -> Result<StaticInfo, LError> {
        for (k, v) in variants {
            self.env.borrow_mut().define(k.clone(), IConstructor(v.curried_arity()));
        }
        Ok(IEmpty)
    }

    fn analyse_struct(&self, name: &Token, fields: &HashMap<String, LType>) -> Result<StaticInfo, LError> {
        Ok(IEmpty)
    }

    fn analyse_expr(&mut self, expr: &Expr) -> Result<StaticInfo, LError> {
        match expr {
            EVariable { name, .. } => self.analyse_var(name),
            EApplication { callee, arg , .. } => self.analyse_application(callee, arg),
            EDataConstructor { name } => self.analyse_data_constructor(name),
            ERecord(token, xs) => self.analyse_record(token, xs),
            ETuple(_, xs) => { for x in xs { self.analyse_expr(x)?; } ; Ok(IEmpty) },
            EAssignment { lvalue, expr } => self.analyse_assignment(lvalue, expr),
            EBlock(xs) => self.analyse_block_e(xs),
            EIf { condition, left, right, .. } => self.analyse_if(condition, left, right),
            EIfLet { token, pattern, scrutinee, left, right } =>
                self.analyse_if_let(token, pattern, scrutinee, left, right),
            EGet { name, expr } => self.analyse_get_expr(name, expr),
            ESet { name, expr, value} => self.analyse_set_expr(name, expr, value),
            EList(_, xs) => { for x in xs { self.analyse_expr(x)?; } ; Ok(IEmpty) },
            EMatch { token, expr, branches} => self.analyse_match(token, expr, branches),
            ELogic { operator, left, right} | EBinary { operator, left, right } =>
                self.analyse_binary(operator, left, right),
            _ => Ok(IEmpty)
        }
    }

    fn analyse_data_constructor(&self, name: &Token) -> Result<StaticInfo, LError> {
        match self.env.borrow().resolve(name) {
            Ok(x) => Ok(x),
            Err(_) => Err(LError::from_token(format!("Undefined Data Constructor {}", name.lexeme), name))
        }
    }

    fn analyse_application(&mut self, callee: &Expr, arg: &Expr) -> Result<StaticInfo, LError> {
        self.analyse_expr(arg)?;
        self.analyse_expr(callee)
    }

    fn analyse_match(&mut self, token: &Token, expr: &Expr, branches: &Vec<(LPattern, Expr)>) -> Result<StaticInfo, LError> {
        if branches.is_empty() {
            return Err(LError::from_token("Empty match statement".to_string(), token))
        }
        self.analyse_expr(expr)?;
        let enclosing = Rc::clone(&self.env);
        self.env = Rc::new(RefCell::new(Env::new(Some(Rc::clone(&self.env)))));
        branches.iter().map(|(_, x)| self.analyse_expr(x)).collect::<Result<Vec<_>, LError>>()?;
        self.env = enclosing;
        Ok(IEmpty)
    }

    fn analyse_if_let(&mut self, token: &Token, pattern: &LPattern, scrutinee: &Expr, left: &Vec<Stmt>, right: &Expr) -> Result<StaticInfo, LError> {
        self.analyse_expr(scrutinee)?;
        let bindings = self.get_pattern_bindings(pattern)?;
        let mut env = Env::new(Some(Rc::clone(&self.env)));
        for (k, v) in bindings { env.define(k,  v); }
        self.analyse_block(left, Rc::new(RefCell::new(env)))?;
        self.analyse_expr(right)
    }

    // Returns the bindings that would occur if a pattern is matched to allow checking for variables
    fn get_pattern_bindings(&self, pattern: &LPattern) -> Result<Vec<(String, StaticInfo)>, LError> {
        match pattern {
            PVariant(name, p) => if let Some(p) = p {
                self.validate_constructor_arity(name, p)?;
                self.get_pattern_bindings(p)
            } else { Ok(vec![]) },
            PConstructor(l, r) => Ok(self.get_pattern_bindings(l)?.into_iter().chain(self.get_pattern_bindings(r)?).collect_vec()),
            PRecord => Ok(vec![]),
            PTuple(xs) => Ok(xs.iter()
                .flat_map(|x| self.get_pattern_bindings(x))
                .flatten()
                .collect_vec()),
            PLiteral(x) => Ok(vec![]),
            PIdentifier(x) => Ok(vec![(x.lexeme.clone(), IEmpty)]),
            PWildcard => Ok(vec![])
        }
    }

    fn validate_constructor_arity(&self, cname: &Token, pattern: &LPattern) -> Result<(), LError> {
        let constructor_data = self.env.borrow().resolve(cname)?;
        if let IConstructor(arity) = constructor_data {
            let p_arity = pattern.constructor_arity();
//            println!("pattern {}, arity {}", pattern, p_arity);
            if  p_arity != arity {
                Err(LError::from_token(format!("The constructor '{}' expected {} arguments, got {}", cname.lexeme, arity, p_arity), cname))
            } else { Ok(()) }
        } else {
            panic!("Constructor had non constructor information")
        }
    }

    fn analyse_record(&mut self, token: &Token, xs: &HashMap<String, Expr>) -> Result<StaticInfo, LError> {
        for x in xs.values() {
            self.analyse_expr(&x)?;
        }
        Ok(IEmpty)
    }

    fn analyse_get_expr(&mut self, name: &Token, expr: &Expr) -> Result<StaticInfo, LError> {
        // Expr must be gettable, not sure how to define that yet
        self.analyse_expr(expr)
    }

    fn analyse_set_expr(&mut self, name: &Token, expr: &Expr, value: &Expr) -> Result<StaticInfo, LError> {
        self.analyse_expr(expr)?;
        self.analyse_expr(value)
    }

    fn analyse_block_e(&mut self, block: &Vec<Stmt>) -> Result<StaticInfo, LError> {
        let env = Rc::new(RefCell::new(Env::new(Some(Rc::clone(&self.env)))));
        self.analyse_block(block, env)
    }

    fn analyse_block(&mut self, block: &Vec<Stmt>, env: Rc<RefCell<Env<StaticInfo>>>) -> Result<StaticInfo, LError> {
        let enclosing = Rc::clone(&self.env);
        self.env = env;
        block.iter().map(|x| self.analyse_stmt(x)).collect::<Result<Vec<_>, LError>>()?;
        self.env = enclosing;
        Ok(IEmpty)
    }

    fn analyse_return_stmt(&mut self, token: &Token, value: &Option<Expr>) -> Result<StaticInfo, LError> {
        if let Some(expr) = value { self.analyse_expr(expr)?; }
        // Check whether return is within a function
        if self.fstack.is_empty() {
            return Err(LError::from_token("Top level return not permitted".to_string(), token));
        }
        Ok(IEmpty)
    }

    fn analyse_var_decl(&mut self, name: &Token, init: &Option<Expr>) -> Result<StaticInfo, LError> {
        if let Some(init) = init {
            self.analyse_expr(init)?;
        }
//        if self.env.borrow().resolve(name).is_ok() {
//            return Err(LError::from_token(format!("Variable '{}' already in scope", name.lexeme), name))
//        }
        self.env.borrow_mut().define(name.lexeme.clone(), IVariable(init.is_some()));
        Ok(IEmpty)
    }

    fn analyse_let_binding(&mut self, name: &Token, init: &Expr) -> Result<StaticInfo, LError> {
        self.analyse_expr(init)?;
        self.env.borrow_mut().define(name.lexeme.clone(), ILetBinding);
        Ok(IEmpty)
    }

    fn analyse_fn_decl(&mut self, name: &Option<String>, param: &Option<Pair<LType>>, body: &Vec<Stmt>) -> Result<StaticInfo, LError> {
        self.fstack.push(FunctionEnv::Function);
        if let Some(name) = name {
            self.env.borrow_mut().define(name.clone(), IFunction);
        }
        let enclosing = self.env.clone();
        self.env = Rc::new(RefCell::new(Env::new(Some(Rc::clone(&self.env)))));
        if let Some(param) = param {
            self.env.borrow_mut().define(param.name.clone(), IVariable(true))
        }
        for stmt in body {
            self.analyse_stmt(stmt)?;
        }
        self.env = enclosing;
        self.fstack.pop();
        Ok(IEmpty)
    }

    fn analyse_curried_fn_decl(&mut self, name: &Option<String>, param: &Pair<LType>, ret: &Stmt) -> Result<StaticInfo, LError> {
        if let Some(name) = name {
            self.env.borrow_mut().define(name.clone(), IFunction);
        }

        let enclosing = self.env.clone();
        self.env = Rc::new(RefCell::new(Env::new(Some(Rc::clone(&self.env)))));
        let p = param.name.clone();
        self.env.borrow_mut().define(p, IVariable(true));
        let res = self.analyse_stmt(ret);
        self.env = enclosing;
        res
    }

    fn analyse_var(&self, name: &Token) -> Result<StaticInfo, LError> {
        let info = self.env.borrow().resolve(name)?;
        match &info {
            IVariable(isinitialized) => if !isinitialized {
                return Err(LError::from_token(format!("Variable {} is not intialized yet", name.lexeme), name))
            }
            _ => {}
        };
        Ok(info)
    }

    fn analyse_binary(&mut self, token: &Token, left: &Expr, right: &Expr) -> Result<StaticInfo, LError> {
        self.analyse_expr(left)?;
        self.analyse_expr(right)
    }

    fn analyse_if(&mut self, op: &Expr, left: &Expr, right: &Expr) -> Result<StaticInfo, LError> {
        self.analyse_expr(left)?;
        self.analyse_expr(right)?;
        self.analyse_expr(op)
    }

    fn analyse_assignment(&mut self, lvalue: &Token, expr: &Expr) -> Result<StaticInfo, LError> {
//        let info = self.analyse_var(lvalue)?;
        let info = self.env.borrow().resolve(lvalue)?;
        if let ILetBinding = info {
            return Err(LError::from_token(format!("Cannot assign to immutable let binding {}", lvalue.lexeme), lvalue))
        }
        self.analyse_expr(expr)
    }

}

