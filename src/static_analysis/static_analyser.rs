use crate::parsing::{Stmt, Expr};
use crate::errors::LError;
use crate::parsing::stmt::Stmt::*;
use crate::interpreting::{Env, LPattern};
use crate::parsing::expr::Expr::*;
use crate::static_analysis::StaticInfo;
use crate::static_analysis::StaticInfo::*;
use crate::lexing::Token;
use crate::types::l_types::{Pair, TypeName};
use crate::types::LType;
use std::collections::HashMap;
use itertools::Itertools;
use std::rc::Rc;
use std::cell::RefCell;
use crate::interpreting::pattern_matching::LPattern::*;
use crate::types::l_types::LType::TVar;

enum FunctionEnv {
//    None,
    Function
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
            LetStmt{ token, pattern , init, ..} => self.analyse_let_binding(token, pattern, init),
            FnStmt { name, param, body, tparams, token, ..} =>
                self.analyse_fn_decl(token, name, param, body, tparams),
            FnCurried{ name, param, ret, tparams, token, ..} =>
                self.analyse_curried_fn_decl(token, name, param, ret, tparams),
            ReturnStmt { token, value } => self.analyse_return_stmt(token, value),
            WhileStmt { condition, body, ..} => {
                self.analyse_expr(condition)?;
                self.analyse_block_e(body)
            }
            TypeAlias {..} => Ok(IEmpty),
            DataDecl { name, variants, .. } => self.analyse_data_decl(name, variants),
            StructDecl { name, fields} => self.analyse_struct(name, fields),
            x => unimplemented!("Unimplemented in analyse stmt {}", x)
        }
    }

    fn analyse_data_decl(&self, _name: &TypeName, variants: &HashMap<String, LType>) -> Result<StaticInfo, LError> {
        for (k, v) in variants {
            self.env.borrow_mut().define(k.clone(), IConstructor(v.curried_arity()));
        }
        Ok(IEmpty)
    }

    fn analyse_struct(&self, _name: &TypeName, _fields: &HashMap<String, LType>) -> Result<StaticInfo, LError> {
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
            EMatch { .. } => panic!("match should be desugared"), //self.analyse_match(token, expr, branches),
            ELogic { operator, left, right} | EBinary { operator, left, right } =>
                self.analyse_binary(operator, left, right),
            ELambda { token, param, body, .. } => self.analyse_lambda(token, param, body),
            _ => Ok(IEmpty)
        }
    }

    fn analyse_data_constructor(&self, name: &Token) -> Result<StaticInfo, LError> {
        match self.env.borrow().resolve(name) {
            Ok(x) => Ok(x),
            Err(_) => Err(LError::from_token(format!("Undefined Data Constructor {}", name.lexeme), name))
        }
    }

    fn analyse_lambda(&mut self, token: &Token, param: &LPattern, body: &Expr) -> Result<StaticInfo, LError> {
        if param.is_refutable() {
            return Err(LError::from_token("Cannot use refutable pattern in lambda binding".to_string(), token))
        }
        let bindings = self.get_pattern_bindings(param)?;
        let enclosing = Rc::clone(&self.env);
        self.env = Rc::new(RefCell::new(Env::new(Some(Rc::clone(&self.env)))));
        for (k, v) in bindings {
            self.env.borrow_mut().define(k, v);
        }
        let res = self.analyse_expr(body);
        self.env = enclosing;
        res
    }

    fn analyse_application(&mut self, callee: &Expr, arg: &Expr) -> Result<StaticInfo, LError> {
        self.analyse_expr(arg)?;
        self.analyse_expr(callee)
    }

//    fn analyse_match(&mut self, token: &Token, expr: &Expr, branches: &Vec<(LPattern, Expr)>) -> Result<StaticInfo, LError> {
//        if branches.is_empty() {
//            return Err(LError::from_token("Empty match statement".to_string(), token))
//        }
//        self.analyse_expr(expr)?;
//        let enclosing = Rc::clone(&self.env);
//        self.env = Rc::new(RefCell::new(Env::new(Some(Rc::clone(&self.env)))));
//        branches.iter().map(|(_, x)| self.analyse_expr(x)).collect::<Result<Vec<_>, LError>>()?;
//        self.env = enclosing;
//        Ok(IEmpty)
//    }

    fn analyse_if_let(&mut self, _token: &Token, pattern: &LPattern, scrutinee: &Expr, left: &Vec<Stmt>, right: &Expr) -> Result<StaticInfo, LError> {
        if !pattern.is_refutable() {
//            return Err(LError::from_token(format!("Cannot")))
        }
        self.analyse_expr(scrutinee)?;
        let bindings = self.get_pattern_bindings(pattern)?;
        let mut env = Env::new(Some(Rc::clone(&self.env)));
        for (k, v) in bindings { env.define(k,  v); }
        self.analyse_block(left, Rc::new(RefCell::new(env)))?;
        self.analyse_expr(right)
    }

    fn analyse_let_binding(&mut self, token: &Token, pattern: &LPattern, init: &Expr) -> Result<StaticInfo, LError> {
        if pattern.is_refutable() {
            return Err(LError::from_token("Cannot bind to a refutable pattern".to_string(), token))
        }
        self.analyse_expr(init)?;
        let bindings = self.get_pattern_bindings(pattern)?;
        for (k, v) in bindings {
            self.env.borrow_mut().define(k, v);
        }
        Ok(IEmpty)
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
            PLiteral(_) => Ok(vec![]),
            PIdentifier(x) => Ok(vec![(x.lexeme.clone(), ILetBinding)]), // Pattern bindings are immutable
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

    fn analyse_record(&mut self, _token: &Token, xs: &HashMap<String, Expr>) -> Result<StaticInfo, LError> {
        for x in xs.values() {
            self.analyse_expr(&x)?;
        }
        Ok(IEmpty)
    }

    fn analyse_get_expr(&mut self, _name: &Token, expr: &Expr) -> Result<StaticInfo, LError> {
        // Expr must be gettable, not sure how to define that yet
        self.analyse_expr(expr)
    }

    fn analyse_set_expr(&mut self, _name: &Token, expr: &Expr, value: &Expr) -> Result<StaticInfo, LError> {
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

    fn analyse_fn_decl(&mut self, token: &Token, name: &Option<String>, param: &Option<Pair<LType>>, body: &Vec<Stmt>, tparams: &Vec<Token>) -> Result<StaticInfo, LError> {
//        if let Some(Pair { value: TVar(t), .. }) = &param {
//            if !tparams.contains(t) {
//                return Err(LError::from_token(format!("Type parameter {} was not declared in function signature", t), token))
//            }
//        }
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

    fn analyse_curried_fn_decl(&mut self, token: &Token, name: &Option<String>, param: &Pair<LType>, ret: &Stmt, tparams: &Vec<Token>) -> Result<StaticInfo, LError> {
//        if let TVar(t) = &param.value {
//            if !tparams.contains(t) {
//                return Err(LError::from_token(format!("Type parameter {} was not declared in function signature", t), token))
//            }
//        }

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

    fn analyse_binary(&mut self, _token: &Token, left: &Expr, right: &Expr) -> Result<StaticInfo, LError> {
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

