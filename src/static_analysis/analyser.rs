use crate::parsing::{Stmt, Expr};
use crate::errors::LError;
use crate::parsing::stmt::Stmt::{LStmt, ExprStmt, VarStmt, LetStmt, FnStmt, FnCurried, ReturnStmt, PrintStmt};
use crate::interpreting::{Env};
use crate::parsing::expr::Expr::{EVariable, EApplication, EAssignment, EBlock, EIf};
use crate::static_analysis::StaticInfo;
use crate::static_analysis::static_info::StaticInfo::{IVariable, ILetBinding, IFunction, IEmpty};
use crate::lexing::Token;
use std::borrow::Borrow;
use crate::types::l_types::NameTypePair;

enum FunctionEnv {
    None, Function
}

// Check validity of code
// Unintialized and undefined variables

pub struct Analyser {
    env: Env<StaticInfo>,
    fstack: Vec<FunctionEnv>
}

impl Analyser {
    pub fn new() -> Analyser {
        Analyser { env: Env::new(None), fstack: Vec::new() }
    }
}

impl Analyser {

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
            FnStmt { name, params, body, ..} => self.analyse_fn_decl(name, params, body),
            FnCurried{ name, ret, ..} => self.analyse_curried_fn_decl(name, ret),
            ReturnStmt { token, value } => self.analyse_return_stmt(token, value),
            x => unimplemented!("Unimplemented in analyse stmt {}", x)
        }
    }

    fn analyse_block(&mut self, block: &Vec<Stmt>) -> Result<StaticInfo, LError> {
        block.iter().map(|x| self.analyse_stmt(x)).collect::<Result<Vec<_>, LError>>()?;
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
        self.env.define(name.lexeme.clone(), IVariable(init.is_some()));
        Ok(IEmpty)
    }

    fn analyse_let_binding(&mut self, name: &Token, init: &Expr) -> Result<StaticInfo, LError> {
        self.analyse_expr(init)?;
        self.env.define(name.lexeme.clone(), ILetBinding);
        Ok(IEmpty)
    }

    fn analyse_fn_decl(&mut self, name: &Option<String>, params: &Vec<NameTypePair>, body: &Vec<Stmt>) -> Result<StaticInfo, LError> {
        self.fstack.push(FunctionEnv::Function);
        if let Some(name) = name {
            self.env.define(name.clone(), IFunction);
        }
        let enclosing = self.env.clone();
        self.env = Env::new(Some(self.env.clone()));
        for nt in params {
            self.env.define(nt.name.clone(), IEmpty)
        }
        for stmt in body {
            self.analyse_stmt(stmt)?;
        }
        self.env = enclosing;
        self.fstack.pop();
        Ok(IEmpty)
    }

    fn analyse_curried_fn_decl(&mut self, name: &Option<String>, ret: &Stmt) -> Result<StaticInfo, LError> {
        if let Some(name) = name {
            self.env.define(name.clone(), IFunction);
        }
        self.analyse_stmt(ret)
    }

    fn analyse_var(&self, name: &Token) -> Result<StaticInfo, LError> {
        let info = self.env.resolve(name)?;
        match &info {
            IVariable(isinitialized) => if !isinitialized {
                return Err(LError::from_token(format!("Variable {} is not intialized yet", name.lexeme), name))
            }
            _ => {}
        };
        Ok(info)
    }

    fn analyse_expr(&mut self, expr: &Expr) -> Result<StaticInfo, LError> {
        match expr {
            EVariable { name, .. } => self.analyse_var(name),
            EApplication { callee, arg , .. } => {
                self.analyse_expr(arg)?;
                self.analyse_expr(callee)
            },
            EAssignment { lvalue, expr } => self.analyse_assignment(lvalue, expr),
            EBlock(xs) => self.analyse_block(xs),
            EIf { condition, left, right, .. } => self.analyse_if_expr(condition, left, right),
            _ => Ok(IEmpty)

        }
    }

    fn analyse_if_expr(&mut self, condition: &Expr, left: &Expr, right: &Expr) -> Result<StaticInfo, LError> {
        self.analyse_expr(left)?;
        self.analyse_expr(right)?;
        self.analyse_expr(condition)
    }

    fn analyse_assignment(&mut self, lvalue: &Token, expr: &Expr) -> Result<StaticInfo, LError> {
        let info = self.analyse_var(lvalue)?;
        if let ILetBinding = info {
            return Err(LError::from_token(format!("Cannot assign to immutable let binding {}", lvalue.lexeme), lvalue))
        }
        self.analyse_expr(expr)
    }

}

