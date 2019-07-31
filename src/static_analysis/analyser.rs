use crate::parsing::{Stmt, Expr};
use crate::errors::LError;
use crate::parsing::stmt::Stmt::{PrintStmt, ExprStmt, VarStmt, LetStmt, FnStmt, Curried};
use crate::interpreting::{Env};
use crate::parsing::expr::Expr::{EVariable, ECurryApplication};
use crate::static_analysis::StaticInfo;
use crate::static_analysis::static_info::StaticInfo::{IVariable, ILetBinding, IFunction};

// Check validity of code
// Unintialized and undefined variables

pub struct Analyser {
    env: Env<StaticInfo>
}

impl Analyser {
    pub fn new() -> Analyser {
        Analyser { env: Env::new(None) }
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

    fn analyse_stmt(&mut self, stmt: &Stmt) -> Result<(), LError> {
        match stmt {
            PrintStmt(expr) | ExprStmt(expr) => self.analyse_expr(expr),
            VarStmt(token, _, expr) => Ok(self.env.define(token.lexeme.clone(), IVariable(expr.is_some()))),
            LetStmt(token, _, _) => Ok(self.env.define(token.lexeme.clone(), ILetBinding())),
//            CurriedFn(token, params, _, _) => Ok(self.env.define(token.lexeme.clone(), IFunction())),
            FnStmt(name, _, _, _, _) => self.analyse_fn_decl(name),
            Curried(name, _, _, _) => self.analyse_fn_decl(name),
            x => unimplemented!("Unimplemented in analyse stmt {}", x)
        }
    }

    fn analyse_fn_decl(&mut self, name: &Option<String>) -> Result<(), LError> {
        if let Some(name) = name { Ok(self.env.define(name.clone(), IFunction())) }
        else { Ok(()) }
    }

    fn analyse_expr(&self, expr: &Expr) -> Result<(), LError> {
        match expr {
            EVariable(token) => match self.env.resolve(token)? {
                IVariable(isinitialized) => if !isinitialized {
                    Err(LError::from_token(format!("Variable {} is not intialized yet", token.lexeme), token))
                } else { Ok(()) },
                IFunction() => Ok(()),
                _ => unreachable!()
            },
            ECurryApplication(_, callee, arg) => {
                self.analyse_expr(callee)?;
                self.analyse_expr(arg)
            },
            _ => Ok(())

        }
    }

}

