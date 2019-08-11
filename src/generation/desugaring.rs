use crate::parsing::{Expr, Stmt};
use crate::parsing::Expr::*;
use crate::parsing::Stmt::*;
use crate::lexing::Token;
use crate::interpreting::LPattern;


// Turns out this index wasn't actually necessary.
// Just leaving in struct form just in case some other state is necessary in the future
pub struct Desugarer {
    // index: usize
}

impl Desugarer {

    pub fn new() -> Desugarer {
        Desugarer { }
    }

    pub fn desugar(&mut self, statements: &mut Vec<Stmt>) {
        for statement in statements {
            self.desugar_statement(statement)
        }
    }

    fn desugar_statement(&mut self, statement: &mut Stmt) {
        match statement {
            LetStmt { init, .. } => self.desugar_expr(init),
            ExprStmt(expr) => self.desugar_expr(expr),
            LStmt(expr) => self.desugar_expr(expr),
            PrintStmt(expr) => self.desugar_expr(expr),
            WhileStmt { condition, body, ..} => {
                self.desugar_expr(condition);
                self.desugar(body)
            }
            TypeAlias { .. } => {}
            ReturnStmt { value, .. } => if let Some(expr) = value { self.desugar_expr(expr) }
            VarStmt { init, .. } => if let Some(expr) = init { self.desugar_expr(expr) }
            FnStmt { body, .. } => self.desugar(body),
            StructDecl { .. } => {}
            FnCurried { ret, .. } => self.desugar_statement(ret),
            DataDecl { .. } => {}
        }
    }

    fn desugar_expr(&mut self, expression: &mut Expr) {
        match expression {
            EMatch { token, expr, branches } => {
//            *expression = EVariable { name: Token::dummy() }
               *expression = self.generate_desugared_match(token, expr, &branches[..]);
            },
            EBinary { left, right, .. } => {
                self.desugar_expr(left);
                self.desugar_expr(right);
            }
            EUnary { operand, .. } => self.desugar_expr(operand),
            ELogic { left, right, .. } => {
                self.desugar_expr(left);
                self.desugar_expr(right);
            }
            EBlock(xs) => self.desugar(xs),
            EGet { expr, .. } => self.desugar_expr(expr),
            ESet { expr, value, .. } => {
                self.desugar_expr(expr);
                self.desugar_expr(value);
            }
            EAssignment { expr, .. } => self.desugar_expr(expr),
            EApplication { callee, arg, .. } => {
                self.desugar_expr(callee);
                self.desugar_expr(arg);
            }
            ETuple(_, xs ) => xs.iter_mut().for_each(|x| self.desugar_expr(x)),
            ERecord(_, xs) => xs.iter_mut().for_each(|(_, x)| self.desugar_expr(x)),
            EIf { condition, left, right, .. } => {
                self.desugar_expr(condition);
                self.desugar_expr(left);
                self.desugar_expr(right);
            }
            EIfLet { scrutinee, left, right, .. } => {
                self.desugar_expr(scrutinee);
                self.desugar(left);
                self.desugar_expr(right);
            }
            EVariant(_, xs) => xs.iter_mut().for_each(|x| self.desugar_expr(x)),
            EList(_, xs) => xs.iter_mut().for_each(|x| self.desugar_expr(x)),
            _ => {}
        }
    }

    fn generate_desugared_match(&mut self, token: &Token, scrutinee: &Box<Expr>, branches: &[(LPattern, Expr)]) -> Expr {
        // Chew through the branches until its empty then return an irrefutable pattern that leads to an error block
        // Assumes has at least one pattern in body
        let (pattern, expr) = branches[0].clone();
        if branches.len() > 1 {
            EIfLet {
                token: token.clone(),
                pattern,
                scrutinee: scrutinee.clone(),
                left: vec![LStmt(expr)],
                right: Box::new(self.generate_desugared_match(token, scrutinee, &branches[1..]))
            }
        } else {
            // branches.len() == 1 here
            EIfLet {
                token: token.clone(),
                pattern,
                scrutinee: scrutinee.clone(),
                left: vec![LStmt(expr)],
                right: Box::new(EPanic)
            }
        }
    }

}




