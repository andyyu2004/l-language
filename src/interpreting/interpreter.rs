use crate::parsing::{Stmt, Expr};
use crate::lexing::TokenType;
use crate::interpreting::Env;
pub struct Interpreter {
    env: Env
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            env: Env::new()
        }
    }
}
// Interpret Statements
impl Interpreter {

    pub fn interpret(&mut self, statements: &Vec<Stmt>) -> Result<(), Vec<String>> {
        let mut errors = Vec::<String>::new();
        for statement in statements {
            if let Err(err) = self.execute(statement) {
                errors.push(err);
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    fn execute(&mut self, statement: &Stmt) -> Result<(), String>{
        match statement {
            Stmt::ExprStmt(_expr) => Ok(()),
            Stmt::PrintStmt(expr) => Ok(println!("{}", self.evaluate(expr)?)),
            Stmt::Var(name, expr) => self.var_stmt(name, expr),
            _ => Ok(())
        }
    }

    fn var_stmt(&mut self, name: &'static str, expr: &Option<Expr>) -> Result<(), String> {
        let value = if let Some(x) = expr {
            Some(self.evaluate(x)?)
        } else { None };
//        Why can't I map this
//        let value2 = expr.map(|ref x| Interpreter::evaluate(x));
        Ok(self.env.define(name, value))
    }
}

// Expressions
impl Interpreter {
    pub fn evaluate(&self, expr: &Expr) -> Result<f64, String> {
        match expr {
            Expr::Binary(op, left, ref right)  => match op {
                TokenType::Plus => Ok(self.evaluate(left)? + self.evaluate(right)?),
                TokenType::Minus => Ok(self.evaluate(left)? - self.evaluate(right)?),
                TokenType::Star => Ok(self.evaluate(left)? * self.evaluate(right)?),
                TokenType::Slash => match self.evaluate(right)? {
//                    0.0 => Err("Division by zero error".to_string()),
                    r => Ok(self.evaluate(left)? / r)
                }
                TokenType::Caret => Ok(f64::powf(self.evaluate(left)?, self.evaluate(right)?)),
                _ => unreachable!()
            },
            Expr::Unary(op, expr) => match op {
                TokenType::Minus => Ok(-self.evaluate(expr)?),
                _ => unreachable!()
            },
            Expr::Operand(x) => Ok(*x),
            Expr::Grouping(expr) => Ok(self.evaluate(expr)?),
            Expr::Variable(name) => self.env.resolve(name),
//            _ => Err("".to_string())
        }
    }


}