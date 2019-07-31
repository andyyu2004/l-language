use crate::interpreting::{Env, Function, LInvocable};
use crate::lexing::{TokenType, Token};
use crate::parsing::{Expr, Stmt};
use crate::errors::LError;
use crate::types::l_types::NameTypePair;
use crate::types::LType;
use crate::interpreting::l_object::LObject::{LBool, LString, LNumber, LFunction, LUnit, LTuple};
use crate::interpreting::l_object::LObject;
use itertools::Itertools;
use crate::parsing::stmt::Stmt::{ExprStmt, PrintStmt, VarStmt, LetStmt, FnStmt, Curried};
use crate::parsing::expr::Expr::{EUnary, ECurryApplication, EVariable, ELiteral, EBinary, ETuple};

pub struct Interpreter {
    pub env: Env<Option<LObject>>
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            env: Env::new(None)
        }
    }
}
// Interpret Statements
impl Interpreter {

    pub fn interpret(&mut self, statements: &Vec<Stmt>) -> Result<(), Vec<LError>> {
        let mut errors = Vec::<LError>::new();
        for statement in statements {
            if let Err(err) = self.execute(statement) {
                errors.push(err);
            }
        }

        if errors.is_empty() { Ok(()) }
        else { Err(errors) }
    }

    fn execute(&mut self, statement: &Stmt) -> Result<(), LError> {
        match statement {
            ExprStmt(expr) => { self.evaluate(expr)?; Ok(()) },
            PrintStmt(expr) => Ok(println!("{}", self.evaluate(expr)?)),
            VarStmt(name, _, expr) => self.var_stmt(name, expr),
            LetStmt(name, _, expr) => self.let_stmt(name, expr),
            FnStmt(name, token, args, ret, body) => self.execute_fn_decl(name, token, args, ret, body),
            Curried(name, token, arg, ret) => self.execute_curried_fn_decl(name, token, arg, ret),
//            CurriedFn(name, args, ret, body) => self.execute_fn_decl(name, args, ret, body),
            _ => Err(LError::new("Unknown statement type".to_string(), 0, 0))
        }
    }

    fn var_stmt(&mut self, name: &Token, expr: &Option<Expr>) -> Result<(), LError> {
        // Can't figure out how to use map on this :(
        let value = if let Some(x) = expr {
            Some(self.evaluate(x)?)
        } else { None };
        Ok(self.env.define(name.lexeme.to_string(), value))
    }

    fn let_stmt(&mut self, name: &Token, expr: &Expr) -> Result<(), LError> {
        let value = Some(self.evaluate(expr)?);
        Ok(self.env.define(name.lexeme.to_string(), value))
    }

    fn execute_fn_decl(&mut self, name: &Option<String>, token: &Token, args: &Vec<NameTypePair>, ret: &LType, body: &Vec<Stmt>) -> Result<(), LError> {
        let lfunction = LFunction(Function::new(Stmt::FnStmt(name.clone(), token.clone(), args.clone(), ret.clone(), body.clone()), self.env.clone()));
        if let Some(name) = name {
            self.env.define(name.clone(), Some(lfunction));
        }
        Ok(())
    }

    fn execute_curried_fn_decl(&mut self, name: &Option<String>, token: &Token, arg: &NameTypePair, ret: &Stmt) -> Result<(), LError> {
        let lfunction = LFunction(Function::new(Curried(name.clone(), token.clone(), arg.clone(), Box::new(ret.clone())), self.env.clone()));
        if let Some(name) = name {
            self.env.define(name.clone(), Some(lfunction));
        }
        Ok(())
    }

    // Ownership of self
    pub fn execute_block(&mut self, statements: &Vec<Stmt>, env: &Env<Option<LObject>>) -> Result<(), LError> {
        let enclosing = self.env.clone();
        self.env = env.clone();
        let errors = statements.
            iter().map(|x| self.execute(x)).
            filter(|x| x.is_err()).
            map(|x| x.err().unwrap())
            .join("\n");
        self.env = enclosing;

        if errors.is_empty() { Ok(()) }
        else { Err(LError::new(errors, -1, -1)) }

    }


}

// Expressions
impl Interpreter {
// pub fn evaluate(&self, expr: &Expr) -> f64 {
//     match expr {
//         Expr::Binary(op, left, ref right)  => match op {
//             TokenType::Plus => self.evaluate(left) + self.evaluate(right),
//             TokenType::Minus => self.evaluate(left) - self.evaluate(right),
//             TokenType::Star => self.evaluate(left) * self.evaluate(right),
//             TokenType::Slash => self.evaluate(left) / self.evaluate(right),
//             TokenType::Caret => f64::powf(self.evaluate(left), self.evaluate(right)),
//             _ => unreachable!()
//         },
//         Expr::Unary(op, expr) => match op {
//             TokenType::Minus => -self.evaluate(expr),
//             _ => unreachable!()
//         },
//         Expr::Operand(x) => *x,
//         Expr::Grouping(expr) => self.evaluate(expr),
//         // Expr::Variable(name) => self.env.resolve(name),
//     //    _ => Err("".to_string())
//     }
// }

    pub fn evaluate(&mut self, expr: &Expr) -> Result<LObject, LError> {
        match expr {
            EBinary(op, left, ref right)  => match op.ttype {
                TokenType::Plus  => Ok(LNumber(self.evaluate(left)?.number() + self.evaluate(right)?.number())),
                TokenType::Minus => Ok(LNumber(self.evaluate(left)?.number() - self.evaluate(right)?.number())),
                TokenType::Star  => Ok(LNumber(self.evaluate(left)?.number() * self.evaluate(right)?.number())),
                TokenType::Slash => Ok(LNumber(self.evaluate(left)?.number() / self.evaluate(right)?.number())),
                TokenType::Caret => Ok(LNumber(f64::powf(self.evaluate(left)?.number(), self.evaluate(right)?.number()))),
                _ => unreachable!()
            },
            EUnary(op, expr) => match op.ttype {
                TokenType::Minus => Ok(LNumber(-self.evaluate(expr)?.number())),
                _ => unreachable!()
            },
            ELiteral(token) => Ok(Interpreter::literal_to_l_object(token)),
            EVariable(name) => self.env.resolve(name).map(|x| x.unwrap()),
            ETuple(xs) => Ok(LTuple(xs.iter().map(|x| self.evaluate(x)).collect::<Result<Vec<LObject>, _>>()?)),
            ECurryApplication(token, callee, arg) => self.evaluate_curried_application(token, callee, arg),
//            Expr::EApplication(token, callee, args) => self.evaluate_application(token, callee, args),
            _ => Err(LError::new("Unknown expr type".to_string(), 0, 0))
        }
    }

    fn literal_to_l_object(literal: &Token) -> LObject {
        match literal.ttype {
            TokenType::True => LBool(true),
            TokenType::False => LBool(false),
            TokenType::Number => LNumber(literal.lexeme.parse::<f64>().expect("Failed to parse float")),
            _ => panic!("Invalid literal conversion")
        }
    }

    fn evaluate_curried_application(&mut self, token: &Token, callee: &Expr, arg: &Expr) -> Result<LObject, LError> {
        let callee_obj = self.evaluate(callee)?;
        let arg_obj = self.evaluate(arg)?;
        if let LFunction(f) = callee_obj {
            f.invoke(self, &arg_obj)
        } else {
            Err(LError::from_token("Attempted invocation on non-function".to_string(), token))
        }
    }

//    fn evaluate_application(&mut self, token: &Token, callee: &Expr, args: &Vec<Expr>) -> Result<LObject, LError> {
//        let callee_obj = self.evaluate(callee)?;
//        let mut arguments = Vec::<LObject>::new(); // Using explicit for loop as ? operator in closure is different
//        for arg in args { arguments.push(self.evaluate(arg)?) }
//        if let LFunction(f) = callee_obj {
//            f.invoke(self, &arguments)?;
//            Ok(LUnit)
//        } else {
//            Err(LError::from_token("Attempted to call a non function".to_string(), token))
//        }
//    }


}