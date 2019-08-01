use crate::interpreting::{Env, Function, LInvocable};
use crate::lexing::{TokenType, Token};
use crate::parsing::{Expr, Stmt};
use crate::errors::LError;
use crate::types::l_types::NameTypePair;
use crate::types::LType;
use crate::interpreting::l_object::LObject::{LBool, LString, LNumber, LFunction, LUnit, LTuple};
use crate::interpreting::l_object::LObject;
use itertools::Itertools;
use crate::parsing::stmt::Stmt::{ExprStmt, PrintStmt, VarStmt, LetStmt, FnStmt, FnCurried, ReturnStmt};
use crate::parsing::expr::Expr::{EUnary, EApplication, EVariable, ELiteral, EBinary, ETuple, EAssignment};
use std::panic;

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

    pub fn interpret(&mut self, statements: Vec<Stmt>) -> Result<(), Vec<LError>> {
        let mut errors = Vec::<LError>::new();
        for statement in statements {
            if let Err(err) = self.execute(statement) {
                errors.push(err);
            }
        }

        if errors.is_empty() { Ok(()) }
        else { Err(errors) }
    }

    fn execute(&mut self, statement: Stmt) -> Result<(), LError> {
        match statement {
            ExprStmt(expr) => { self.evaluate(&expr)?; Ok(()) },
            PrintStmt(expr) => Ok(println!("{}", self.evaluate(&expr)?)),
            VarStmt { name, init, ..} => self.var_stmt(name, init),
            LetStmt {name, init, .. } => self.let_stmt(name, init),
            FnStmt { name, token, params, ret_type, body} =>
                self.execute_fn_decl(name, token, params, ret_type, body),
            FnCurried { name, token, param, ret} => self.execute_curried_fn_decl(name, token, param, *ret),
            ReturnStmt { value, .. } => self.execute_return_stmt(value),
//            CurriedFn(name, args, ret, body) => self.execute_fn_decl(name, args, ret, body),
            _ => Err(LError::new("Unknown statement type".to_string(), 0, 0))
        }
    }

    fn var_stmt(&mut self, name: Token, expr: Option<Expr>) -> Result<(), LError> {
        // Can't figure out how to use map on this :(
        let value = if let Some(x) = expr {
            Some(self.evaluate(&x)?)
        } else { None };
        Ok(self.env.define(name.lexeme.to_string(), value))
    }

    fn let_stmt(&mut self, name: Token, expr: Expr) -> Result<(), LError> {
        let value = Some(self.evaluate(&expr)?);
        Ok(self.env.define(name.lexeme, value))
    }

    fn execute_fn_decl(&mut self, name: Option<String>, token: Token, params: Vec<NameTypePair>, ret_type: LType, body: Vec<Stmt>) -> Result<(), LError> {
        let lfunction = LFunction(Function::new(FnStmt { name: name.clone(), token, params, ret_type, body }, self.env.clone()));
        if let Some(name) = name {
            self.env.define(name, Some(lfunction));
        }
        Ok(())
    }

    fn execute_curried_fn_decl(&mut self, name: Option<String>, token: Token, param: NameTypePair, ret: Stmt) -> Result<(), LError> {
        let lfunction = LFunction(Function::new(FnCurried { name: name.clone(), token, param, ret: Box::new(ret) }, self.env.clone()));
        if let Some(name) = name {
            self.env.define(name, Some(lfunction));
        }
        Ok(())
    }

    // Ownership of self
    pub fn execute_block(&mut self, statements: &Vec<Stmt>, env: Env<Option<LObject>>) -> Result<Env<Option<LObject>>, LError> {
        let enclosing = self.env.clone();
        self.env = env;
        let errors = statements.
            iter().map(|x| self.execute(x.clone())).
            filter(|x| x.is_err()).
            map(|x| x.err().unwrap())
            .join("\n");
        let updated_env = self.env.clone();
        self.env = enclosing.clone();
        if errors.is_empty() { Ok((updated_env)) }
        else { Err(LError::new(errors, -1, -1)) }
    }

    fn execute_return_stmt(&mut self, value: Option<Expr>) -> Result<(), LError> {
        let expr = if let Some(val) = value { self.evaluate(&val)? } else { LUnit };
        panic::set_hook(Box::new(|_| {}));
        panic!(expr)
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
            EBinary { operator, left, ref right}  => match operator.ttype {
                TokenType::Plus  => Ok(LNumber(self.evaluate(left)?.number() + self.evaluate(right)?.number())),
                TokenType::Minus => Ok(LNumber(self.evaluate(left)?.number() - self.evaluate(right)?.number())),
                TokenType::Star  => Ok(LNumber(self.evaluate(left)?.number() * self.evaluate(right)?.number())),
                TokenType::Slash => Ok(LNumber(self.evaluate(left)?.number() / self.evaluate(right)?.number())),
                TokenType::Caret => Ok(LNumber(f64::powf(self.evaluate(left)?.number(), self.evaluate(right)?.number()))),
                TokenType::DoubleEqual => Ok(LBool(self.evaluate(left)? == self.evaluate(right)?)),
//                Need to figure out typeclasses first
//                TokenType::Less => Ok(LBool(self.evaluate(left)? < self.evaluate(right)?)),
//                TokenType::LessEqual => Ok(LBool(self.evaluate(left)? <= self.evaluate(right)?)),
//                TokenType::Greater => Ok(LBool(self.evaluate(left)? > self.evaluate(right)?)),
//                TokenType::GreaterEqual => Ok(LBool(self.evaluate(left)? >= self.evaluate(right)?)),
                _ => unreachable!()
            },
            EUnary{ operator, operand} => match operator.ttype {
                TokenType::Minus => Ok(LNumber(-self.evaluate(operand)?.number())),
                _ => unreachable!()
            },
            ELiteral(token) => Ok(Interpreter::literal_to_l_object(token)),
            EVariable { name, ..} => self.env.resolve(name).map(|x| x.unwrap()),
            ETuple(xs) => Ok(LTuple(xs.iter().map(|x| self.evaluate(x)).collect::<Result<Vec<LObject>, _>>()?)),
            EApplication { token, callee, arg } => self.evaluate_curried_application(token, callee, arg),
            EAssignment { lvalue, expr} => self.evaluate_assignment(lvalue, expr),
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

    fn evaluate_assignment(&mut self, lvalue: &Token, expr: &Expr) -> Result<LObject, LError> {
        let obj = self.evaluate(expr);
        self.env.update(&lvalue.lexeme, Some(obj.clone()?));
        obj
    }

    fn evaluate_curried_application(&mut self, token: &Token, callee: &Expr, arg: &Expr) -> Result<LObject, LError> {
        let mut callee_obj = self.evaluate(callee)?;
        let arg_obj = self.evaluate(arg)?;
        let res = callee_obj.function().invoke(self, &arg_obj);
        if let EVariable { name, ..} = callee {
            self.env.update(&name.lexeme, Some(callee_obj))
        }
        res

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