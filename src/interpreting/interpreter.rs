use crate::interpreting::{Env, InterpreterError, LPattern};
use crate::lexing::{TokenType, Token};
use crate::parsing::{Expr, Stmt, Mode};
use crate::errors::LError;
use crate::types::l_types::Pair;
use crate::types::LType;
use crate::parsing::stmt::Stmt::{ExprStmt, LStmt, VarStmt, LetStmt, FnStmt, FnCurried, ReturnStmt, PrintStmt, TypeAlias, WhileStmt, StructDecl, DataDecl};
use crate::parsing::expr::Expr::{EUnary, EApplication, EVariable, ELiteral, EBinary, ETuple, EAssignment, EBlock, EIf, ERecord, ELogic, EGet, ESet, EDataConstructor, EVariant, EIfLet};
use std::panic;
use crate::interpreting::interpreter_error::InterpreterError::{Return};
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::{RefCell};
use crate::lexing::token::TokenType::{BangEqual, DoubleEqual, LessEqual, GreaterEqual, Caret, Slash, Plus, Star, Less, Minus, Greater};
use crate::types::l_types::LType::{TArrow, TName, TRecord, TTuple, TBool, TUnit};
use crate::generation::generate_function_from_type;
use crate::interpreting::pattern_matching::Matchable;
use crate::interpreting::objects::l_object::LObject::{LFunction, LNumber, LBool, LVariant, LRecord, LTuple, LUnit, LStruct};
use crate::interpreting::objects::{LObject, LInvocable, Variant, Function, Struct, Tuple};

pub struct Interpreter {
    pub env: Rc<RefCell<Env<Option<Rc<RefCell<LObject>>>>>>,
    mode: Mode
}

impl Interpreter {
    pub fn new(mode: Mode) -> Interpreter {
        Interpreter {
            env: Rc::new(RefCell::new(Env::new(None))),
            mode
        }
    }
}
// Interpret Statements
impl Interpreter {

    pub fn interpret(&mut self, statements: Vec<Stmt>) -> Result<(), Vec<InterpreterError>> {
        let mut errors = Vec::<InterpreterError>::new();
        for statement in statements {
            if let Err(err) = self.execute(statement) {
                errors.push(err);
            }
        }

        if errors.is_empty() { Ok(()) }
        else { Err(errors) }
    }

    fn execute(&mut self, statement: Stmt) -> Result<(), InterpreterError> {
        match statement {
            LStmt(expr) => match self.mode {
                Mode::Interactive => self.execute(PrintStmt(expr)),
                Mode::Interpreted => self.execute(ExprStmt(expr))
            },
            ExprStmt(expr) => { self.evaluate(&expr)?; Ok(()) },
            PrintStmt(expr) => Ok(println!("{}", self.evaluate(&expr)?.borrow())),
            VarStmt { name, init, ..} => self.var_stmt(name, init),
            LetStmt {name, init, .. } => self.let_stmt(name, init),
            FnStmt { name, token, params, ret_type, body} =>
                self.execute_fn_decl(name, token, params, ret_type, body),
            FnCurried { name, token, param, ret} => self.execute_curried_fn_decl(name, token, param, *ret),
            ReturnStmt { value, .. } => self.execute_return_stmt(value),
            WhileStmt { condition, body, .. } => self.execute_while(condition, body),
            TypeAlias {..} => Ok(()),
            DataDecl { name, variants } => self.execute_data_decl(name, variants),
            StructDecl { name, fields } => self.execute_struct(name, fields),
            x => Err(InterpreterError::from(LError::new(format!("Unknown stmt type {}", x), -1, -1)))
        }
    }

    fn execute_data_decl(&mut self, name: Token, variants: HashMap<String, LType>) -> Result<(), InterpreterError> {
        for (k, v) in &variants {
//            let f = generate_function_from_type(&v, EVariant)?;
            if let TArrow(l, r) = v {
                self.execute(FnStmt {
                    name: Some(k.clone()),
                    token: Token::dummy(),
                    params: vec![Pair::new("x".to_string(), *l.clone())],
                    ret_type: *r.clone(),
                    body: vec![
//                        PrintStmt (
//                            EVariable {
//                                name: Token::new(TokenType::Identifier, "x".to_string(), -1, -1)
//                            }
//                        ),
                        ReturnStmt {
                            token: Token::dummy(),
                            value: Some(EVariant(k.clone(), Box::new(EVariable {
                                name: Token::new(TokenType::Identifier, "x".to_string(), -1, -1)
                            })))
                        }]
                })?;
            } else {
                self.env.borrow_mut().define(k.clone(), Some(self.wrap(LVariant(Variant::new(k.clone(), self.wrap(LUnit))))))
            }
        }
        Ok(())
    }

    fn execute_struct(&mut self, name: Token, fields: HashMap<String, LType>) -> Result<(), InterpreterError> {
        let lstruct = Struct::new(name.clone(), fields);
        self.env.borrow_mut().define(name.lexeme, Some(self.wrap(LStruct(lstruct))));
        Ok(())
    }

    fn execute_while(&mut self, condition: Expr, body: Vec<Stmt>) -> Result<(), InterpreterError> {
        while self.evaluate(&condition)?.borrow().boolean() == true {
            let res = self.execute_block(&body, Rc::new(RefCell::new(Env::new(Some(Rc::clone(&self.env))))))?;
        }
        Ok(())
    }

    fn var_stmt(&mut self, name: Token, expr: Option<Expr>) -> Result<(), InterpreterError> {
        // Can't figure out how to use map on this :(
        let value = if let Some(x) = expr {
            Some(self.evaluate(&x)?)
        } else { None };
        self.env = Rc::new(RefCell::new(Env::new(Some(Rc::clone(&self.env)))));
        Ok(self.env.borrow_mut().define(name.lexeme, value))
    }

    fn let_stmt(&mut self, name: Token, expr: Expr) -> Result<(), InterpreterError> {
        self.var_stmt(name, Some(expr))
    }

    fn execute_fn_decl(&mut self, name: Option<String>, token: Token, params: Vec<Pair<LType>>, ret_type: LType, body: Vec<Stmt>) -> Result<(), InterpreterError> {
        let fstmt = FnStmt {
            name: name.clone(),
            token: token.clone(),
            params: params.clone(),
            ret_type: ret_type.clone(),
            body: body.clone()
        };
        let lfunction = self.wrap(LFunction(Function::new(fstmt.clone(), Rc::clone(&self.env))));
        if let Some(name) = name {
            self.env.borrow_mut().define(name, Some(Rc::clone(&lfunction)));
            lfunction.borrow_mut().function_mut().closure = Rc::clone(&self.env);
        }
        Ok(())
    }

    fn execute_curried_fn_decl(&mut self, name: Option<String>, token: Token, param: Pair<LType>, ret: Stmt) -> Result<(), InterpreterError> {
        let lfunction = self.wrap(LFunction(
            Function::new(FnCurried { name: name.clone(), token, param, ret: Box::new(ret) }, Rc::clone(&self.env))
        ));
        if let Some(name) = name {
            self.env.borrow_mut().define(name, Some(Rc::clone(&lfunction)));
            lfunction.borrow_mut().function_mut().closure = Rc::clone(&self.env);
        }
        Ok(())
    }

    // Pass environment back to caller, hard to get lifetimes right
    pub fn execute_block(&mut self, statements: &Vec<Stmt>, env: Rc<RefCell<Env<Option<Rc<RefCell<LObject>>>>>>) -> Result<Rc<RefCell<LObject>>, InterpreterError> {
         let enclosing = Rc::clone(&self.env);
         self.env = env;

         let mut statements = statements.clone();
         let last = statements.pop();
         for stmt in statements {
             let x = self.execute(stmt);
             // Consider return when in block thats not a functional block
             if let Err(Return(obj)) = x { // Intercept return value and handle appropriately
                 self.env = enclosing.clone();
                 return Ok(obj)
             } else if let Err(error) = x {
                 return Err(error)
             }
         }

         let ret_val = if let Some(stmt) = last {
             match stmt {
                 LStmt(ref e) => self.evaluate(e)?,
                 ReturnStmt { ref value, .. } => match value {
                     Some(expr) => self.evaluate(expr)?,
                     None => self.wrap(LUnit)
                 },
                 _ => { self.execute(stmt)?; self.wrap(LUnit) }
             }
         } else { self.wrap(LUnit) };

        self.env = enclosing;
        Ok(ret_val)
    }


    fn execute_return_stmt(&mut self, value: Option<Expr>) -> Result<(), InterpreterError> {
        let expr = if let Some(val) = value { self.evaluate(&val)? } else { Rc::new(RefCell::new(LUnit)) };
        Err(Return(expr))
//        panic::set_hook(Box::new(|_| {}));
//        panic!(expr)
    }

    fn wrap<T>(&self, x: T) -> Rc<RefCell<T>> {
        Rc::new(RefCell::new(x))
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

    pub fn evaluate(&mut self, expr: &Expr) -> Result<Rc<RefCell<LObject>>, InterpreterError> {
        match expr {
            EBinary { operator, left, right}  => self.evaluate_binary(operator, left, right),
            ELogic { operator, left, right} => {
                // Short circuit
                let lobj = self.evaluate(left)?;
                if operator.ttype == TokenType::DoubleAmpersand {
                    if *lobj.borrow_mut().boolean_mut() == false { return Ok(lobj) }
                } else if operator.ttype == TokenType::DoublePipe {
                    if *lobj.borrow_mut().boolean_mut() == true { return Ok(lobj) }
                }
                self.evaluate(right)
            }
            EUnary{ operator, operand} => match operator.ttype {
                TokenType::Minus => {
                    let obj = self.evaluate(operand)?;
                    *obj.borrow_mut().number_mut() = -*obj.borrow_mut().number_mut();
                    Ok(obj)
                },
                _ => unreachable!()
            },
            ELiteral(token) => Ok(Interpreter::literal_to_l_object(token)),
            EVariable { name, ..} | EDataConstructor { name } => self.evaluate_variable(name),
            ETuple(_, xs) => Ok(Rc::new(RefCell::new((LTuple(Tuple::new(
                xs.iter().map(|x| self.evaluate(x)).collect::<Result<Vec<Rc<RefCell<LObject>>>, _>>()?
            )))))),
            EApplication { token, callee, arg } => self.evaluate_curried_application(token, callee, arg),
            EAssignment { lvalue, expr} => self.evaluate_assignment(lvalue, expr),
            EBlock(xs) => self.execute_block(&xs, self.wrap(Env::new(Some(Rc::clone(&self.env))))),
            EIf { token, condition, left, right } => self.evaluate_if(token, condition, left, right),
            ERecord(_, xs) => self.evaluate_record(xs),
            EGet { name, expr } => self.evaluate_get_expr(name, expr),
            ESet { name, expr, value } => self.evaluate_set_expr(name, expr, value),
            EVariant(name, expr) => {
                let obj = self.evaluate(expr)?;
                Ok(self.wrap(LVariant(Variant::new(name.clone(), obj))))
            },
            EIfLet { token, pattern, scrutinee, left, right } =>
                self.evaluate_if_let(token, pattern, scrutinee, left, right),
            _ => Err(InterpreterError::from(LError::new("Unknown expr type".to_string(), 0, 0)))
        }
    }

    fn evaluate_binary(&mut self, operator: &Token, left: &Expr, right: &Expr) -> Result<Rc<RefCell<LObject>>, InterpreterError> {
        let l = self.evaluate(left)?;
        let r = self.evaluate(right)?;
        let num_ops = vec![Star, Plus, Minus, Slash, Caret];
        let cmp_ops = vec![Greater, GreaterEqual, Less, LessEqual];
        let eq_ops = vec![DoubleEqual, BangEqual];
        if num_ops.contains(&operator.ttype) {
            let res = match operator.ttype {
                Plus => self.wrap(LNumber(l.borrow().number() + r.borrow().number())),
                Minus => self.wrap(LNumber(l.borrow().number() - r.borrow().number())),
                Star => self.wrap(LNumber(l.borrow().number() * r.borrow().number())),
                Slash => self.wrap(LNumber(l.borrow().number() / r.borrow().number())),
                _ => panic!()
            };
            Ok(res)
        } else if cmp_ops.contains(&operator.ttype) {
            let b = match operator.ttype {
                Greater => self.wrap(LBool(l.borrow().number() > r.borrow().number())),
                GreaterEqual => self.wrap(LBool(l.borrow().number() >= r.borrow().number())),
                LessEqual => self.wrap(LBool(l.borrow().number() <= r.borrow().number())),
                Less => self.wrap(LBool(l.borrow().number() < r.borrow().number())),
                _ => panic!()
            };
            Ok(b)
        } else if eq_ops.contains(&operator.ttype) {
            let b = match operator.ttype {
                DoubleEqual => self.wrap(LBool(*l == *r)),
                BangEqual => self.wrap(LBool(*l == *r)),
                _ => panic!()
            };
            Ok(b)
        } else {
            panic!()
        }
    }

    fn evaluate_get_expr(&mut self, name: &Token, expr: &Expr) -> Result<Rc<RefCell<LObject>>, InterpreterError> {
        match *self.evaluate(expr)?.borrow() {
            LRecord(ref xs) => {
                Ok(xs.get(&name.lexeme).unwrap().clone())
            },
            _ => unreachable!(),
        }
    }

    fn evaluate_set_expr(&mut self, name: &Token, expr: &Expr, value: &Expr) -> Result<Rc<RefCell<LObject>>, InterpreterError> {
        let val_obj = self.evaluate(value)?;
        match *self.evaluate(expr)?.borrow_mut() {
            LRecord(ref mut xs) => {
                xs.insert(name.lexeme.clone(), Rc::clone(&val_obj));
            },
            _ => unreachable!(),
        }
        Ok(val_obj)
    }

    fn evaluate_record(&mut self, xs: &HashMap<String, Expr>) -> Result<Rc<RefCell<LObject>>, InterpreterError> {
        let mut map = HashMap::new();
        for (k, v) in xs {
            map.insert(k.clone(), self.evaluate(v)?);
        }
        Ok(Rc::new(RefCell::new(LRecord(map))))
    }

    fn evaluate_variable(&mut self, name: &Token) -> Result<Rc<RefCell<LObject>>, InterpreterError> {
        match self.env.borrow().resolve(name) {
            Ok(val) => Ok(val.unwrap()),
            Err(lerror) => Err(InterpreterError::from(lerror))
        }
    }

    fn evaluate_if(&mut self, token: &Token, condition: &Expr, left: &Expr, right: &Expr) -> Result<Rc<RefCell<LObject>>, InterpreterError> {
        let cond = self.evaluate(condition)?;
        if cond.borrow().boolean() { self.evaluate(left) }
        else { self.evaluate(right) }
    }

    fn evaluate_if_let(&mut self, token: &Token, pattern: &LPattern, scrutinee: &Expr, left: &Vec<Stmt>, right: &Expr) -> Result<Rc<RefCell<LObject>>, InterpreterError> {
        let s_obj = self.evaluate(scrutinee)?; // .borrow() as &dyn Matchable;
        let matched = s_obj.borrow().is_match(pattern);
        if matched {
            let mut env = Env::new(Some(Rc::clone(&self.env)));
            let bindings = s_obj.borrow().bindings(pattern);
            for (k, v) in bindings {
                env.define(k, Some(self.wrap(v)));
            }
            self.execute_block(left, self.wrap(env))
        }
        else { self.evaluate(right) }
    }

//    fn create_scope<F>(&mut self, f: F) -> Result<Rc<RefCell<LObject>>, InterpreterError>
//        where F: FnOnce() -> Result<Rc<RefCell<LObject>>, InterpreterError>
//    {
//        let enclosing = Rc::clone(&self.env);
//        self.env = self.wrap(Env::new(Some(Rc::clone(&self.env))));
//        let res = f();
//        self.env = enclosing;
//        res
//    }

    pub fn literal_to_l_object(literal: &Token) -> Rc<RefCell<LObject>> {
        let obj = match literal.ttype {
            TokenType::True => LBool(true),
            TokenType::False => LBool(false),
            TokenType::Number => LNumber(literal.lexeme.parse::<f64>().expect("Failed to parse float")),
            _ => panic!("Invalid literal conversion")
        };
        Rc::new(RefCell::new(obj))
    }

    fn evaluate_assignment(&mut self, lvalue: &Token, expr: &Expr) -> Result<Rc<RefCell<LObject>>, InterpreterError> {
        let obj = self.evaluate(expr);
        self.env.borrow_mut().update(&lvalue.lexeme, Some(obj.clone()?));
        obj
    }

    fn evaluate_curried_application(&mut self, token: &Token, callee: &Expr, arg: &Expr) -> Result<Rc<RefCell<LObject>>, InterpreterError> {
        let callee_obj = self.evaluate(callee)?;
        let arg_obj = self.evaluate(arg)?;
        if let LFunction(ref f) = *Rc::clone(&callee_obj).borrow() {
            f.invoke(arg_obj, self)
        } else { panic!("Non function") }
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