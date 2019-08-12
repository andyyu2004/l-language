use crate::interpreting::objects::{LInvocable, LObject};
use crate::interpreting::{InterpreterError, Interpreter, Env, LPattern};
use std::cell::RefCell;
use std::rc::Rc;
use std::fmt::{Display, Formatter, Error};
use crate::parsing::Expr;
use crate::parsing::stmt::Stmt::LStmt;
use crate::interpreting::pattern_matching::Matchable;
use crate::errors::LError;

// Rewrite functionality seperately from the LFunctions, perhaps if this is better lfunction can be merged into this
#[derive(Debug, PartialEq, Clone)]
pub struct Lambda {
    param: LPattern,
    body: Expr,
    closure: Rc<RefCell<Env<Option<Rc<RefCell<LObject>>>>>>
}

impl Lambda {
    pub fn new(param: LPattern, body: Expr, closure: Rc<RefCell<Env<Option<Rc<RefCell<LObject>>>>>>) -> Lambda {
        Lambda { param, body, closure }
    }
}

impl LInvocable for Lambda {
    fn invoke(&self, arg: Rc<RefCell<LObject>>, interpreter: &mut Interpreter) -> Result<Rc<RefCell<LObject>>, InterpreterError> {
        // Static analyser should ensure only irrefutable patterns are used
        // Sanity check
        if !arg.borrow().is_match(&self.param) { panic!("Lambda pattern failed to match during execution. Bug!") }
        let mut env = Env::new(Some(Rc::clone(&self.closure)));
        arg.borrow_mut()
            .bindings(&self.param)
            .into_iter()
            .for_each(|(k, v)| env.define(k, Some(Rc::new(RefCell::new(v)))));
        interpreter.execute_block(&vec![LStmt(self.body.clone())], Rc::new(RefCell::new(env)))
    }
}

impl Display for Lambda {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "λ {} -> {}", self.param, self.body)
    }
}