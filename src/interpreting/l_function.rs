use crate::interpreting::{LInvocable, LObject, Interpreter, Env, InterpreterError};
use crate::parsing::Stmt;
use std::fmt::{Display, Error, Formatter};
use std::panic;
use crate::interpreting::l_object::LObject::{LTuple, LFunction};
use crate::parsing::stmt::Stmt::{FnCurried, FnStmt};
use std::rc::Rc;
use std::cell::RefCell;

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    declaration: Stmt,
    pub closure: Rc<RefCell<Env<Option<Rc<RefCell<LObject>>>>>>,
}

impl Function {

    pub fn new(declaration: Stmt, closure: Rc<RefCell<Env<Option<Rc<RefCell<LObject>>>>>>) -> Function {
        Function { declaration, closure }
    }
}

impl LInvocable for Function {
    fn invoke(&self, arg: Rc<RefCell<LObject>>, interpreter: &mut Interpreter) -> Result<Rc<RefCell<LObject>>, InterpreterError> {
        let env = Rc::new(RefCell::new(Env::new(Some(Rc::clone(&self.closure)))));
        match &self.declaration {
            FnCurried { name, token, param, ret } => {
                env.borrow_mut().define(param.name.clone(), Some(Rc::clone(&arg)));
                Ok(Rc::new(RefCell::new(LFunction(Function::new(*ret.clone(), Rc::clone(&env))))))
            },

            FnStmt { params, ret_type, body, name,.. } => {
                let paramnames = params.iter().map(|x| x.clone().name).collect::<Vec<String>>();
                if let LTuple(ref xs) = *arg.borrow() {
                    for (i, p) in paramnames.iter().enumerate() {
                        env.borrow_mut().define(p.to_string(), Some(xs[i].clone()))
                    }
                } else if paramnames.len() == 1 {
                    env.borrow_mut().define(paramnames[0].clone(), Some(arg.clone()))
                }

                interpreter.execute_block(body, env)
            },
            _ => panic!("Invoke on non function")
        }
    }
}


impl Display for Function {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match &self.declaration {
            FnStmt { name, .. } => match name {
                Some(name) => write!(f, "[Function {}]", name),
                None => write!(f, "[Function]"),
            }
            _ => write!(f, "[CFunction]")
        }
    }
}

//
//                match panic::catch_unwind(AssertUnwindSafe(|| interpreter.execute_block(body, env))) {
//                    Ok(res) => {
//                        // Manually updating function closure for state changes that occur in block
//                        let (ret_val, new_env) = res?;
//                        // self.closure = new_env.enclosing().clone().unwrap();
//                        Ok(ret_val)
//
//                    },
//                    Err(ret) => {
//                        panic::take_hook();
//                        let ret_val = match ret.downcast::<LObject>() {
//                            Ok(val) => *val,
//                            Err(err) => LUnit
//                        };
//                        interpreter.env = enclosing; // Manually restore interpreter environment if interrupted
//                        Ok(ret_val)
//                    }
//                }