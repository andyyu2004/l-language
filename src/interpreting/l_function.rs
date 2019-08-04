use crate::interpreting::{LInvocable, LObject, Interpreter, Env, InterpreterError};
use crate::parsing::Stmt;
use std::fmt::{Display, Error, Formatter};
use std::panic;
use crate::interpreting::l_object::LObject::{LTuple, LFunction};
use crate::parsing::stmt::Stmt::{FnCurried, FnStmt};

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    declaration: Stmt,
    pub closure: Env<Option<LObject>>,
}

impl Function {

    pub fn new(declaration: Stmt, closure: Env<Option<LObject>>) -> Function {
        Function { declaration, closure }
    }
}

impl LInvocable for Function {
    // In these particular instances cloning instead of reference is important so the closures carry the relevant information at that particular instance in time
    // An alternative would be to use persistent data structures but performance is not a priority
    fn invoke(&mut self, interpreter: &mut Interpreter, arg: &LObject) -> Result<LObject, InterpreterError> {
        let mut env = Env::new(Some(self.closure.clone()));
        match &self.declaration {
            FnCurried { name, token, param, ret } => {
                env.define(param.name.clone(), Some(arg.clone()));
                Ok(LFunction(Function::new(*ret.clone(), env)))
            },

            FnStmt { params, ret_type, body, name,.. } => {
                // A horrifically inefficient way to allow recursion just like everything else in this interpreter
                // To do so without manually adding a self reference each invocation requires use of references and lifetimes are hard
                if let Some(name) = name {
                    env.define(name.clone(), Some(LFunction(self.clone())));
                }
                let paramnames = params.iter().map(|x| x.clone().name).collect::<Vec<String>>();
                if let LTuple(xs) = arg {
                    for (i, p) in paramnames.iter().enumerate() {
                        env.define(p.to_string(), Some(xs[i].clone()))
                    }
                } else if paramnames.len() == 1 {
                    env.define(paramnames[0].clone(), Some(arg.clone()))
                }
                match interpreter.execute_block(body, env) {
                    Ok((ret_val, new_env)) => {
                        // Update function closure here and the interpreter updates the environment copy
                        self.closure = new_env;
                        Ok(ret_val)
                    },
                    Err(e) => Err(e)
                }
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

