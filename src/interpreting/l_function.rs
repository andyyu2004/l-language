use crate::interpreting::{LInvocable, LObject, Interpreter, Env};
use crate::parsing::Stmt;
use crate::errors::LError;
use std::fmt::{Display, Error, Formatter};
use std::panic;
use crate::interpreting::l_object::LObject::{LBool, LTuple, LFunction, LUnit};
use crate::parsing::stmt::Stmt::{FnCurried, FnStmt};
use std::panic::AssertUnwindSafe;

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    declaration: Stmt,
    closure: Env<Option<LObject>>,
}

impl Function {
    pub fn new(declaration: Stmt, closure: Env<Option<LObject>>) -> Function {
        Function { declaration, closure }
    }
}

impl LInvocable for Function {
    // In these particular instances cloning instead of reference is important so the closures carry the relevant information at that particular instance in time
    // An alternative would be to use persistent data structures but performance is not a priority
    fn invoke(&mut self, interpreter: &mut Interpreter, arg: &LObject) -> Result<LObject, LError> {
        match &self.declaration {
            FnCurried { name, token, param, ret } => {
                let mut env = Env::new(Some(self.closure.clone()));
                env.define(param.name.clone(), Some(arg.clone()));
                Ok(LFunction(Function::new(*ret.clone(), env)))
            },

            FnStmt { params, ret_type, body, .. } => {
                let mut env = Env::new(Some(self.closure.clone()));
                let paramnames = params.iter().map(|x| x.clone().name).collect::<Vec<String>>();
                if let LTuple(xs) = arg {
                    for (i, p) in paramnames.iter().enumerate() {
                        env.define(p.to_string(), Some(xs[i].clone()))
                    }
                } else if paramnames.len() == 1 {
                    env.define(paramnames[0].clone(), Some(arg.clone()))
                }
                let enclosing = interpreter.env.clone(); // The corresponding code in execute_block will not be executed due to not having a finally block
                match panic::catch_unwind(AssertUnwindSafe(|| interpreter.execute_block(body, env))) {
                    Ok(new_env) => {
                        self.closure = new_env?.enclosing().clone().unwrap();
                    },
                    Err(ret) => {
                        let retvalue = *ret.downcast::<LObject>().ok().unwrap();
                        println!("ret {:?}", retvalue);
                        interpreter.env = enclosing; // Manually restore interpreter environment if interrupted
                        return Ok(retvalue)
                    }
                };
                Ok(LUnit)
            }
            _ => panic!("Invoke on non function")
        }
    }
}


impl Display for Function {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match &self.declaration {
            FnStmt { name, .. } => if let Some(name) = name { write!(f, "[Function {}]", name) } else { Ok(()) },
            _ => write!(f, "Function")
        }
    }
}

