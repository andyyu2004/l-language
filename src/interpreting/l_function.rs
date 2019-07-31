use crate::interpreting::{LInvocable, LObject, Interpreter, Env};
use crate::parsing::Stmt;
use crate::errors::LError;
use std::fmt::{Display, Error, Formatter};
use crate::interpreting::l_object::LObject::{LBool, LTuple, LFunction, LUnit};
use crate::parsing::stmt::Stmt::{Curried, FnStmt};

#[derive(Debug, Clone)]
pub struct Function {
    declaration: Stmt,
    closure: Env<Option<LObject>>
}

impl Function {
    pub fn new(declaration: Stmt, closure: Env<Option<LObject>>) -> Function {
        Function { declaration, closure }
    }
}

impl LInvocable for Function {

    // In these particular instances cloning instead of reference is important so the closures carry the relevant information at that particular instance in time
    // An alternative would be to use persistent data structures but performance is not a priority
    fn invoke(&self, interpreter: &mut Interpreter, arg: &LObject) -> Result<LObject, LError> {
        match &self.declaration {

            Curried(name, token, param, ret) => {
                let mut env = Env::new(Some(self.closure.clone()));
                env.define(param.name.clone(), Some(arg.clone()));
                Ok(LFunction(Function::new(*ret.clone(), env)))
            },

            FnStmt(_, _, params, rettype, body) => {
                let mut env = Env::new(Some(self.closure.clone()));
                let paramnames = params.iter().map(|x| x.clone().name).collect::<Vec<String>>();
                if let LTuple(xs) = arg {
                    for (i, p) in paramnames.iter().enumerate() {
                        env.define(p.to_string(), Some(xs[i].clone()))
                    }
                } else if paramnames.len() == 1 {
                    env.define(paramnames[0].clone(), Some(arg.clone()))
                }
                interpreter.execute_block(body, &env)?;
                Ok(LUnit)
            }
            _ => panic!("Invoke on non function")
        }
    }

//    fn invoke(&self, interpreter: &mut Interpreter, args: &Vec<LObject>) -> Result<(), LError> {
//        if let Stmt::FnStmt(_, params, _, statements) = &self.declaration {
//            let mut env = Env::new(None);
//            for (i, arg) in args.iter().enumerate() {
//                env.define(params[i].name.clone(), Some(arg.clone()))
//            }
//            interpreter.execute_block(statements, env)
//        } else { panic!("Passed non function decl to Function") }
//
//    }
//}
}


impl Display for Function {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match &self.declaration {
            Stmt::FnStmt(name, _, _, _, _) => if let Some(name) = name { write!(f, "[Function {}]", name) } else { Ok(()) },
            _ => write!(f, "Function")
        }
    }
}

