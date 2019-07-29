use crate::interpreting::{LInvocable, LObject, Interpreter, Env};
use crate::parsing::Stmt;
use crate::errors::LError;

#[derive(Debug, Clone)]
pub struct Function {
    declaration: Stmt
}

impl Function {
    pub fn new(declaration: Stmt) -> Function {
        Function { declaration }
    }
}

impl LInvocable for Function {
    fn invoke(&self, interpreter: &mut Interpreter, args: &Vec<LObject>) -> Result<(), LError> {
        if let Stmt::Fn(_, params, _, statements) = &self.declaration {
            let mut env = Env::new(None);
            for (i, arg) in args.iter().enumerate() {
                env.define(params[i].name.clone(), Some(arg.clone()))
            }
            interpreter.execute_block(statements, env)
        } else { panic!("Passed non function decl to LFunction") }

    }
}

