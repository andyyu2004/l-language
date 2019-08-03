use crate::interpreting::{Interpreter, LObject, InterpreterError};
use crate::errors::LError;

pub trait LInvocable {
    fn invoke(&self, interpreter: &mut Interpreter, arg: &LObject) -> Result<LObject, InterpreterError>;
}