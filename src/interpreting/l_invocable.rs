use crate::interpreting::{Interpreter, LObject, InterpreterError};
use std::cell::RefCell;
use std::rc::Rc;

pub trait LInvocable {
    fn invoke(&self, arg: Rc<RefCell<LObject>>, interpreter: &mut Interpreter) -> Result<Rc<RefCell<LObject>>, InterpreterError>;
}