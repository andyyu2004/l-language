use crate::interpreting::{Interpreter, InterpreterError};
use std::cell::RefCell;
use std::rc::Rc;
use crate::interpreting::objects::LObject;

pub trait LInvocable {
    fn invoke(&self, arg: Rc<RefCell<LObject>>, interpreter: &mut Interpreter) -> Result<Rc<RefCell<LObject>>, InterpreterError>;
}