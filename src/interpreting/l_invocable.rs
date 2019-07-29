use crate::interpreting::{Interpreter, LObject};
use crate::errors::LError;

pub trait LInvocable {
    fn invoke(&self, interpreter: &mut Interpreter, args: &Vec<LObject>) -> Result<(), LError>;
}