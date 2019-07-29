use std::collections::HashMap;
use crate::errors::LError;
use crate::lexing::Token;
use crate::interpreting::l_object::LObject;


#[derive(Debug, Clone)]
pub struct Env {
    vars: HashMap<String, Option<LObject>>,
    outer: Box<Option<Env>>
}

impl Env {
    pub fn new(outer: Option<Env>) -> Env {
        Env { vars: HashMap::new() , outer: Box::new(outer) }
    }

    // Hashmap::insert():
    // If the map did not have this key present, None is returned.
    // If the map did have this key present, the value is updated, and the old value is returned

    // Allows redefinition without warning
    pub fn define(&mut self, key: String, value: Option<LObject>) {
        self.vars.insert(key, value);
    }

    pub fn resolve(&self, key: &Token) -> Result<LObject, LError> {
        match self.vars.get(&key.lexeme) {
            Some(x) => match x {
                Some(obj) => Ok(obj.clone()),
                None => Err(LError::from_token(format!("Uninitialized variable '{}'", key.lexeme), key))
            },
            None => Err(LError::from_token(format!("Undefined variable '{}'", key.lexeme), key))
        }

    }
}