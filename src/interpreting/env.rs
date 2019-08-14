use std::collections::HashMap;
use crate::errors::LError;
use crate::lexing::Token;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub struct Env<T> where T : Clone {
    pub vars: HashMap<String, T>,
    outer: Option<Rc<RefCell<Env<T>>>>
}

impl<T> Env<T> where T : Clone {

    pub fn new(outer: Option<Rc<RefCell<Env<T>>>>) -> Env<T> {
        Env { vars: HashMap::new() , outer }
    }

    // Hashmap::insert():
    // If the map did not have this key present, None is returned.
    // If the map did have this key present, the value is updated, and the old value is returned

    // Allows redefinition without warning
    pub fn define(&mut self, key: String, value: T) {
        self.vars.insert(key, value);
    }

    // Assignment validity is checked by static analysis
    pub fn update(&mut self, key: &str, value: T) {
        match self.vars.get_mut(key) {
            Some(x) => *x = value,
            None => match &self.outer {
                Some(env) => env.borrow_mut().update(key, value),
                None => panic!("Failed in env update, var not found in any scope")
            }
        };
    }

    // Climb the cactus stack to resolve bindings
    pub fn resolve(&self, key: &Token) -> Result<T, LError> {
        match self.vars.get(&key.lexeme) {
            Some(x) => Ok(x.clone()),
            None => match &self.outer {
                Some(env) => env.borrow().resolve(key),
                None => Err(LError::from_token(format!("Undefined variable '{}'", key.lexeme), key))
            }
        }
    }

    pub fn resolve_str(&self, key: &str) -> Option<&T> {
        self.vars.get(key)
    }
}
















