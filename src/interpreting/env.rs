use std::collections::HashMap;
use crate::errors::LError;
use crate::lexing::Token;
use crate::interpreting::{Function, LObject};
use crate::interpreting::l_object::LObject::{LFunction, LUnit};
use crate::parsing::stmt::Stmt::FnStmt;
use crate::types::l_types::LType::TUnit;

#[derive(Debug, Clone, PartialEq)]
pub struct Env<T> where T : Clone {
    vars: HashMap<String, T>,
    outer: Box<Option<Env<T>>>
}

impl<T> Env<T> where T : Clone {

    pub fn new(outer: Option<Env<T>>) -> Env<T> {
        Env { vars: HashMap::new() , outer: Box::new(outer) }
    }

    // Hashmap::insert():
    // If the map did not have this key present, None is returned.
    // If the map did have this key present, the value is updated, and the old value is returned

    // Allows redefinition without warning
    pub fn define(&mut self, key: String, value: T) {
        self.vars.insert(key, value);
    }

    // Assignment validity is checked by static analysis
    pub fn update(&mut self, key: &String, value: T) {
        match self.vars.get(key) {
            Some(x) => { self.vars.insert(key.to_string(), value); },
            None => match *self.outer {
                Some(ref mut env) => env.update(key, value),
                None => panic!("Failed in env update, var not found in any scope")
            }
        };
    }

    pub fn enclosing(&self) -> &Option<Env<T>> {
        &*self.outer
    }

    // Climb the cactus stack to resolve bindings
    pub fn resolve(&self, key: &Token) -> Result<T, LError> {
        match self.vars.get(&key.lexeme) {
            Some(x) => Ok(x.clone()),
            None => match *self.outer {
                Some(ref env) => env.resolve(key),
                None => Err(LError::from_token(format!("Undefined variable '{}'", key.lexeme), key))
            }
        }
    }

    pub fn resolve_str(&self, key: &str) -> Option<&T> {
        self.vars.get(key)
    }
}

//impl Env<Option<LObject>>  {
//    pub fn resolve_obj(&self, key: &Token) -> Result<Option<LObject>, LError> {
//        match self.vars.get(&key.lexeme) {
//            Some(x) => if let Some(LFunction(_)) = x { // if is function, return the one higher up
//                match *self.outer {
//                    Some(ref env) => {
//                        env.resolve(key)
//                    },
//                    None => Ok(x.clone())
//                }
//            } else {
//                Ok(x.clone())
//            },
//            None => match *self.outer {
//                Some(ref env) => env.resolve_obj(key),
//                None => Err(LError::from_token(format!("Undefined variable '{}'", key.lexeme), key))
//            }
//        }
//    }
//}





















