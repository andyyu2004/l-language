use std::collections::HashMap;

pub struct Env {
    vars: HashMap<&'static str, Option<f64>>
}

impl Env {
    pub fn new() -> Env {
        Env { vars: HashMap::new() }
    }

    // Hashmap::insert():
    // If the map did not have this key present, None is returned.
    // If the map did have this key present, the value is updated, and the old value is returned

    // Allows redefinition without warning
    pub fn define(&mut self, key: &'static str, value: Option<f64>) {
        self.vars.insert(key, value);
    }

    pub fn resolve(&self, key: &'static str) -> Result<f64, String> {
        match self.vars.get(key) {
            Some(&x) => match x {
                Some(f) => Ok(f),
                None => Err(format!("Uninitialized variable {}", key))
            },
            None => Err(format!("Undefined variable {}", key))
        }

    }
}