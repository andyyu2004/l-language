use crate::lexing::Token;
use std::collections::HashMap;
use crate::types::LType;

#[derive(Debug, Clone, PartialEq)]
pub struct Struct {
    name: Token,
    fields: HashMap<String, LType>
}

impl Struct {
    pub fn new(name: Token, fields: HashMap<String, LType>) -> Struct {
        Struct {
            name,
            fields
        }
    }
}