use crate::lexing::Token;
use std::collections::HashMap;
use crate::types::LType;
use crate::types::l_types::TypeName;

#[derive(Debug, Clone, PartialEq)]
pub struct Struct {
    name: TypeName,
    fields: HashMap<String, LType>
}

impl Struct {
    pub fn new(name: TypeName, fields: HashMap<String, LType>) -> Struct {
        Struct {
            name,
            fields
        }
    }
}