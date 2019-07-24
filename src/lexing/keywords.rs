use std::collections::HashMap;
use crate::lexing::TokenType;

pub struct Keywords {

}

macro_rules! hashmap {
    ($( $key: expr => $val: expr ),*) => {{
         let mut map = ::std::collections::HashMap::new();
         $( map.insert($key, $val); )*
         map
    }}
}

impl Keywords {
    pub fn map() -> HashMap<&'static str, TokenType> {
        hashmap![
            "var" => TokenType::Var,
            "let" => TokenType::Let,
            "print" => TokenType::Print
        ]
    }
}
