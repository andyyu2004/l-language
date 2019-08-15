use std::collections::HashMap;

use crate::lexing::TokenType;

pub struct Keywords {}

#[macro_export] macro_rules! hashmap {
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
            "print" => TokenType::Print,
            "true" => TokenType::True,
            "false" => TokenType::False,
            "fn" => TokenType::Fn,
            "return" => TokenType::Return,
            "if" => TokenType::If,
            "else" => TokenType::Else,
            "record" => TokenType::Record,
            "type" => TokenType::Type,
            "while" => TokenType::While,
            "struct" => TokenType::Struct,
            "data" => TokenType::Data,
            "match" => TokenType::Match,
            "class" => TokenType::Class
        ]
    }
}
